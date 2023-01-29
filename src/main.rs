mod parser;
use std::{
    collections::{HashMap, HashSet},
    env::args,
    io::{Read, Seek},
};

use parser::{
    parse, parse_arg, AstString, AstStringPiece, Context, HttpMethod, JsonKey, JsonPath, JsonValue,
};

use crate::parser::ValueKind;

/// Get a value from a serde json object using our [JsonPath] type.
/// Returns None if the path is invalid.
/// TODO: Eeturn an error with path informatiob.
fn get_value<'a>(mut obj: &'a serde_json::Value, path: &JsonPath) -> Option<&'a serde_json::Value> {
    for key in &path.0 {
        match key {
            JsonKey::Int(v) => {
                obj = obj.get(*v as usize)?;
            }
            JsonKey::String(v) => {
                obj = obj.get(v)?;
            }
        }
    }
    Some(obj)
}

/// Fill in string substitutilons using a context object.
/// Returns None if some value isn't found.
/// TODO: Eeturn an error with path informatiob.
fn construct_string(context: &serde_json::Value, seq: &AstString) -> Option<String> {
    let mut result = String::new();
    for item in &seq.0 {
        match item {
            AstStringPiece::Plain(v) => {
                result.push_str(v);
            }
            AstStringPiece::Substitution(path) => {
                result.push_str(get_value(context, path)?.as_str()?);
            }
        }
    }
    Some(result)
}

/// Constructs a context object from [Context] blocks.
/// Only enabled blocks are included.
/// Later context blocks are favored if multiple contain the same key.
/// TODO: This fails silently if some value can't be resolved. Return an error instead.
/// FIXME: This fails to resolve a string again if the substitutions change on a later iteration.
fn construct_context(ctxs: &[Context], enabled: HashSet<String>) -> Option<serde_json::Value> {
    let mut map = serde_json::Map::new();
    let mut priority = HashMap::new();

    map.insert("_".to_string(), serde_json::Map::new().into());

    loop {
        let mut added = 0;
        for (idx, ctx) in ctxs.iter().enumerate() {
            if let Some(name) = &ctx.name {
                if !enabled.contains(name) {
                    continue;
                }
                if !map.contains_key(name) {
                    map.insert(name.to_string(), serde_json::Map::new().into());
                }
            }

            for (name, value) in &ctx.values {
                let json = serde_json::Value::from(map.clone());
                let Some(v) = construct_string(&json, value) else { continue; };
                let v: serde_json::Value = v.into();

                if priority.get(name).map_or(false, |x| *x > idx) {
                    continue;
                }
                priority.insert(name, idx);

                if let Some(name) = &ctx.name {
                    let local = map.get_mut(name).unwrap().as_object_mut().unwrap();
                    local.insert(name.clone(), v.clone());
                }
                let global = map.get_mut("_").unwrap().as_object_mut().unwrap();
                if global.get(name) != Some(&v) {
                    global.insert(name.clone(), v.clone());
                    added += 1;
                }
            }
        }

        if added == 0 {
            break;
        }
    }

    Some(map.into())
}

/// Converts a [JsonValue] object to a serde json object, filling in string substitutilons.
/// TODO: Currently panics if a substitution fails. Return an error instead.
fn value_to_json(context: &serde_json::Value, value: &JsonValue) -> serde_json::Value {
    match value {
        JsonValue::String(s) => construct_string(context, s).unwrap().into(),
        JsonValue::Object(obj) => {
            let mut map = serde_json::Map::new();
            for (key, value) in obj {
                let key = construct_string(context, key).unwrap();
                let value = value_to_json(context, value);
                map.insert(key, value);
            }
            map.into()
        }
    }
}

/// Loads definitions from a configuration file.
/// TODO: Make the path configurable.
/// TODO: Give useful errors if parsing fails.
fn load_definitions() -> std::io::Result<parser::Builder> {
    let mut file = std::fs::File::open("def.txt")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    Ok(parse(&content))
}

/// A state context ovject that updates the state file on drop.
struct StateCtx {
    file: std::fs::File,
    state: serde_json::Value,
}

impl StateCtx {
    /// Load the state file and parse it to a json object.
    /// The file is created if it doesn't exist already.
    /// This file is generated automatically, so parsing should never fail. Nevertheless:
    /// TODO: Give a useful error if parsing fails.
    fn get_state() -> Result<Self, std::io::Error> {
        let mut file = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open("state.json")
            .unwrap();

        let state: serde_json::Value = {
            let mut c = String::new();
            file.read_to_string(&mut c)?;
            serde_json::from_str(&c).unwrap_or_else(|_| serde_json::Map::new().into())
        };

        Ok(Self { file, state })
    }
}

impl std::ops::Deref for StateCtx {
    type Target = serde_json::Value;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl std::ops::DerefMut for StateCtx {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl Drop for StateCtx {
    fn drop(&mut self) {
        self.file.rewind().unwrap();
        self.file.set_len(0).unwrap();
        serde_json::to_writer(&mut self.file, &self.state).unwrap();
    }
}

/// Performs a call following the rules given by a [Definition] object.
/// TODO: Don't panic on network or parsing errors.
fn perform_call(target: &parser::Definition, context: &serde_json::Value) -> String {
    match &target.call {
        Some((HttpMethod::Get, url)) => {
            let url = construct_string(context, url).unwrap();
            reqwest::blocking::get(url).unwrap().text().unwrap()
        }
        Some((HttpMethod::Post, url)) => {
            let url = construct_string(context, url).unwrap();
            let client = reqwest::blocking::Client::new();
            let mut req = client.post(url);
            match &target.body {
                Some(ValueKind::Json(value)) => {
                    let value = value_to_json(context, value);
                    req = req.json(&value);
                }
                None => {}
            }
            req.send().unwrap().text().unwrap()
        }
        None => unreachable!(),
    }
}

/// Captures values from the json object.
fn perform_capture(
    data: &str,
    context: &mut serde_json::Value,
    target: &parser::Definition,
    state: &mut serde_json::Value,
) {
    let json: serde_json::Value = serde_json::from_str(data).unwrap();
    let c = context.as_object_mut().unwrap();
    c.insert("response".to_string(), json);

    for group in &target.capture {
        println!("Group {}", group.name);
        let s = state.as_object_mut().unwrap();
        if !s.contains_key(&group.name) {
            s.insert(group.name.clone(), serde_json::Map::new().into());
        }

        let local = s.get_mut(&group.name).unwrap().as_object_mut().unwrap();
        for (name, val) in &group.values {
            let v = construct_string(&context, val).unwrap();
            println!("{} = {:?}", name, v);
            local.insert(name.clone(), v.into());
        }
    }
}

fn main() -> std::io::Result<()> {
    let data = load_definitions()?;

    let mut state = StateCtx::get_state()?;
    println!("{:?}", &*state);

    // TODO: Use clap.
    let target = args().skip(1).next().unwrap();
    let (scopes, target) = parse_arg(&target).unwrap();
    let target = data.defs.iter().find(|x| x.name == target).unwrap();

    let mut context = construct_context(&data.ctxs, scopes.into_iter().collect()).unwrap();

    // Insert top-level state objects to the context.
    for item in state.state.as_object().unwrap().iter() {
        context
            .as_object_mut()
            .unwrap()
            .insert(item.0.clone(), item.1.clone());
    }

    let out = perform_call(target, &context);
    println!("{}", out);

    perform_capture(&out, &mut context, target, &mut state);

    Ok(())
}
