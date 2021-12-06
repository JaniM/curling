mod parser;
use std::{collections::{HashSet, HashMap}, env::args, io::{Read, Seek}};

use parser::{parse, parse_arg, AstString, AstStringPiece, Context, HttpMethod, JsonKey, JsonPath, JsonValue};

use crate::parser::ValueKind;

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

fn construct_context(ctxs: &[Context], enabled: HashSet<String>) -> Option<serde_json::Value> {
    let mut map = serde_json::Map::new();
    let mut seen = HashSet::new();
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
                if seen.contains(&(&ctx.name, name)) {
                    continue;
                }

                let json = serde_json::Value::from(map.clone());
                if let Some(v) = construct_string(&json, value) {
                    seen.insert((&ctx.name, name));
                    if priority.get(name).map(|x| *x > idx).unwrap_or(false) {
                        continue;
                    }
                    priority.insert(name, idx);

                    if let Some(name) = &ctx.name {
                        let local = map.get_mut(name).unwrap().as_object_mut().unwrap();
                        local.insert(name.clone(), v.clone().into());
                    }
                    let global = map.get_mut("_").unwrap().as_object_mut().unwrap();
                    global.insert(name.clone(), v.clone().into());
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

fn main() -> std::io::Result<()> {
    let data = {
        let mut file = std::fs::File::open("def.txt")?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        parse(&content)
    };

    let mut state_file = std::fs::OpenOptions::new().read(true).write(true).create(true).open("state.json").unwrap();
    let mut state: serde_json::Value = {
        let mut c = String::new();
        state_file.read_to_string(&mut c)?;
        serde_json::from_str(&c).unwrap_or_else(|_| serde_json::Map::new().into())
    };
    println!("{:?}", state);

    let target = args().skip(1).next().unwrap();
    let (scopes, target) = parse_arg(&target).unwrap();
    let target = data.defs.iter().find(|x| x.name == target).unwrap();

    let mut context = construct_context(&data.ctxs, scopes.into_iter().collect()).unwrap();

    for item in state.as_object().unwrap().iter() {
        context.as_object_mut().unwrap().insert(item.0.clone(), item.1.clone());
    }

    let out = match &target.call {
        Some((HttpMethod::Get, url)) => {
            let url = construct_string(&context, url).unwrap();
            reqwest::blocking::get(url).unwrap().text().unwrap()
        }
        Some((HttpMethod::Post, url)) => {
            let url = construct_string(&context, url).unwrap();
            let client = reqwest::blocking::Client::new();
            let mut req = client.post(url);
            match &target.body {
                Some(ValueKind::Json(value)) => {
                    let value = value_to_json(&context, value);
                    req = req.json(&value);
                }
                None => {}
            }
            req.send().unwrap().text().unwrap()
        }
        None => unreachable!(),
    };

    println!("{}", out);
    let json: serde_json::Value = serde_json::from_str(&out).unwrap();
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

    state_file.rewind().unwrap();
    state_file.set_len(0).unwrap();
    serde_json::to_writer(&mut state_file, &state).unwrap();

    Ok(())
}
