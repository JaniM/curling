use std::ops::Range;

use chumsky::{prelude::*, Stream};

type Span = Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum JsonKey {
    Int(i32),
    String(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct JsonPath(pub Vec<JsonKey>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HttpMethod {
    Get,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AstStringPiece {
    Plain(String),
    Substitution(JsonPath),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AstString(pub Vec<AstStringPiece>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Def,
    Context,
    Capture,
    Ident(String),
    String(AstString),
    HttpMethod(HttpMethod),
    JsonPath(JsonPath),
    OpenBlock,
    CloseBlock,
    Assign,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DefAst {
    Operation(HttpMethod, AstString),
    Capture(String, Vec<CtxAst>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtxAst {
    Assign(String, AstString),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Define(String, Vec<DefAst>),
    Context(Option<String>, Vec<CtxAst>),
}

fn raw_string() -> impl Parser<char, String, Error = Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );
    filter(|c| *c != '\\' && *c != '"')
        .or(escape)
        .repeated()
        .delimited_by('"', '"')
        .collect::<String>()
        .labelled("string")
}

fn string_with_substitutions() -> impl Parser<char, AstString, Error = Simple<char>> {
    let subst = json_path()
        .delimited_by('{', '}')
        .map(AstStringPiece::Substitution);
    let other = filter(|c| *c != '"' && *c != '{')
        .repeated()
        .at_least(1)
        .collect()
        .map(AstStringPiece::Plain);

    subst
        .or(other)
        .repeated()
        .delimited_by('"', '"')
        .map(AstString)
}

fn json_path() -> impl Parser<char, JsonPath, Error = Simple<char>> {
    let key = raw_string().or(text::ident()).map(JsonKey::String);
    let int = text::int(10).map(|x: String| JsonKey::Int(x.parse().unwrap()));
    key.or(int)
        .separated_by(just('.'))
        .at_least(1)
        .map(JsonPath)
        .padded()
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let ident = text::ident().map(|x: String| match &*x {
        "def" => Token::Def,
        "context" => Token::Context,
        "capture" => Token::Capture,
        "GET" => Token::HttpMethod(HttpMethod::Get),
        _ => Token::Ident(x),
    });
    let blocks = just('{')
        .to(Token::OpenBlock)
        .or(just('}').to(Token::CloseBlock));
    let string = string_with_substitutions().map(Token::String);
    let path = just('#').ignore_then(json_path()).map(Token::JsonPath);

    let ops = just('=').to(Token::Assign);

    let token = ident
        .or(string)
        .or(path)
        .or(blocks)
        .or(ops)
        .recover_with(skip_then_retry_until([]))
        .map_with_span(|v, s| (v, s))
        .padded();

    token.repeated().then_ignore(end())
}

fn symbol() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    filter_map(|span, x| match x {
        Token::Ident(x) => Ok(x),
        _ => Err(Simple::custom(span, "Expected an identifier")),
    })
}

fn method() -> impl Parser<Token, HttpMethod, Error = Simple<Token>> + Clone {
    filter_map(|span, x| match x {
        Token::HttpMethod(x) => Ok(x),
        _ => Err(Simple::custom(span, "Expected an http method")),
    })
}

fn string() -> impl Parser<Token, AstString, Error = Simple<Token>> + Clone {
    filter_map(|span, x| match x {
        Token::String(x) => Ok(x),
        _ => Err(Simple::custom(span, "Expected a string")),
    })
}

fn block<O, E: chumsky::Error<Token>>(
    p: impl Parser<Token, O, Error = E> + Clone,
) -> impl Parser<Token, O, Error = E> + Clone {
    p.delimited_by(Token::OpenBlock, Token::CloseBlock)
}

fn parser() -> impl Parser<Token, Vec<Ast>, Error = Simple<Token>> {
    let assign = symbol()
        .then_ignore(just(Token::Assign))
        .then(string())
        .map(|(n, s)| CtxAst::Assign(n, s));

    let req = method()
        .then(string())
        .map(|(m, s)| DefAst::Operation(m, s));

    let capture = just(Token::Capture)
        .ignore_then(symbol())
        .then(block(assign.clone().repeated()))
        .map(|(n, s)| DefAst::Capture(n, s));

    let op = req.or(capture);

    let define = just(Token::Def)
        .ignore_then(symbol())
        .then(block(op.repeated()))
        .map(|(n, o)| Ast::Define(n, o));

    let context = just(Token::Context)
        .ignore_then(symbol().or_not())
        .then(block(assign.repeated()))
        .map(|(n, b)| Ast::Context(n, b));

    define
        .or(context)
        .recover_with(skip_then_retry_until([]))
        .repeated()
        .then_ignore(end())
}

pub fn parse(source: &str) -> Builder {
    let len = source.chars().count();
    let tokens = lexer().parse(source).unwrap();
    let ast = parser()
        .parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
        .unwrap();
    Builder::build(ast)
}

pub fn parse_arg(arg: &str) -> Result<(Vec<String>, String), Vec<Simple<char>>> {
    let contexts = text::ident().separated_by(just('+'));
    let def = text::ident();
    let parser = contexts
        .then_ignore(just(':'))
        .or_not()
        .map(|x| x.unwrap_or_else(Vec::new))
        .then(def);

    parser.parse(arg)
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct CaptureGroup {
    pub name: String,
    pub values: Vec<(String, AstString)>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Definition {
    pub name: String,
    pub call: Option<(HttpMethod, AstString)>,
    pub capture: Vec<CaptureGroup>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Context {
    pub name: Option<String>,
    pub values: Vec<(String, AstString)>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Builder {
    pub defs: Vec<Definition>,
    pub ctxs: Vec<Context>,
}

impl Builder {
    fn build(ast: Vec<Ast>) -> Builder {
        let mut builder = Builder::default();
        for node in ast {
            builder.one(node);
        }
        builder
    }

    fn one(&mut self, ast: Ast) {
        match ast {
            Ast::Define(name, ops) => {
                let mut def = Definition {
                    name,
                    ..Definition::default()
                };

                for op in ops {
                    self.build_def(&mut def, op);
                }

                self.defs.push(def);
            }
            Ast::Context(name, ops) => {
                let mut ctx = Context {
                    name,
                    ..Context::default()
                };

                for op in ops {
                    self.build_ctx(&mut ctx, op);
                }

                self.ctxs.push(ctx);
            }
        }
    }

    fn build_def(&mut self, def: &mut Definition, op: DefAst) {
        match op {
            DefAst::Operation(method, url) => {
                def.call = Some((method, url));
            }
            DefAst::Capture(name, ops) => {
                let mut ctx = CaptureGroup {
                    name,
                    ..CaptureGroup::default()
                };

                for op in ops {
                    self.build_capture(&mut ctx, op);
                }
                def.capture.push(ctx);
            }
        }
    }

    fn build_ctx(&mut self, ctx: &mut Context, op: CtxAst) {
        match op {
            CtxAst::Assign(name, value) => {
                ctx.values.push((name, value));
            }
        }
    }

    fn build_capture(&mut self, ctx: &mut CaptureGroup, op: CtxAst) {
        match op {
            CtxAst::Assign(name, value) => {
                ctx.values.push((name, value));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn get() {
        let s = r#"
        def google {
            GET "{url}/foo"
            capture foo {
                bar = ""
            }
        }
        "#;
        let len = s.chars().count();
        let tokens = lexer().parse(s).unwrap();
        let ast = parser()
            .parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
            .unwrap();

        assert_eq!(
            ast,
            vec![Ast::Define(
                "google".to_string(),
                vec![
                    DefAst::Operation(
                        HttpMethod::Get,
                        AstString(vec![
                            AstStringPiece::Substitution(JsonPath(vec![JsonKey::String(
                                "url".to_string()
                            )])),
                            AstStringPiece::Plain("/foo".to_string())
                        ])
                    ),
                    DefAst::Capture(
                        "foo".to_string(),
                        vec![CtxAst::Assign("bar".to_string(), AstString(vec![]))]
                    )
                ]
            )]
        );

        let builder = Builder::build(ast);

        assert_eq!(
            builder.defs,
            vec![Definition {
                name: "google".to_string(),
                call: Some((
                    HttpMethod::Get,
                    AstString(vec![
                        AstStringPiece::Substitution(JsonPath(vec![JsonKey::String(
                            "url".to_string()
                        )])),
                        AstStringPiece::Plain("/foo".to_string())
                    ])
                )),
                capture: vec![CaptureGroup {
                    name: "foo".to_string(),
                    values: vec![("bar".to_string(), AstString(vec![]))]
                }]
            }]
        )
    }
}
