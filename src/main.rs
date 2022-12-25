use std::collections::HashMap;
mod larynx;

fn main() {
    let filename = std::env::args().skip(1).next().unwrap_or_else(|| {
        larynx::err_with("larynx expects a filename. example usage:\nlarynx file.lx")
    });

    let contents = std::fs::read_to_string(&filename).unwrap_or_else(|_| {
        larynx::err_with(&format!(
        "there was a problem opening {filename}. are you sure it exists at that exact location?"
    ))
    });

    let tokens: Vec<larynx::Token> = larynx::lex(&contents);
    let ast: Vec<larynx::Expr> = larynx::parse(tokens);

    let mut variables: HashMap<String, larynx::Value> = HashMap::new();
    for expr in ast {
        println!("evaluating expr: {expr:#?}");
        expr.eval(&mut variables);
    }
}
