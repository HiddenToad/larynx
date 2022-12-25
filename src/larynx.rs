use std::{collections::HashMap, sync::RwLock, fmt::Display};

static LINE_NUM: RwLock<u32> = RwLock::new(0);

pub fn err_with(msg: &str) -> ! {
    eprintln!("larynx error at line {}:\n{msg}", LINE_NUM.read().unwrap());
    std::process::exit(1);
}

fn op_err(op: &str, t: &str) -> ! {
    err_with(&format!("no `{op}` operator exists for type `{t}`!"));
}

fn bin_op_err(op: &str, t1: &str, t2: &str) -> ! {
    err_with(&format!(
        "no `{op}` operator exists between types `{t1}` and `{t2}`!"
    ))
}

fn no_var_err(varname: &str) -> ! {
    err_with(&format!(
        "variable `{varname}` not found! are you sure you declared it?"
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(f64),
    Str(String),
    True,
    False,
    Plus,
    Minus,
    Times,
    Divided,
    By,
    Say,
    Not,
    And,
    Or,
    Equals,
    Nothing,
    Is,
    Ident(String),
    SentinelOp,
    Newline,
}

impl Token {

    pub fn priority(&self) -> u32 {
        match self {
            Token::SentinelOp => 0,
            Token::Say | Token::Is => 1,
            Token::And | Token::Or | Token::Equals => 2,
            Token::Plus | Token::Minus => 3,
            Token::Divided | Token::Times => 4 ,
            Token::Not => 5,
            _ => unreachable!(),
        }
    }

    pub fn to_binop(&self, a: Expr, b: Expr) -> Option<Expr> {
        match self {
            Token::Plus => Some(Expr::BinOp(BinOp::Plus(Box::new(a), Box::new(b)))),
            Token::Minus => Some(Expr::BinOp(BinOp::Minus(Box::new(a), Box::new(b)))),
            Token::Times => Some(Expr::BinOp(BinOp::Times(Box::new(a), Box::new(b)))),
            Token::Divided => Some(Expr::BinOp(BinOp::Div(Box::new(a), Box::new(b)))),
            Token::Is => Some(Expr::BinOp(BinOp::Is(Box::new(a), Box::new(b)))),
            Token::Or => Some(Expr::BinOp(BinOp::Or(Box::new(a), Box::new(b)))),
            Token::And => Some(Expr::BinOp(BinOp::And(Box::new(a), Box::new(b)))),
            Token::Equals => Some(Expr::BinOp(BinOp::Equals(Box::new(a), Box::new(b)))),
            _ => None
        }
    }

    pub fn to_unop(&self, arg: Expr) -> Option<Expr>{
        match self{
            Token::Say => Some(Expr::UnOp(UnOp::Say(Box::new(arg)))),
            Token::Not => Some(Expr::UnOp(UnOp::Not(Box::new(arg)))),
            _ => None
        }
    }
}

impl Display for Token{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!("{self:?}").to_lowercase()
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Text(String),
    Truth(bool),
    Nothing,
}

impl Value {
    fn type_str(&self) -> &str {
        match self {
            Value::Number(_) => "number",
            Value::Text(_) => "text",
            Value::Truth(_) => "truth",
            Value::Nothing => "nothing",
        }
    }

    fn to_string(&self) -> String {
        match self {
            Value::Number(n) => {
                if n.floor() == *n {
                    (*n as i64).to_string()
                } else {
                    n.to_string()
                }
            }
            Value::Text(t) => t.clone(),
            Value::Truth(b) => b.to_string(),
            Value::Nothing => "Nothing".into(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Not(Box<Expr>),
    Say(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Is(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>)
}

#[derive(Clone, Debug)]
pub enum Expr {
    Value(Value),
    Ident(String),
    UnOp(UnOp),
    BinOp(BinOp),
    Nothing,
}


impl Expr {

    pub fn eval(&self, variables: &mut HashMap<String, Value>) -> Value {
        match self {
            Expr::Value(val) => val.clone(),
            Expr::UnOp(op) => match op {
                UnOp::Not(arg) => {
                    let arg = arg.eval(variables);
                    if let Value::Truth(b) = arg {
                        Value::Truth(!b)
                    } else {
                        op_err("not", arg.type_str());
                    }
                }
                UnOp::Say(arg) => {
                    println!("{}", arg.eval(variables).to_string());
                    Value::Nothing
                }
            },
            Expr::BinOp(op) => {
                match op {
                    BinOp::Plus(arg1, arg2) =>{ 
                        let arg1 = arg1.eval(variables);
                        match arg1 {
                        Value::Number(n1) => {
                            let arg2 = arg2.eval(variables);
                            if let Value::Number(n2) = arg2 {
                                Value::Number(n1 + n2)
                            } else {
                                bin_op_err("plus", "number", arg2.type_str())
                            }
                        }
                        Value::Text(s1) => {
                            let arg2 = arg2.eval(variables);
                            if let Value::Text(s2) = arg2 {
                                Value::Text(s1 + &s2)
                            } else {
                                bin_op_err("plus", "text", arg2.type_str())
                            }
                        }
                        _ => op_err("plus", arg1.type_str()),
                    }},

                    BinOp::Minus(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);
                        if let Value::Number(n1) = arg1{
                            if let Value::Number(n2) = arg2 {
                                Value::Number(n1 - n2)
                            } else {
                                bin_op_err("minus", "number", arg2.type_str())
                            }
                        } else {
                            op_err("plus", arg2.type_str())
                        }
                    }

                    BinOp::Times(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);
                        if let Value::Number(n1) = arg1 {
                            if let Value::Number(n2) = arg2 {
                                Value::Number(n1 * n2)
                            } else {
                                bin_op_err("times", "number", arg2.type_str())
                            }
                        } else {
                            op_err("times", arg1.type_str())
                        }
                    }

                    BinOp::Div(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);
                        if let Value::Number(n1) = arg1 {
                            if let Value::Number(n2) = arg2 {
                                if n2 != 0. {
                                    Value::Number(n1 / n2)
                                } else {
                                    err_with("cannot divide by zero!")
                                }
                            } else {
                                bin_op_err("divided by", "number", arg2.type_str())
                            }
                        } else {
                            op_err("divided by", arg1.type_str())
                        }
                    }

                    BinOp::Is(name, arg2) => {
                        if let Expr::Ident(name) = &**name{
                            let arg2 = arg2.eval(variables);
                            variables.insert(name.clone(), arg2.clone());
                        }
                        Value::Nothing
                    }

                    BinOp::And(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);

                        if let Value::Truth(truth1) = arg1{
                            if let Value::Truth(truth2) = arg2{
                                Value::Truth(truth1 && truth2)
                            } else {
                                bin_op_err("and", "truth", arg2.type_str())
                            }
                        } else {
                            op_err("and", arg1.type_str());
                        }
                    },

                    BinOp::Or(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);

                        if let Value::Truth(truth1) = arg1{
                            if let Value::Truth(truth2) = arg2{
                                Value::Truth(truth1 || truth2)
                            } else {
                                bin_op_err("or", "truth", arg2.type_str())
                            }
                        } else {
                            op_err("or", arg1.type_str());
                        }
                    },

                    
                    BinOp::Equals(arg1, arg2) => {
                        let arg1 = arg1.eval(variables);
                        let arg2 = arg2.eval(variables);

                        if arg1.type_str() == arg2.type_str(){
                            Value::Truth(arg1 == arg2)
                        } else {
                            bin_op_err("equals", arg1.type_str(), arg2.type_str())
                        }
                    }

                }
            }
            Expr::Ident(s) => variables.get(s).unwrap_or_else(|| no_var_err(&s)).clone(),
            Expr::Nothing => Value::Nothing,
        }
    }
}

// prepare a line for any text literals in it. 
// replace all spaces with U+00AD as a placeholder
// so that spaces in strings aren't parsed as token separators.
pub fn prep_text(line: &str) -> String{
    if !line.contains('"'){
        return line.to_owned();
    }

    let chars = line.chars();
    let mut in_quote = false;
    let mut result = String::new();

    for c in chars{
        if c == '"'{
            in_quote = !in_quote;
        }

        if in_quote && c == ' '{
            result.push('­');
        } else {
            result.push(c);
        }
    }
    if in_quote{
        err_with("invalid text literal! make sure you only have 2 quotes wrapping the text.");
    }
    result
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    for line in input.lines() {

        let line = prep_text(line);
        let line = line.split_ascii_whitespace();
        for word in line {
            tokens.push(match word {
                "plus" => Token::Plus,
                "minus" => Token::Minus,
                "times" => Token::Times,
                "divided" => Token::Divided,
                "by" => Token::By,
                "say" => Token::Say,
                "nothing" => Token::Nothing,
                "true" => Token::True,
                "false" => Token::False,
                "is" => Token::Is,
                "not" => Token::Not,
                "and" => Token::And,
                "or" => Token::Or,
                "equals" => Token::Equals,
                _ if word.starts_with('"') && word.ends_with('"') => {
                    let mut trimmed = word.to_string().replace("­", " ");
                    trimmed.remove(0);
                    trimmed.pop();
                    if trimmed.contains('"') {
                        err_with("invalid text literal!");
                    } else {
                        Token::Str(trimmed)
                    }
                }
                _ if word.parse::<f64>().is_ok() => Token::Num(word.parse().unwrap()),
                _ => Token::Ident(word.to_owned()),
            })
        }
        tokens.push(Token::Newline);
        *LINE_NUM.write().unwrap() += 1;
    }
    tokens
}

fn get_bin(operands: &mut Vec<Expr>) -> (Expr, Expr) {
    let b = operands.pop().unwrap_or_else(|| {
        err_with("expected 2 expressional arguments to operator!")
    });
    let a = operands.pop().unwrap_or_else(|| {
        err_with("expected 2 expressional arguments to operator!")
    });
    (a, b)
}

fn get_un(operands: &mut Vec<Expr>) -> Expr{
    operands.pop().unwrap_or_else(|| {
        err_with("expected an expressional argument to operator!")
    })
}

fn make_tree(last: &Token, operands: &mut Vec<Expr>,){
        match last {
            Token::Plus | Token::Minus | Token::Times | Token::Divided | Token::Is | Token::And | Token::Or | Token::Equals => {
                let (a, b) = get_bin(operands);
                operands.push(last.to_binop(a, b).unwrap());
            }
            Token::Say | Token::Not => {
                let arg = get_un(operands);
                operands.push(last.to_unop(arg).unwrap())
            },
            _ => unreachable!()
        }
}

pub fn parse(input: Vec<Token>) -> Vec<Expr> {
    println!("{input:#?}");
    let mut operands = vec![];
    let mut output = vec![];
    let mut iter = input.iter().peekable();
    let mut operators: Vec<Token> = vec![Token::SentinelOp];
    while let Some(token) = iter.next() {
        println!("operators: {operators:#?}\n\noperands: {operands:#?}\n\noutput: {output:#?}\n\n");
        match token {
            Token::Num(n) => {
                operands.push(Expr::Value(Value::Number(*n)));
                
            }
            Token::Str(s) => {
                operands.push(Expr::Value(Value::Text(s.clone())));
                
            }
            Token::Ident(ident) => {
                operands.push(Expr::Ident(ident.clone()));
            }
            Token::True | Token::False => {
                operands.push(Expr::Value(Value::Truth(*token == Token::True)));
                
            }
            Token::Nothing => {
                operands.push(Expr::Nothing);
                
            }
            Token::Plus
            | Token::Minus
            | Token::Times
            | Token::Divided
            | Token::Is
            | Token::And
            | Token::Or
            | Token::Equals
            | Token::Say
            | Token::Not => {
                if *token == Token::Divided {
                    if *iter.next().unwrap_or_else(||{
                        err_with("expected `by` after `divided`!");
                    }) != Token::By {
                        err_with("expected `by` after `divided`!");
                    }
                }
                let last = operators.last().unwrap();
                if token.priority() <= last.priority() {
                    make_tree(last, &mut operands);
                    operators.pop();
                } 
                operators.push(token.clone());
 
            }
            Token::By => {},
            Token::SentinelOp => unreachable!(),
            Token::Newline => {
                if let Some(_) = operators.last(){
                    while iter.peek() == Some(&&Token::Newline){
                        *LINE_NUM.write().unwrap() += 1;
                        iter.next();
                    }
                    while let Some(op) = operators.pop(){
                        if op == Token::SentinelOp{
                            break;
                        }
                        make_tree(&op, &mut operands)
                    } 
                    operators = vec![Token::SentinelOp];
                    output.push(operands.pop().unwrap());
                }
            }
        }
    }

    output
}
