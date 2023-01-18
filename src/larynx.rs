use std::{collections::HashMap, fmt::Display, sync::RwLock};

use crate::{DEBUG_BLOCK_PARSING, DEBUG_LEXER, DEBUG_OPERATOR_STACK};

static LINE_NUM: RwLock<u32> = RwLock::new(1);

pub fn err(msg: &str) -> ! {
    eprintln!("larynx error at line {}:\n{msg}", LINE_NUM.read().unwrap());
    std::process::exit(1);
}

fn op_err(op: &str, t: &str) -> ! {
    err(&format!("no `{op}` operator exists for type `{t}`!"));
}

fn bin_op_err(op: &str, t1: &str, t2: &str) -> ! {
    err(&format!(
        "no `{op}` operator exists between types `{t1}` and `{t2}`!"
    ))
}

fn no_var_err(varname: &str) -> ! {
    err(&format!(
        "variable `{varname}` not found! are you sure you declared it and didn't delete it?"
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
    Remainder,
    By,
    Say,
    Not,
    And,
    Or,
    Equals,
    Nothing,
    Is,
    If,
    Then,
    While,
    Do,
    End,
    Else,
    Delete,
    For,
    Every,
    In,
    From,
    To,
    Stepping,

    Ident(String),
    SentinelOp,
    Newline,
}

impl Token {
    pub fn priority(&self) -> u32 {
        match self {
            Token::SentinelOp => 0,
            Token::Say | Token::Is | Token::Not | Token::Delete => 1,
            Token::And | Token::Or => 2,
            Token::Equals => 3,
            Token::Plus | Token::Minus => 4,
            Token::Divided | Token::Times | Token::Remainder => 5,
            _ => unreachable!(),
        }
    }

    pub fn to_binop(&self, a: Expr, b: Expr) -> Option<Expr> {
        let a = Box::new(a);
        let b = Box::new(b);
        match self {
            Token::Plus => Some(Expr::BinOp(BinOp::Plus(a, b))),
            Token::Minus => Some(Expr::BinOp(BinOp::Minus(a, b))),
            Token::Times => Some(Expr::BinOp(BinOp::Times(a, b))),
            Token::Divided => Some(Expr::BinOp(BinOp::Div(a, b))),
            Token::Is => Some(Expr::BinOp(BinOp::Is(a, b))),
            Token::Or => Some(Expr::BinOp(BinOp::Or(a, b))),
            Token::And => Some(Expr::BinOp(BinOp::And(a, b))),
            Token::Equals => Some(Expr::BinOp(BinOp::Equals(a, b))),
            Token::Remainder => Some(Expr::BinOp(BinOp::Remainder(a, b))),
            _ => None,
        }
    }

    pub fn to_unop(&self, arg: Expr) -> Option<Expr> {
        match self {
            Token::Say => Some(Expr::UnOp(UnOp::Say(Box::new(arg)))),
            Token::Not => Some(Expr::UnOp(UnOp::Not(Box::new(arg)))),
            Token::Delete => Some(Expr::UnOp(UnOp::Delete(Box::new(arg)))),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").to_lowercase())
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
    Delete(Box<Expr>),
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
    Equals(Box<Expr>, Box<Expr>),
    Remainder(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum IterDirection {
    Forwards,
    Backwards,
}

#[derive(Clone, Debug)]
pub enum ForBlock {
    Iter(Box<Expr>, Box<Expr>, Box<Expr>, usize, IterDirection), //Iter(Ident, Collection, Block, Step, Direction)
    Range(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>), //Iter(Ident, Start, End, Block, Step, Direction)
}

#[derive(Clone, Debug, Default)]
pub enum Expr {
    Value(Value),
    Ident(String),
    UnOp(UnOp),
    BinOp(BinOp),
    Block(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>), //If(Condition, Block, Option<Elseblock>)
    While(Box<Expr>, Box<Expr>),                 //While(Condition, Block)
    For(ForBlock),
    #[default]
    Nothing,
}

impl Expr {
    pub fn eval(&self, variables: &mut HashMap<String, Value>) -> Value {
        //println!("eval on {self:?}");
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
                UnOp::Delete(arg) => {
                    if let Expr::Ident(s) = &**arg {
                        variables.remove(s).unwrap_or_else(|| {
                            err(&format!(
                                "cannot delete variable {s} because it doesn't exist."
                            ))
                        });
                    } else {
                        unreachable!()
                    }

                    Value::Nothing
                }
            },
            Expr::BinOp(op) => match op {
                BinOp::Plus(arg1, arg2) => {
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
                    }
                }

                BinOp::Minus(arg1, arg2) => {
                    let arg1 = arg1.eval(variables);
                    let arg2 = arg2.eval(variables);
                    if let Value::Number(n1) = arg1 {
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
                                err("cannot divide by zero!")
                            }
                        } else {
                            bin_op_err("divided by", "number", arg2.type_str())
                        }
                    } else {
                        op_err("divided by", arg1.type_str())
                    }
                }

                BinOp::Remainder(arg1, arg2) => {
                    let arg1 = arg1.eval(variables);
                    let arg2 = arg2.eval(variables);
                    if let Value::Number(n1) = arg1 {
                        if let Value::Number(n2) = arg2 {
                            if n2 != 0. {
                                Value::Number(n1 % n2)
                            } else {
                                err("cannot calculate remainder when divided by zero!")
                            }
                        } else {
                            bin_op_err("remainder", "number", arg2.type_str());
                        }
                    } else {
                        op_err("remainder", arg1.type_str());
                    }
                }

                BinOp::Is(name, arg2) => {
                    if let Expr::Ident(name) = &**name {
                        let arg2 = arg2.eval(variables);
                        variables.insert(name.clone(), arg2.clone());
                    }
                    Value::Nothing
                }

                BinOp::And(arg1, arg2) => {
                    let arg1 = arg1.eval(variables);
                    let arg2 = arg2.eval(variables);

                    if let Value::Truth(truth1) = arg1 {
                        if let Value::Truth(truth2) = arg2 {
                            Value::Truth(truth1 && truth2)
                        } else {
                            bin_op_err("and", "truth", arg2.type_str())
                        }
                    } else {
                        op_err("and", arg1.type_str());
                    }
                }

                BinOp::Or(arg1, arg2) => {
                    let arg1 = arg1.eval(variables);
                    let arg2 = arg2.eval(variables);

                    if let Value::Truth(truth1) = arg1 {
                        if let Value::Truth(truth2) = arg2 {
                            Value::Truth(truth1 || truth2)
                        } else {
                            bin_op_err("or", "truth", arg2.type_str())
                        }
                    } else {
                        op_err("or", arg1.type_str());
                    }
                }

                BinOp::Equals(arg1, arg2) => {
                    let arg1 = arg1.eval(variables);
                    let arg2 = arg2.eval(variables);

                    if arg1.type_str() == arg2.type_str() {
                        Value::Truth(arg1 == arg2)
                    } else {
                        bin_op_err("equals", arg1.type_str(), arg2.type_str())
                    }
                }
            },
            Expr::Block(exprs) => {
                let iter = exprs.iter();
                let mut final_val = Value::Nothing;

                for expr in iter {
                    final_val = expr.eval(variables);
                }
                final_val
            }
            Expr::If(cond, block, elseblock) => {
                if let Value::Truth(t) = cond.eval(variables) {
                    if let Expr::Block(_) = &**block {
                        if t {
                            block.eval(variables)
                        } else {
                            if let Some(block) = elseblock {
                                block.eval(variables)
                            } else {
                                Value::Nothing
                            }
                        }
                    } else {
                        err(&format!(
                            "expression between 'then' and 'end' must be a block, not `{}`",
                            block.eval(variables).type_str()
                        ))
                    }
                } else {
                    err(&format!(
                        "expression between 'if' and 'then' must be a truth, not `{}`",
                        cond.eval(variables).type_str()
                    ))
                }
            }

            Expr::While(cond, block) => {
                if let Value::Truth(t) = cond.eval(variables) {
                    if let Expr::Block(_) = &**block {
                        let mut final_val = Value::Nothing;
                        if t {
                            final_val = block.eval(variables);
                        }
                        while cond.eval(variables) == Value::Truth(true) {
                            final_val = block.eval(variables);
                        }
                        final_val
                    } else {
                        err(&format!(
                            "expression between 'do' and 'end' must be a block, not `{}`",
                            block.eval(variables).type_str()
                        ))
                    }
                } else {
                    err(&format!(
                        "expression between 'while' and 'do' must be a truth, not `{}`",
                        cond.eval(variables).type_str()
                    ))
                }
            }

            Expr::For(ForBlock::Range(ident, start, end, block, step)) => {
                if let Expr::Ident(name) = &**ident {
                    let (start, end) = (start.eval(variables), end.eval(variables));
                    let Value::Number(start) = start else{
                            err("start of for loop must be a number");
                        };
                    let Value::Number(end) = end else{
                            err("end of for loop must be a number");
                        };

                    if start.fract() != 0. {
                        err("start of for loop must be a whole number");
                    }

                    if end.fract() != 0. {
                        err("end of for loop must be a whole number");
                    }

                    let (start, end) = (start as i64, end as i64);
                    variables.insert(name.to_string(), Value::Number(start as f64));

                    let Value::Number(step) = step.eval(variables) else{
                            err("step of for loop must be a number");
                        };

                    if step.fract() != 0. {
                        err("for loop step must be a whole number");
                    }
                    let step = step as usize;
                    match start < end {
                        true => {
                            let mut ret: Value = Value::Nothing;
                            for i in (start..=end).step_by(step) {
                                let var: &mut Value =
                                    variables.get_mut(name).unwrap_or_else(|| {
                                        no_var_err(&name);
                                    });
                                *var = Value::Number(i as f64);
                                ret = block.eval(variables);
                            }
                            ret
                        }
                        false => {
                            let mut ret: Value = Value::Nothing;
                            for i in (end..=start).rev().step_by(step) {
                                let var: &mut Value =
                                    variables.get_mut(name).unwrap_or_else(|| {
                                        no_var_err(&name);
                                    });
                                *var = Value::Number(i as f64);
                                ret = block.eval(variables);
                            }
                            ret
                        }
                    }
                } else {
                    err("end of range must be a number in for loop");
                }
            }

            Expr::For(ForBlock::Iter(ident, collection, block, step, direction)) => {
                unimplemented!()
            }

            Expr::Ident(s) => variables.get(s).unwrap_or_else(|| no_var_err(&s)).clone(),
            Expr::Nothing => Value::Nothing,
        }
    }
}

// prepare a line for any text literals in it.
// replace all spaces with U+00AD as a placeholder
// so that spaces in strings aren't parsed as token separators.
pub fn prep_text(line: &str) -> String {
    if !line.contains('"') {
        return line.to_owned();
    }

    let chars = line.chars();
    let mut in_quote = false;
    let mut result = String::new();

    for c in chars {
        if c == '"' {
            in_quote = !in_quote;
        }

        if in_quote && c == ' ' {
            result.push('­');
        } else {
            result.push(c);
        }
    }
    if in_quote {
        err("invalid text literal! make sure you only have 2 quotes wrapping the text.");
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
                "remainder" => Token::Remainder,
                "say" => Token::Say,
                "nothing" => Token::Nothing,
                "true" => Token::True,
                "false" => Token::False,
                "is" => Token::Is,
                "not" => Token::Not,
                "and" => Token::And,
                "or" => Token::Or,
                "equals" => Token::Equals,
                "if" => Token::If,
                "then" => Token::Then,
                "while" => Token::While,
                "do" => Token::Do,
                "end" => Token::End,
                "else" => Token::Else,
                "delete" => Token::Delete,
                "for" => Token::For,
                "every" => Token::Every,
                "in" => Token::In, //unimplemented, for collections which dont exist
                "from" => Token::From, //for ranges
                "to" => Token::To,
                "stepping" => Token::Stepping,
                _ if word.starts_with('"') && word.ends_with('"') => {
                    let mut trimmed = word.to_string().replace("­", " ");
                    trimmed.remove(0);
                    trimmed.pop();
                    if trimmed.contains('"') {
                        err("invalid text literal!");
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
    *LINE_NUM.write().unwrap() = 0;

    tokens
}

fn get_bin(operands: &mut Vec<Expr>) -> (Expr, Expr) {
    let b = operands
        .pop()
        .unwrap_or_else(|| err("expected 2 expressional arguments to operator!"));
    let a = operands
        .pop()
        .unwrap_or_else(|| err("expected 2 expressional arguments to operator!"));
    (a, b)
}

fn get_un(operands: &mut Vec<Expr>) -> Expr {
    operands
        .pop()
        .unwrap_or_else(|| err("expected an expressional argument to operator!"))
}

fn make_tree(last: &Token, operands: &mut Vec<Expr>) {
    match last {
        Token::Plus
        | Token::Minus
        | Token::Times
        | Token::Divided
        | Token::Is
        | Token::And
        | Token::Or
        | Token::Equals
        | Token::Remainder => {
            let (a, b) = get_bin(operands);
            operands.push(last.to_binop(a, b).unwrap());
        }
        Token::Say | Token::Not | Token::Delete => {
            let arg = get_un(operands);
            operands.push(last.to_unop(arg).unwrap())
        }
        _ => unreachable!(),
    }
}

fn eat<'a>(i: &mut impl Iterator<Item = &'a Token>, val: Token) {
    if *i
        .next()
        .unwrap_or_else(|| err(&("expected token ".to_owned() + &val.to_string())))
        != val
    {
        err(&("expected token ".to_owned() + &val.to_string()))
    }
}

fn eat_block_delimited_by<'a>(
    iter: &mut std::iter::Peekable<impl Iterator<Item = &'a Token>>,
    start: Token,
    end: Token,
) -> Vec<Token> {
    if DEBUG_BLOCK_PARSING {
        println!("eating block with format {start} BLOCK {end}");
    }
    let mut block_tokens = vec![];
    let mut found_block = false;
    let mut depth = 1;

    while iter.peek() == Some(&&Token::Newline) {
        *LINE_NUM.write().unwrap() += 1;
        iter.next();
    }
    while let Some(token) = iter.next() {
        if token == &end {
            depth -= 1;
        } else if token == &start
            || token == &Token::If
            || token == &Token::While
            || token == &Token::Else
            || token == &Token::For
        {
            if token == &Token::Else
                && iter
                    .peek()
                    .unwrap_or_else(|| err("unexpected end of file when parsing block"))
                    == &&Token::If
            {
                continue;
            }
            depth += 1;
        } else if token == &Token::Newline {
            *LINE_NUM.write().unwrap() += 1;
        }

        if depth == 0 {
            found_block = true;
            break;
        }
        if DEBUG_BLOCK_PARSING {
            println!("found {} token", token);
        }
        block_tokens.push(token.clone());
    }

    if !found_block {
        err(&format!("expected a block between `{start}` and `{end}`"));
    } else {
        if block_tokens.is_empty() {
            block_tokens.push(Token::Nothing);
        }

        block_tokens.dedup_by(|a, b| (a == &mut Token::Newline) && b == a);
        if DEBUG_BLOCK_PARSING {
            println!("final block: {block_tokens:?}");
        }

        block_tokens
    }
}

pub fn parse(input: Vec<Token>) -> Vec<Expr> {
    if DEBUG_LEXER {
        println!("parse called with tokens: {input:#?}");
    }
    let mut operands = vec![];
    let mut output = vec![];
    let mut iter = input.iter().peekable();
    let mut operators: Vec<Token> = vec![Token::SentinelOp];
    while let Some(token) = iter.next() {
        if DEBUG_OPERATOR_STACK {
            println!(
                "operators: {operators:#?}\n\noperands: {operands:#?}\n\noutput: {output:#?}\n\n"
            );
        }
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

            Token::Divided
            | Token::Plus
            | Token::Minus
            | Token::Times
            | Token::Is
            | Token::And
            | Token::Or
            | Token::Equals
            | Token::Say
            | Token::Not
            | Token::Delete
            | Token::Remainder => {
                match *token {
                    Token::Divided => eat(&mut iter, Token::By),
                    _ => {}
                }

                let last = operators.last().unwrap();
                if token.priority() <= last.priority() {
                    make_tree(last, &mut operands);
                    operators.pop();
                }
                operators.push(token.clone());
            }
            Token::While => {
                let mut cond_tokens = vec![];
                let mut found_cond = false;
                while let Some(token) = iter.next() {
                    if token != &Token::Do {
                        cond_tokens.push(token.clone());
                    } else {
                        found_cond = true;
                        break;
                    }
                }
                if !found_cond {
                    err("expected truth expression in while loop between `while` and `do`!");
                }

                let block_tokens = eat_block_delimited_by(&mut iter, Token::Do, Token::End);
                cond_tokens.push(Token::Newline);

                let cond_expr = parse(cond_tokens)[0].clone();
                *LINE_NUM.write().unwrap() -= 1;
                let block_expr = parse(block_tokens);

                output.push(Expr::While(
                    Box::new(cond_expr),
                    Box::new(Expr::Block(block_expr)),
                ));
            }

            Token::If => {
                let mut cond_tokens = vec![];
                let mut found_cond = false;
                while let Some(token) = iter.next() {
                    if token != &Token::Then {
                        cond_tokens.push(token.clone());
                    } else {
                        found_cond = true;
                        break;
                    }
                }
                if !found_cond {
                    err("expected truth expression in if statement!");
                }

                let block_tokens = eat_block_delimited_by(&mut iter, Token::Then, Token::End);

                while iter.peek() == Some(&&Token::Newline) {
                    eat(&mut iter, Token::Newline);
                }

                let falseblock_tokens = {
                    if iter.peek() == Some(&&Token::Else) {
                        iter.next();
                        Some(Box::new(
                            parse(eat_block_delimited_by(&mut iter, Token::Else, Token::End))[0]
                                .clone(),
                        ))
                    } else {
                        None
                    }
                };
                while iter.peek() == Some(&&Token::Newline) {
                    eat(&mut iter, Token::Newline);
                }

                cond_tokens.push(Token::Newline);
                let cond_expr = parse(cond_tokens)[0].clone();
                let block_expr = parse(block_tokens);

                output.push(Expr::If(
                    Box::new(cond_expr),
                    Box::new(Expr::Block(block_expr)),
                    falseblock_tokens,
                ));
            }

            Token::For => {
                let mut step: usize = 1;
                let mut direction = IterDirection::Forwards;
                eat(&mut iter, Token::Every);
                if let Some(Token::Ident(name)) = iter.peek() {
                    iter.next(); //consume ident
                    match iter.peek() {
                        Some(Token::In) => unimplemented!(),
                        Some(Token::From) => {
                            iter.next();
                            let mut start: Vec<Token> = vec![];
                            let mut end: Vec<Token> = vec![];
                            let mut step: Vec<Token> = vec![];

                            while let Some(token) = iter.next() {
                                if *token != Token::To {
                                    start.push(token.clone());
                                } else {
                                    break;
                                }
                            }

                            while let Some(&token) = iter.peek() {
                                if token != &Token::Stepping && token != &Token::Do {
                                    iter.next();
                                    end.push(token.clone());
                                } else {
                                    break;
                                }
                            }

                            if start.is_empty() {
                                err("expected start of for loop");
                            }
                            if end.is_empty() {
                                err("expected end of for loop");
                            }

                            start.push(Token::Newline);
                            end.push(Token::Newline);

                            let start = parse(start)[0].clone();
                            let end = parse(end)[0].clone();

                            if let Some(Token::Stepping) = iter.peek() {
                                iter.next();

                                if let Some(Token::By) = iter.peek() {
                                    iter.next();

                                    while let Some(&token) = iter.peek() {
                                        if token != &Token::Do {
                                            iter.next();
                                            step.push(token.clone());
                                        } else {
                                            break;
                                        }
                                    }
                                }
                            }
                            if step.is_empty() {
                                step.push(Token::Num(1.))
                            }
                            step.push(Token::Newline);
                            let step = parse(step)[0].clone();
                            eat(&mut iter, Token::Do);
                            let block_tokens =
                                eat_block_delimited_by(&mut iter, Token::Do, Token::End);
                            output.push(Expr::For(ForBlock::Range(
                                Box::new(Expr::Ident(name.clone())),
                                Box::new(start),
                                Box::new(end),
                                Box::new(Expr::Block(parse(block_tokens))),
                                Box::new(step),
                            )));
                        }
                        Some(tok) => err(&format!("expected 'in' or 'from', not '{}'", tok)),
                        None => {
                            err("expected 'in' or 'from', not end of file");
                        }
                    }
                } else {
                    err("expected new variable name in for loop");
                }
            }

            Token::By | Token::Else | Token::End => err(&format!("unexpected token {token}")),
            Token::Newline => {
                //           *LINE_NUM.write().unwrap() += 1;

                //println!("newline found at end");
                if let Some(_) = operators.last() {
                    while iter.peek() == Some(&&Token::Newline) {
                        //println!("newline found subsequent");
                        *LINE_NUM.write().unwrap() += 1;
                        iter.next();
                    }
                    while let Some(op) = operators.pop() {
                        if op == Token::SentinelOp {
                            break;
                        }
                        make_tree(&op, &mut operands)
                    }
                    operators = vec![Token::SentinelOp];

                    output.push(operands.pop().unwrap_or(Expr::Nothing));
                }

                //println!("operators: {operators:#?}\n\noperands: {operands:#?}\n\noutput: {output:#?}\n\n");
            }
            _ => panic!("{}", token),
        }
    }

    output
}
