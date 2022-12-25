
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
