use super::lex::Token;
use super::peekable::Peekable;

use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum Mon {
    Conjugate,
    Negate,
    Sign,
    Reciprocal,
    Exponential,
    Square,
    Floor,
    Ceiling,
    SortUp,
    SortDown,
    Not,
    Absolute,
    Less,
    Enclose,
    Merge,
    Greater,
    Rank,
    Length,
    Depth,
    Shape,
    IdentityL,
    IdentityR,
    Deshape,
    Join,
    Solo,
    Enlist,
    Prefixes,
    Suffixes,
    Range,
    NudgeL,
    NudgeR,
    Reverse,
    Transpose,
    Indices,
    GradeUp,
    GradeDown,
    FirstCell,
    First,
    Classify,
    Occurrence,
    MarkFirsts,
    Deduplicate,
    Group,
    Assert,
}

impl From<char> for Mon {
    fn from(value: char) -> Self {
        let prims = [
            '+', '-', '×', '÷', '⋆', '√', '⌊', '⌈', '∧', '∨', '¬', '|', '<', '>', '=', '≠', '≡',
            '≢', '⊣', '⊢', '⥊', '∾', '≍', '⋈', '↑', '↓', '↕', '»', '«', '⌽', '⍉', '/', '⍋', '⍒',
            '⊏', '⊑', '⊐', '⊒', '∊', '⍷', '⊔', '!',
        ];
        match value {
            _ if prims.contains(&value) => unsafe {
                std::mem::transmute(prims.iter().position(|&x| x == value).unwrap() as u8)
            },
            _ => {
                panic!("can't convert")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Dy {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Root,
    Minimum,
    Maximum,
    And,
    Or,
    Span,
    Modulus,
    Leq,
    Le,
    Ge,
    Geq,
    Eq,
    Neq,
    Match,
    NotMatch,
    Left,
    Right,
    Reshape,
    Join,
    Couple,
    Pair,
    Take,
    Drop,
    Windows,
    Before,
    After,
    Rotate,
    Axes,
    Replicate,
    BinsUp,
    BinsDown,
    Select,
    Pick,
    Index,
    ProgIndex,
    Member,
    Find,
    Group,
    AssertMessage,
}

impl From<char> for Dy {
    fn from(value: char) -> Self {
        let prims = [
            '+', '-', '×', '÷', '⋆', '√', '⌊', '⌈', '∧', '∨', '¬', '|', '≤', '<', '>', '≥', '=',
            '≠', '≡', '≢', '⊣', '⊢', '⥊', '∾', '≍', '⋈', '↑', '↓', '↕', '»', '«', '⌽', '⍉', '/',
            '⍋', '⍒', '⊏', '⊑', '⊐', '⊒', '∊', '⍷', '⊔', '!',
        ];
        match value {
            _ if prims.contains(&value) => unsafe {
                std::mem::transmute(prims.iter().position(|&x| x == value).unwrap() as u8)
            },
            _ => {
                panic!("can't convert");
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OneModifier {
    Constant,
    Selff,
    Cells,
    Each,
    Table,
    Undo,
    Fold,
    Insert,
    Scan,
}
impl From<char> for OneModifier {
    fn from(value: char) -> Self {
        let modifs = ['˙', '˜', '˘', '¨', '⌜', '⁼', '´', '˝', '`'];
        match value {
            _ if modifs.contains(&value) => unsafe {
                std::mem::transmute(modifs.iter().position(|&x| x == value).unwrap() as u8)
            },
            _ => {
                panic!("can't convert")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TwoModifier {
    Atop,
    Over,
    Before,
    After,
    Valences,
    Choose,
    Under,
    Catch,
    Rank,
    Depth,
    Repeat,
}

impl From<char> for TwoModifier {
    fn from(value: char) -> Self {
        let modifs = ['∘', '○', '⊸', '⟜', '⊘', '◶', '⌾', '⎊', '⎉', '⚇', '⍟'];
        match value {
            _ if modifs.contains(&value) => unsafe {
                std::mem::transmute(modifs.iter().position(|&x| x == value).unwrap() as u8)
            },
            _ => {
                panic!("can't convert")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Primitive {
    Monadic(Mon),
    Dyadic(Dy),
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Null,
    Number(f64),
    Character(char),
    String(String),
    Ident(String),
    Array(Vec<Node>),
    Stride(Vec<Node>),
    List(Vec<Node>),
    Expr(Vec<Node>),
    Function(Box<Node>),
    OneMod {
        verb: OneModifier,
        rhs: Option<Box<Node>>,
    },
    TwoMod {
        verb: TwoModifier,
        lhs: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
    Ap {
        verb: Primitive,
        lhs: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
    Train {
        lhs: Option<Box<Node>>,
        mid: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
}

impl Display for Node {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
        // todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub ast: Vec<Node>,
}

#[derive(Debug)]
pub enum ParseError {
    UnclosedParen,
    UnclosedQuote,
    UnclosedBracket,
    CatchAllError,
}

pub fn parse(source: &mut Peekable) -> Result<Ast, ParseError> {
    let mut ast = vec![];

    while let Ok(n) = parse_next(source) {
        ast.push(n)
    }
    Ok(Ast { ast })
}

pub fn parse_atom(source: &mut Peekable) -> Result<Node, ParseError> {
    if let Some(p) = source.peek() {
        match p.0 {
            //
            Token::Char => return Ok(Node::Character(p.2.chars().next().unwrap())),
            Token::String => return Ok(Node::String(p.2.to_string())),
            Token::Number => return Ok(Node::Number(p.2.parse().unwrap())),
            Token::Null => return Ok(Node::Null),
            //
            Token::LParen => {}
            _ => return Err(ParseError::CatchAllError),
        }
    }
    Err(ParseError::CatchAllError)
}

pub fn parse_next(source: &mut Peekable) -> Result<Node, ParseError> {
    if let Some(tok) = source.next() {
        if source.peek().is_none() {
            match tok.0 {
                Token::Char => return Ok(Node::Character(tok.2.chars().next().unwrap())),
                Token::String => return Ok(Node::String(tok.2.to_string())),
                Token::Number => return Ok(Node::Number(tok.2.parse().unwrap())),
                _ => {}
            }
        }

        return Ok(match tok.0 {
            Token::Number => {
                if let Some((Token::Primitive, _, val)) = source.peek() {
                    source.next();
                    Node::Ap {
                        verb: Primitive::Dyadic(val.chars().next().unwrap().into()),
                        lhs: Some(Box::new(Node::Number(tok.2.parse().unwrap()))),
                        rhs: Some(Box::new(parse_next(source).unwrap())),
                    }
                } else {
                    dbg!(source.peek());
                    unreachable!()
                }
            }
            Token::Primitive => Node::Ap {
                verb: Primitive::Monadic(tok.2.chars().next().unwrap().into()),
                lhs: None,
                rhs: None,
            },
            Token::LParen => {
                while let Some(n) = source.next() {
                    if n.0 == Token::RParen {}
                }
                Node::Null
            }
            _ => unreachable!(),
        });
    } else {
        Err(ParseError::UnclosedParen)
    }
}
