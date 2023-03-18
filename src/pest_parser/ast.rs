use pest::error::Error;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive;
use std::fmt::Display;

const MONADIC_PRIMS: [char; 42] = [
    '+', '-', '×', '÷', '⋆', '√', '⌊', '⌈', '∧', '∨', '¬', '|', '<', '>', '=', '≠', '≡', '≢', '⊣',
    '⊢', '⥊', '∾', '≍', '⋈', '↑', '↓', '↕', '»', '«', '⌽', '⍉', '/', '⍋', '⍒', '⊏', '⊑', '⊐', '⊒',
    '∊', '⍷', '⊔', '!',
];
const DYADIC_PRIMS: [char; 44] = [
    '+', '-', '×', '÷', '⋆', '√', '⌊', '⌈', '∧', '∨', '¬', '|', '≤', '<', '>', '≥', '=', '≠', '≡',
    '≢', '⊣', '⊢', '⥊', '∾', '≍', '⋈', '↑', '↓', '↕', '»', '«', '⌽', '⍉', '/', '⍋', '⍒', '⊏', '⊑',
    '⊐', '⊒', '∊', '⍷', '⊔', '!',
];
const MOD1: [char; 9] = ['˙', '˜', '˘', '¨', '⌜', '⁼', '´', '˝', '`'];
const MOD2: [char; 11] = ['∘', '○', '⊸', '⟜', '⌾', '⊘', '◶', '⎉', '⚇', '⍟', '⎊'];
const DELIMS: [char; 3] = ['⋄', ',', '\n'];

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Enclose,
    Merge,
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

macro_rules! impl_from_for_enum {
    ($arr:expr, $ty:ty) => {
        impl From<char> for $ty {
            fn from(value: char) -> Self {
                if $arr.contains(&value) {
                    unsafe {
                        std::mem::transmute($arr.iter().position(|&x| x == value).unwrap() as u8)
                    }
                } else {
                    panic!("unable to convert")
                }
            }
        }
    };
}

macro_rules! impl_enum_to_string {
    ($arr:expr, $ty:ty) => {
        impl From<$ty> for String {
            fn from(value: $ty) -> Self {
                unsafe {
                    $arr.map(|c| c.to_string())[std::mem::transmute::<$ty, u8>(value) as usize]
                        .clone()
                }
            }
        }
    };
}
impl_from_for_enum!(MONADIC_PRIMS, Mon);
impl_from_for_enum!(DYADIC_PRIMS, Dy);
impl_from_for_enum!(MOD1, OneModifier);
impl_from_for_enum!(MOD2, TwoModifier);
impl_from_for_enum!(DELIMS, Delim);
impl_enum_to_string!(MONADIC_PRIMS, Mon);
impl_enum_to_string!(DYADIC_PRIMS, Dy);
impl_enum_to_string!(MOD1, OneModifier);
impl_enum_to_string!(MOD2, TwoModifier);
impl_enum_to_string!(DELIMS, Delim);

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Primitive {
    Monadic(Mon),
    Dyadic(Dy),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Monadic(m) => f.write_str(&Into::<String>::into(*m)),
            Primitive::Dyadic(d) => f.write_str(&Into::<String>::into(*d)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Prim(Primitive),
    Node(Box<Node>),
}

#[derive(Debug, PartialEq)]
pub struct OneModAp {
    pub lhs: Function,
    pub rhs: OneModifier,
}

impl Display for OneModAp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.lhs {
            Function::Prim(p) => f.write_str(&p.to_string()),
            Function::Node(n) => f.write_str(&n.to_string()),
        }?;

        f.write_str(&unsafe {
            MOD1.map(|c| c.to_string())
                [std::mem::transmute::<OneModifier, u8>(self.rhs.clone()) as usize]
                .clone()
        })?;
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct TwoModAp {
    pub lhs: Function,
    pub rhs: TwoModifier,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Delim {
    Diamond,
    Comma,
    Newline,
}

#[derive(Debug, PartialEq)]
pub struct Stmt {
    pub node: Node,
    pub delim: Option<Delim>,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Null,
    Asgn {
        ident: String,
        body: Box<Node>,
        func: Option<Box<Node>>,
    },
    Number(f64),
    Character(char),
    String(String),
    Ident(String),
    Array(Vec<Node>),
    Stride(Vec<Node>),
    List(Vec<Node>),
    Expr(Vec<Stmt>),
    Function(Function),
    OneMod {
        modifier: OneModAp,
        lhs: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
    TwoMod {
        modifier: TwoModAp,
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
    Fork {
        lhs: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
}

impl Node {
    pub fn is_unit_type(&self) -> bool {
        matches!(
            self,
            Self::Null | Self::Character(_) | Self::String(_) | Self::Number(_) | Self::Stride(_)
        )
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Null => f.write_str("@"),
            Node::Asgn { ident, body, func } => {
                if func.is_none() {
                    f.write_str(&format!("{ident}←{body}",))
                } else {
                    let func = func.as_ref().unwrap();
                    f.write_str(&format!("{ident}{func}←{body}",))
                }
            }
            Node::Number(n) => f.write_str(&n.to_string()),
            Node::Character(c) => f.write_str(&c.to_string()),
            Node::String(s) => f.write_str(s),
            Node::Ident(i) => f.write_str(i),
            Node::Array(v) => {
                f.write_str("[")?;
                for (i, n) in v.iter().enumerate() {
                    f.write_str(&format!("{n}"))?;
                    if i < v.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str("]")?;
                Ok(())
            }
            Node::Stride(v) => {
                for (i, n) in v.iter().enumerate() {
                    if !n.is_unit_type() {
                        // NOTE add parens for non-unit types in stranding
                        // not entirely sure if this is the best way to recover parens
                        f.write_str(&format!("({n})"))?;
                    } else {
                        f.write_str(&format!("{n}"))?;
                    }
                    if i < v.len() - 1 {
                        f.write_str("‿")?;
                    }
                }
                Ok(())
            }
            // TODO:
            Node::List(_) => todo!(),
            Node::Expr(es) => {
                for e in es {
                    f.write_str(&e.node.to_string())?;
                    if let Some(delim) = e.delim {
                        f.write_str(&Into::<String>::into(delim))?
                    };
                }
                Ok(())
            }
            Node::Function(fun) => match fun {
                Function::Prim(p) => f.write_str(&p.to_string()),
                Function::Node(n) => f.write_str(&n.to_string()),
            },
            Node::OneMod { lhs, modifier, rhs } => {
                if lhs.is_some() {
                    f.write_str(&lhs.as_ref().unwrap().to_string())?
                };

                f.write_str(&modifier.to_string())?;

                if rhs.is_some() {
                    f.write_str(&rhs.as_ref().unwrap().to_string())?
                };
                Ok(())
            }
            Node::TwoMod { modifier, lhs, rhs } => todo!(),
            Node::Ap { verb, lhs, rhs } => {
                if let Some(lhsv) = lhs {
                    let lhs = if lhsv.is_unit_type() {
                        lhsv.to_string()
                    } else {
                        format!("({})", lhsv)
                    };
                    f.write_str(&lhs)?;
                }
                f.write_str(&verb.to_string())?;
                if let Some(rhsv) = rhs {
                    let rhs = match (rhsv.is_unit_type(), lhs.is_none()) {
                        (true, _) => rhsv.to_string(),
                        (false, false) => format!("({})", rhsv),
                        (false, true) => format!("{}", rhsv),
                    };
                    f.write_str(&rhs)?;
                }
                Ok(())
            }
            Node::Train { lhs, mid, rhs } => {
                f.write_str("(")?;
                [lhs, mid, rhs]
                    .into_iter()
                    .filter_map(Option::as_ref)
                    .try_for_each(|value| f.write_str(&value.to_string()))?;
                f.write_str(")")?;
                Ok(())
            }
            Node::Fork { lhs, rhs } => Ok([lhs, rhs]
                .into_iter()
                .filter_map(Option::as_ref)
                .try_for_each(|value| f.write_str(&value.to_string()))?),
        }?;
        Ok(())
    }
}

pub fn sl(pair: Pair<Rule>) -> Node {
    let slide = pair.clone().into_inner().next().unwrap();
    match slide.as_rule() {
        Rule::char => Node::Character(pair.as_str().chars().next().unwrap()),
        Rule::string => Node::String(pair.as_str().to_string()),
        Rule::number => {
            let istr = pair.as_str();
            let (sign, istr) = match &istr[..1] {
                "_" => (-1, &istr[1..]),
                _ => (1, istr),
            };
            let integer: i32 = istr.parse().unwrap();
            Node::Number((sign * integer).into())
        }
        Rule::null => Node::Null,
        _ => unreachable!(),
    }
}

pub fn atom(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::subExpr => subexpr(pair),
        Rule::array => Node::Array(
            pair.into_inner()
                .filter_map(|n| {
                    if !matches!(n.as_rule(), Rule::DELIM) {
                        Some(expr(n))
                    } else {
                        None
                    }
                })
                .collect(),
        ),
        Rule::val => todo!(),
        Rule::sl => sl(pair),
        _ => unreachable!(),
    }
}

pub fn lhs(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::val => Node::Ident(pair.as_str().to_string()),
        Rule::lhsComp => {
            unimplemented!()
        }
        _ => unreachable!(),
    }
}

pub fn subexpr(pair: Pair<Rule>) -> Node {
    let mut slide = pair.into_inner();
    let inner = slide.next().unwrap();
    if let Rule::arg = inner.as_rule() {
        arg(inner)
    } else {
        let lhsv = lhs(inner);
        let next = slide.next().unwrap();
        let body = slide.next().unwrap();
        if let Rule::ASGN = next.as_rule() {
            Node::Asgn {
                ident: lhsv.to_string(),
                body: Box::new(subexpr(body)),
                func: None,
            }
        } else {
            Node::Asgn {
                ident: lhsv.to_string(),
                body: Box::new(subexpr(slide.next().unwrap())),
                func: Some(Box::new(Node::Null)), // unimplemented!()
            }
        }
    }
}

pub fn derv(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();
    if inner.clone().count() == 1 {
        func(inner.next().unwrap())
    } else {
        let lhs = inner.next().unwrap();
        let mid = inner.next().unwrap();
        let rhs = inner.next().unwrap();
        Node::Train {
            lhs: Some(Box::new(match &lhs.as_rule() {
                Rule::subject => subject(lhs),
                Rule::Func => func(lhs),
                _ => unreachable!(),
            })),
            mid: Some(Box::new(func(mid))),
            rhs: Some(Box::new(func(rhs))),
        }
    }
}

pub fn fork(pair: Pair<Rule>) -> Node {
    let slide = pair.into_inner().next().unwrap();
    match slide.as_rule() {
        Rule::Derv => derv(slide),
        _ => unimplemented!(),
    }
}

pub fn train(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    if let Rule::Derv = pair.as_rule() {
        // NOTE Derv ~ Fork
        derv(pair)
        // unimplemented!
    } else {
        // NOTE just Fork
        fork(pair)
    }
}

pub fn funcexpr(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    if let Rule::Train = pair.as_rule() {
        train(pair)
    } else {
        // assignment
        unimplemented!()
    }
}

pub fn expr(pair: Pair<Rule>) -> Node {
    let elide = pair.into_inner().next().unwrap();
    match elide.as_rule() {
        Rule::FuncExpr => funcexpr(elide),
        Rule::subExpr => subexpr(elide),
        Rule::_m2Expr_ => todo!(),
        Rule::_m1Expr => todo!(),
        _ => unreachable!(),
    }
}

pub fn ap_resolve(pair: Pair<Rule>, dyadic: bool) -> Primitive {
    if let Rule::Fl = pair
        .clone()
        .into_inner()
        .next()
        .unwrap()
        .into_inner()
        .next()
        .unwrap()
        .as_rule()
    {
        if dyadic {
            Primitive::Dyadic(pair.into())
        } else {
            Primitive::Monadic(pair.into())
        }
    } else {
        unreachable!()
    }
}

impl<'a> From<Pair<'a, Rule>> for Dy {
    fn from(value: Pair<'a, Rule>) -> Self {
        value.as_str().chars().next().unwrap().into()
    }
}

impl<'a> From<Pair<'a, Rule>> for Mon {
    fn from(value: Pair<'a, Rule>) -> Self {
        value.as_str().chars().next().unwrap().into()
    }
}

impl<'a> From<Pair<'a, Rule>> for Delim {
    fn from(value: Pair<'a, Rule>) -> Self {
        value.as_str().chars().next().unwrap().into()
    }
}

pub fn arg(pair: Pair<Rule>) -> Node {
    let mut pair = pair.into_inner();
    let cnt = pair.clone().count();
    if cnt == 3 {
        let subj = subject(pair.next().unwrap());
        let derv = ap_resolve(pair.next().unwrap(), true);
        let subexpr = subexpr(pair.next().unwrap());
        Node::Ap {
            verb: derv,
            lhs: Some(Box::new(subj)),
            rhs: Some(Box::new(subexpr)),
        }
    } else {
        subject(pair.next().unwrap())
    }
}

pub fn some(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::atom => atom(pair),
        Rule::Func => func(pair),
        Rule::_mod1 => unimplemented!(),
        Rule::_mod2_ => unimplemented!(),
        _ => unreachable!(),
    }
}

pub fn subject(pair: Pair<Rule>) -> Node {
    let cnt = pair.clone().into_inner().count();
    if cnt == 1 {
        let pair = pair.into_inner().next().unwrap();
        atom(pair)
    } else {
        Node::Stride(pair.into_inner().map(some).collect())
    }
}

pub fn func(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    if let Rule::Fl = pair.as_rule() {
        Node::Function(Function::Prim(Primitive::Monadic(pair.into())))
    } else {
        match pair.as_rule() {
            Rule::FuncExpr => funcexpr(pair),
            _ => unreachable!(),
        }
    }
}

pub fn stmt(pair: Pair<Rule>) -> Node {
    let pair = pair.into_inner().next().unwrap();
    if let Rule::EXPR = pair.as_rule() {
        expr(pair)
    } else {
        unreachable!()
    }
}

pub fn program(pair: pest::iterators::Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::PROGRAM => Node::Expr({
            let mut inner = pair.into_inner();
            let mut v: Vec<Stmt> = vec![];
            while let Some(next) = inner.next() {
                v.push(Stmt {
                    node: stmt(next),
                    delim: inner.next().map(|c| c.into()),
                });
            }
            v
        }),
        Rule::STMT => stmt(pair),
        Rule::Func => func(pair),
        Rule::atom => atom(pair),
        Rule::EXPR => expr(pair),
        Rule::subExpr => subexpr(pair),
        Rule::arg => arg(pair),
        Rule::sl => sl(pair),
        Rule::SOME => some(pair),
        Rule::subject => subject(pair),
        Rule::Derv => derv(pair),
        Rule::Fl => Node::Ap {
            verb: Primitive::Monadic(pair.into()),
            lhs: { None },
            rhs: { None },
        },
        _ => {
            dbg!(pair.as_rule());
            dbg!(pair.as_str());
            todo!()
        }
    }
}

#[derive(pest_derive::Parser, Debug, PartialEq)]
#[grammar = "pest_parser/bqn.pest"]
pub struct BqnParser {
    pub ast: Node,
}

#[allow(clippy::result_large_err)]
pub fn pest_parse(source: &str) -> Result<BqnParser, Error<Rule>> {
    let mut pairs = BqnParser::parse(Rule::PROGRAM, source)?;
    Ok(BqnParser {
        ast: program(pairs.next().unwrap()),
    })
}
