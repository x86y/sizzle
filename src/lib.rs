#![feature(iter_next_chunk)]
#[cfg(feature = "handrolled")]
pub mod handrolled_parser;
#[cfg(feature = "pest")]
pub mod pest_parser;

#[cfg(feature = "handrolled")]
#[cfg(test)]
mod handrolled_tests {
    use handrolled_parser::*;

    use super::*;
    use ast::parse;
    use ast::Node;
    use handrolled_parser::lex::Token;
    use logos::Logos;
    #[test]
    fn lexes() {
        let lex = lex::Token::lexer("123.3+4‿5+'a'+\"hello world\"");
        let lexed: Vec<Token> = lex.flatten().collect();
        let correct = [
            Token::Number,
            Token::Primitive,
            Token::Number,
            Token::Stranding,
            Token::Number,
            Token::Primitive,
            Token::Char,
            Token::Primitive,
            Token::String,
        ]
        .to_vec();
        assert_eq!(lexed, correct);
    }
    #[test]
    fn parses_simple() {
        let mut pkable = peekable::Peekable {
            lexer: Token::lexer("12+5"),
            peeked: None,
        };
        let res = parse(&mut pkable).unwrap();
        assert_eq!(
            res.ast[0],
            Node::Ap {
                verb: ast::Primitive::Dyadic(ast::Dy::Add),
                lhs: Some(Box::new(Node::Number(12.0))),
                rhs: Some(Box::new(Node::Number(5.0)))
            }
        );
    }
    #[test]
    fn parses_advanced() {
        let mut pkable = peekable::Peekable {
            lexer: Token::lexer("12+5-3"),
            peeked: None,
        };
        let res = parse(&mut pkable).unwrap();
        assert_eq!(
            res.ast[0],
            Node::Ap {
                verb: ast::Primitive::Dyadic(ast::Dy::Add),
                lhs: Some(Box::new(Node::Number(12.0))),
                rhs: Some(Box::new(Node::Ap {
                    verb: ast::Primitive::Dyadic(ast::Dy::Subtract),
                    lhs: Some(Box::new(Node::Number(5.0))),
                    rhs: Some(Box::new(Node::Number(3.0)))
                }))
            }
        );

        let mut pkable = peekable::Peekable {
            lexer: Token::lexer("(12+5)-3"),
            peeked: None,
        };
        let res = parse(&mut pkable).unwrap();
        assert_eq!(
            res.ast[0],
            Node::Ap {
                verb: ast::Primitive::Dyadic(ast::Dy::Subtract),
                rhs: Some(Box::new(Node::Number(12.0))),
                lhs: Some(Box::new(Node::Ap {
                    verb: ast::Primitive::Dyadic(ast::Dy::Add),
                    lhs: Some(Box::new(Node::Number(12.0))),
                    rhs: Some(Box::new(Node::Number(5.0)))
                }))
            }
        );
    }
}

#[cfg(feature = "pest")]
#[cfg(test)]
mod pest_tests {
    use crate::pest_parser::ast::{pest_parse, Node, Primitive};
    use crate::pest_parser::ast::{Dy, Stmt};
    #[cfg(test)]
    fn extract_only_node(n: &Node) -> &Stmt {
        match n {
            Node::Expr(es) => &es[0],
            _ => unreachable!(),
        }
    }

    #[test]
    fn parses_simple_calc() {
        let lhs = Node::Ap {
            verb: Primitive::Dyadic(crate::pest_parser::ast::Dy::Add),
            lhs: Some(Box::new(Node::Number(1.0))),
            rhs: Some(Box::new(Node::Number(3.0))),
        };
        let rhs = pest_parse("1+3").unwrap_or_else(|e| panic!("{}", e)).ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }

    #[test]
    fn parses_simple_arr() {
        let lhs = Node::Array(vec![Node::Number(123.0), Node::Number(321.0)]);
        let rhs = pest_parse("[123,321]")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }

    #[test]
    fn parses_nested_arr() {
        let lhs = Node::Array(vec![
            Node::Number(123.0),
            Node::Number(321.0),
            Node::Array(vec![Node::Number(444.0), Node::Number(1.0)]),
        ]);
        let rhs = pest_parse("[123,321,[444,1]]")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }

    #[test]
    fn parses_nested_heterogenous() {
        let lhs = Node::Array(vec![
            Node::Number(123.0),
            Node::Number(321.0),
            Node::Array(vec![
                Node::Number(444.0),
                Node::Ap {
                    verb: Primitive::Dyadic(crate::pest_parser::ast::Dy::Divide),
                    lhs: Some(Box::new(Node::Number(1.0))),
                    rhs: Some(Box::new(Node::Number(3.0))),
                },
            ]),
        ]);
        let rhs = pest_parse("([(123),(321),[444,1÷3]])")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }

    #[test]
    fn parses_monadic_prim() {
        use crate::pest_parser::ast::{pest_parse, Function, Mon, Node, Primitive};
        let lhs = Node::Function(Function::Prim(Primitive::Monadic(Mon::Conjugate)));
        let rhs = pest_parse("+").unwrap_or_else(|e| panic!("{}", e)).ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }

    #[test]
    fn delims() {
        let lhs = Node::Expr(vec![
            Stmt {
                node: Node::Number(123.0),
                delim: Some(crate::pest_parser::ast::Delim::Newline),
            },
            Stmt {
                node: Node::Ap {
                    verb: Primitive::Dyadic(Dy::Subtract),
                    lhs: Some(Box::new(Node::Number(1.0))),
                    rhs: Some(Box::new(Node::Number(3.0))),
                },
                delim: Some(crate::pest_parser::ast::Delim::Comma),
            },
            Stmt {
                node: Node::Number(4.0),
                delim: Some(crate::pest_parser::ast::Delim::Diamond),
            },
            Stmt {
                node: Node::Number(5.0),
                delim: None,
            },
        ]);
        let rhs = pest_parse(
            "123
1-3,4⋄5",
        )
        .unwrap_or_else(|e| panic!("{}", e))
        .ast;
        assert_eq!(lhs, rhs);
    }

    fn parses_code_1() {
        use crate::pest_parser::ast::*;
        let lhs = Node::Ap {
            verb: Primitive::Monadic(Mon::Deshape),
            lhs: None,
            rhs: Some(Box::new(Node::OneMod {
                modifier: OneModAp {
                    lhs: Function::Prim(Primitive::Monadic(Mon::Conjugate)),
                    rhs: OneModifier::Constant,
                },
                lhs: Some(Box::new(Node::Array(vec![
                    Node::Number(1.0),
                    Node::Number(2.0),
                ]))),
                rhs: Some(Box::new(Node::Array(vec![
                    Node::Number(3.0),
                    Node::Number(4.0),
                ]))),
            })),
        };
        let rhs = pest_parse("⥊[1,2]≍˘[3,4]")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, extract_only_node(&rhs).node);
    }
}

#[cfg(feature = "pest")]
#[cfg(test)]
mod formatting_tests {
    use crate::pest_parser::ast::pest_parse;

    #[test]
    fn nested_arr_with_parens() {
        let lhs = "[123, 321, [444, 1]]"; // FIXME: later
        let rhs = &pest_parse("[(123),321,[444,1]]")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn ap() {
        let lhs = "12+(321-3)";
        let rhs = &pest_parse("((12)+(321-3))")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn stride() {
        let lhs = "1‿2‿3";
        let rhs = &pest_parse("1‿2‿3").unwrap_or_else(|e| panic!("{}", e)).ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn stride_op() {
        let lhs = "500×1‿2‿((3+3)×3)";

        let rhs = &pest_parse("(500×(((1)‿2)‿((3+3)×3)))")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn train() {
        let lhs = "(-+(---))";
        let rhs = &pest_parse("(-+(---))")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn asgn() {
        let lhs = "a←5";
        let rhs = &pest_parse("(a←((5)))")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn mod_ap() {
        use crate::pest_parser::ast::*;
        let node = &Node::Ap {
            lhs: None,
            verb: Primitive::Monadic(Mon::Deshape),
            rhs: Some(Box::new(Node::OneMod {
                modifier: OneModAp {
                    lhs: Function::Prim(Primitive::Dyadic(Dy::Couple)),
                    rhs: OneModifier::Cells,
                },
                lhs: Some(Box::new(Node::Array(vec![
                    Node::Number(1.0),
                    Node::Number(2.0),
                ]))),
                rhs: Some(Box::new(Node::Array(vec![
                    Node::Number(3.0),
                    Node::Number(4.0),
                ]))),
            })),
        };
        assert_eq!(node.to_string(), "⥊[1, 2]≍˘[3, 4]")
    }

    #[test]
    fn destructured_assign() {
        let lhs = "⟨a,b⟩←⟨1,3⟩";
        let rhs = &pest_parse("⟨a,b⟩←⟨1,(3)⟩")
            .unwrap_or_else(|e| panic!("{}", e))
            .ast;
        assert_eq!(lhs, rhs.to_string());
    }

    #[test]
    fn delims() {
        let lhs = "123
1-3,4⋄5";
        let rhs = &pest_parse(
            "123
1-3,4⋄5",
        )
        .unwrap_or_else(|e| panic!("{}", e))
        .ast;
        dbg!(&rhs);
        assert_eq!(lhs, rhs.to_string());
    }
}
