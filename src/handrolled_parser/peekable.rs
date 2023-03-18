use logos::{Lexer, Span};

use super::lex::Token;

pub struct Peekable<'source> {
    pub lexer: Lexer<'source, Token>,
    pub peeked: Option<(Option<Token>, Span, &'source str)>,
}

impl<'source> Peekable<'source> {
    pub fn peek(&mut self) -> Option<(Token, Span, &'source str)> {
        if self.peeked.is_none() {
            self.peeked = self
                .lexer
                .next()
                .map(|c| (c.ok(), self.lexer.span(), self.lexer.slice()));
        }
        self.peeked.clone().map(|(c, v, s)| (c.unwrap(), v, s))
    }
}

impl<'source> Iterator for Peekable<'source> {
    type Item = (Token, Span, &'source str);

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked
            .take()
            .map(|(c, v, s)| (c.unwrap(), v, s))
            .or_else(|| {
                self.lexer
                    .next()
                    .map(|c| (c.unwrap(), self.lexer.span(), self.lexer.slice()))
            })
    }
}
