use crate::util::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]

pub enum TokenKind {
    Eoi,
    Unknown,
    Ident,
    Number,
    Comma,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    KWWith,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, text: Span) -> Self {
        Token { kind, span: text }
    }

    pub fn is_one_of(&self, ks: &[TokenKind]) -> bool {
        ks.contains(&self.kind)
    }
}
