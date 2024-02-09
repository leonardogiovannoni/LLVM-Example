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
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a [char],
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a [char]) -> Token<'a> {
        Token { kind, text }
    }

    pub fn is(&self, k: TokenKind) -> bool {
        self.kind == k
    }

    pub fn is_one_of(&self, ks: &[TokenKind]) -> bool {
        ks.contains(&self.kind)
    }
}
