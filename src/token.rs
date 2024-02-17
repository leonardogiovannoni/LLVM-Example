use refslice::RefSlice;

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
    pub text: RefSlice<char>,
}

impl Token {
    pub fn new(kind: TokenKind, text: RefSlice<char>) -> Self {
        Token { kind, text }
    }

    pub fn is(&self, k: TokenKind) -> bool {
        self.kind == k
    }

    pub fn is_one_of(&self, ks: &[TokenKind]) -> bool {
        ks.contains(&self.kind)
    }
}
