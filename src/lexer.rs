use crate::*;

pub struct Lexer {
    pub text: RefSlice<char>,
}

impl Lexer {
    pub fn new(input: RefSlice<char>) -> Self {
        Lexer { text: input }
    }

    pub fn form_token(&mut self, token: &mut Token, tok_end: usize, kind: TokenKind) {
        token.kind = kind;
        token.text = self.text.index(..tok_end);
        self.text = self.text.index(tok_end..);
    }

    pub fn next(&mut self, token: &mut Token) {
        let i = self.text.iter().take_while(|x| x.is_whitespace()).count();
        self.text = self.text.index(i..);
        let first = self.text.get(0);
        let Some(c) = first else {
            token.kind = TokenKind::Eoi;
            token.text = self.text.index(..0);
            return;
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self.text.iter().take_while(|x| x.is_alphabetic()).count();
                let name = self.text.index(..i);
                let kind = match name.as_ref() {
                    ['w', 'i', 't', 'h'] => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(token, i, kind)
            }
            x if x.is_ascii_digit() => {
                let i = self.text.iter().take_while(|x| x.is_ascii_digit()).count();
                self.form_token(token, i, TokenKind::Number)
            }
            '+' => self.form_token(token, 1, TokenKind::Plus),
            '-' => self.form_token(token, 1, TokenKind::Minus),
            '*' => self.form_token(token, 1, TokenKind::Star),
            '/' => self.form_token(token, 1, TokenKind::Slash),
            '(' => self.form_token(token, 1, TokenKind::LParen),
            ')' => self.form_token(token, 1, TokenKind::RParen),
            ':' => self.form_token(token, 1, TokenKind::Colon),
            ',' => self.form_token(token, 1, TokenKind::Comma),
            _ => self.form_token(token, 1, TokenKind::Unknown),
        }
    }
}
