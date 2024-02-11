use crate::*;
pub struct Lexer<'a> {
    //buffer_start: &'a [char],
    buffer_ptr: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [char]) -> Lexer<'a> {
        Lexer {
            //buffer_start: input,
            buffer_ptr: input,
        }
    }

    pub fn form_token(&mut self, token: &mut Token<'a>, tok_end: usize, kind: TokenKind) {
        token.kind = kind;
        token.text = &self.buffer_ptr[..tok_end];
        self.buffer_ptr = &self.buffer_ptr[tok_end..];
    }

    pub fn next(&mut self, token: &mut Token<'a>) {
        let i = self
            .buffer_ptr
            .iter()
            .take_while(|x| x.is_whitespace())
            .count();

        self.buffer_ptr = &self.buffer_ptr[i..];
        let Some(c) = self.buffer_ptr.first() else {
            token.kind = TokenKind::Eoi;
            token.text = &[];
            return;
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self.buffer_ptr[1..]
                    .iter()
                    .take_while(|x| x.is_alphabetic())
                    .count();
                let name = &self.buffer_ptr[..i + 1];
                let kind = match name {
                    ['w', 'i', 't', 'h'] => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(token, i + 1, kind)
            }
            x if x.is_ascii_digit() => {
                let i = self
                    .buffer_ptr
                    .iter()
                    .take_while(|x| x.is_ascii_digit())
                    .count();
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
