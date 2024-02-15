use crate::*;

pub struct Lexer {
    pub text: Rc<[char]>,
    pub buffer_ptr: Span,
}

impl Lexer {
    pub fn new(input: Rc<[char]>) -> Self {
        Lexer {
            buffer_ptr: Span::new(0, input.len()),
            text: input,
        }
    }

    pub fn form_token(&mut self, token: &mut Token, tok_end: usize, kind: TokenKind) {
        token.kind = kind;
        token.text = Span {
            start: self.buffer_ptr.start,
            end: self.buffer_ptr.start + tok_end,
        };
        self.buffer_ptr.start += tok_end;
    }

    pub fn next(&mut self, token: &mut Token) {
        let i = Some(self.buffer_ptr)
            .map(|x| {
                self.text[x.start..x.end]
                    .iter()
                    .take_while(|x| x.is_whitespace())
                    .count()
            })
            .unwrap_or(0);
        self.buffer_ptr.start += i;
        let first = self.text.get(self.buffer_ptr.start);
        let Some(c) = first else {
            token.kind = TokenKind::Eoi;
            token.text = Span {
                start: self.buffer_ptr.start,
                end: self.buffer_ptr.start,
            };
            return;
        };
        match c {
            x if x.is_alphabetic() => {
                let i = Some(self.buffer_ptr)
                    .map(|x| {
                        self.text[x.start + 1..x.end]
                            .iter()
                            .take_while(|x| x.is_alphabetic())
                            .count()
                    })
                    .unwrap_or(0);
                let name = &self.text[self.buffer_ptr.start..self.buffer_ptr.start + i + 1];
                let kind = match name {
                    ['w', 'i', 't', 'h'] => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(token, i + 1, kind)
            }
            x if x.is_ascii_digit() => {
                let i = Some(self.buffer_ptr)
                    .map(|x| {
                        self.text[x.start..x.end]
                            .iter()
                            .take_while(|x| x.is_ascii_digit())
                            .count()
                    })
                    .unwrap_or(0);
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
