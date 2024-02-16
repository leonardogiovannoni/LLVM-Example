use crate::*;

pub struct Lexer {
    pub text: Rc<[char]>,
    pub span: Span,
}

impl Lexer {
    pub fn new(input: Rc<[char]>) -> Self {
        Lexer {
            span: Span::new(0, input.len()),
            text: input,
        }
    }

    pub fn form_token(&mut self, token: &mut Token, tok_end: usize, kind: TokenKind) {
        token.kind = kind;
        token.text = Span {
            start: self.span.start,
            end: self.span.start + tok_end,
        };
        self.span.start += tok_end;
    }

    pub fn next(&mut self, token: &mut Token) {
        let i = self.text[self.span.start..]
            .iter()
            .take_while(|x| x.is_whitespace())
            .count();
        self.span.start += i;
        let first = self.text.get(self.span.start);
        let Some(c) = first else {
            token.kind = TokenKind::Eoi;
            token.text = Span {
                start: self.span.start,
                end: self.span.start,
            };
            return;
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self.text[self.span.start + 1..self.span.end]
                    .iter()
                    .take_while(|x| x.is_alphabetic())
                    .count();
                let name = &self.text[self.span.start..self.span.start + i + 1];
                let kind = match name {
                    ['w', 'i', 't', 'h'] => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(token, i + 1, kind)
            }
            x if x.is_ascii_digit() => {
                let i = self.text[self.span.start + 1..self.span.end]
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
