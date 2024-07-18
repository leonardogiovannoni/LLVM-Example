use crate::*;
use crate::util::Span;

pub struct Lexer {
    pub span: Span,
    pub text: Rc<str>,
}
impl Lexer {
    pub fn new(input: Rc<str>) -> Self {
        Lexer {
            span: Span {
                begin: 0,
                end: input.len(),
            },
            text: input,
        }
    }

    pub fn form_token(&mut self, token: &mut Token, tok_end: usize, kind: TokenKind) {
        token.kind = kind;
        token.span = Span {
            begin: self.span.begin,
            end: self.span.begin + tok_end,
        };
        self.span.begin += tok_end;
    }

    fn text(&self) -> &str {
        &self.text[self.span.begin..self.span.end]
    }

    pub fn next(&mut self, token: &mut Token) {
        let i: usize = self
            .text()
            .chars()
            .take_while(|x| x.is_whitespace())
            .map(|x| x.len_utf8())
            .sum();
        self.span.begin += i;
        let first = self.text().chars().next();
        let Some(c) = first else {
            token.kind = TokenKind::Eoi;
            token.span = Span::empty();
            return;
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self
                    .text()
                    .chars()
                    .take_while(|x| x.is_alphabetic())
                    .map(|x| x.len_utf8())
                    .sum();
                let name = self.text()[..i].to_string();
                let kind = match name.as_str() {
                    "with" => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(token, i, kind)
            }
            x if x.is_ascii_digit() => {
                let i = self
                    .text()
                    .chars()
                    .take_while(|x| x.is_ascii_digit())
                    .map(|x| x.len_utf8())
                    .sum();
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
