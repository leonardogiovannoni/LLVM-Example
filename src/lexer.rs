use crate::util::Span;
use crate::*;

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

    pub fn form_token(&mut self, tok_end: usize, kind: TokenKind) -> Token {
        let token = Token::new(kind,  Span {
            begin: self.span.begin,
            end: self.span.begin + tok_end,
        });
        self.span.begin += tok_end;
        token
    }

    fn text(&self) -> &str {
        &self.text[self.span.begin..self.span.end]
    }

    pub fn next(&mut self) -> Token {
        let i: usize = self
            .text()
            .chars()
            .take_while(|x| x.is_whitespace())
            .map(|x| x.len_utf8())
            .sum();
        self.span.begin += i;
        let Some(c) = self.text().chars().next() else {
            return Token::new(TokenKind::Eoi, Span::empty());
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self
                    .text()
                    .chars()
                    .take_while(|x| x.is_alphabetic())
                    .map(|x| x.len_utf8())
                    .sum();
                let kind = match &self.text()[..i] {
                    "with" => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.form_token(i, kind)
            }
            x if x.is_ascii_digit() => {
                let i = self
                    .text()
                    .chars()
                    .take_while(|x| x.is_ascii_digit())
                    .map(|x| x.len_utf8())
                    .sum();
                self.form_token(i, TokenKind::Number)
            }
            '+' => self.form_token(c.len_utf8(), TokenKind::Plus),
            '-' => self.form_token(c.len_utf8(), TokenKind::Minus),
            '*' => self.form_token(c.len_utf8(), TokenKind::Star),
            '/' => self.form_token(c.len_utf8(), TokenKind::Slash),
            '(' => self.form_token(c.len_utf8(), TokenKind::LParen),
            ')' => self.form_token(c.len_utf8(), TokenKind::RParen),
            ':' => self.form_token(c.len_utf8(), TokenKind::Colon),
            ',' => self.form_token(c.len_utf8(), TokenKind::Comma),
            _ => self.form_token(c.len_utf8(), TokenKind::Unknown),
        }
    }
}
