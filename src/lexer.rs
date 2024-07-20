use crate::util::Span;
use crate::*;

pub struct Lexer<'a> {
    pub pos: usize,
    pub text: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            pos: 0,
            text: input,
        }
    }

    pub fn token_and_advance(&mut self, kind: TokenKind, token_len: usize) -> Token {
        let token = Token::new(kind, Span::new(self.pos, self.pos + token_len));
        self.pos += token_len;
        token
    }

    fn count_bytes_until(&self, f: impl Fn(char) -> bool) -> usize {
        self.text[self.pos..]
            .chars()
            .take_while(|x| f(*x))
            .map(|x| x.len_utf8())
            .sum()
    }

    fn skip_while(&mut self, f: impl Fn(char) -> bool) {
        self.pos += self.count_bytes_until(f);
    }

    fn peek(&self) -> Option<char> {
        self.text[self.pos..].chars().next()
    }

    fn peek_str(&self, len: usize) -> Option<&str> {
        self.text.get(self.pos..self.pos + len)
    }

    pub fn next(&mut self) -> Token {
        self.skip_while(char::is_whitespace);
        let Some(c) = self.peek() else {
            return Token::new(TokenKind::Eoi, Span::empty());
        };
        match c {
            x if x.is_alphabetic() => {
                let i = self.count_bytes_until(char::is_alphabetic);
                let kind = match self.peek_str(i) {
                    Some("with") => TokenKind::KWWith,
                    _ => TokenKind::Ident,
                };
                self.token_and_advance(kind, i)
            }
            x if x.is_ascii_digit() => {
                let i = self.count_bytes_until(|x| x.is_ascii_digit());
                self.token_and_advance(TokenKind::Number, i)
            }
            _ => self.token_and_advance(
                match c {
                    '+' => TokenKind::Plus,
                    '-' => TokenKind::Minus,
                    '*' => TokenKind::Star,
                    '/' => TokenKind::Slash,
                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    ':' => TokenKind::Colon,
                    ',' => TokenKind::Comma,
                    _ => TokenKind::Unknown,
                },
                c.len_utf8(),
            ),
        }
    }
}
