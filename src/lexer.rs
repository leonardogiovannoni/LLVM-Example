use crate::util::Span;
use crate::*;
use itertools::{peek_nth, PeekNth};
use std::str::CharIndices;

pub struct Lexer<'a> {
    chars: PeekNth<CharIndices<'a>>,
    pub text: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: peek_nth(input.char_indices()),
            text: input,
        }
    }




    fn match_while(&mut self, start: usize, f: impl Fn(char) -> bool) -> (&'a str, usize) {
        let end;
        loop {
            if let Some(&(i, ch)) = self.chars.peek() {
                if f(ch) {
                    self.chars.next();
                } else {
                    end = i;
                    break;
                }
            } else {
                end = self.text.len();
                break;
            }
        }
        (&self.text[start..end], end)
    }

    fn match_identifier(&mut self, start: usize) -> (&'a str, usize) {
        self.match_while(start, |ch| ch.is_alphabetic())
    }

    fn match_number(&mut self, start: usize) -> (&'a str, usize) {
        self.match_while(start, |ch| ch.is_ascii_digit())
    }

    fn token(&self, kind: TokenKind, start: usize, end: usize) -> Token {
        Token::new(kind, Span::new(start, end))
    }

    pub fn next(&mut self) -> Token {
        loop {
            match self.chars.next() {
                Some((_, c)) if c.is_whitespace() => {
                    continue;
                }
                Some((start, c)) if c.is_alphabetic() => {
                    let (ident, end) = self.match_identifier(start);
                    break if ident == "with" {
                        self.token(TokenKind::KWWith, start, end)
                    } else {
                        self.token(TokenKind::Ident, start, end)
                    };
                }
                Some((start, c)) if c.is_ascii_digit() => {
                    let (_, end) = self.match_number(start);
                    break self.token(TokenKind::Number, start, end);
                }
                Some((start, c)) => {
                    break self.token(
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
                        start,
                        start + c.len_utf8(),
                    );
                }
                None => break self.token(TokenKind::Eoi, self.text.len(), self.text.len()),
            };
        }
    }
}
