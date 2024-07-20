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

    fn match_identifier(&mut self, start: usize) -> (&'a str, usize) {
        let end;
        loop {
            if let Some((i, ch)) = self.chars.peek() {
                if ch.is_alphabetic() {
                    self.chars.next();
                } else {
                    end = *i;
                    break;
                }
            } else {
                end = self.text.len();
                break;
            }
        }
        (&self.text[start..end], end)
    }

    fn match_number(&mut self, start: usize) -> (&'a str, usize) {
        let end;
        loop {
            if let Some((i, ch)) = self.chars.peek() {
                if ch.is_ascii_digit() {
                    self.chars.next();
                } else {
                    end = *i;
                    break;
                }
            } else {
                end = self.text.len();
                break;
            }
        }
        (&self.text[start..end], end)
    }

    pub fn next(&mut self) -> Token {
        loop {
            match self.chars.next() {
                Some((_, c)) if c.is_whitespace() => {
                    continue;
                }
                Some((i, c)) if c.is_alphabetic() => {
                    let (ident, end) = self.match_identifier(i);
                    break match ident {
                        "with" => Token::new(TokenKind::KWWith, Span::new(i, end)),
                        _ => Token::new(TokenKind::Ident, Span::new(i, end)),
                    };
                }
                Some((i, c)) if c.is_ascii_digit() => {
                    let (_, end) = self.match_number(i);
                    break Token::new(TokenKind::Number, Span::new(i, end));
                }
                Some((i, c)) => {
                    break Token::new(
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
                        Span::new(i, i + c.len_utf8()),
                    );
                }

                None => break Token::new(TokenKind::Eoi, Span::empty()),
            };
        }
    }
}
