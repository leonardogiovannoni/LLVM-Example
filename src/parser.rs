use anyhow::Error;

use crate::*;

pub struct Parser {
    pub lexer: Lexer,
    pub token: Token,
    pub has_error: bool,
    pub text: RefStr,
}

pub struct ParseError;

type PResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(lexer: Lexer, buf: RefStr) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::new(TokenKind::Unknown, RefStr::new("")),
            has_error: false,
            text: buf,
        };
        parser.advance();
        parser
    }

    pub fn error(&mut self) {
        self.has_error = true;
    }

    pub fn advance(&mut self) {
        self.lexer.next(&mut self.token);
    }

    pub fn expect(&mut self, kind: TokenKind) -> PResult<()> {
        if self.token.kind == kind {
            Ok(())
        } else {
            self.error();
            Err(ParseError)
        }
    }

    pub fn consume(&mut self, kind: TokenKind) -> PResult<()> {
        if self.token.kind == kind {
            self.advance();
            Ok(())
        } else {
            self.error();
            Err(ParseError)
        }
    }

    fn skip_until(&mut self, f: impl Fn(TokenKind) -> bool) {
        while !f(self.token.kind) {
            self.advance();
        }
    }

    pub fn parse_calc(&mut self) -> PResult<Ast> {
        (|| -> PResult<Ast> {
            let mut vars = Vec::new();
            if let TokenKind::KWWith = self.token.kind {
                self.consume(TokenKind::KWWith)?;
                self.expect(TokenKind::Ident)?;
                vars.push(self.token.text.index(..));
                self.advance();
                while let TokenKind::Comma = self.token.kind {
                    self.consume(TokenKind::Comma)?;
                    self.expect(TokenKind::Ident)?;
                    vars.push(self.token.text.index(..));
                    self.advance();
                }
                self.consume(TokenKind::Colon)?;
            }
            let e = self.parse_expr()?;
            self.expect(TokenKind::Eoi)?;

            if vars.is_empty() {
                Ok(Ast::Expr(Box::new(e)))
            } else {
                let buf = self.text.index(..);
                Ok(Ast::WithDecl(Box::new(WithDecl::new(vars, buf, e))))
            }
        })()
        .map_err(|_| {
            self.skip_until(|t| t == TokenKind::Eoi);
            ParseError
        })
    }

    pub fn parse_expr(&mut self) -> PResult<Expr> {
        let mut left = self.parse_term()?;
        while let TokenKind::Plus | TokenKind::Minus = self.token.kind {
            let op = match self.token.kind {
                TokenKind::Plus => Operator::Plus,
                TokenKind::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term()?;
            left = Expr::BinaryOp(BinaryOp::new(left, right, op));
        }
        Ok(left)
    }

    pub fn parse_term(&mut self) -> PResult<Expr> {
        let mut left = self.parse_factor()?;
        while let TokenKind::Star | TokenKind::Slash = self.token.kind {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                TokenKind::Slash => Operator::Div,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::BinaryOp(BinaryOp::new(left, right, op));
        }
        Ok(left)
    }

    pub fn parse_factor(&mut self) -> PResult<Expr> {
        (|| -> PResult<Expr> {
            if let TokenKind::Ident | TokenKind::Number = self.token.kind {
                let x = match self.token.kind {
                    TokenKind::Ident => ValueKind::Ident,
                    TokenKind::Number => ValueKind::Number,
                    _ => unreachable!(),
                };
                let text = self.token.text.index(..);
                let expr = Expr::Factor(Factor::new(x, text));
                self.advance();
                Ok(expr)
            } else {
                self.consume(TokenKind::LParen)?;
                let res = self.parse_expr();
                self.consume(TokenKind::RParen)?;
                res
            }
        })()
        .map_err(|_| {
            self.skip_until(|t| {
                matches!(
                    t,
                    TokenKind::Eoi
                        | TokenKind::RParen
                        | TokenKind::Slash
                        | TokenKind::Star
                        | TokenKind::Plus
                        | TokenKind::Minus
                )
            });
            ParseError
        })
    }

    pub fn parse(&mut self) -> Result<Ast, Error> {
        let ast = self.parse_calc();
        let _ = self.expect(TokenKind::Eoi);
        ast.map_err(|_| anyhow::anyhow!("parse error"))
    }
}
