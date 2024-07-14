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
        if self.token.kind != kind {
            self.error();
            Err(ParseError)
        } else {
            Ok(())
        }
    }

    pub fn consume(&mut self, kind: TokenKind) -> PResult<()> {
        if self.expect(kind).is_err() {
            Err(ParseError)
        } else {
            self.advance();
            Ok(())
        }
    }

    fn skip_until(&mut self, elems: &[TokenKind]) -> PResult<()> {
        while !self.token.is_one_of(elems) {
            self.advance();
        }
        Err(ParseError)
    }

    pub fn guard<F, S>(&mut self, f: F, elems: &[TokenKind]) -> PResult<S>
    where
        F: FnOnce(&mut Self) -> PResult<S>,
    {
        match f(self) {
            Ok(t) => Ok(t),
            Err(_err) => Err(self.skip_until(elems).unwrap_err()),
        }
    }

    pub fn parse_calc(&mut self) -> PResult<Ast> {
        self.guard(
            |p| {
                let mut vars = Vec::new();
                if p.token.kind == TokenKind::KWWith {
                    p.advance();
                    p.expect(TokenKind::Ident)?;

                    vars.push(p.token.text.index(..));
                    p.advance();
                    while p.token.kind == TokenKind::Comma {
                        p.advance();
                        p.expect(TokenKind::Ident)?;
                        vars.push(p.token.text.index(..));
                        p.advance();
                    }

                    p.consume(TokenKind::Colon)?;
                }
                let e = p.parse_expr()?;
                p.expect(TokenKind::Eoi)?;

                if vars.is_empty() {
                    Ok(Ast::Expr(Box::new(e)))
                } else {
                    let buf = p.text.index(..);
                    Ok(Ast::WithDecl(Box::new(WithDecl::new(vars, buf, e))))
                }
            },
            &[TokenKind::Eoi],
        )
    }

    pub fn parse_expr(&mut self) -> PResult<Expr> {
        let mut left = self.parse_term()?;
        while self.token.is_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
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
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
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
        self.guard(
            |p| match p.token.kind {
                x @ (TokenKind::Ident | TokenKind::Number) => {
                    let x = match x {
                        TokenKind::Ident => ValueKind::Ident,
                        TokenKind::Number => ValueKind::Number,
                        _ => unreachable!(),
                    };
                    let text = p.token.text.index(..);
                    let expr = Expr::Factor(Factor::new(x, text));
                    p.advance();
                    Ok(expr)
                }

                TokenKind::LParen => {
                    p.advance();
                    let res = p.parse_expr();
                    p.consume(TokenKind::RParen)?;
                    res
                }
                _ => Err(ParseError),
            },
            &[
                TokenKind::Eoi,
                TokenKind::RParen,
                TokenKind::Slash,
                TokenKind::Star,
                TokenKind::Plus,
                TokenKind::Minus,
            ],
        )
    }

    pub fn parse(&mut self) -> Result<Ast, Error> {
        let ast = self.parse_calc();
        let _ = self.expect(TokenKind::Eoi);
        ast.map_err(|_| anyhow::anyhow!("parse error"))
    }
}
