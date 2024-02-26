use anyhow::Error;

use crate::*;

pub struct Parser {
    pub lexer: Lexer,
    pub token: Token,
    pub has_error: bool,
    pub text: RefStr,
    pub state: Rc<State>,
}

pub struct ParseError;

impl Parser {
    pub fn new(lexer: Lexer, buf: RefStr, state: Rc<State>) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::new(TokenKind::Unknown, RefStr::new("")),
            has_error: false,
            text: buf,
            state,
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

    pub fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.token.kind != kind {
            self.error();
            Err(ParseError)
        } else {
            Ok(())
        }
    }

    pub fn consume(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.expect(kind).is_err() {
            Err(ParseError)
        } else {
            self.advance();
            Ok(())
        }
    }

    fn skip_until(&mut self, elems: &[TokenKind]) -> Result<(), ParseError> {
        while !self.token.is_one_of(elems) {
            self.advance();
        }
        Err(ParseError)
    }

    pub fn guard<F, S>(&mut self, f: F, elems: &[TokenKind]) -> Result<S, ParseError>
    where
        F: FnOnce(&mut Self) -> Result<S, ParseError>,
    {
        match f(self) {
            Ok(t) => Ok(t),
            Err(_err) => Err(self.skip_until(elems).unwrap_err()),
        }
    }

    pub fn parse_calc(&mut self) -> Result<Ast, ParseError> {
        self.guard(
            |p| {
                let mut vars = Vec::new();
                if p.token.kind ==TokenKind::KWWith {
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
                    Ok(Ast::Expr(e))
                } else {
                    let buf = p.text.index(..);
                    Ok(Ast::WithDecl(WithDecl::new(vars, buf, e)))
                }
            },
            &[TokenKind::Eoi],
        )
    }

    pub fn parse_expr(&mut self) -> Result<usize, ParseError> {
        let mut left = self.parse_term()?;
        while self.token.is_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.token.kind {
                TokenKind::Plus => Operator::Plus,
                TokenKind::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term()?;
            let binary_op = BinaryOp::new(left, right, op);
            let expr = Expr::BinaryOp(binary_op);
            let id = self.state.exprs.insert(expr);
            left = id;
        }
        Ok(left)
    }

    pub fn parse_term(&mut self) -> Result<usize, ParseError> {
        let mut left = self.parse_factor()?;
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                _ => Operator::Div,
            };
            self.advance();
            let right = self.parse_factor()?;
            let binary_op = BinaryOp::new(left, right, op);
            let expr = Expr::BinaryOp(binary_op);
            left = self.state.exprs.insert(expr);
        }
        Ok(left)
    }

    pub fn parse_factor(&mut self) -> Result<usize, ParseError> {
        self.guard(
            |p| match p.token.kind {
                TokenKind::Ident => {
                    let text = p.token.text.index(..);
                    let id = p
                        .state
                        .exprs
                        .insert(Expr::Factor(Factor::new(ValueKind::Ident, text)));
                    p.advance();
                    Ok(id)
                }
                TokenKind::Number => {
                    let text = p.token.text.index(..);
                    let id = p
                        .state
                        .exprs
                        .insert(Expr::Factor(Factor::new(ValueKind::Number, text)));
                    p.advance();
                    Ok(id)
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
