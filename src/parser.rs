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

    pub fn parse_calc_begin(&mut self) -> Result<Vec<RefStr>, ParseError> {
        let mut vars = Vec::new();
        if self.token.is(TokenKind::KWWith) {
            self.advance();
            self.expect(TokenKind::Ident)?;

            vars.push(self.token.text.index(..));
            self.advance();
            while self.token.is(TokenKind::Comma) {
                self.advance();
                self.expect(TokenKind::Ident)?;
                vars.push(self.token.text.index(..));
                self.advance();
            }

            if self.consume(TokenKind::Colon).is_err() {
                return Err(ParseError);
            }
        }
        Ok(vars)
    }

    pub fn parse_calc_mid(&mut self) -> Result<Ast, ParseError> {
        let vars = self.parse_calc_begin()?;
        let e = self.parse_expr()?;
        self.expect(TokenKind::Eoi)?;

        if vars.is_empty() {
            Ok(Ast::Expr(e))
        } else {
            let buf = self.text.index(..);
            Ok(Ast::WithDecl(WithDecl::new(vars, buf, e)))
        }
    }

    pub fn parse_calc(&mut self) -> Result<Ast, ParseError> {
        match self.parse_calc_mid() {
            Ok(ast) => Ok(ast),
            Err(_) => {
                while self.token.kind != TokenKind::Eoi {
                    self.advance();
                }
                Err(ParseError)
            }
        }
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
        match self.token.kind {
            TokenKind::Ident => {
                let text = self.token.text.index(..);
                let id = self
                    .state
                    .exprs
                    .insert(Expr::Factor(Factor::new(ValueKind::Ident, text)));
                self.advance();
                return Ok(id);
            }
            TokenKind::Number => {
                let text = self.token.text.index(..);
                let id = self
                    .state
                    .exprs
                    .insert(Expr::Factor(Factor::new(ValueKind::Number, text)));
                self.advance();
                return Ok(id);
            }
            TokenKind::LParen => {
                self.advance();
                let res = self.parse_expr();
                self.consume(TokenKind::RParen)?;
                res
            }
            _ => {
                while !self.token.is_one_of(&[
                    TokenKind::Eoi,
                    TokenKind::RParen,
                    TokenKind::Slash,
                    TokenKind::Star,
                    TokenKind::Plus,
                    TokenKind::Minus,
                ]) {
                    self.advance();
                }
                Err(ParseError)
            }
        }
    }

    pub fn parse(&mut self) -> Result<Ast, Error> {
        let ast = self.parse_calc();
        let _ = self.expect(TokenKind::Eoi);
        ast.map_err(|_| anyhow::anyhow!("parse error"))
    }
}
