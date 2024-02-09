use crate::*;

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub token: Token<'a>,
    pub has_error: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            token: Token::new(TokenKind::Unknown, &['"', '"']),
            has_error: false,
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

    pub fn expect(&mut self, kind: TokenKind) -> bool {
        if self.token.kind != kind {
            self.error();
            true
        } else {
            false
        }
    }

    pub fn consume(&mut self, kind: TokenKind) -> bool {
        if self.expect(kind) {
            true
        } else {
            self.advance();
            false
        }
    }

    pub fn parse_calc_begin(&mut self) -> Option<Vec<&'a [char]>> {
        let mut vars = Vec::new();
        if self.token.is(TokenKind::KWWith) {
            self.advance();
            if self.expect(TokenKind::Ident) {
                return None;
            }
            vars.push(self.token.text);
            self.advance();
            while self.token.is(TokenKind::Comma) {
                self.advance();
                if self.expect(TokenKind::Ident) {
                    return None;
                }
                vars.push(self.token.text);
                self.advance();
            }

            if self.consume(TokenKind::Colon) {
                return None;
            }
        }
        Some(vars)
    }

    pub fn parse_calc_mid(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<AST<'a>> {
        let vars = self.parse_calc_begin()?;
        let e = self.parse_expr(exprs);
        let Some(e) = e else {
            return None;
        };
        if self.expect(TokenKind::Eoi) {
            return None;
        }

        if vars.is_empty() {
            Some(e.into())
        } else {
            Some(WithDecl::new(vars, Some(e)).into())
        }
    }

    pub fn parse_calc(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<AST<'a>> {
        self.parse_calc_mid(exprs).or_else(|| {
            while self.token.kind != TokenKind::Eoi {
                self.advance();
            }
            None
        })
    }

    pub fn parse_expr(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<ExprIndex> {
        let mut left = self.parse_term(exprs);
        while self.token.is_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.token.kind {
                TokenKind::Plus => Operator::Plus,
                TokenKind::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term(exprs);
            let binary_op = BinaryOp::new(left, right, op);
            exprs.push(binary_op.into());
            left = Some(ExprIndex(exprs.len() - 1));
        }
        left
    }

    pub fn parse_term(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<ExprIndex> {
        let mut left = self.parse_factor(exprs);
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                _ => Operator::Div,
            };
            self.advance();
            let right = self.parse_factor(exprs);
            let binary_op = BinaryOp::new(left, right, op);
            exprs.push(Expr::BinaryOp(binary_op));
            left = Some(ExprIndex(exprs.len() - 1));
        }
        left
    }

    pub fn parse_factor(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<ExprIndex> {
        let mut res = None;
        match self.token.kind {
            TokenKind::Ident => {
                exprs.push(Factor::new(ValueKind::Ident, self.token.text).into());
                res = Some(ExprIndex(exprs.len() - 1));
                self.advance();
            }
            TokenKind::Number => {
                exprs.push(Factor::new(ValueKind::Number, self.token.text).into());
                res = Some(ExprIndex(exprs.len() - 1));
                self.advance();
            }
            TokenKind::LParen => {
                self.advance();
                res = self.parse_expr(exprs);
                if !self.consume(TokenKind::RParen) {
                    if res.is_some() {
                        self.error();
                    }
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
                }
            }
            _ => {
                if res.is_some() {
                    self.error();
                }
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
            }
        }
        res
    }

    pub fn parse(&mut self, exprs: &mut Vec<Expr<'a>>) -> Option<AST<'a>> {
        let ast = self.parse_calc(exprs);
        let _ = self.expect(TokenKind::Eoi);
        ast
    }
}
