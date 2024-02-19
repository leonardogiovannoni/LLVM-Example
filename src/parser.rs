use crate::*;

pub struct Parser {
    pub lexer: Lexer,
    pub token: Token,
    pub has_error: bool,
    pub text: RefStr,
}

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

    pub fn parse_calc_begin(&mut self) -> Option<Vec<RefStr>> {
        let mut vars = Vec::new();
        if self.token.is(TokenKind::KWWith) {
            self.advance();
            if self.expect(TokenKind::Ident) {
                return None;
            }

            vars.push(self.token.text.index(..));
            self.advance();
            while self.token.is(TokenKind::Comma) {
                self.advance();
                if self.expect(TokenKind::Ident) {
                    return None;
                }
                vars.push(self.token.text.index(..));
                self.advance();
            }

            if self.consume(TokenKind::Colon) {
                return None;
            }
        }
        Some(vars)
    }

    pub fn parse_calc_mid(&mut self) -> Option<Ast> {
        let vars = self.parse_calc_begin()?;
        let e = self.parse_expr()?;
        if self.expect(TokenKind::Eoi) {
            return None;
        }

        if vars.is_empty() {
            Some(Ast::Expr(e))
        } else {
            let buf = self.text.index(..);
            Some(Ast::WithDecl(WithDecl::new(vars, buf, e)))
        }
    }

    pub fn parse_calc(&mut self) -> Option<Ast> {
        self.parse_calc_mid().or_else(|| {
            while self.token.kind != TokenKind::Eoi {
                self.advance();
            }
            None
        })
    }

    pub fn parse_expr(&mut self) -> Option<Rc<Expr>> {
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
            let id = Rc::new(Expr::BinaryOp(binary_op));
            left = id;
        }
        Some(left)
    }

    pub fn parse_term(&mut self) -> Option<Rc<Expr>> {
        let mut left = self.parse_factor()?;
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                _ => Operator::Div,
            };
            self.advance();
            let right = self.parse_factor()?;
            let binary_op = BinaryOp::new(left, right, op);
            let binary_op = Rc::new(Expr::BinaryOp(binary_op));
            left = binary_op;
        }
        Some(left)
    }

    pub fn parse_factor(&mut self) -> Option<Rc<Expr>> {
        let mut res = None;
        match self.token.kind {
            TokenKind::Ident => {
                let text = self.token.text.index(..);
                let id = Rc::new(Expr::Factor(Factor::new(ValueKind::Ident, text)));
                res = Some(id);
                self.advance();
            }
            TokenKind::Number => {
                let text = self.token.text.index(..);
                let id = Rc::new(Expr::Factor(Factor::new(ValueKind::Number, text)));
                res = Some(id);
                self.advance();
            }
            TokenKind::LParen => {
                self.advance();
                res = self.parse_expr();
                if !self.consume(TokenKind::RParen) {
                    return res;
                }
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
            }
        }
        res
    }

    pub fn parse(&mut self) -> Option<Ast> {
        let ast = self.parse_calc();
        let _ = self.expect(TokenKind::Eoi);
        ast
    }
}
