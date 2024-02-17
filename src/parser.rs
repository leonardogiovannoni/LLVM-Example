use crate::*;

pub struct Parser {
    pub lexer: Lexer,
    pub token: Token,
    pub has_error: bool,
    pub text: RefSlice<char>,
}

impl Parser {
    pub fn new(lexer: Lexer, buf: RefSlice<char>) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::new(TokenKind::Unknown, RefSlice::from([])),
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

    pub fn parse_calc_begin(&mut self) -> Option<Vec<RefSlice<char>>> {
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

    pub fn parse_calc_mid(&mut self, exprs: &State) -> Option<Ast> {
        let vars = self.parse_calc_begin()?;
        let text = self.text.index(..);
        let e = self.parse_expr(text.as_ref(), exprs)?;
        if self.expect(TokenKind::Eoi) {
            return None;
        }

        if vars.is_empty() {
            Some(e.into())
        } else {
            let buf = text.index(..);
            Some(WithDecl::new(vars, buf, Some(e)).into())
        }
    }

    pub fn parse_calc(&mut self, exprs: &State) -> Option<Ast> {
        self.parse_calc_mid(exprs).or_else(|| {
            while self.token.kind != TokenKind::Eoi {
                self.advance();
            }
            None
        })
    }

    pub fn parse_expr(&mut self, buf: &[char], state: &State) -> Option<ExprIndex> {
        let exprs = &state.exprs;
        let mut left = self.parse_term(buf, state);
        while self.token.is_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.token.kind {
                TokenKind::Plus => Operator::Plus,
                TokenKind::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term(buf, state);
            let binary_op = BinaryOp::new(left, right, op);
            exprs.borrow_mut().push(Expr::BinaryOp(binary_op));
            left = Some(ExprIndex(exprs.borrow().len() - 1));
        }
        left
    }

    pub fn parse_term(&mut self, buf: &[char], state: &State) -> Option<ExprIndex> {
        let mut left = self.parse_factor(buf, state);
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                _ => Operator::Div,
            };
            self.advance();
            let right = self.parse_factor(buf, state);
            let binary_op = BinaryOp::new(left, right, op);
            let exprs = &state.exprs;
            exprs.borrow_mut().push(Expr::BinaryOp(binary_op));
            left = Some(ExprIndex(exprs.borrow().len() - 1));
        }
        left
    }

    pub fn parse_factor(&mut self, buf: &[char], state: &State) -> Option<ExprIndex> {
        let mut res = None;
        let exprs = &state.exprs;
        match self.token.kind {
            TokenKind::Ident => {
                let text = self.token.text.index(..);
                exprs
                    .borrow_mut()
                    .push(Expr::Factor(Factor::new(ValueKind::Ident, text)));
                res = Some(ExprIndex(exprs.borrow().len() - 1));
                self.advance();
            }
            TokenKind::Number => {
                let text = self.text.index(..);
                exprs
                    .borrow_mut()
                    .push(Expr::Factor(Factor::new(ValueKind::Number, text)));
                res = Some(ExprIndex(exprs.borrow().len() - 1));
                self.advance();
            }
            TokenKind::LParen => {
                self.advance();
                res = self.parse_expr(buf, state);
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

                loop {
                    match self.token.kind {
                        TokenKind::Eoi
                        | TokenKind::RParen
                        | TokenKind::Slash
                        | TokenKind::Star
                        | TokenKind::Plus
                        | TokenKind::Minus => break,
                        _ => self.advance(),
                    }
                }
            }
        }
        res
    }

    pub fn parse(&mut self, exprs: &State) -> Option<Ast> {
        let ast = self.parse_calc(exprs);
        let _ = self.expect(TokenKind::Eoi);
        ast
    }
}
