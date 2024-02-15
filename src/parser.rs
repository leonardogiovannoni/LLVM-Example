use crate::*;

pub struct Parser {
    pub lexer: Lexer,
    pub token: Token,
    pub has_error: bool,
    pub text: Rc<[char]>,
}

impl Parser {
    pub fn new(lexer: Lexer, buf: Rc<[char]>) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::new(TokenKind::Unknown, Span::new(0, 0)),
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

    pub fn parse_calc_begin<'a>(&mut self) -> Option<Vec<Span>> {
        let mut vars = Vec::new();
        if self.token.is(TokenKind::KWWith) {
            self.advance();
            if self.expect(TokenKind::Ident) {
                return None;
            }
            vars.push(Span::new(self.token.text.start, self.token.text.end));
            self.advance();
            while self.token.is(TokenKind::Comma) {
                self.advance();
                if self.expect(TokenKind::Ident) {
                    return None;
                }
                vars.push(Span::new(self.token.text.start, self.token.text.end));
                self.advance();
            }

            if self.consume(TokenKind::Colon) {
                return None;
            }
        }
        Some(vars)
    }

    pub fn parse_calc_mid<'a>(&mut self, exprs: &mut Vec<Expr>) -> Option<Ast> {
        let vars = self.parse_calc_begin()?;
        let text = Rc::clone(&self.text);
        let e = self.parse_expr(&text, exprs)?;
        if self.expect(TokenKind::Eoi) {
            return None;
        }

        if vars.is_empty() {
            Some(e.into())
        } else {
            let buf = Rc::clone(&self.text);
            Some(WithDecl::new(vars, buf, Some(e)).into())
        }
    }

    pub fn parse_calc<'a>(&mut self, exprs: &mut Vec<Expr>) -> Option<Ast> {
        self.parse_calc_mid(exprs).or_else(|| {
            while self.token.kind != TokenKind::Eoi {
                self.advance();
            }
            None
        })
    }

    pub fn parse_expr<'a>(&mut self, buf: &'a [char], exprs: &mut Vec<Expr>) -> Option<ExprIndex> {
        let mut left = self.parse_term(buf, exprs);
        while self.token.is_one_of(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.token.kind {
                TokenKind::Plus => Operator::Plus,
                TokenKind::Minus => Operator::Minus,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_term(buf, exprs);
            let binary_op = BinaryOp::new(left, right, op);
            exprs.push(Expr::BinaryOp(binary_op));
            left = Some(ExprIndex(exprs.len() - 1));
        }
        left
    }

    pub fn parse_term<'a>(&mut self, buf: &'a [char], exprs: &mut Vec<Expr>) -> Option<ExprIndex> {
        let mut left = self.parse_factor(buf, exprs);
        while self.token.is_one_of(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match self.token.kind {
                TokenKind::Star => Operator::Mul,
                _ => Operator::Div,
            };
            self.advance();
            let right = self.parse_factor(buf, exprs);
            let binary_op = BinaryOp::new(left, right, op);
            exprs.push(Expr::BinaryOp(binary_op));
            left = Some(ExprIndex(exprs.len() - 1));
        }
        left
    }

    pub fn parse_factor<'a>(
        &mut self,
        buf: &'a [char],
        exprs: &mut Vec<Expr>,
    ) -> Option<ExprIndex> {
        let mut res = None;
        match self.token.kind {
            TokenKind::Ident => {
                //let text = &buf[self.token.text.start..self.token.text.end];
                let text = Rc::clone(&self.text);
                let span = self.token.text;
                exprs.push(Expr::Factor(Factor::new(ValueKind::Ident, text, span)));
                res = Some(ExprIndex(exprs.len() - 1));
                self.advance();
            }
            TokenKind::Number => {
                let text = Rc::clone(&self.text);
                let span = self.token.text;
                exprs.push(Expr::Factor(Factor::new(ValueKind::Number, text, span)));
                res = Some(ExprIndex(exprs.len() - 1));
                self.advance();
            }
            TokenKind::LParen => {
                self.advance();
                res = self.parse_expr(buf, exprs);
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

    pub fn parse<'a>(&mut self, exprs: &mut Vec<Expr>) -> Option<Ast> {
        let ast = self.parse_calc(exprs);
        let _ = self.expect(TokenKind::Eoi);
        ast
    }
}
