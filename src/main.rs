use std::io;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(f64),
    Plus,
    Minus,
    Multiply,
    Divide,
    Exponent,
    LeftParen,
    RightParen,
    EndOfInput,
}

struct Lexer {
    chars: Vec<char>,
    index: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer {
            chars: input.chars().collect(),
            index: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.index < self.chars.len() && self.chars[self.index].is_whitespace() {
            self.index += 1;
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();
        if self.index >= self.chars.len() {
            return Ok(Token::EndOfInput);
        }

        let c = self.chars[self.index];
        match c {
            '+' => {
                self.index += 1;
                Ok(Token::Plus)
            }
            '-' => {
                self.index += 1;
                Ok(Token::Minus)
            }
            '*' => {
                self.index += 1;
                Ok(Token::Multiply)
            }
            '/' => {
                self.index += 1;
                Ok(Token::Divide)
            }
            '^' => {
                self.index += 1;
                Ok(Token::Exponent)
            }
            '(' => {
                self.index += 1;
                Ok(Token::LeftParen)
            }
            ')' => {
                self.index += 1;
                Ok(Token::RightParen)
            }
            '0'..='9' | '.' => self.scan_number(),
            _ => Err(format!("Unexpected character: '{}'", c)),
        }
    }

    fn scan_number(&mut self) -> Result<Token, String> {
        let mut s = String::new();
        let mut seen_dot = false;
        let mut seen_digit = false;

        if self.index < self.chars.len() && self.chars[self.index] == '.' {
            s.push('.');
            seen_dot = true;
            self.index += 1;
        }

        while self.index < self.chars.len() && self.chars[self.index].is_digit(10) {
            s.push(self.chars[self.index]);
            seen_digit = true;
            self.index += 1;
        }

        if !seen_dot && self.index < self.chars.len() && self.chars[self.index] == '.' {
            s.push('.');
            seen_dot = true;
            self.index += 1;
            while self.index < self.chars.len() && self.chars[self.index].is_digit(10) {
                s.push(self.chars[self.index]);
                seen_digit = true;
                self.index += 1;
            }
        }

        if !seen_digit {
            return Err("Invalid number: no digits".to_string());
        }

        match s.parse::<f64>() {
            Ok(num) => Ok(Token::Number(num)),
            Err(_) => Err(format!("Invalid number format: '{}'", s)),
        }
    }
}

#[derive(Debug)]
enum Expr {
    Number(f64),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
}

#[derive(Debug)]
enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponent,
}

#[derive(Debug)]
enum UnaryOp {
    Negate,
}

struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    fn new(input: &str) -> Result<Self, String> {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token()?;
        Ok(Parser {
            lexer,
            current_token,
        })
    }

    fn eat(&mut self, expected: Token) -> Result<(), String> {
        if std::mem::discriminant(&self.current_token) == std::mem::discriminant(&expected) {
            self.current_token = self.lexer.next_token()?;
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, found {:?}",
                expected, self.current_token
            ))
        }
    }

    fn parse(&mut self) -> Result<Expr, String> {
        self.expr()
    }

    fn expr(&mut self) -> Result<Expr, String> {
        let mut left = self.term()?;
        while matches!(self.current_token, Token::Plus | Token::Minus) {
            let op = match self.current_token {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Subtract,
                _ => unreachable!(),
            };
            self.eat(self.current_token.clone())?;
            let right = self.term()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut left = self.factor()?;
        while matches!(self.current_token, Token::Multiply | Token::Divide) {
            let op = match self.current_token {
                Token::Multiply => BinOp::Multiply,
                Token::Divide => BinOp::Divide,
                _ => unreachable!(),
            };
            self.eat(self.current_token.clone())?;
            let right = self.factor()?;
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        if self.current_token == Token::Minus {
            self.eat(Token::Minus)?;
            let expr = self.factor()?;
            return Ok(Expr::UnaryOp(UnaryOp::Negate, Box::new(expr)));
        }
        let mut base_expr = self.base()?;
        if self.current_token == Token::Exponent {
            self.eat(Token::Exponent)?;
            let exponent_expr = self.factor()?;
            base_expr = Expr::BinOp(
                Box::new(base_expr),
                BinOp::Exponent,
                Box::new(exponent_expr),
            );
        }
        Ok(base_expr)
    }

    fn base(&mut self) -> Result<Expr, String> {
        match self.current_token {
            Token::Number(num) => {
                self.eat(Token::Number(num))?;
                Ok(Expr::Number(num))
            }
            Token::LeftParen => {
                self.eat(Token::LeftParen)?;
                let expr = self.expr()?;
                self.eat(Token::RightParen)?;
                Ok(expr)
            }
            _ => Err(format!(
                "Expected number or '(', found {:?}",
                self.current_token
            )),
        }
    }
}

impl Expr {
    fn evaluate(&self) -> Result<f64, String> {
        match self {
            Expr::Number(n) => Ok(*n),
            Expr::BinOp(left, op, right) => {
                let left_val = left.evaluate()?;
                let right_val = right.evaluate()?;
                match op {
                    BinOp::Add => Ok(left_val + right_val),
                    BinOp::Subtract => Ok(left_val - right_val),
                    BinOp::Multiply => Ok(left_val * right_val),
                    BinOp::Divide => {
                        if right_val == 0.0 {
                            Err("Division by zero".to_string())
                        } else {
                            Ok(left_val / right_val)
                        }
                    }
                    BinOp::Exponent => Ok(left_val.powf(right_val)),
                }
            }
            Expr::UnaryOp(UnaryOp::Negate, expr) => {
                let val = expr.evaluate()?;
                Ok(-val)
            }
        }
    }
}

fn main() {
    println!("Enter an arithmetic expression (e.g., 2 + 3 * 4):");
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim();

    match Parser::new(input) {
        Ok(mut parser) => match parser.parse() {
            Ok(expr) => match expr.evaluate() {
                Ok(result) => println!("Result: {}", result),
                Err(e) => println!("Evaluation error: {}", e),
            },
            Err(e) => println!("Parse error: {}", e),
        },
        Err(e) => println!("Lexer error: {}", e),
    }
}
