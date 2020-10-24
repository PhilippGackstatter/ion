use crate::types::Token;
use crate::types::TokenKind::{self, *};
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Default)]
pub struct Lexer {
    pub tokens: Vec<Token>,
    current: usize,
    keywords: HashMap<String, TokenKind>,
    is_newline: bool,
}

impl Lexer {
    pub fn new() -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("and".to_owned(), And);
        keywords.insert("class".to_owned(), Class);
        keywords.insert("else".to_owned(), ElseToken);
        keywords.insert("false".to_owned(), FalseToken);
        keywords.insert("for".to_owned(), For);
        keywords.insert("fn".to_owned(), Fun);
        keywords.insert("if".to_owned(), IfToken);
        keywords.insert("or".to_owned(), Or);
        keywords.insert("print".to_owned(), PrintToken);
        keywords.insert("return".to_owned(), Return);
        keywords.insert("super".to_owned(), Super);
        keywords.insert("this".to_owned(), This);
        keywords.insert("true".to_owned(), TrueToken);
        keywords.insert("var".to_owned(), VarToken);
        keywords.insert("while".to_owned(), WhileToken);
        keywords.insert("struct".to_owned(), StructToken);
        keywords.insert("impl".to_owned(), ImplToken);

        Lexer {
            tokens: vec![],
            current: 0,
            keywords,
            is_newline: true,
        }
    }

    pub fn lex(&mut self, program: &str) {
        let mut chars = program.char_indices().peekable();

        while let Some((index, char_)) = chars.next() {
            self.current = index;
            match char_ {
                '(' => self.add_token(1, LeftParen),
                ')' => self.add_token(1, RightParen),
                '+' => self.add_token(1, Plus),
                '*' => self.add_token(1, Star),
                '{' => self.add_token(1, LeftBrace),
                '}' => self.add_token(1, RightBrace),
                ',' => self.add_token(1, Comma),
                '.' => self.add_token(1, Dot),
                ';' => self.add_token(1, Semicolon),
                ':' => self.add_token(1, Colon),
                '-' => {
                    if self.match_(&mut chars, '>') {
                        self.add_token(2, Arrow);
                    } else {
                        self.add_token(1, Minus);
                    }
                }
                '!' => {
                    if self.match_(&mut chars, '=') {
                        self.add_token(2, BangEqual);
                    } else {
                        self.add_token(1, Bang);
                    }
                }
                '=' => {
                    if self.match_(&mut chars, '=') {
                        self.add_token(2, EqualEqual);
                    } else {
                        self.add_token(1, Equal);
                    }
                }
                '>' => {
                    if self.match_(&mut chars, '=') {
                        self.add_token(2, GreaterEqual);
                    } else {
                        self.add_token(1, Greater);
                    }
                }
                '<' => {
                    if self.match_(&mut chars, '=') {
                        self.add_token(2, LessEqual);
                    } else {
                        self.add_token(1, Less);
                    }
                }
                '/' => {
                    if self.match_(&mut chars, '/') {
                        while !self.match_(&mut chars, '\n') {
                            chars.next();
                        }
                    } else {
                        self.add_token(1, Slash);
                    }
                }
                '"' => {
                    self.string(&mut chars);
                }
                '\n' => self.is_newline = true,
                c => {
                    if c.is_whitespace() {
                        if !self.is_newline {
                            continue;
                        }
                        self.whitespace(c, &mut chars);
                    } else if c.is_ascii_digit() {
                        self.number(c, &mut chars);
                    } else {
                        self.identifier(c, &mut chars);
                    }
                }
            }
        }
        self.add_token(1, EndOfFile);
    }

    fn identifier(&mut self, first: char, chars: &mut Peekable<CharIndices<'_>>) {
        let mut identifier = String::new();
        identifier.push(first);
        while let Some((_index, char_)) = chars.peek() {
            if *char_ == '\0' {
                panic!("Premature EOF");
            } else if char_.is_alphanumeric() || *char_ == '_' {
                identifier.push(*char_);
                chars.next();
            } else {
                break;
            }
        }

        if let Some(keyword) = self.keywords.get(&identifier) {
            let kw = keyword.clone();
            self.add_token(identifier.len() as u8, kw);
        } else {
            self.add_token(identifier.len() as u8, IdToken(identifier));
        }
    }

    fn string(&mut self, chars: &mut Peekable<CharIndices<'_>>) {
        let mut str_lit = String::new();
        let mut reached_end_of_string = false;
        // while let Some((_index, char_)) = chars.next() {
        for (_index, char_) in chars {
            if char_ != '"' {
                str_lit.push(char_);
            } else {
                reached_end_of_string = true;
                break;
            }
        }

        if !reached_end_of_string {
            panic!("Premature EOF");
        }

        self.add_token((str_lit.len() + 2) as u8, String_(str_lit));
    }

    fn number(&mut self, first: char, chars: &mut Peekable<CharIndices<'_>>) {
        let mut num_str = String::new();
        num_str.push(first);
        let mut is_float = false;
        while let Some((_index, char_)) = chars.peek() {
            if char_.is_ascii_digit() || *char_ == '.' {
                if *char_ == '.' {
                    is_float = true;
                }
                num_str.push(*char_);
                chars.next();
            } else {
                break;
            }
        }
        if is_float {
            self.add_token(num_str.len() as u8, FloatNum(num_str.parse().unwrap()));
        } else {
            self.add_token(num_str.len() as u8, Num(num_str.parse().unwrap()));
        }
    }

    fn whitespace(&mut self, first: char, chars: &mut Peekable<CharIndices<'_>>) {
        let mut whitespace_count = 1;

        while let Some((_index, char_)) = chars.peek() {
            // Assume responsible input for now...
            if first == *char_ && (*char_ == ' ' || *char_ == '\t') {
                whitespace_count += 1;
                chars.next();
            } else {
                self.add_token(whitespace_count, WhiteSpace(whitespace_count));
                break;
            }
        }
    }

    // Check if the next character matches the given one, and consume if it does
    fn match_(&mut self, chars: &mut Peekable<CharIndices<'_>>, check: char) -> bool {
        if let Some((_, char_)) = chars.peek() {
            if check == *char_ {
                chars.next();
                return true;
            }
        }
        false
    }

    fn add_token(&mut self, length: u8, kind: TokenKind) {
        self.is_newline = false;
        self.tokens
            .push(Token::new(self.current as u32, length, kind));
    }

    // Debug

    pub fn print_tokens(&self) {
        println!("\nTokens");
        println!("======");
        for token in self.tokens.iter() {
            println!("{:?}", token.kind);
        }
    }

    pub fn new_from_tokenkind(tokens: Vec<TokenKind>) -> Self {
        let mut lex = Lexer::new();
        lex.tokens = tokens
            .iter()
            .map(|tk| Token::new_debug(tk.clone()))
            .collect();
        lex
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_var_while_loop() {
        let input = r#"
            var i = 0;
            while (i < 5) i = i + 1;
            print i;
        "#;
        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            VarToken,
            IdToken("i".to_owned()),
            Equal,
            Num(0),
            Semicolon,
            WhileToken,
            LeftParen,
            IdToken("i".to_owned()),
            Less,
            Num(5),
            RightParen,
            IdToken("i".to_owned()),
            Equal,
            IdToken("i".to_owned()),
            Plus,
            Num(1),
            Semicolon,
            PrintToken,
            IdToken("i".to_owned()),
            Semicolon,
            EndOfFile,
        ];

        assert_eq!(
            lexer
                .tokens
                .iter()
                .map(|tk| tk.kind.clone())
                .collect::<Vec<TokenKind>>(),
            expected
        );
    }

    #[test]
    fn test_if_print() {
        // Comments need a newline
        let input = "// Just a comment\nif (6 >= 2) print 5; else print 9;";

        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            IfToken,
            LeftParen,
            Num(6),
            GreaterEqual,
            Num(2),
            RightParen,
            PrintToken,
            Num(5),
            Semicolon,
            ElseToken,
            PrintToken,
            Num(9),
            Semicolon,
            EndOfFile,
        ];

        assert_eq!(
            lexer
                .tokens
                .iter()
                .map(|tk| tk.kind.clone())
                .collect::<Vec<TokenKind>>(),
            expected
        );
    }

    #[test]
    fn test_expression_precedence() {
        let input = r#"
            print 8 + 3 * 4 - 10 / 2;
        "#;
        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            PrintToken,
            Num(8),
            Plus,
            Num(3),
            Star,
            Num(4),
            Minus,
            Num(10),
            Slash,
            Num(2),
            Semicolon,
            EndOfFile,
        ];

        assert_eq!(
            lexer
                .tokens
                .iter()
                .map(|tk| tk.kind.clone())
                .collect::<Vec<TokenKind>>(),
            expected
        );
    }
}
