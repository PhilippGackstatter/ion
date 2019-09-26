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
}

impl Lexer {
    pub fn new() -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("and".to_owned(), And);
        keywords.insert("class".to_owned(), Class);
        keywords.insert("else".to_owned(), ElseToken);
        keywords.insert("false".to_owned(), FalseToken);
        keywords.insert("for".to_owned(), For);
        keywords.insert("fun".to_owned(), Fun);
        keywords.insert("if".to_owned(), IfToken);
        keywords.insert("or".to_owned(), Or);
        keywords.insert("print".to_owned(), PrintToken);
        keywords.insert("return".to_owned(), Return);
        keywords.insert("super".to_owned(), Super);
        keywords.insert("this".to_owned(), This);
        keywords.insert("true".to_owned(), TrueToken);
        keywords.insert("var".to_owned(), VarToken);
        keywords.insert("while".to_owned(), WhileToken);

        Lexer {
            tokens: vec![],
            current: 0,
            keywords,
        }
    }

    pub fn lex(&mut self, program: String) {
        let mut chars = program.char_indices().peekable();

        while let Some((index, char_)) = chars.next() {
            self.current = index;
            match char_ {
                '(' => self.add_token(1, LeftParen),
                ')' => self.add_token(1, RightParen),
                '+' => self.add_token(1, Plus),
                '-' => self.add_token(1, Minus),
                '*' => self.add_token(1, Star),
                '{' => self.add_token(1, LeftBrace),
                '}' => self.add_token(1, RightBrace),
                ',' => self.add_token(1, Comma),
                '.' => self.add_token(1, Dot),
                ';' => self.add_token(1, Semicolon),
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
                        while self.match_(&mut chars, '\n') {}
                    } else {
                        self.add_token(1, Slash);
                    }
                }
                '"' => {
                    self.string(&mut chars);
                }
                c => {
                    if c.is_whitespace() {
                        continue;
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
            } else if char_.is_alphanumeric() {
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
        while let Some((_index, char_)) = chars.next() {
            if char_ == '\0' {
                panic!("Premature EOF");
            } else if char_ != '"' {
                str_lit.push(char_);
            } else {
                break;
            }
        }

        self.add_token(str_lit.len() as u8, String_(str_lit));
    }

    fn number(&mut self, first: char, chars: &mut Peekable<CharIndices<'_>>) {
        let mut num_str = String::new();
        num_str.push(first);
        let mut is_float = false;
        while let Some((_index, char_)) = chars.peek() {
            if char_.is_ascii_digit() || *char_ == '.' {
                is_float = *char_ == '.';
                num_str.push(*char_);
                chars.next();
            } else {
                break;
            }
        }
        if is_float {
            self.add_token(num_str.len() as u8, Double(num_str.parse().unwrap()));
        } else {
            self.add_token(num_str.len() as u8, Num(num_str.parse().unwrap()));
        }
    }

    fn match_(&mut self, chars: &mut Peekable<CharIndices<'_>>, check: char) -> bool {
        if let Some((_, char_)) = chars.peek() {
            check == *char_
        } else {
            false
        }
    }

    fn add_token(&mut self, length: u8, kind: TokenKind) {
        self.tokens
            .push(Token::new(self.current as u32, length, kind));
    }

    pub fn print_tokens(&self) {
        println!("\nTokens");
        println!("======");
        for token in self.tokens.iter() {
            println!("{:?}", token.kind);
        }
    }

    pub fn new1() -> Self {
        // print 8 + 3 * 4 - 10 / 2;
        let tokens = vec![
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
            PrintToken,
            Num(55),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new2() -> Self {
        // if (6 > 2) print 5; else print 9;
        let tokens = vec![
            IfToken,
            LeftParen,
            Num(1),
            Greater,
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
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new3() -> Self {
        // var x = 5;
        // print x;
        let tokens = vec![
            VarToken,
            IdToken("x".to_owned()),
            Equal,
            Num(5),
            Semicolon,
            PrintToken,
            IdToken("x".to_owned()),
            Semicolon,
            EndOfFile,
        ];
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new4() -> Self {
        // var i = 0;
        // while (i < 5) i = i + 1;
        // print i;
        let tokens = vec![
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
        Lexer::new_from_tokenkind(tokens)
    }

    pub fn new_from_tokenkind(tokens: Vec<TokenKind>) -> Self {
        Lexer {
            tokens: tokens
                .iter()
                .map(|tk| Token::new_debug(tk.clone()))
                .collect(),
            current: 0,
            keywords: HashMap::new(),
        }
    }
}
