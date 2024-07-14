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
    // Don't parse subsequent newlines
    is_newline: bool,
    // Used to track the beginnings and ends of () and {}
    parentheses_stack: u8,
    // Don't lex spaces and newlines inside () or {}
    ignore_whitespace: bool,
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
        keywords.insert("self".to_owned(), SelfToken);
        keywords.insert("return".to_owned(), Return);
        keywords.insert("super".to_owned(), Super);
        keywords.insert("this".to_owned(), This);
        keywords.insert("true".to_owned(), TrueToken);
        keywords.insert("var".to_owned(), VarToken);
        keywords.insert("while".to_owned(), WhileToken);
        keywords.insert("trait".to_owned(), TraitToken);
        keywords.insert("struct".to_owned(), StructToken);
        keywords.insert("impl".to_owned(), ImplToken);

        Lexer {
            tokens: vec![],
            current: 0,
            keywords,
            is_newline: true,
            parentheses_stack: 0,
            ignore_whitespace: false,
        }
    }

    pub fn lex(&mut self, program: &str) {
        let mut chars = program.char_indices().peekable();

        while let Some((index, char_)) = chars.next() {
            self.current = index;
            match char_ {
                '(' => {
                    self.parentheses_stack += 1;
                    self.ignore_whitespace = true;
                    self.add_token(1, LeftParen)
                }
                ')' => {
                    self.parentheses_stack -= 1;
                    self.ignore_whitespace = self.parentheses_stack != 0;
                    self.add_token(1, RightParen)
                }
                '+' => self.add_token(1, Plus),
                '*' => self.add_token(1, Star),
                '{' => {
                    self.parentheses_stack += 1;
                    self.ignore_whitespace = true;
                    self.add_token(1, LeftBrace)
                }
                '}' => {
                    self.parentheses_stack -= 1;
                    self.ignore_whitespace = self.parentheses_stack != 0;

                    self.add_token(1, RightBrace)
                }
                ',' => self.add_token(1, Comma),
                '.' => self.add_token(1, Dot),
                ':' => {
                    if self.match_(&mut chars, ':') {
                        self.add_token(2, DoubleColon)
                    } else {
                        self.add_token(1, Colon)
                    }
                }
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
                        while chars.peek().is_some() && !self.match_(&mut chars, '\n') {
                            chars.next();
                        }
                    } else {
                        self.add_token(1, Slash);
                    }
                }
                '"' => {
                    self.string(&mut chars);
                }
                '\n' => {
                    if !self.ignore_whitespace && !self.is_newline {
                        self.add_token(0, NewLine);
                    }

                    while self.match_(&mut chars, '\n') {}
                    self.is_newline = true
                }
                c => {
                    if c.is_whitespace() {
                        if !self.is_newline || self.ignore_whitespace {
                            continue;
                        }

                        let count = self.whitespace(c, &mut chars);

                        // Only add whitespace if there is a useful token in this line
                        // i.e. not newline or EOF
                        if !self.match_(&mut chars, '\n') && chars.peek().is_some() {
                            self.add_token(count, WhiteSpace(count));
                        }
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

        if Self::is_valid_identifier_char(first) {
            identifier.push(first);
        } else {
            // TODO: Turn into LexerError
            panic!("Unexpected '{}'", first);
        }

        while let Some((_index, char_)) = chars.peek() {
            if Self::is_valid_identifier_char(*char_) {
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

    fn is_valid_identifier_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn string(&mut self, chars: &mut Peekable<CharIndices<'_>>) {
        let mut str_lit = String::new();
        let mut reached_end_of_string = false;
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

    // Parses whitespace (spaces or tabs) and returns the parsed number
    // Does *not* add a token
    fn whitespace(&mut self, first: char, chars: &mut Peekable<CharIndices<'_>>) -> u8 {
        let mut whitespace_count = 1;

        while let Some((_index, char_)) = chars.peek() {
            // Assume responsible (non-mixed) input for now...
            if first == *char_ && (*char_ == ' ' || *char_ == '\t') {
                whitespace_count += 1;
                chars.next();
            } else {
                break;
            }
        }
        whitespace_count
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

    fn unpacked_tokens(tokens: Vec<Token>) -> Vec<TokenKind> {
        tokens
            .iter()
            .map(|tk| tk.kind.clone())
            .collect::<Vec<TokenKind>>()
    }

    #[test]
    fn test_var_while_loop() {
        let input = "
var i = 0
while i < 5
    i = i + 1
print i
        ";
        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            VarToken,
            IdToken("i".to_owned()),
            Equal,
            Num(0),
            NewLine,
            WhileToken,
            IdToken("i".to_owned()),
            Less,
            Num(5),
            NewLine,
            WhiteSpace(4),
            IdToken("i".to_owned()),
            Equal,
            IdToken("i".to_owned()),
            Plus,
            Num(1),
            NewLine,
            PrintToken,
            IdToken("i".to_owned()),
            NewLine,
            EndOfFile,
        ];

        assert_eq!(unpacked_tokens(lexer.tokens), expected);
    }

    #[test]
    fn test_if_print() {
        // Comments need a newline
        let input = "
// Just a comment
if 6 >= 2
    print 5
else
    print 9
    ";

        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            IfToken,
            Num(6),
            GreaterEqual,
            Num(2),
            NewLine,
            WhiteSpace(4),
            PrintToken,
            Num(5),
            NewLine,
            ElseToken,
            NewLine,
            WhiteSpace(4),
            PrintToken,
            Num(9),
            NewLine,
            EndOfFile,
        ];

        assert_eq!(unpacked_tokens(lexer.tokens), expected);
    }

    #[test]
    fn test_expression_precedence() {
        let input = "print 8 + 3 * 4 - 10 / 2";
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
            EndOfFile,
        ];

        assert_eq!(unpacked_tokens(lexer.tokens), expected);
    }

    #[test]
    fn test_ignore_whitespace_in_nested_parens() {
        let input = "
var x = Outer {
    field: 1,
    inner: Inner {
        field: 3,
    }
}
        
";
        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            VarToken,
            IdToken("x".into()),
            Equal,
            IdToken("Outer".into()),
            LeftBrace,
            IdToken("field".into()),
            Colon,
            Num(1),
            Comma,
            IdToken("inner".into()),
            Colon,
            IdToken("Inner".into()),
            LeftBrace,
            IdToken("field".into()),
            Colon,
            Num(3),
            Comma,
            RightBrace,
            RightBrace,
            NewLine,
            EndOfFile,
        ];

        assert_eq!(unpacked_tokens(lexer.tokens), expected);
    }

    #[test]
    fn test_trait_declaration() {
        let input = "
trait Shape
  calculate_area(self) -> i32
";
        let mut lexer = Lexer::new();
        lexer.lex(input);

        let expected = vec![
            TraitToken,
            IdToken("Shape".into()),
            NewLine,
            WhiteSpace(2),
            IdToken("calculate_area".into()),
            LeftParen,
            SelfToken,
            RightParen,
            Arrow,
            IdToken("i32".into()),
            NewLine,
            EndOfFile,
        ];

        assert_eq!(unpacked_tokens(lexer.tokens), expected);
    }
}
