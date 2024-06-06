#![allow(dead_code)]
#![allow(unused_doc_comments)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A regular expression can be considered a tree, where each node represents a
    /// component of the expression. We refer to these components as tokens. By
    /// definition, a token is recursive and can consist of other tokens.
    ///
    /// The base case of a token is either a specific literal character, any literal
    /// character, or no character at all. In the tree, this token is a leaf node.
    ///
    /// The general case of a token is a concatenator (i.e., this token followed by that
    /// token), a quantifier (i.e., this token repeated some number of times), or an
    /// alternator (i.e., this token or that token).
    Concatenator(Box<Token>, Box<Token>),
    Alternator(Box<Token>, Box<Token>),
    Plus(Box<Token>),
    Star(Box<Token>),
    Literal(char),
    Dot,
    None,
}

impl Token {
    pub fn new(expression: &str) -> Token {
        /// Generate a token representing a regular expression. This may act as the
        /// entry point for an entire expression, or for a group (i.e., parentheses).
        if expression.is_empty() {
            return Token::None;
        }
        let mut chars = expression.chars().peekable();
        Self::parse_sequence(&mut Self::parse_token(&mut chars), &mut chars)
    }

    fn parse_sequence(left: &mut Token, chars: &mut std::iter::Peekable<std::str::Chars>) -> Token {
        /// Handle a continuous concatenation (i.e., a sequence) of tokens.
        while let Some(&next) = chars.peek() {
            match next {
                // Case of quantifier (*). Its left side is wrapped with a Star, and
                // concatenated to a token representing its right side.
                '*' => {
                    chars.next();
                    let left_side = Token::Star(Box::new(left.clone()));
                    let right_side = Self::parse_token(chars);
                    *left = Token::Concatenator(Box::new(left_side), Box::new(right_side));
                }
                // Case of quantifier (+). Its left side is wrapped with a Plus, and
                // concatenated to a token representing its right side.
                '+' => {
                    chars.next();
                    let left_side = Token::Plus(Box::new(left.clone()));
                    let right_side = Self::parse_token(chars);
                    *left = Token::Concatenator(Box::new(left_side), Box::new(right_side));
                }
                // Case of alternator (|). Using an Alternator, its left side is wrapped
                // with a new token representing its right side.
                '|' => {
                    chars.next();
                    let right = Self::parse_token(chars);
                    *left = Token::Alternator(Box::new(left.clone()), Box::new(right));
                }
                // Case of group, or otherwise, a literal
                _ => {
                    let right = Self::parse_token(chars);
                    if let Token::None = right {
                    } else {
                        *left = Token::Concatenator(Box::new(left.clone()), Box::new(right));
                    }
                }
            }
        }
        left.clone()
    }

    fn parse_token(chars: &mut std::iter::Peekable<std::str::Chars>) -> Token {
        /// Handle either a literal character or a group. Used as an entry point to
        /// initialise the left side of a sequence, or otherwise, to parse as standard.
        match chars.next() {
            // Case of start of a group
            Some('(') => {
                let token = Self::new(&chars.into_iter().collect::<String>());
                chars.next();
                token
            }
            // Case of end of a group
            Some(')') => Token::None,
            // Case of literal
            Some('.') => Token::Dot,
            Some(' ') => Token::Literal(' '),
            Some(c) if c.is_ascii_alphanumeric() => Token::Literal(c),
            // Ignore character in any other case
            _ => Token::None,
        }
    }
}
