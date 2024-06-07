/// Implementation of "Parsing Regex with Recursive Descent"
/// https://deniskyashif.com/2020/08/17/parsing-regex-with-recursive-descent/
///
/// <Expression> ::= <Term> | <Term>'|'<Expression>
/// <Term>       ::= <Factor> | <Factor><Term>
/// <Factor>     ::= <Atom> | <Atom><Quantifier>
/// <Atom>       ::= <Character> | '('<Expression>')'
/// <Character>  ::= <Literal> | '\'<Literal> | '.'
/// <Quantifier> ::= '?' | '*' | '+'
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// The structure of an entire expression, i.e., the highest level in the hierarchy.
    /// Either a single `Term`, or the disjunction of a `Term` and another `Expression`.
    Term(Box<Term>),
    Or(Box<Term>, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    /// A sequence of patterns that must match in succession (i.e., concatenation).
    /// Either a single `Factor`, or the sequence of a `Factor` and another `Term`.
    Factor(Box<Factor>),
    Sequence(Box<Factor>, Box<Term>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Factor {
    /// Either a singleton atom, or a quantified atom.
    Atom(Box<Atom>),
    Quantified(Box<Atom>, Box<Quantifier>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    /// The fundamental unit from which a regular expression is built.
    /// Either a single `Character`, or an `Expression` representing a group enclosed
    /// within parentheses in the expression.
    Character(Box<Character>),
    Expression(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Character {
    /// The literal, base content of an atom.
    /// Special characters (i.e., preceded by a backslash) are escaped.
    Literal(char),
    Escaped(char),
    Any,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Quantifier {
    /// Specifies how many times an atom should occur.
    /// Covers `?` (zero or one), `*` (zero or more), and `+` (one or more).
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

impl Expression {
    pub fn new(expression: &str) -> Self {
        let mut chars = expression.chars().peekable();
        Self::parse_expression(&mut chars)
    }

    fn parse_expression(chars: &mut Peekable<Chars>) -> Self {
        let term = Self::parse_term(chars);

        if chars.peek() == Some(&'|') {
            chars.next();
            let expr = Self::parse_expression(chars);
            Expression::Or(Box::new(term), Box::new(expr))
        } else {
            Expression::Term(Box::new(term))
        }
    }

    fn parse_term(chars: &mut Peekable<Chars>) -> Term {
        let factor = Self::parse_factor(chars);

        if let Some(&next) = chars.peek() {
            if next != '|' && next != ')' {
                let term = Self::parse_term(chars);
                return Term::Sequence(Box::new(factor), Box::new(term));
            }
        }
        Term::Factor(Box::new(factor))
    }

    fn parse_factor(chars: &mut Peekable<Chars>) -> Factor {
        let atom = Self::parse_atom(chars);

        match chars.peek() {
            Some(&'?') => {
                chars.next();
                Factor::Quantified(Box::new(atom), Box::new(Quantifier::ZeroOrOne))
            }
            Some(&'*') => {
                chars.next();
                Factor::Quantified(Box::new(atom), Box::new(Quantifier::ZeroOrMore))
            }
            Some(&'+') => {
                chars.next();
                Factor::Quantified(Box::new(atom), Box::new(Quantifier::OneOrMore))
            }
            _ => Factor::Atom(Box::new(atom)),
        }
    }

    fn parse_atom(chars: &mut Peekable<Chars>) -> Atom {
        match chars.next() {
            Some('(') => {
                let expr = Self::parse_expression(chars);
                chars.next();
                Atom::Expression(Box::new(expr))
            }
            Some('\\') => {
                let escaped_char = chars.next().expect("Expected character after backslash");
                Atom::Character(Box::new(Character::Escaped(escaped_char)))
            }
            Some('.') => Atom::Character(Box::new(Character::Any)),
            Some(literal) => Atom::Character(Box::new(Character::Literal(literal))),
            None => panic!("Unexpected end of input while parsing atom"),
        }
    }
}

#[cfg(test)]
#[test]
fn test_expression() {
    assert_eq!(
        Expression::new("a"),
        Expression::Term(Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
            Atom::Character(Box::new(Character::Literal('a')))
        ))))))
    )
}
