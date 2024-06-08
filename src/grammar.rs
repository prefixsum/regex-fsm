/// Implementation of "Parsing Regex with Recursive Descent"
/// https://deniskyashif.com/2020/08/17/parsing-regex-with-recursive-descent/
///
/// <Expression> ::= <Term> | <Term>'|'<Expression>
/// <Term>       ::= <Factor> | <Factor><Term>
/// <Factor>     ::= <Atom> | <Atom><Quantifier>
/// <Atom>       ::= <Character> | '('<Expression>')'
/// <Character>  ::= <char> | '\'<char> | '.'
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
    Character(Character),
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

    fn parse_expression(chars: &mut Peekable<Chars>) -> Expression {
        let term = Self::parse_term(chars);

        if chars.peek() == Some(&'|') {
            chars.next();
            let expression = Self::parse_expression(chars);
            Expression::Or(Box::new(term), Box::new(expression))
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
                let expression = Self::parse_expression(chars);
                chars.next();
                Atom::Expression(Box::new(expression))
            }
            Some('\\') => {
                let escaped_char = chars.next().expect("Expected character after backslash");
                Atom::Character(Character::Escaped(escaped_char))
            }
            Some('.') => Atom::Character(Character::Any),
            Some(literal) => Atom::Character(Character::Literal(literal)),
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
            Atom::Character(Character::Literal('a'))
        ))))))
    )
}

#[test]
fn test_escaped() {
    assert_eq!(
        Expression::new("\\*"),
        Expression::Term(Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
            Atom::Character(Character::Escaped('*'))
        ))))))
    )
}

#[test]
fn test_any() {
    assert_eq!(
        Expression::new("."),
        Expression::Term(Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
            Atom::Character(Character::Any)
        ))))))
    )
}

#[test]
fn test_sequence() {
    assert_eq!(
        Expression::new("ab"),
        Expression::Term(Box::new(Term::Sequence(
            Box::new(Factor::Atom(Box::new(Atom::Character(Character::Literal(
                'a'
            ))))),
            Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                Atom::Character(Character::Literal('b'))
            )))))
        )))
    )
}

#[test]
fn test_triple_sequence() {
    assert_eq!(
        Expression::new("abc"),
        Expression::Term(Box::new(Term::Sequence(
            Box::new(Factor::Atom(Box::new(Atom::Character(Character::Literal(
                'a'
            ))))),
            Box::new(Term::Sequence(
                Box::new(Factor::Atom(Box::new(Atom::Character(Character::Literal(
                    'b'
                ))))),
                Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                    Atom::Character(Character::Literal('c'))
                )))))
            ))
        )))
    )
}

#[test]
fn test_quantifier() {
    assert_eq!(
        Expression::new("ab*c"),
        Expression::Term(Box::new(Term::Sequence(
            Box::new(Factor::Atom(Box::new(Atom::Character(Character::Literal(
                'a'
            ))))),
            Box::new(Term::Sequence(
                Box::new(Factor::Quantified(
                    Box::new(Atom::Character(Character::Literal('b'))),
                    Box::new(Quantifier::ZeroOrMore)
                )),
                Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                    Atom::Character(Character::Literal('c'))
                )))))
            ))
        )))
    )
}

#[test]
fn test_disjunction() {
    assert_eq!(
        Expression::new("a|b"),
        Expression::Or(
            Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                Atom::Character(Character::Literal('a'))
            ))))),
            Box::new(Expression::Term(Box::new(Term::Factor(Box::new(
                Factor::Atom(Box::new(Atom::Character(Character::Literal('b'))))
            ))))),
        )
    )
}

#[test]
fn test_group() {
    assert_eq!(
        Expression::new("(a)b"),
        Expression::Term(Box::new(Term::Sequence(
            Box::new(Factor::Atom(Box::new(Atom::Expression(Box::new(
                Expression::Term(Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                    Atom::Character(Character::Literal('a'))
                ))))))
            ))))),
            Box::new(Term::Factor(Box::new(Factor::Atom(Box::new(
                Atom::Character(Character::Literal('b'))
            )))))
        )))
    )
}
