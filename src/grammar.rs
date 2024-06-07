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
