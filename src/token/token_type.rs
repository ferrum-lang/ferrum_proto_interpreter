use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-char tokens
    LeftParen,  // (
    RightParen, // )

    LeftBrace,  // {
    RightBrace, // }

    LeftBracket,  // [
    RightBracket, // ]

    Comma,     // ,
    Semicolon, // ;
    At,        // @
    Dollar,    // $

    // One or more char tokens
    Dot,    // .
    DotDot, // ..

    Colon,      // :
    ColonColon, // ::

    Minus,        // -
    MinusEqual,   // -=
    MinusGreater, // ->

    Plus,      // +
    PlusEqual, // +=

    Slash,      // /
    SlashEqual, // /=

    Asterisk,      // *
    AsteriskEqual, // *=

    Percent,      // %
    PercentEqual, // %=

    Carat,      // ^
    CaratEqual, // ^=

    Bang,      // !
    BangEqual, // !=

    Equal,        // =
    EqualEqual,   // ==
    EqualGreater, // =>

    Greater,        // >
    GreaterGreater, // >>
    GreaterEqual,   // >=
    GreaterDot,     // >.

    Less,      // <
    LessEqual, // <=

    Tilde,      // ~
    TildeEqual, // ~=

    Question,         // ?
    QuestionQuestion, // ??

    Amp,    // &
    AmpAmp, // &&

    Hash,            // #
    HashLeftBrace,   // #{
    HashLeftBracket, // #[

    BarBar, // ||

    // Literals
    Identifier,

    PlainString,
    FormatStringOpen,
    FormatStringMid,
    FormatStringClose,

    Char,

    Number,

    // Keywords
    And,
    As,
    Const,
    Else,
    False,
    Fn,
    For,
    If,
    Impl,
    In,
    Match,
    Mut,
    Or,
    Pub,
    Pure,
    Return,
    Safe,
    SelfVal,  // self
    SelfType, // Self
    Struct,
    Trait,
    True,
    Type,
    Unsafe,
    Use,
    While,
    Yield,

    // Markers
    Newline,
    EOF, // End of file
}
