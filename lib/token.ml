type pos = { line : int; col : int; offset : int }
type span = { start : pos; stop : pos }

type t =
  (* Keywords *)
  | Fn
  | Let
  | Mut
  | If
  | Else
  | Match
  | Return
  | Struct
  | Enum
  | For
  | While
  | Loop
  | Break
  | Continue
  | In
  | As
  | Pub
  | Use
  | Mod
  | Impl
  | Trait
  | SelfType
  | SelfValue
  (* Boolean literals *)
  | True
  | False
  (* Type keywords *)
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | F32
  | F64
  | Bool
  | Str
  | StringType
  | OptionType
  | ResultType
  | VecType
  | HashMapType
  (* Literals *)
  | IntLit of string
  | FloatLit of string
  | StringLit of string
  (* Identifiers *)
  | Ident of string
  (* Operators and punctuation *)
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | EqEq
  | NotEq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | AmpAmp
  | PipePipe
  | Bang
  | Eq
  | PlusEq
  | MinusEq
  | StarEq
  | SlashEq
  | Arrow
  | FatArrow
  | ColonColon
  | Dot
  | Comma
  | Semi
  | Colon
  | Question
  | Amp
  | Pipe
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  (* Special *)
  | Underscore
  | Eof

type located = { token : t; span : span }

let show = function
  | Fn -> "fn"
  | Let -> "let"
  | Mut -> "mut"
  | If -> "if"
  | Else -> "else"
  | Match -> "match"
  | Return -> "return"
  | Struct -> "struct"
  | Enum -> "enum"
  | For -> "for"
  | While -> "while"
  | Loop -> "loop"
  | Break -> "break"
  | Continue -> "continue"
  | In -> "in"
  | As -> "as"
  | Pub -> "pub"
  | Use -> "use"
  | Mod -> "mod"
  | Impl -> "impl"
  | Trait -> "trait"
  | SelfType -> "Self"
  | SelfValue -> "self"
  | True -> "true"
  | False -> "false"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | F32 -> "f32"
  | F64 -> "f64"
  | Bool -> "bool"
  | Str -> "str"
  | StringType -> "String"
  | OptionType -> "Option"
  | ResultType -> "Result"
  | VecType -> "Vec"
  | HashMapType -> "HashMap"
  | IntLit s -> "IntLit(" ^ s ^ ")"
  | FloatLit s -> "FloatLit(" ^ s ^ ")"
  | StringLit s -> "StringLit(\"" ^ String.escaped s ^ "\")"
  | Ident s -> "Ident(" ^ s ^ ")"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | EqEq -> "=="
  | NotEq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | AmpAmp -> "&&"
  | PipePipe -> "||"
  | Bang -> "!"
  | Eq -> "="
  | PlusEq -> "+="
  | MinusEq -> "-="
  | StarEq -> "*="
  | SlashEq -> "/="
  | Arrow -> "->"
  | FatArrow -> "=>"
  | ColonColon -> "::"
  | Dot -> "."
  | Comma -> ","
  | Semi -> ";"
  | Colon -> ":"
  | Question -> "?"
  | Amp -> "&"
  | Pipe -> "|"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Underscore -> "_"
  | Eof -> "EOF"

let show_located loc =
  Printf.sprintf "%s@%d:%d" (show loc.token) loc.span.start.line
    loc.span.start.col
