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
  | SelfType  (** Self *)
  | SelfValue  (** self *)
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
  | Arrow  (** -> *)
  | FatArrow  (** => *)
  | ColonColon  (** :: *)
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

val show : t -> string
val show_located : located -> string
