(* Type representation for the rgo type checker.
   These types are used for inference and checking; they are separate
   from the AST surface syntax types. *)

(* Type variables for inference *)
type tvar = int

(* The internal type representation *)
type ty =
  | TInt of int (* bit-width: 8,16,32,64 *)
  | TUint of int
  | TFloat of int (* 32 or 64 *)
  | TBool
  | TString
  | TVoid (* unit / no return value *)
  | TStruct of string * ty list (* name, type args *)
  | TEnum of string * ty list
  | TVec of ty
  | THashMap of ty * ty
  | TOption of ty
  | TResult of ty * ty
  | TRef of ty
  | TTuple of ty list
  | TFn of ty list * ty (* params, return *)
  | TVar of tvar (* unification variable *)
  | TParam of string (* generic type parameter like T *)
  | TSelf (* Self inside impl/trait *)
  | TImported of
      string * string (* package alias, type name e.g. "http", "Request" *)

let rec pp_ty fmt t =
  match t with
  | TInt 8 -> Format.fprintf fmt "i8"
  | TInt 16 -> Format.fprintf fmt "i16"
  | TInt 32 -> Format.fprintf fmt "i32"
  | TInt 64 -> Format.fprintf fmt "i64"
  | TInt n -> Format.fprintf fmt "i%d" n
  | TUint 8 -> Format.fprintf fmt "u8"
  | TUint 16 -> Format.fprintf fmt "u16"
  | TUint 32 -> Format.fprintf fmt "u32"
  | TUint 64 -> Format.fprintf fmt "u64"
  | TUint n -> Format.fprintf fmt "u%d" n
  | TFloat 32 -> Format.fprintf fmt "f32"
  | TFloat 64 -> Format.fprintf fmt "f64"
  | TFloat n -> Format.fprintf fmt "f%d" n
  | TBool -> Format.fprintf fmt "bool"
  | TString -> Format.fprintf fmt "str"
  | TVoid -> Format.fprintf fmt "()"
  | TStruct (name, []) -> Format.fprintf fmt "%s" name
  | TStruct (name, args) -> Format.fprintf fmt "%s<%a>" name pp_ty_list args
  | TEnum (name, []) -> Format.fprintf fmt "%s" name
  | TEnum (name, args) -> Format.fprintf fmt "%s<%a>" name pp_ty_list args
  | TVec t -> Format.fprintf fmt "Vec<%a>" pp_ty t
  | THashMap (k, v) -> Format.fprintf fmt "HashMap<%a, %a>" pp_ty k pp_ty v
  | TOption t -> Format.fprintf fmt "Option<%a>" pp_ty t
  | TResult (t, e) -> Format.fprintf fmt "Result<%a, %a>" pp_ty t pp_ty e
  | TRef t -> Format.fprintf fmt "&%a" pp_ty t
  | TTuple ts -> Format.fprintf fmt "(%a)" pp_ty_list ts
  | TFn (params, ret) ->
      Format.fprintf fmt "fn(%a) -> %a" pp_ty_list params pp_ty ret
  | TVar n -> Format.fprintf fmt "?%d" n
  | TParam s -> Format.fprintf fmt "%s" s
  | TSelf -> Format.fprintf fmt "Self"
  | TImported (pkg, name) -> Format.fprintf fmt "%s::%s" pkg name

and pp_ty_list fmt ts =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    pp_ty fmt ts

let show_ty t =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  pp_ty fmt t;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
