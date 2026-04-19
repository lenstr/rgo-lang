(** Internal type representation for the rgo type checker. *)

type tvar = int

type ty =
  | TInt of int
  | TUint of int
  | TFloat of int
  | TBool
  | TString
  | TVoid
  | TStruct of string * ty list
  | TEnum of string * ty list
  | TVec of ty
  | THashMap of ty * ty
  | TOption of ty
  | TResult of ty * ty
  | TRef of ty
  | TTuple of ty list
  | TFn of ty list * ty
  | TVar of tvar
  | TParam of string
  | TSelf

val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string
