type ident = string [@@deriving show]
type pos = { line : int; col : int } [@@deriving show]
type span = { start : pos; stop : pos } [@@deriving show]
type 'a located = { node : 'a; span : span } [@@deriving show]

(* ---------- types ---------- *)
type ty =
  | TyName of ident located
  | TyGeneric of ident located * ty list
  | TyRef of ty
  | TyTuple of ty list
  | TySelf
[@@deriving show]

(* ---------- patterns ---------- *)
type pat =
  | PatWild
  | PatBind of ident located
  | PatLit of lit
  | PatTuple of ident located * ident located * pat list
  | PatStruct of ident located * ident located * field_pat list
[@@deriving show]

and field_pat = { fp_name : ident located; fp_pat : pat option }
[@@deriving show]

(* ---------- literals ---------- *)
and lit =
  | LitInt of string
  | LitFloat of string
  | LitString of string
  | LitBool of bool
[@@deriving show]

(* ---------- expressions ---------- *)
type unop = Neg | Not [@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
[@@deriving show]

type assign_op = Assign | AddAssign | SubAssign | MulAssign | DivAssign
[@@deriving show]

type expr =
  | ExprLit of lit
  | ExprIdent of ident located
  | ExprSelf
  | ExprUnary of unop * expr
  | ExprBinary of binop * expr * expr
  | ExprCall of expr * expr list
  | ExprMethodCall of expr * ident located * expr list
  | ExprFieldAccess of expr * ident located
  | ExprPath of ident located * ident located
  | ExprStruct of ty * struct_field_init list
  | ExprStructVariant of ident located * ident located * struct_field_init list
  | ExprIf of expr * block * block option
  | ExprMatch of expr * match_arm list
  | ExprBlock of block
  | ExprReturn of expr option
  | ExprBreak
  | ExprContinue
  | ExprAssign of assign_op * expr * expr
  | ExprQuestion of expr
  | ExprArray of expr list
  | ExprRepeat of expr * expr
  | ExprIndex of expr * expr
  | ExprCast of expr * ty
  | ExprLoop of expr option * block
  | ExprWhile of expr * block
  | ExprFor of ident located * expr * block
[@@deriving show]

and struct_field_init = { sf_name : ident located; sf_expr : expr }
[@@deriving show]

and match_arm = { arm_pat : pat; arm_expr : expr } [@@deriving show]
and block = { stmts : stmt list; final_expr : expr option } [@@deriving show]

(* ---------- statements ---------- *)
and stmt =
  | StmtLet of { is_mut : bool; pat : pat; ty : ty option; init : expr }
  | StmtExpr of expr
[@@deriving show]

(* ---------- self param ---------- *)
type self_param = SelfValue | SelfRef | SelfMutRef [@@deriving show]

(* ---------- function parameters ---------- *)
type param = { p_mut : bool; p_name : ident located; p_ty : ty }
[@@deriving show]

(* ---------- type parameters (generics) ---------- *)
type trait_bound = ident located list [@@deriving show]

type type_param = { tp_name : ident located; tp_bound : trait_bound option }
[@@deriving show]

(* ---------- top-level items ---------- *)
type fn_decl = {
  fn_pub : bool;
  fn_name : ident located;
  fn_generics : type_param list;
  fn_self : self_param option;
  fn_params : param list;
  fn_ret : ty option;
  fn_body : block;
}
[@@deriving show]

type fn_sig = {
  sig_name : ident located;
  sig_generics : type_param list;
  sig_self : self_param option;
  sig_params : param list;
  sig_ret : ty option;
}
[@@deriving show]

type field = { fd_pub : bool; fd_name : ident located; fd_ty : ty }
[@@deriving show]

type variant_fields = TupleFields of ty list | StructFields of field list
[@@deriving show]

type variant = { var_name : ident located; var_fields : variant_fields option }
[@@deriving show]

type trait_item = TraitFnSig of fn_sig | TraitFnDecl of fn_decl
[@@deriving show]

type item =
  | ItemFn of fn_decl
  | ItemStruct of {
      s_pub : bool;
      s_name : ident located;
      s_generics : type_param list;
      s_fields : field list;
    }
  | ItemEnum of {
      e_pub : bool;
      e_name : ident located;
      e_generics : type_param list;
      e_variants : variant list;
    }
  | ItemImpl of {
      i_generics : type_param list;
      i_ty : ty;
      i_items : fn_decl list;
    }
  | ItemTraitImpl of {
      ti_generics : type_param list;
      ti_trait : ident located;
      ti_ty : ty;
      ti_items : fn_decl list;
    }
  | ItemTrait of {
      t_pub : bool;
      t_name : ident located;
      t_generics : type_param list;
      t_items : trait_item list;
    }
[@@deriving show]

type program = { items : item list } [@@deriving show]
