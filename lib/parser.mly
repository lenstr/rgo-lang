%{
open Ast

let mk_span (s, e) =
  let sp = s.Lexing.pos_lnum in
  let sc = s.Lexing.pos_cnum - s.Lexing.pos_bol + 1 in
  let ep = e.Lexing.pos_lnum in
  let ec = e.Lexing.pos_cnum - e.Lexing.pos_bol + 1 in
  { start = { line = sp; col = sc }; stop = { line = ep; col = ec } }

let mk_loc node (s, e) = { node; span = mk_span (s, e) }
%}

(* ---- tokens ---- *)
%token FN LET MUT IF ELSE MATCH RETURN STRUCT ENUM FOR WHILE LOOP
%token BREAK CONTINUE IN AS PUB MOD IMPL TRAIT
%token USE
%token SELF_TYPE SELF_VALUE
%token TRUE FALSE
%token I8 I16 I32 I64 U8 U16 U32 U64 F32 F64 BOOL_TYPE STR STRING_TYPE
%token OPTION_TYPE RESULT_TYPE VEC_TYPE HASHMAP_TYPE
%token <string> INT_LIT FLOAT_LIT STRING_LIT IDENT
%token PLUS MINUS STAR SLASH PERCENT
%token EQEQ NEQ LT GT LTEQ GTEQ AMPAMP PIPEPIPE BANG
%token EQ PLUSEQ MINUSEQ STAREQ SLASHEQ
%token ARROW FAT_ARROW COLON_COLON DOT COMMA SEMI COLON QUESTION AMP
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token UNDERSCORE
%token EOF

%start <Ast.program> program

%%

program:
  | us = list(use_decl); is = list(item); EOF { { imports = us; items = is } }
  ;

use_decl:
  | USE; segs = separated_nonempty_list(COLON_COLON, located_ident); SEMI
    { { imp_segments = segs;
        imp_span = mk_span ($startpos, $endpos) } }
  ;

item:
  | f = fn_decl_item       { ItemFn f }
  | s = struct_item         { s }
  | e = enum_item           { e }
  | i = impl_item           { i }
  | t = trait_item_decl     { t }
  ;

(* ===================== FUNCTIONS ===================== *)
fn_decl_item:
  | p = boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN; ps = params_list; RPAREN;
    ret = ret_ty; body = block_expr
    { { fn_pub = p; fn_name = name; fn_generics = gs;
        fn_self = None; fn_params = ps; fn_ret = ret; fn_body = body } }
  ;

fn_decl_method:
  | p = boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN;
    sp = self_param; COMMA; ps = params_list;
    RPAREN; ret = ret_ty; body = block_expr
    { { fn_pub = p; fn_name = name; fn_generics = gs;
        fn_self = Some sp; fn_params = ps; fn_ret = ret; fn_body = body } }
  | p = boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN;
    sp = self_param;
    RPAREN; ret = ret_ty; body = block_expr
    { { fn_pub = p; fn_name = name; fn_generics = gs;
        fn_self = Some sp; fn_params = []; fn_ret = ret; fn_body = body } }
  | f = fn_decl_item { f }
  ;



ret_ty:
  |                     { None }
  | ARROW; t = ty       { Some t }
  ;

self_param:
  | AMP; MUT; SELF_VALUE    { SelfMutRef }
  | AMP; SELF_VALUE         { SelfRef }
  | SELF_VALUE              { SelfValue }
  ;

params_list:
  | ps = separated_list(COMMA, param) { ps }
  ;

param:
  | m = boption(MUT); name = located_ident; COLON; t = ty
    { { p_mut = m; p_name = name; p_ty = t } }
  ;

generics:
  |                                                        { [] }
  | LT; tps = separated_nonempty_list(COMMA, type_param); GT { tps }
  ;

type_param:
  | name = located_ident; COLON;
    bounds = separated_nonempty_list(PLUS, located_ident)
    { { tp_name = name; tp_bound = Some bounds } }
  | name = located_ident
    { { tp_name = name; tp_bound = None } }
  ;

(* ===================== TYPES ===================== *)
ty:
  | AMP; MUT; t = ty_atom { TyRef t }
  | AMP; t = ty_atom      { TyRef t }
  | t = ty_atom           { t }
  ;

ty_atom:
  | SELF_TYPE             { TySelf }
  | LPAREN; ts = separated_nonempty_list(COMMA, ty); RPAREN
    { match ts with [t] -> t | _ -> TyTuple ts }
  | n = located_type_name; LT; args = separated_nonempty_list(COMMA, ty); GT
    { TyGeneric (n, args) }
  | n = located_type_name
    { TyName n }
  ;

type_name:
  | s = IDENT   { s }
  | I8      { "i8" }
  | I16     { "i16" }
  | I32     { "i32" }
  | I64     { "i64" }
  | U8      { "u8" }
  | U16     { "u16" }
  | U32     { "u32" }
  | U64     { "u64" }
  | F32     { "f32" }
  | F64     { "f64" }
  | BOOL_TYPE { "bool" }
  | STR     { "str" }
  | STRING_TYPE { "String" }
  | OPTION_TYPE { "Option" }
  | RESULT_TYPE { "Result" }
  | VEC_TYPE   { "Vec" }
  | HASHMAP_TYPE { "HashMap" }
  ;

(* ===================== STRUCT ===================== *)
struct_item:
  | p = boption(PUB); STRUCT; name = located_ident;
    gs = generics;
    LBRACE; fs = trailing_list(COMMA, field); RBRACE
    { ItemStruct { s_pub = p; s_name = name; s_generics = gs; s_fields = fs } }
  ;

field:
  | p = boption(PUB); name = located_ident; COLON; t = ty
    { { fd_pub = p; fd_name = name; fd_ty = t } }
  ;

(* ===================== ENUM ===================== *)
enum_item:
  | p = boption(PUB); ENUM; name = located_ident;
    gs = generics;
    LBRACE; vs = trailing_list(COMMA, variant); RBRACE
    { ItemEnum { e_pub = p; e_name = name; e_generics = gs; e_variants = vs } }
  ;

variant:
  | name = located_ident
    { { var_name = name; var_fields = None } }
  | name = located_ident; LPAREN;
    ts = separated_nonempty_list(COMMA, ty); RPAREN
    { { var_name = name; var_fields = Some (TupleFields ts) } }
  | name = located_ident; LBRACE;
    fs = trailing_list(COMMA, field); RBRACE
    { { var_name = name; var_fields = Some (StructFields fs) } }
  ;

(* ===================== IMPL ===================== *)
impl_item:
  | IMPL; gs = generics; tr = located_ident; FOR; t = ty;
    LBRACE; ms = list(fn_decl_method); RBRACE
    { ItemTraitImpl { ti_generics = gs; ti_trait = tr;
                      ti_ty = t; ti_items = ms } }
  | IMPL; gs = generics; t = ty;
    LBRACE; ms = list(fn_decl_method); RBRACE
    { ItemImpl { i_generics = gs; i_ty = t; i_items = ms } }
  ;

(* ===================== TRAIT ===================== *)
trait_item_decl:
  | p = boption(PUB); TRAIT; name = located_ident;
    gs = generics; LBRACE; tis = list(trait_member); RBRACE
    { ItemTrait { t_pub = p; t_name = name; t_generics = gs; t_items = tis } }
  ;

trait_member:
  (* A trait member is either a signature (ending with ;) or a default body.
     Since the prefix FN name generics (params) ret_ty is shared, we factor
     it out: parse the common prefix, then branch on ; vs { *)
  | boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN;
    sp = self_param; COMMA; ps = params_list;
    RPAREN; ret = ret_ty; SEMI
    { TraitFnSig { sig_name = name; sig_generics = gs;
                   sig_self = Some sp; sig_params = ps; sig_ret = ret } }
  | boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN;
    sp = self_param;
    RPAREN; ret = ret_ty; SEMI
    { TraitFnSig { sig_name = name; sig_generics = gs;
                   sig_self = Some sp; sig_params = []; sig_ret = ret } }
  | boption(PUB); FN; name = located_ident;
    gs = generics; LPAREN; ps = params_list; RPAREN;
    ret = ret_ty; SEMI
    { TraitFnSig { sig_name = name; sig_generics = gs;
                   sig_self = None; sig_params = ps; sig_ret = ret } }
  | d = fn_decl_method { TraitFnDecl d }
  ;

(* ===================== BLOCKS & STMTS ===================== *)
block_expr:
  | LBRACE; b = block_body; RBRACE { b }
  ;

block_body:
  | (* empty *)            { { stmts = []; final_expr = None } }
  | s = stmt_semi; b = block_body
    { { b with stmts = s :: b.stmts } }
  (* block-like expressions don't need semicolons in stmt position *)
  | e = expr_block_like; b = block_body
    { { b with stmts = (StmtExpr e) :: b.stmts } }
  | e = expr               { { stmts = []; final_expr = Some e } }
  ;

stmt_semi:
  | LET; m = boption(MUT); name = located_ident;
    t = option(preceded(COLON, ty)); EQ; e = expr; SEMI
    { StmtLet { is_mut = m; pat = PatBind name; ty = t; init = e } }
  | LET; UNDERSCORE;
    t = option(preceded(COLON, ty)); EQ; e = expr; SEMI
    { StmtLet { is_mut = false; pat = PatWild; ty = t; init = e } }
  | e = expr; SEMI
    { StmtExpr e }
  ;

(* Expressions that end with a block and don't need ; in statement position *)
expr_block_like:
  | IF; c = expr_no_struct; tb = block_expr;
    ELSE; eb = else_branch
    { ExprIf (c, tb, Some eb) }
  | IF; c = expr_no_struct; tb = block_expr
    { ExprIf (c, tb, None) }
  | MATCH; e = expr_no_struct;
    LBRACE; arms = trailing_list(COMMA, match_arm); RBRACE
    { ExprMatch (e, arms) }
  | LOOP; b = block_expr
    { ExprLoop (None, b) }
  | WHILE; c = expr_no_struct; b = block_expr
    { ExprWhile (c, b) }
  | FOR; name = located_ident; IN; iter = expr_no_struct; b = block_expr
    { ExprFor (name, iter, b) }
  | b = block_expr
    { ExprBlock b }
  ;

(* ===================== EXPRESSIONS ===================== *)
(* We use a recursive descent approach with explicit precedence levels *)

expr:
  | RETURN; e = expr          { ExprReturn (Some e) }
  | RETURN                    { ExprReturn None }
  | BREAK                     { ExprBreak }
  | CONTINUE                  { ExprContinue }
  | e = expr_assign           { e }
  ;

(* expr without struct literal — used in if/match/while/for conditions *)
expr_no_struct:
  | RETURN; e = expr          { ExprReturn (Some e) }
  | RETURN                    { ExprReturn None }
  | BREAK                     { ExprBreak }
  | CONTINUE                  { ExprContinue }
  | e = expr_assign_no_struct { e }
  ;

expr_assign_no_struct:
  | l = expr_or_no_struct; EQ; r = expr_assign      { ExprAssign (Assign, l, r) }
  | l = expr_or_no_struct; PLUSEQ; r = expr_assign   { ExprAssign (AddAssign, l, r) }
  | l = expr_or_no_struct; MINUSEQ; r = expr_assign  { ExprAssign (SubAssign, l, r) }
  | l = expr_or_no_struct; STAREQ; r = expr_assign   { ExprAssign (MulAssign, l, r) }
  | l = expr_or_no_struct; SLASHEQ; r = expr_assign  { ExprAssign (DivAssign, l, r) }
  | e = expr_or_no_struct                             { e }
  ;

expr_or_no_struct:
  | l = expr_or_no_struct; PIPEPIPE; r = expr_and_no_struct  { ExprBinary (Or, l, r) }
  | e = expr_and_no_struct                                    { e }
  ;

expr_and_no_struct:
  | l = expr_and_no_struct; AMPAMP; r = expr_eq_no_struct    { ExprBinary (And, l, r) }
  | e = expr_eq_no_struct                                     { e }
  ;

expr_eq_no_struct:
  | l = expr_eq_no_struct; EQEQ; r = expr_cmp_no_struct     { ExprBinary (Eq, l, r) }
  | l = expr_eq_no_struct; NEQ; r = expr_cmp_no_struct       { ExprBinary (Ne, l, r) }
  | e = expr_cmp_no_struct                                    { e }
  ;

expr_cmp_no_struct:
  | l = expr_cmp_no_struct; LT; r = expr_add_no_struct       { ExprBinary (Lt, l, r) }
  | l = expr_cmp_no_struct; GT; r = expr_add_no_struct        { ExprBinary (Gt, l, r) }
  | l = expr_cmp_no_struct; LTEQ; r = expr_add_no_struct     { ExprBinary (Le, l, r) }
  | l = expr_cmp_no_struct; GTEQ; r = expr_add_no_struct     { ExprBinary (Ge, l, r) }
  | e = expr_add_no_struct                                    { e }
  ;

expr_add_no_struct:
  | l = expr_add_no_struct; PLUS; r = expr_mul_no_struct     { ExprBinary (Add, l, r) }
  | l = expr_add_no_struct; MINUS; r = expr_mul_no_struct    { ExprBinary (Sub, l, r) }
  | e = expr_mul_no_struct                                    { e }
  ;

expr_mul_no_struct:
  | l = expr_mul_no_struct; STAR; r = expr_cast_no_struct    { ExprBinary (Mul, l, r) }
  | l = expr_mul_no_struct; SLASH; r = expr_cast_no_struct   { ExprBinary (Div, l, r) }
  | l = expr_mul_no_struct; PERCENT; r = expr_cast_no_struct { ExprBinary (Mod, l, r) }
  | e = expr_cast_no_struct                                   { e }
  ;

expr_cast_no_struct:
  | e = expr_cast_no_struct; AS; t = ty  { ExprCast (e, t) }
  | e = expr_unary_no_struct              { e }
  ;

expr_unary_no_struct:
  | MINUS; e = expr_unary_no_struct  { ExprUnary (Neg, e) }
  | BANG; e = expr_unary_no_struct   { ExprUnary (Not, e) }
  | e = expr_postfix_no_struct       { e }
  ;

expr_postfix_no_struct:
  | e = expr_postfix_no_struct; QUESTION    { ExprQuestion e }
  | e = expr_postfix_no_struct; DOT; f = located_ident; LPAREN;
    args = separated_list(COMMA, expr); RPAREN
    { ExprMethodCall (e, f, args) }
  | e = expr_postfix_no_struct; DOT; f = located_ident
    { ExprFieldAccess (e, f) }
  | e = expr_postfix_no_struct; LBRACKET; i = expr; RBRACKET
    { ExprIndex (e, i) }
  | e = expr_call_no_struct    { e }
  ;

expr_call_no_struct:
  | f = expr_call_no_struct; LPAREN;
    args = separated_list(COMMA, expr); RPAREN
    { ExprCall (f, args) }
  | e = expr_primary_no_struct   { e }
  ;

expr_primary_no_struct:
  | n = INT_LIT               { ExprLit (LitInt n) }
  | n = FLOAT_LIT             { ExprLit (LitFloat n) }
  | s = STRING_LIT            { ExprLit (LitString s) }
  | TRUE                      { ExprLit (LitBool true) }
  | FALSE                     { ExprLit (LitBool false) }
  | SELF_VALUE                { ExprSelf }
  | a = located_ident; COLON_COLON; b = located_ident
    { ExprPath (a, b) }
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident
    { ExprPath (a, b) }
  (* NO struct literal here — that's the point *)
  | id = located_ident        { ExprIdent id }
  | IF; c = expr_no_struct; tb = block_expr;
    ELSE; eb = else_branch
    { ExprIf (c, tb, Some eb) }
  | IF; c = expr_no_struct; tb = block_expr
    { ExprIf (c, tb, None) }
  | MATCH; e = expr_no_struct;
    LBRACE; arms = trailing_list(COMMA, match_arm); RBRACE
    { ExprMatch (e, arms) }
  | b = block_expr            { ExprBlock b }
  | LOOP; b = block_expr
    { ExprLoop (None, b) }
  | WHILE; c = expr_no_struct; b = block_expr
    { ExprWhile (c, b) }
  | FOR; name = located_ident; IN; iter = expr_no_struct; b = block_expr
    { ExprFor (name, iter, b) }
  | LPAREN; e = expr; RPAREN  { e }
  | LBRACKET; RBRACKET
    { ExprArray [] }
  | LBRACKET; e = expr; SEMI; n = expr; RBRACKET
    { ExprRepeat (e, n) }
  | LBRACKET; es = separated_nonempty_list(COMMA, expr); option(COMMA); RBRACKET
    { ExprArray es }
  ;

expr_assign:
  | l = expr_or; EQ; r = expr_assign      { ExprAssign (Assign, l, r) }
  | l = expr_or; PLUSEQ; r = expr_assign   { ExprAssign (AddAssign, l, r) }
  | l = expr_or; MINUSEQ; r = expr_assign  { ExprAssign (SubAssign, l, r) }
  | l = expr_or; STAREQ; r = expr_assign   { ExprAssign (MulAssign, l, r) }
  | l = expr_or; SLASHEQ; r = expr_assign  { ExprAssign (DivAssign, l, r) }
  | e = expr_or                             { e }
  ;

expr_or:
  | l = expr_or; PIPEPIPE; r = expr_and    { ExprBinary (Or, l, r) }
  | e = expr_and                            { e }
  ;

expr_and:
  | l = expr_and; AMPAMP; r = expr_eq      { ExprBinary (And, l, r) }
  | e = expr_eq                             { e }
  ;

expr_eq:
  | l = expr_eq; EQEQ; r = expr_cmp       { ExprBinary (Eq, l, r) }
  | l = expr_eq; NEQ; r = expr_cmp         { ExprBinary (Ne, l, r) }
  | e = expr_cmp                            { e }
  ;

expr_cmp:
  | l = expr_cmp; LT; r = expr_add        { ExprBinary (Lt, l, r) }
  | l = expr_cmp; GT; r = expr_add         { ExprBinary (Gt, l, r) }
  | l = expr_cmp; LTEQ; r = expr_add       { ExprBinary (Le, l, r) }
  | l = expr_cmp; GTEQ; r = expr_add       { ExprBinary (Ge, l, r) }
  | e = expr_add                            { e }
  ;

expr_add:
  | l = expr_add; PLUS; r = expr_mul       { ExprBinary (Add, l, r) }
  | l = expr_add; MINUS; r = expr_mul      { ExprBinary (Sub, l, r) }
  | e = expr_mul                            { e }
  ;

expr_mul:
  | l = expr_mul; STAR; r = expr_cast      { ExprBinary (Mul, l, r) }
  | l = expr_mul; SLASH; r = expr_cast     { ExprBinary (Div, l, r) }
  | l = expr_mul; PERCENT; r = expr_cast   { ExprBinary (Mod, l, r) }
  | e = expr_cast                           { e }
  ;

expr_cast:
  | e = expr_cast; AS; t = ty              { ExprCast (e, t) }
  | e = expr_unary                          { e }
  ;

expr_unary:
  | MINUS; e = expr_unary  { ExprUnary (Neg, e) }
  | BANG; e = expr_unary   { ExprUnary (Not, e) }
  | e = expr_postfix       { e }
  ;

expr_postfix:
  | e = expr_postfix; QUESTION    { ExprQuestion e }
  | e = expr_postfix; DOT; f = located_ident; LPAREN;
    args = separated_list(COMMA, expr); RPAREN
    { ExprMethodCall (e, f, args) }
  | e = expr_postfix; DOT; f = located_ident
    { ExprFieldAccess (e, f) }
  | e = expr_postfix; LBRACKET; i = expr; RBRACKET
    { ExprIndex (e, i) }
  | e = expr_call            { e }
  ;

expr_call:
  | f = expr_call; LPAREN;
    args = separated_list(COMMA, expr); RPAREN
    { ExprCall (f, args) }
  | e = expr_primary         { e }
  ;

expr_primary:
  (* Literals *)
  | n = INT_LIT               { ExprLit (LitInt n) }
  | n = FLOAT_LIT             { ExprLit (LitFloat n) }
  | s = STRING_LIT            { ExprLit (LitString s) }
  | TRUE                      { ExprLit (LitBool true) }
  | FALSE                     { ExprLit (LitBool false) }
  (* self *)
  | SELF_VALUE                { ExprSelf }
  (* Path Type::variant with struct fields *)
  | a = located_ident; COLON_COLON; b = located_ident;
    LBRACE; fs = trailing_list(COMMA, struct_field_init); RBRACE
    { ExprStructVariant (a, b, fs) }
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident;
    LBRACE; fs = trailing_list(COMMA, struct_field_init); RBRACE
    { ExprStructVariant (a, b, fs) }
  (* Path Type::variant *)
  | a = located_ident; COLON_COLON; b = located_ident
    { ExprPath (a, b) }
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident
    { ExprPath (a, b) }
  (* Struct literal -- must be before plain ident to resolve the reduce/reduce
     conflict: when we see IDENT followed by LBRACE, prefer struct literal *)
  | n = located_ident; LT; args = separated_nonempty_list(COMMA, ty); GT;
    LBRACE; fs = trailing_list(COMMA, struct_field_init); RBRACE
    { ExprStruct (TyGeneric (n, args), fs) }
  | n = located_ident;
    LBRACE; fs = trailing_list(COMMA, struct_field_init); RBRACE
    { ExprStruct (TyName n, fs) }
  (* Plain ident *)
  | id = located_ident        { ExprIdent id }
  (* if *)
  | IF; c = expr_no_struct; tb = block_expr;
    ELSE; eb = else_branch
    { ExprIf (c, tb, Some eb) }
  | IF; c = expr_no_struct; tb = block_expr
    { ExprIf (c, tb, None) }
  (* match *)
  | MATCH; e = expr_no_struct;
    LBRACE; arms = trailing_list(COMMA, match_arm); RBRACE
    { ExprMatch (e, arms) }
  (* block *)
  | b = block_expr            { ExprBlock b }

  (* loops *)
  | LOOP; b = block_expr
    { ExprLoop (None, b) }
  | WHILE; c = expr_no_struct; b = block_expr
    { ExprWhile (c, b) }
  | FOR; name = located_ident; IN; iter = expr_no_struct; b = block_expr
    { ExprFor (name, iter, b) }
  (* Parens *)
  | LPAREN; e = expr; RPAREN  { e }
  (* Array *)
  | LBRACKET; RBRACKET
    { ExprArray [] }
  | LBRACKET; e = expr; SEMI; n = expr; RBRACKET
    { ExprRepeat (e, n) }
  | LBRACKET; es = separated_nonempty_list(COMMA, expr); option(COMMA); RBRACKET
    { ExprArray es }
  ;

else_branch:
  | IF; c = expr_no_struct; tb = block_expr; ELSE; eb = else_branch
    { { stmts = []; final_expr = Some (ExprIf (c, tb, Some eb)) } }
  | IF; c = expr_no_struct; tb = block_expr
    { { stmts = []; final_expr = Some (ExprIf (c, tb, None)) } }
  | b = block_expr { b }
  ;



struct_field_init:
  | name = located_ident; COLON; e = expr
    { { sf_name = name; sf_expr = e } }
  | name = located_ident
    { { sf_name = name; sf_expr = ExprIdent name } }
  ;

match_arm:
  | p = pattern; FAT_ARROW; e = expr
    { { arm_pat = p; arm_expr = e } }
  ;

pattern:
  | UNDERSCORE                     { PatWild }
  | n = INT_LIT                    { PatLit (LitInt n) }
  | MINUS; n = INT_LIT             { PatLit (LitInt ("-" ^ n)) }
  | s = STRING_LIT                 { PatLit (LitString s) }
  | TRUE                           { PatLit (LitBool true) }
  | FALSE                          { PatLit (LitBool false) }
  (* Qualified variant with tuple fields *)
  | a = located_ident; COLON_COLON; b = located_ident;
    LPAREN; ps = separated_nonempty_list(COMMA, pattern); RPAREN
    { PatTuple (a, b, ps) }
  (* Qualified variant with struct fields *)
  | a = located_ident; COLON_COLON; b = located_ident;
    LBRACE; fps = trailing_list(COMMA, field_pat); RBRACE
    { PatStruct (a, b, fps) }
  (* Qualified variant, unit *)
  | a = located_ident; COLON_COLON; b = located_ident
    { PatTuple (a, b, []) }
  (* Type keyword as enum name *)
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident;
    LPAREN; ps = separated_nonempty_list(COMMA, pattern); RPAREN
    { PatTuple (a, b, ps) }
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident;
    LBRACE; fps = trailing_list(COMMA, field_pat); RBRACE
    { PatStruct (a, b, fps) }
  | a = located_type_kw_as_ident; COLON_COLON; b = located_ident
    { PatTuple (a, b, []) }
  (* Binding *)
  | id = located_ident            { PatBind id }
  ;

field_pat:
  | name = located_ident; COLON; p = pattern
    { { fp_name = name; fp_pat = Some p } }
  | name = located_ident
    { { fp_name = name; fp_pat = None } }
  ;

(* ---- helpers ---- *)
trailing_list(SEP, X):
  |                                        { [] }
  | x = X                                 { [x] }
  | x = X; SEP; xs = trailing_list(SEP, X) { x :: xs }
  ;

located_ident:
  | s = IDENT { mk_loc s ($startpos, $endpos) }
  ;

located_type_name:
  | s = type_name { mk_loc s ($startpos, $endpos) }
  ;

located_type_kw_as_ident:
  | OPTION_TYPE  { mk_loc "Option" ($startpos, $endpos) }
  | RESULT_TYPE  { mk_loc "Result" ($startpos, $endpos) }
  | VEC_TYPE     { mk_loc "Vec" ($startpos, $endpos) }
  | HASHMAP_TYPE { mk_loc "HashMap" ($startpos, $endpos) }
  ;
