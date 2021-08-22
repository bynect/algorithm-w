%{
open Ast

let rec fun_apply exp = function
  | [exp'] -> App (exp, exp')
  | h :: ((_ :: _) as t) ->
    fun_apply (App (exp, h)) t
  | _ -> failwith "Unreachable"
%}

%token <string> VAR BVAR
%token <bool> BOOL
%token <int> INT

%token ARROW LPAREN RPAREN COMMA DOT COL
%token EQ NE AND OR PLUS MINUS STAR SLASH
%token FORALL FUN IF LET IN THEN ELSE
%token EOF

%nonassoc LET FUN COL
%nonassoc IF
%left COMMA
%right AND OR
%left EQ NE
%left PLUS MINUS
%left STAR SLASH

%start <Ast.exp list> main
%%

main:
  | e = exp*; EOF { e }
  ;

exp:
  | e = sexp { e }
  | l = tuple { Tup (List.rev l) }
  | e = sexp; es = sexp+ { fun_apply e es }
  | e1 = sexp; x = BVAR; e2 = sexp { fun_apply (Var x) [e1; e2] }
  | FUN; x = VAR; ARROW; b = exp { Fun (x, b) }
  | LET; x = VAR; EQ; v = exp; IN; b = exp { Let (x, v, b) }
  | IF; c = exp; THEN; t = exp; ELSE; e = exp { If (c, t, e) }
  | e1 = exp; x = op; e2 = exp { fun_apply (Var x) [e1; e2] }
  | e = exp; COL; ty = typ { Annot (e, ty) }
  ;

tuple:
  | e1 = exp; COMMA; e2 = exp { [e2; e1] }
  | t = tuple; COMMA; e = exp { e :: t }
  ;

sexp:
  | x = VAR { Var x }
  | LPAREN; x = op; RPAREN { Var x }
  | LPAREN; RPAREN { Tup [] }
  | l = lit { Lit l }
  | LPAREN; e = exp; RPAREN { e }
  ;

lit:
  | b = BOOL { Bool b }
  | i = INT { Int i }
  ;

op:
  | EQ { "=" }
  | NE { "<>" }
  | AND { "&&" }
  | OR { "||" }
  | PLUS { "+" }
  | MINUS { "-" }
  | STAR { "*" }
  | SLASH { "/" }
  ;

typ:
  | x = VAR
    {
      match x with
      | "int" -> TInt
      | "bool" -> TBool
      | "unit" -> TUnit
      | _ -> Printf.sprintf "Invalid type name %s" x |> failwith
    }
  | p = typ; ARROW; r = typ { TFun (p, r) }
  | t = tuplet { TTup (List.rev t) }
  ;

tuplet:
  | ty1 = typ; COMMA; ty2 = typ { [ty1; ty2] }
  | t = tuplet; COMMA; ty = typ { ty :: t }
  ;

scheme:
  | FORALL; xs = VAR+; DOT; ty = typ { Scheme (xs, ty) }
  | ty = typ { Scheme ([], ty) }
  ;
