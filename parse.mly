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

%token FUN IF LET IN THEN ELSE
%token ARROW LPAREN RPAREN COMMA EQ NE
%token AND OR PLUS MINUS STAR SLASH
%token EOF

%nonassoc LET FUN
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
  ;

binary(E1, E2, OP, v):
  | e1 = E1; OP; e2 = E2 { fun_apply (Var v) [e1; e2] }
  ;

tuple:
  | e1 = exp; COMMA; e2 = exp { [e2; e1] }
  | t = tuple; COMMA; e = exp { e :: t }
  ;

sexp:
  | x = VAR { Var x }
  | LPAREN; x = op; RPAREN { Var x }
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
