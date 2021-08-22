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
%token ARROW LPAREN RPAREN EQ COMMA
%token EOF

%nonassoc IN ELSE
%left COMMA

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
  ;

tuple:
  | e1 = exp; COMMA; e2 = exp { [e2; e1] }
  | t = tuple; COMMA; e = exp { e :: t }
  ;

sexp:
  | x = VAR { Var x }
  | l = lit { Lit l }
  | LPAREN; e = exp; RPAREN { e }
  ;

lit:
  | b = BOOL { Bool b }
  | i = INT { Int i }
  ;
