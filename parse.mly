%{
open Com

let rec fun_apply exp = function
  | [exp'] -> App (exp, exp')
  | h :: ((_ :: _) as t) ->
    fun_apply (App (exp, h)) t
  | _ -> failwith "Unreachable"
%}

%token <string> VAR
%token <bool> BOOL
%token <int> INT

%token FUN IF LET IN THEN ELSE
%token ARROW LPAREN RPAREN EQ
%token EOF

%start <Com.exp> main
%%

main:
  | e = exp; EOF { e }
  ;

exp:
  | e = sexp { e }
  | e = sexp; es = sexp+ { fun_apply e es }
  | FUN; x = VAR; ARROW; b = exp { Fun (x, b) }
  | LET; x = VAR; EQ; v = exp; IN; b = exp { Let (x, v, b) }
  | IF; c = exp; THEN; t = exp; ELSE; e = exp { If (c, t, e) }
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
