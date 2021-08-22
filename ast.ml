module Set = Set.Make (String)
module Map = Map.Make (String)

type exp =
  | Var of string
  | App of exp * exp
  | Fun of string * exp
  | Let of string * exp * exp
  | If of exp * exp * exp
  | Lit of lit

and lit = Bool of bool | Int of int

let string_of_exp exp =
  let rec str_simple = function
    | Var v -> Printf.sprintf "Var %s" v
    | App (f, a) -> "App (" ^ str_simple f ^ ") (" ^ str_simple a ^ ")"
    | Fun (x, b) -> "Fun " ^ x ^ " (" ^ str_simple b ^ ")"
    | Let (x, v, b) ->
        "Let " ^ x ^ " (" ^ str_simple v ^ ") (" ^ str_simple b ^ ")"
    | If (c, t, e) ->
        "If (" ^ str_simple c ^ ") (" ^ str_simple t ^ ") (" ^ str_simple e
        ^ ")"
    | Lit (Bool b) -> Printf.sprintf "Bool %b" b
    | Lit (Int i) -> Printf.sprintf "Int %d" i
  in
  str_simple exp

type typ = Var of string | Int | Bool | Fun of typ * typ

let string_of_typ ty =
  let rec str_simple ty =
    match ty with
    | Var v -> v
    | Int -> "int"
    | Bool -> "bool"
    | Fun (p, r) -> str_paren p ^ " -> " ^ str_simple r
  and str_paren ty =
    match ty with Fun (_, _) -> "(" ^ str_simple ty ^ ")" | _ -> str_simple ty
  in
  str_simple ty

type scheme = Scheme of string list * typ

let string_of_scheme = function
  | Scheme (vars, typ) ->
      if List.length vars != 0 then (
        let buf = Buffer.create 50 in
        Buffer.add_string buf "forall";
        List.iter
          (fun v ->
            Buffer.add_char buf ' ';
            Buffer.add_string buf v)
          vars;
        Buffer.add_string buf ". ";
        (Buffer.to_bytes buf |> Bytes.to_string) ^ string_of_typ typ)
      else string_of_typ typ

type ctx = scheme Map.t

let string_of_ctx ctx =
  let buf = Buffer.create 50 in
  Map.iter
    (fun v scheme ->
      v ^ " :: " ^ string_of_scheme scheme |> Buffer.add_string buf;
      Buffer.add_char buf '\n')
    ctx;
  Buffer.to_bytes buf |> Bytes.to_string
