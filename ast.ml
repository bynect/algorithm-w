module Set = Set.Make (String)
module Map = Map.Make (String)

type exp =
  | Var of string
  | App of exp * exp
  | Fun of string * exp
  | Let of string * exp * exp
  | If of exp * exp * exp
  | Tup of exp list
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
    | Tup l ->
        let buf = Buffer.create 50 in
        let append ty = str_simple ty |> Buffer.add_string buf in
        let rec iter = function
          | [] -> ()
          | [ ty ] -> append ty
          | h :: t ->
              append h;
              Buffer.add_string buf ", ";
              iter t
        in
        iter l;
        "Tup (" ^ (Buffer.to_bytes buf |> Bytes.to_string) ^ ")"
    | Lit (Bool b) -> Printf.sprintf "Bool %b" b
    | Lit (Int i) -> Printf.sprintf "Int %d" i
  in
  str_simple exp

type typ = Var of string | Int | Bool | Fun of typ * typ | Tup of typ list

let string_of_typ ty =
  let rec str_simple ty =
    match ty with
    | Var v -> v
    | Int -> "int"
    | Bool -> "bool"
    | Fun (p, r) -> str_paren p ^ " -> " ^ str_simple r
    | Tup l ->
        let buf = Buffer.create 50 in
        let append ty = str_paren ty |> Buffer.add_string buf in
        let rec iter = function
          | [] -> ()
          | [ ty ] -> append ty
          | h :: t ->
              append h;
              Buffer.add_string buf " * ";
              iter t
        in
        iter l;
        Buffer.to_bytes buf |> Bytes.to_string
  and str_paren ty =
    match ty with
    | Fun (_, _) | Tup _ -> "(" ^ str_simple ty ^ ")"
    | _ -> str_simple ty
  in
  str_simple ty

type scheme = Scheme of string list * typ

let string_of_scheme = function
  | Scheme ([], ty) -> string_of_typ ty
  | Scheme (vars, ty) ->
      let rec rename ty (v1, v2) =
        match ty with
        | Var v -> Var (if v = v1 then v2 else v)
        | Bool | Int -> ty
        | Fun (p, r) -> Fun (rename p (v1, v2), rename r (v1, v2))
        | Tup l -> Tup (List.map (fun ty -> rename ty (v1, v2)) l)
      in
      let scheme_var i =
        let off, count = (i mod 26, i / 26) in
        let chr = Char.code 'a' + off |> Char.chr in
        "\'" ^ String.make count chr
      in
      let vars' = List.mapi (fun i v -> (v, scheme_var i)) vars in
      let ty' = List.fold_left rename ty vars' in
      let buf = Buffer.create 50 in
      Buffer.add_string buf "forall";
      List.iter
        (fun (_, v) ->
          Buffer.add_char buf ' ';
          Buffer.add_string buf v)
        vars';
      Buffer.add_string buf ". ";
      (Buffer.to_bytes buf |> Bytes.to_string) ^ string_of_typ ty'

type ctx = scheme Map.t

let string_of_ctx ctx =
  let buf = Buffer.create 50 in
  Map.iter
    (fun v scheme ->
      v ^ " :: " ^ string_of_scheme scheme |> Buffer.add_string buf;
      Buffer.add_char buf '\n')
    ctx;
  Buffer.to_bytes buf |> Bytes.to_string
