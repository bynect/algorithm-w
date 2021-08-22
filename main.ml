open Ast

let ctx : ctx =
  let typs : (string * scheme) list =
    [ ("id", Scheme ([ "a" ], Fun (Var "a", Var "a"))) ]
  in
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

let infer x = Infer.infer_exp x ctx

let pipeline exp =
  string_of_exp exp |> print_endline;
  print_string "=> ";
  infer exp |> string_of_typ |> print_endline

let () =
  try
    while true do
      print_string "> ";
      let line = read_line () in
      if String.equal line ":ctx" then print_endline (string_of_ctx ctx)
      else
        try
          let lexbuf = Lexing.from_string line in
          let exps = Parse.main Lex.token lexbuf in
          List.iter pipeline exps
        with
        | Parse.Error -> print_endline "Parsing error"
        | Failure msg -> print_endline msg
    done
  with _ -> print_newline ()

(*
let test () =
  let exps =
    [
      Let ("a", Lit (Int 1), Var "a");
      If (Lit (Int 1), Lit (Bool true), Lit (Bool true));
    ]
  in
  let print x =
    string_of_exp x |> print_endline;
    print_string "=> ";
    try infer x |> string_of_typ |> print_endline
    with Failure msg -> print_endline msg
  in
  List.iter print exps
*)
