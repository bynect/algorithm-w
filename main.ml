open Ast

let ctx : ctx =
  let typs : (string * scheme) list =
    [
      ("id", Scheme ([ "'a" ], Fun (Var "'a", Var "'a")));
      ( "const",
        Scheme ([ "'a"; "'b" ], Fun (Var "'a", Fun (Var "'b", Var "'a"))) );
      ("eq", Scheme ([ "'a" ], Fun (Var "'a", Fun (Var "'a", Bool))));
    ]
  in
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

let infer exp = Infer.infer_exp exp ctx

and generalize ty = Infer.generalize ty ctx

let pipeline exp =
  string_of_exp exp |> print_endline;
  let typ = infer exp in
  print_string "=> ";
  (*typ |> string_of_typ |> print_endline*)
  generalize typ |> string_of_scheme |> print_endline

let loop () =
  while true do
    print_string "> ";
    let line = read_line () in
    if String.equal line ":ctx" then string_of_ctx ctx |> print_string
    else
      try
        let lexbuf = Lexing.from_string line in
        let exps = Parse.main Lex.token lexbuf in
        List.iter pipeline exps
      with
      | Parse.Error -> print_endline "Parsing error"
      | Failure msg -> print_endline msg
  done

let () = try loop () with _ -> print_newline ()
