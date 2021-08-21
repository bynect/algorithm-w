open Com

let ctx = Map.empty

let infer x = Hm.infer_exp x ctx

let pipeline lexbuf =
  try
    let exp = Parse.main Lex.token lexbuf in
    string_of_exp exp |> print_endline;
    print_string "=> ";
    infer exp |> string_of_typ |> print_endline
  with
  | Parse.Error -> print_endline "Parsing error"
  | Failure msg -> print_endline msg

let () =
  try
    while true do
      print_string "> ";
      let lexbuf = Lexing.from_string (read_line ()) in
      pipeline lexbuf
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
