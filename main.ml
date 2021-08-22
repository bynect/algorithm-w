open Ast

let ctx : ctx =
  let typs : (string * scheme) list =
    [
      ("=", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      ("<>", Scheme ([ "'a" ], TFun (TVar "'a", TFun (TVar "'a", TBool))));
      ("&&", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("||", Scheme ([], TFun (TBool, TFun (TBool, TBool))));
      ("+", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("-", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("*", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("/", Scheme ([], TFun (TInt, TFun (TInt, TInt))));
      ("id", Scheme ([ "'a" ], TFun (TVar "'a", TVar "'a")));
      ( "const",
        Scheme ([ "'a"; "'b" ], TFun (TVar "'a", TFun (TVar "'b", TVar "'a")))
      );
      ( "pair",
        Scheme
          ( [ "'a"; "'b" ],
            TFun (TVar "'a", TFun (TVar "'b", TTup [ TVar "'a"; TVar "'b" ])) )
      );
      ( "fst",
        Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'a"))
      );
      ( "snd",
        Scheme ([ "'a"; "'b" ], TFun (TTup [ TVar "'a"; TVar "'b" ], TVar "'b"))
      );
    ]
  in
  let f acc (v, scheme) = Map.add v scheme acc in
  List.fold_left f Map.empty typs

let infer exp = Infer.infer_exp exp ctx

and generalize ty = Infer.generalize ty ctx

let print ty = generalize ty |> string_of_scheme |> print_endline

let pipeline exp =
  string_of_exp exp |> print_endline;
  let typ = infer exp in
  print_string "=> ";
  print typ

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
