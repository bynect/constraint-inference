open Infer

let env = Map.empty

let main () =
  while true do
    print_string "-> ";
    let line = read_line () in
    try
      let buf = Lexing.from_string line in
      let e = Parse.main Lex.token buf in
      let s, ty = infer env e in
        print_string "S = ";
        Pretty.string_of_subst s |> print_endline;
        print_string "ty = ";
        Pretty.string_of_ty ty |> print_endline
    with
    | Lex.Lex_error -> print_endline "Error while lexing"
    | Parsing.Parse_error -> print_endline "Error while parsing"
    | Infer.Error (UnboundVar var) ->
        Printf.printf "Unbound variable `%s`\n" var
    | Infer.Error (OccurFail (var, ty)) ->
        Printf.printf "Occurs check failed in `%s = %s`\n" var
          (Pretty.string_of_ty ty)
    | Infer.Error (UnifyFail (t1, t2)) ->
        Printf.printf "Unification failed in `%s = %s`\n"
          (Pretty.string_of_ty t1) (Pretty.string_of_ty t2)
  done

let () =
  try main () with
  | _ -> print_newline ()
