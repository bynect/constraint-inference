open Infer

let normalize (ty : ty) : scheme =
  let letter (i : int) : var =
    let off, count = (i mod 26, i / 26) in
    let chr = Char.code 'a' + off |> Char.chr in
      "\'" ^ String.make (count + 1) chr
  in
  let vars = freevars ty |> Set.to_seq |> List.of_seq in
  let vars' = List.mapi (fun i _ -> letter i) vars in
  let s = List.fold_left2 (fun acc var var' -> Map.add var (TVar(var')) acc)
    Map.empty vars vars' in
  Scheme(vars', apply s ty)

let main (gamma : env) =
  while true do
    print_string "-> ";
    let line = read_line () in
    try
      let buf = Lexing.from_string line in
      let e = Parse.main Lex.token buf in
      let s, ty = infer gamma e in
        print_string "S = ";
        Pretty.string_of_subst s |> print_endline;
        print_string "ty = ";
        Pretty.string_of_ty ty |> print_endline;
        print_string "scheme = ";
        normalize ty |> Pretty.string_of_scheme |> print_endline
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
  try main Map.empty with
  | _ -> print_newline ()
