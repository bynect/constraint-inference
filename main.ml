open Infer

let normalize (ty : ty) : subst * scheme =
  let letter (i : int) : var =
    let off, count = (i mod 26, i / 26) in
    let chr = Char.code 'a' + off |> Char.chr in
      "\'" ^ String.make (count + 1) chr
  in
  let vars = freevars ty |> Set.to_seq |> List.of_seq in
  let vars' = List.mapi (fun i _ -> letter i) vars in
  let s = List.fold_left2 (fun acc var var' -> Map.add var (TVar(var')) acc)
    Map.empty vars vars' in
  s, Scheme(vars', apply s ty)

let normalize' (ty : ty) : scheme =
    let _, sigma = normalize ty in
    sigma

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
        normalize' ty |> Pretty.string_of_scheme |> print_endline
    with
    | Lex.Lex_error -> print_endline "Error while lexing"
    | Parsing.Parse_error -> print_endline "Error while parsing"
    | Infer.Error (UnboundVar var) ->
        Printf.printf "Unbound variable %s\n" var
    | Infer.Error (OccurFail (var, ty)) ->
        let s, Scheme(_, ty') = normalize ty in
        Printf.printf "Occurs check failed in %s = %s\n"
        (match Map.find_opt var s with
        | Some(TVar(var')) -> var'
        | _ -> failwith "Unreachable")
          (Pretty.string_of_ty ty')
    | Infer.Error (UnifyFail (t1, t2)) ->
        let Scheme(_, t1') = normalize' t1
        and Scheme(_, t2') = normalize' t2 in
        Printf.printf "Unification failed in %s = %s\n"
          (Pretty.string_of_ty t1') (Pretty.string_of_ty t2')
  done

let () =
  let base =
    [
      ("fix", Scheme(["'a"], TFun (TFun (TVar "'a", TVar "'a"), TVar "'a")));
      ("id", Scheme(["'a"], TFun (TVar "'a", TVar "'a")));
      ("eq", Scheme(["'a"], TFun (TVar "'a", TFun (TVar "'a", TConst "Bool"))));
      ("sub", Scheme([], TFun (TVar "Int", TFun (TVar "Int", TConst "Int"))));
      ("mul", Scheme([], TFun (TVar "Int", TFun (TVar "Int", TConst "Int"))));
      ("fst", Scheme(["'a";"'b"], TFun (TTup [TVar "'a"; TVar "'b"], TVar "'a")));
      ("snd", Scheme(["'a";"'b"], TFun (TTup [TVar "'a"; TVar "'b"], TVar "'b")));
    ]
  in
  let env = List.fold_left (fun acc (var, sigma) -> Map.add var sigma acc)
    Map.empty base
  in
  try main env with
  | _ -> print_newline ()
