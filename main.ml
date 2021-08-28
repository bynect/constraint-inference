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

let main gamma e =
  try
    let s, ty = infer gamma e in
	print_string "S = ";
	Pretty.string_of_subst s |> print_endline;
	print_string "ty = ";
	Pretty.string_of_ty ty |> print_endline;
	print_string "scheme = ";
	normalize ty |> Pretty.string_of_scheme |> print_endline
  with
  | Error (UnboundVar var) ->
      Printf.printf "Unbound variable `%s`" var
  | Error (OccurFail (var, ty)) ->
      Printf.printf "Occurs check failed in `%s = %s`" var
        (Pretty.string_of_ty ty)
  | Error (UnifyFail (t1, t2)) ->
      Printf.printf "Unification failed in `%s = %s`"
        (Pretty.string_of_ty t1) (Pretty.string_of_ty t2)

let () =
  let e =
    Abs
      ( "m",
        Let ("y", Var "m", Let ("x", App (Var "y", Lit (Bool true)), Var "x"))
      )
  in
  main Map.empty e
