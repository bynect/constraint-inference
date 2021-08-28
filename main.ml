open Infer

let main e =
    try
        let (s, ty) = infer Map.empty e in
            print_endline "Substitutions";
            Pretty.string_of_subst s |> print_endline;
            print_endline "Type";
            Pretty.string_of_ty ty |> print_endline
    with
    | Error(UnboundVar var) -> Printf.printf "Unbound variable `%s`" var
    | Error(OccurFail(var, ty)) ->
            Printf.printf "Occurs check failed in `%s = %s`"
            var (Pretty.string_of_ty ty)
    | Error(UnifyFail(t1, t2)) ->
            Printf.printf "Unification failed in `%s = %s`"
            (Pretty.string_of_ty t1) (Pretty.string_of_ty t2)



let () =
    let e = Abs("m",
                Let("y", Var("m"),
                    Let("x", App(Var("y"), Lit(Bool true)),
                    Var("x"))))
    in
    main e
