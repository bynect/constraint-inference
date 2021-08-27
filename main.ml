open Infer

let () =
    let e = Abs("m",
                Let("y", Var("m"),
                    Let("x", App(Var("y"), Lit(Bool true)),
                    Var("x"))))
    (*let expr = Let("id", Abs("x", Var "x"),
                Let("_", App(Var("id"), Var("id")),
                    App(Var("id"), Lit(Bool true))))
                    *)
    in
    let (ty, a, c) = collect Set.empty e
    in
    print_endline "Assumptions";
    List.iter (fun a' -> Print.string_of_assump a' |> print_endline) a;
    print_endline "Constraints";
    List.iter (fun c' -> Print.string_of_constr c' |> print_endline) c;
    let s = solve c in
    print_endline "Substitutions";
    Print.string_of_subst s |> print_endline;
    let ty' = apply s ty
    in
    print_endline "Type";
    Printf.printf "%s => %s\n" (Print.string_of_ty ty) (Print.string_of_ty ty');
    let (s, ty) = infer Map.empty e in
    print_endline "Substitutions";
    Print.string_of_subst s |> print_endline;
    print_endline "Type";
    Printf.printf "%s\n" (Print.string_of_ty ty)



