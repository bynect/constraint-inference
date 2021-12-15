open Infer

let for_all f s =
  try
    for i = 0 to String.length s - 1 do
      assert (f s.[i])
    done;
    true
  with _ -> false

let rec string_of_ty : ty -> string = function
  | TVar var -> var
  | TCon (name, []) -> name
  | TCon (name, (ty :: ts)) ->
      if for_all ((==) ',') name then
        List.fold_left (fun acc ty -> acc ^ " * " ^ string_of_ty' ty)
        (string_of_ty' ty) ts
      else
        List.fold_left (fun acc ty -> acc ^ " " ^ string_of_ty' ty) name (ty :: ts)
  | TFun (t1, t2) -> string_of_ty' t1 ^ " -> " ^ string_of_ty t2

and string_of_ty' : ty -> string = function
  | TCon (_, _::_) as ty -> "(" ^ string_of_ty ty ^ ")"
  | TFun (_, _) as ty -> "(" ^ string_of_ty ty ^ ")"
  | ty -> string_of_ty ty

let string_of_scheme : scheme -> string = function
  | Scheme ([], ty) -> string_of_ty ty
  | Scheme (vars, ty) ->
      (List.fold_left (fun acc var -> acc ^ var ^ " ") "" vars)
      ^ ". " ^ string_of_ty ty

let string_of_assump : assump -> string = function
  | Assumption (x, ty) -> x ^ " A= " ^ string_of_ty ty

let string_of_constr : constr -> string = function
  | Equality (t1, t2) -> string_of_ty' t1 ^ " == " ^ string_of_ty' t2
  | ImplInstance (t1, t2, m) ->
      let rec recstring (acc : string) : var list -> string = function
        | [] -> acc ^ "} "
        | [ var ] -> acc ^ var ^ " } "
        | var :: m' ->
            let acc' = acc ^ var ^ ", " in
            recstring acc' m'
      in
      string_of_ty' t1 ^ " <=M" ^ recstring "{ " m ^ string_of_ty' t2
  | ExplInstance (ty, sigma) ->
      string_of_ty' ty ^ " <= " ^ string_of_scheme sigma

let string_of_subst (s : subst) : string =
  let rec recstring (acc : string) : (var * ty) list -> string = function
    | [] -> acc ^ "}"
    | [ (var, ty) ] -> acc ^ var ^ " := " ^ string_of_ty ty ^ " }"
    | (var, ty) :: s ->
        let acc' = acc ^ var ^ " := " ^ string_of_ty ty ^ ", " in
        recstring acc' s
  in
  recstring "{ " (Map.to_seq s |> List.of_seq)
