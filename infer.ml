(**
 * This code is public domain, and
 * a full code listing can be found at
 * github.com/bynect/constraint-inference
 *
 * This is an indipendent implementation of the
 * bottom-up type inference algorithm
 * described in the following paper
 *
 * Bastiaan Heeren, Jurriaan Hage, Doaitse Swierstra
 * Generalizing Hindley-Milner Type Inference Algorithms
 * Institute of Information and Computing Sciences,
 * Utrecht University, 2002
 * [UU-CS-2002-031]
 *
 * A copy of the original paper can be found at
 * www.cs.uu.nl/research/techreps/repo/CS-2002/2002-031.pdf
 *
 * NOTE: Some sentences are either taken verbatim
 * or rewording of excerpt from the paper
 *
 * NOTE: While the main algorithm described in
 * the paper is unchanged, there may be small
 * divergences between this implementation and
 * the paper pseudo-implementation
 *)

type expr =
  | Var of string
  | App of expr * expr
  | Abs of string * expr
  | Let of string * expr * expr
  | Lit of lit

and lit = Int of int | Bool of bool

type var = string

let var_fresh : unit -> var =
  let state = ref 1 in
  fun () ->
    let n = !state in
    state := n + 1;
    Printf.sprintf "t%d" n

type ty = TVar of var | TConst of string | TFun of ty * ty

type scheme = Scheme of var list * ty

type assump = Assumption of string * ty

type constr =
  | Equality of ty * ty
  | ImplInstance of ty * ty * var list
  | ExplInstance of ty * scheme

module Set = Set.Make (String)

type mono = Set.t

module Map = Map.Make (String)

type subst = ty Map.t

let rec apply (s : subst) : ty -> ty = function
  | TVar var -> begin
      match Map.find_opt var s with
      | Some ty -> ty
      | None -> TVar var
    end
  | TConst name -> TConst name
  | TFun (t1, t2) -> TFun (apply s t1, apply s t2)

let apply' (s : subst) : scheme -> scheme = function
  | Scheme ([], ty) -> Scheme ([], apply s ty)
  | Scheme (vars, ty) ->
      let s' = List.fold_right (fun v acc -> Map.remove v acc) vars s in
      Scheme (vars, apply s' ty)

let compose (s1 : subst) (s2 : subst) : subst =
  Map.map (fun ty -> apply s1 ty) s2 |> Map.union (fun _ ty _ -> Some ty) s1

type err = UnboundVar of string | OccurFail of var * ty | UnifyFail of ty * ty

exception Error of err

let rec collect (m : mono) : expr -> ty * assump list * constr list = function
  | Var x ->
      let ty = TVar (var_fresh ()) in
      (ty, [ Assumption (x, ty) ], [])
  | App (e1, e2) ->
      let beta = var_fresh ()
      and t1, a1, c1 = collect m e1
      and t2, a2, c2 = collect m e2 in
      (TVar beta, a1 @ a2, Equality (t1, TFun (t2, TVar beta)) :: (c1 @ c2))
  | Abs (x, e) ->
      let beta = var_fresh () in
      let t, a, c = collect (Set.add beta m) e in
      let a', c' =
        List.fold_left
          (fun (acc, acc') a' ->
            let (Assumption (x', t')) = a' in
            if x' = x then (acc, Equality (t', TVar beta) :: acc')
            else (Assumption (x', t') :: acc, acc'))
          ([], c) a
      in
      (TFun (TVar beta, t), a', c')
  | Let (x, e1, e2) ->
      let t1, a1, c1 = collect m e1 and t2, a2, c2 = collect m e2 in
      let m' = Set.to_seq m |> List.of_seq in
      let a', c' =
        List.fold_left
          (fun (acc, acc') a' ->
            let (Assumption (x', t')) = a' in
            if x' = x then (acc, ImplInstance (t', t1, m') :: acc')
            else (Assumption (x', t') :: acc, acc'))
          ([], []) a2
      in
      (t2, a1 @ a', c1 @ c2 @ c')
  | Lit (Int _) -> (TConst "Int", [], [])
  | Lit (Bool _) -> (TConst "Bool", [], [])

let rec freevars : ty -> Set.t = function
  | TVar var -> Set.singleton var
  | TConst _ -> Set.empty
  | TFun (t1, t2) -> Set.union (freevars t1) (freevars t2)

let freevars' : scheme -> Set.t = function
  | Scheme ([], ty) -> freevars ty
  | Scheme (vars, ty) -> Set.diff (freevars ty) (Set.of_list vars)

let apply_constr (s : subst) : constr -> constr = function
  | Equality (t1, t2) -> Equality (apply s t1, apply s t2)
  | ImplInstance (t1, t2, m) ->
      let m' =
        List.fold_left
          (fun acc var ->
            match Map.find_opt var s with
            | Some ty ->
                let vars = freevars (apply s ty) |> Set.to_seq |> List.of_seq in
                vars @ acc
            | None -> var :: acc)
          [] m
      in
      ImplInstance (apply s t1, apply s t2, m')
  | ExplInstance (t, sigma) -> ExplInstance (apply s t, apply' s sigma)

let apply_constr' (s : subst) (c : constr list) : constr list =
  List.map (apply_constr s) c

let activevars : constr -> Set.t = function
  | Equality (t1, t2) -> Set.union (freevars t1) (freevars t2)
  | ImplInstance (t1, t2, m) ->
      let m' = Set.of_list m in
      Set.union (freevars t1) (Set.inter m' (freevars t2))
  | ExplInstance (t, sigma) -> Set.union (freevars t) (freevars' sigma)

let activevars' : constr list -> Set.t = function
  | [] -> Set.empty
  | c ->
      List.fold_left (fun acc c' -> Set.union (activevars c') acc) Set.empty c

let instantiate : scheme -> ty = function
  | Scheme ([], ty) -> ty
  | Scheme (vars, ty) ->
      let s =
        List.fold_left
          (fun acc var -> Map.add var (TVar (var_fresh ())) acc)
          Map.empty vars
      in
      apply s ty

let generalize (m : var list) (ty : ty) : scheme =
  let m = Set.diff (freevars ty) (Set.of_list m) in
  Scheme (Set.to_seq m |> List.of_seq, ty)

let rec mgu (t1 : ty) (t2 : ty) : subst =
  let bind var ty =
    match ty with
    | TVar var' when var = var' -> Map.empty
    | _ when Set.mem var (freevars ty) -> Error (OccurFail (var, ty)) |> raise
    | _ -> Map.singleton var ty
  in
  match (t1, t2) with
  | t1, t2 when t1 == t2 -> Map.empty
  | TVar var, ty | ty, TVar var -> bind var ty
  | TFun (t1, t2), TFun (t1', t2') ->
      let s1 = mgu t1 t1' in
      let s2 = mgu (apply s1 t2) (apply s1 t2') in
      compose s2 s1
  | _ -> Error (UnifyFail (t1, t2)) |> raise

let rec solve : constr list -> subst = function
  | [] -> Map.empty
  | Equality (t1, t2) :: c ->
      (* Find the substitution S for the most general unifier
       * and apply it to the constraint set C *)
      let s = mgu t1 t2 in
      compose (apply_constr' s c |> solve) s
  | ImplInstance (t1, t2, m) :: c ->
      (* If the  constraint is solvable then
       * create a type scheme σ by generalizing
       * the type τ2 with the monomorphic set M
       * and transform the implicit instance constraint
       * to an explicit instance constraint *)
      let ftv = Set.diff (freevars t2) (Set.of_list m) in
      if Set.inter ftv (activevars' c) |> Set.is_empty then
        solve (ExplInstance (t1, generalize m t2) :: c)
        (* TODO: Check if this is correct and terminates for
         * all inputs *)
      else solve (c @ [ ImplInstance (t1, t2, m) ])
  | ExplInstance (t, sigma) :: c ->
      (* Instantiate the type scheme σ and convert
       * the explicit instance constraint to an
       * equality constraint between the type τ and
       * the newly instantiated type *)
      solve (Equality (t, instantiate sigma) :: c)

type env = scheme Map.t

let env_aux (gamma : env) (a : assump list) : constr list =
  match a with
  | [] -> []
  | _ ->
      List.fold_left
        (fun acc a' ->
          let (Assumption (var, ty)) = a' in
          match Map.find_opt var gamma with
          | Some sigma -> ExplInstance (ty, sigma) :: acc
          (* If domain(A) not a subset of domain(Γ) then
           * an undefined variable was referenced *)
          | None -> Error (UnboundVar var) |> raise)
        [] a

let infer (gamma : env) (e : expr) : subst * ty =
  let ty, a, c = collect Set.empty e in
  let c' = env_aux gamma a in
  let s = solve (c @ c') in
  (s, apply s ty)
