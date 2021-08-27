(**
 * This is an OCaml implementation of the
 * bottom-up type inference algorithm
 * described in the paper
 *
 * Bastiaan Heeren, Jurriaan Hage, Doaitse Swierstra
 * Generalizing Hindley-Milner Type Inference Algorithms
 * Institute of Information and Computing Sciences,
 * Utrecht University, 2002
 * [UU-CS-2002-031]
 *)

(**
 * An expression is denoted by E (uppercase e)
 *
 * E ::=
 *  | x
 *  | E1 E2
 *  | λx -> E
 *  | let x = E1 in E2
 *  | lit
 *)
type expr =
    | Var of string
    | App of expr * expr
    | Abs of string * expr
    | Let of string * expr * expr
    | Lit of lit

and lit =
    | Int of int
    | Bool of bool

(**
 * A type variable is denoted by α (lowercase alpha)
 * A fresh type variable is denoted by β (lowercase beta)
 *
 * While there should be infinitely many fresh
 * type variables available, this implementation
 * uses a reasonable limit
 *)
type var = string

(**
 * Generate an unique fresh type variable β
 *
 * NOTE: This function is stateful
 *)
let var_fresh : unit -> var =
    let state = ref 1 in
    fun () ->
        let n = !state in
        state := n + 1;
        Printf.sprintf "t%d" n

(**
 * A type is denoted by τ (lowercase tau)
 *
 * τ ::=
 *  | α1...αn
 *  | Int
 *  | Bool
 *  | τ1 -> τ2
 *)
type ty =
    | TVar of var
    | TConst of string
    | TFun of ty * ty

(**
 * A type scheme is denoted by σ (lowercase sigma)
 *
 * In a type scheme a set of type variables α1...αn,
 * representing the polymorphic type variables, are
 * bound to an universal quantifier
 *
 * σ ::= ∀α.τ
 *)
type scheme = Scheme of var list * ty

(**
 * An assumption is denoted by A (uppercase a)
 *
 * An assumption set is used to record the type variables
 * that are assigned to the occurrences of free variables
 * instead of a type environment Γ used in Algorithm W
 *
 * Unlike the common type environment Γ, in an assumption set
 * there can be different assumptions for a given variable
 *)
type assump = Assumption of string * ty

(**
 * A constraint is denoted by C (uppercase c)
 *
 * A set of constraints is collected instead of constructing
 * a substitution by postponing the unification process
 *
 * The equality constraint (τ1 ≡ τ2) reflects that type τ1 and
 * type τ2 should be unified
 *
 * The implicit instance constraint (τ1 ≤M τ2) expresses that
 * type τ1 should be an instance of the type scheme obtained
 * by generalizing type τ2 with the set of monomorphic
 * type variables M
 *
 * The explicit instance constraint (τ ≤ σ) states that
 * type τ has to be a generic instance of the type scheme σ
 *
 * C ::=
 *  | τ1 ≡ τ2
 *  | τ1 ≤M τ2
 *  | τ ≤ σ
 *)
type constr =
    | Equality of ty * ty
    | ImplInstance of ty * ty * var list
    | ExplInstance of ty * scheme

(**
 * A substitution is denoted by S (uppercase s)
 *
 * Substitutions are mapping of type variables to types
 * A substitution for a set of type variables {α1, ..., αn}
 * is [α1 := τ1, ..., αn := τn]
 *
 * Substitutions are idempotent, so that S(Sτ) = Sτ
 *)
module Map = Map.Make(String)

type subst = ty Map.t

(**
 * Substitution application for a type
 **)
let rec apply (s : subst) : ty -> ty = function
    | TVar var ->
            begin
                match Map.find_opt var s with
                | Some ty -> ty
                | None -> TVar var
            end
    | TConst name -> TConst name
    | TFun(t1, t2) -> TFun(apply s t1, apply s t2)

(**
 * Substitution application for a type scheme
 *)
let apply' (s : subst) : scheme -> scheme = function
    | Scheme([], ty) -> Scheme([], apply s ty)
    | Scheme(vars, ty) ->
        let s' = List.fold_right (fun v acc -> Map.remove v acc) vars s in
        Scheme (vars, apply s' ty)

(**
 * Composing substitution S1 and substitution S2 is
 * written as (S2 ◦ S1) and results in another substitution
 *
 * NOTE: Substitution composition is left biased
 *)
let compose (s1 : subst) (s2 : subst) : subst =
    Map.map (fun ty -> apply s1 ty) s2 |> Map.union (fun _ ty _ -> Some ty) s1

(**
 * A monomorphic set is denoted by M (uppercase m)
 *
 * Unlike the rest of the algorithm, which is bottom-up,
 * to compute a set of monomorphic type variables M,
 * a single top-down computation is used
 *
 * Unlike a type environment Γ, a monomorphic set M
 * contains only type variables, not a mapping from
 * name to type, and only the type variables introduced
 * by lambdas at higher levels in the abstract
 * syntax tree will be added to the set
 *)
module Set = Set.Make(String)

type mono = Set.t

(**
 * Constraints collection
 *
 * The algorithm for gathering constraint and
 * assumptions is bottom-up, and the only
 * process which is top-down is the creation
 * of the monomorphic set M
 *
 * Every time a variable is used, a fresh type
 * variable β is generated and appended to the
 * assumption set, and there can be multiple
 * different assumption regarding the same
 * variable name
 *
 * Applications create a fresh type
 * variable β, merge and propagate the assumptions
 * and constraints collected by the subexpressions
 * while adding an equality constraint to ensure
 * that the type of the first expression matches
 * with the type of the second expression and β
 *
 * Lambda abstractions create a fresh type
 * variable β representing the bound variable
 * and add it to the monomorphic set M before
 * the collection of the subexpression
 *
 * An equality constraint is generated for each
 * type variable β in the assumption set that is
 * associated with the variable bound by the lambda,
 * and the assumptions concerning this variable
 * are removed from the set before being propagated
 *
 * Let expressions introduce polymorphism by
 * generating an implicit instance constraint,
 * using the monomorphic set M, for each type
 * variable β in the assumption set that is
 * associated with the variable bound by the let,
 * while removing the assumptions concerning this
 * variable from the set before being propagated
 *
 * Literals return their type without any
 * new constraint or assumption
 *
 * NOTE: Although the set of constrains is
 * not ordered, an implicit instance constraint
 * requires some constraints to be solved before
 * it becomes solvable
 *
 * NOTE: In this implementation the monomorphic set M
 * is propagated alongside the abstract syntax tree
 * traversal by the collect function, but it could be
 * also easily implemented as a separate function
 *)
let rec collect (m : mono) : expr -> ty * assump list * constr list = function
    | Var (x) ->
            let ty = TVar (var_fresh ()) in
            ty, [Assumption (x, ty)], []
    | App (e1, e2) ->
            let beta = var_fresh ()
            and (t1, a1, c1) = collect m e1
            and (t2, a2, c2) = collect m e2 in
            TVar beta, a1 @ a2, Equality (t1, TFun(t2, TVar beta)) :: (c1 @ c2)
    | Abs (x, e) ->
        let beta = var_fresh () in
        let (t, a, c) = collect (Set.add beta m) e in
        let a', c' = List.fold_left (fun (acc, acc') a' ->
            let Assumption(x', t') = a' in
            if x' = x then (acc, Equality (t', TVar beta)::acc')
            else (Assumption(x', t')::acc, acc'))
        ([], c) a in
        TFun(TVar beta, t), a', c'
    | Let (x, e1, e2) ->
         let (t1, a1, c1) = collect m e1
         and (t2, a2, c2) = collect m e2 in
         let m' = Set.to_seq m |> List.of_seq in
          let a', c' = List.fold_left (fun (acc, acc') a' ->
            let Assumption(x', t') = a' in
            if x' = x then (acc, ImplInstance (t', t1, m')::acc')
            else (Assumption(x', t')::acc, acc'))
          ([], []) a2 in
         t2, a1 @ a', c1 @ c2 @ c'
    | Lit (Int _) -> TConst("Int"), [], []
    | Lit (Bool _) -> TConst ("Bool"), [], []

(**
 * The set of free type variables of a type τ is
 * denoted by freevars(τ) and consinst of all
 * the type variables in τ
 *)
let rec freevars : ty -> Set.t = function
    | TVar var -> Set.singleton var
    | TConst _ -> Set.empty
    | TFun (t1, t2) -> Set.union (freevars t1) (freevars t2)

(**
 * The set of free type variables of a type scheme σ
 * is denoted by freevars(∀α.τ) and is equal to
 * freevars(τ) - α
 *)
let freevars' : scheme -> Set.t = function
    | Scheme([], ty) -> freevars ty
    | Scheme(vars, ty) -> Set.diff (freevars ty) (Set.of_list vars)

(**
 * A substitution S applied to a constraint set
 * is simply applied to all the types and
 * type schemes contained therein
 *
 * For implicit instance constraints, the
 * substitution S is also applied to the set
 * of monomorphic type variables M
 *)
let apply_constr (s : subst) : constr -> constr = function
    | Equality(t1, t2) -> Equality(apply s t1, apply s t2)
    | ImplInstance(t1, t2, m) ->
            let m' = List.fold_left (fun acc var ->
                match Map.find_opt var s with
                | Some ty ->
                    let vars = freevars (apply s ty) |> Set.to_seq |> List.of_seq in
                    vars @ acc
                | None -> var::acc)
            [] m in
            ImplInstance(apply s t1, apply s t2, m')
    | ExplInstance(t, sigma) -> ExplInstance (apply s t, apply' s sigma)

let apply_constr' (s : subst) (c : constr list) : constr list =
    List.map (apply_constr s) c

(**
 * The active type variables in a constraint are
 * defined as follows
 *
 * activevars (τ1 ≡ τ2)  = freevars(τ1) ∪ freevars(τ2)
 * activevars (τ1 ≤M τ2) = freevars(τ1) ∪ (freevars(M) ∩ freevars(τ2))
 * activevars (τ ≤ σ)    = freevars(τ) ∪ freevars(σ)
 *
 * NOTE: freevars(M) is M
 *)
let activevars : constr -> Set.t = function
    | Equality(t1, t2) -> Set.union (freevars t1) (freevars t2)
    | ImplInstance(t1, t2, m) ->
        let m' = Set.of_list m in
        Set.union (freevars t1) (Set.inter m' (freevars t2))
    | ExplInstance(t, sigma) -> Set.union (freevars t) (freevars' sigma)

let activevars' : constr list -> Set.t = function
    | [] -> Set.empty
    | c -> List.fold_left (fun acc c' -> Set.union (activevars c') acc) Set.empty c

(**
 * Instantiate the type scheme σ by subsituting
 * all the bound type β variables α with fresh
 * type variables in the type τ
 *
 * instantiate(∀α1, ..., αn.τ) = [α1 := β1, ...,  αn := βn]τ
 *                               where β1, ..., βn are fresh
 *)
let instantiate : scheme -> ty = function
    | Scheme ([], ty) -> ty
    | Scheme(vars, ty) ->
       let s = List.fold_left (fun acc var ->
           Map.add var (TVar (var_fresh ())) acc ) Map.empty vars
       in apply s ty

(**
 * Generalize a type τ to create a type
 * scheme σ with respect to the set of
 * type variables M
 *
 * generalize(M, τ) = ∀α.τ
 *                    where ~α = freevars(τ) −M
 *)
let generalize (m : var list) (ty : ty) : scheme =
    let m = Set.diff (freevars ty) (Set.of_list m) in
    Scheme (Set.to_seq m |> List.of_seq, ty)

(**
 * Most General Unification
 *
 * Given types τ1 and τ2, mgu(τ1,τ2) returns
 * either the most general unifier, which is
 * a substitution S, or an error if unification
 * is impossible
 *
 * For the resulting substitution S it holds
 * by definition that Sτ1 = Sτ2
 *)
let rec mgu (t1 : ty) (t2 : ty) : subst =
    let bind var ty =
        match ty with
        | TVar var' when var = var' -> Map.empty
        | _ when Set.mem var (freevars ty) ->
                failwith "occurs check failed"
        | _ -> Map.singleton var ty
    in
    match (t1, t2) with
    | t1, t2 when t1 == t2 -> Map.empty
    | TVar var, ty | ty, TVar var -> bind var ty
    | TFun(t1, t2), TFun(t1', t2') ->
         let s1 = mgu t1 t1' in
         let s2 = mgu (apply s1 t2) (apply s1 t2') in
         compose s2 s1
    | _ -> failwith "mgu failed"

(**
 * Constraints solving
 *
 * After the generation of a constraint set,
 * a substitution is constructed that satisfies
 * each constraint in the set
 *
 * Satisfaction of a constraint C by a
 * substitution S is defined as
 *
 * S satisfies (τ1 ≡ τ2)  = Sτ1 = Sτ2
 * S satisfies (τ1 ≤M τ2) = Sτ1 ≤ generalize(SM, Sτ2)
 * S satisfies (τ ≤ σ)    = Sτ ≤ Sσ
 *
 * After substitution, the two types of an
 * equality constraint should be syntactically equal
 *
 * The equality constraint can be satified by the
 * most general unifier of types τ1 and τ2
 *
 * The implicit instance constraint applies the
 * substitution not only to both types τ1 and τ2,
 * but also to the monomorphic type variables set M
 *
 * For explicit instance constraint the substitution
 * is applied only to the type τ and type scheme σ,
 * leaving untouched the quantified type variables
 *
 * Due to the fact that in general generalize(SM, Sτ)
 * is not equal to S(generalize(M, τ)),
 * implicit and explicit instance constraints
 * really have different semantics
 *
 * Note that for two types τ1 and τ2 the following
 * properties are valid
 *
 * S satisfies τ1 ≡ τ2  ⇐⇒  S satisfies τ1 ≤ τ2
 * S satisfies τ1 ≡ τ2  ⇐⇒  S satisfies τ1 ≤M=freevars(τ2) τ2
 * S satisfies τ1 ≤M τ2 ⇐⇒  S satisfies τ1 ≤ generalize(SM, Sτ2)
 *
 * The first two properties show that every equality
 * constraint can be expressed as an instance constraint
 * of either type, and the justification of these properties
 * is that the only generic instance of a type is the
 * type itself, and because generalize(freevars(τ), τ)
 * equals τ for all types τ
 *
 * The third property is justified by the fact that
 * substitution S is idempotent, from which follows that
 * S(generalize(SM, Sτ)) is equal to generalize(SM, Sτ).
 *
 * The algorithm used in the solve function assumes
 * that the constraint set always contains a constraint
 * which can be solved, and in particular, if it contains
 * only implicit instance constraints, then there is one
 * for which its condition is fulfilled
 *)
let rec solve : constr list -> subst = function
    | [] -> Map.empty
    | Equality(t1, t2)::c ->
        (* Find the substitution S for the most general unifier
         * and apply it to the constraint set C *)
        let s = mgu t1 t2 in
        compose (apply_constr' s c |> solve) s
    | ImplInstance(t1, t2, m)::c ->
        (* If the  constraint is solvable then
         * create a type scheme σ by generalizing
         * the type τ2 with the monomorphic set M
         * and transform the implicit instance constraint
         * to an explicit instance constraint *)
        let ftv = Set.diff (freevars t2) (Set.of_list m) in
        if Set.inter ftv (activevars' c) |> Set.is_empty
        then solve (ExplInstance(t1, generalize m t2)::c)
         (* TODO: Check if this is correct and terminates for
          * all inputs *)
        else solve (c @ [ImplInstance(t1, t2, m)])
    | ExplInstance(t, sigma)::c ->
        (* Instantiate the type scheme σ and convert
         * the explicit instance constraint to an
         * equality constraint between the type τ and
         * the newly instantiated type *)
            solve (Equality(t, instantiate sigma)::c)

(**
 * A type environment is denoted by Γ (uppercase gamma)
 *
 * In a type environment a type scheme σ
 * is paired with a variable name x
 *
 * Unlike algorithm W and M, which use the
 * type environment with top-down rules,
 * this algorithm the type environment Γ
 * is combined with the assumption set
 * to create an extra set of constraints
 *
 * A ≤ Γ = { τ ≤ σ | x:τ ∈ A, x:σ ∈ Γ }
 *
 * The following properties can be derived
 * from the definition of ≤
 *
 * (A1 ∪ A2) ≤ Γ = (A1 ≤ Γ) ∪ (A2 ≤ Γ)
 * A ≤ (Γ1 ∪ Γ2) = (A ≤ Γ1) ∪ (A ≤ Γ2)
 * A ≤ Γ\x       = A\x ≤ Γ
 * A ≤ {x: τ}    = {τ' ≡ τ | x:τ' ∈ A}
 *)
type env = scheme Map.t

(**
 * This function creates a constraint set C
 * given a type environment Γ and an
 * assumption set A
 *
 * In a correct program dom(A) ⊆ dom(Γ),
 * meaning that all the variable names
 * present in the assumption set A must
 * be bound in the type environment Γ
 *
 * TODO: Find a better name
 *)
let env_aux (gamma : env) (a : assump list) : constr list =
    match a with
    | [] -> []
    | _ ->
       List.fold_left (fun acc a' ->
           let Assumption(var, ty) = a' in
            match Map.find_opt var gamma with
            | Some sigma -> ExplInstance(ty, sigma) :: acc
            (* If domain(A) not a subset of domain(Γ) then
             * and undefined variable was referenced *)
            | None -> "Unbound variable " ^ var |> failwith
       ) [] a

(**
 * This algorithm computes a type τ
 * for an expression E given a type
 * environment Γ, also returning a
 * substitution S, giving it the same
 * signature of algorithm W
 *
 * Because there is no distinction
 * between the type variables introduced
 * by applying the inference rules,
 * and those (monomorphic) type variables
 * that occur in the initial type environment Γ,
 * a substitution can change the types in Γ
 *
 * infer(Γ, E) = (S, Sτ)
 *
 * NOTE: Algorithm W is a deterministic
 * instance of infer
 *)
let infer (gamma : env) (e : expr) : (subst * ty) =
    let (ty, a, c) = collect Set.empty e in
    let c' = env_aux gamma a in
    let s = solve (c @ c') in
    s, apply s ty
