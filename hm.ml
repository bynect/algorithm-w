open Com

type scheme = Scheme of string list * typ

type ctx = scheme Map.t

type subst = typ Map.t

let rec ftv_typ = function
  | Var v -> Set.singleton v
  | Int | Bool -> Set.empty
  | Fun (p, r) -> Set.union (ftv_typ p) (ftv_typ r)

let rec apply_typ ty s =
  match ty with
  | Var v -> ( match Map.find_opt v s with Some t -> t | None -> ty)
  | Int | Bool -> ty
  | Fun (p, r) -> Fun (apply_typ p s, apply_typ r s)

let ftv_scheme = function
  | Scheme (vars, ty) -> Set.diff (ftv_typ ty) (Set.of_list vars)

let apply_scheme scheme s =
  match scheme with
  | Scheme (vars, ty) ->
      let s' = List.fold_right (fun v acc -> Map.remove v acc) vars s in
      Scheme (vars, apply_typ ty s')

let ftv_ctx ctx =
  Set.empty
  |> Map.fold (fun _ scheme acc -> Set.union acc (ftv_scheme scheme)) ctx

let apply_ctx (ctx : ctx) s = Map.map (fun scheme -> apply_scheme scheme s) ctx

let compose s1 s2 =
  Map.map (fun t -> apply_typ t s1) s2 |> Map.union (fun _ a _ -> Some a) s1

let ( ++ ) = compose

let new_var pref : typ = Var (State.next () |> Printf.sprintf "%s%i" pref)

let bind_var ty v =
  match ty with
  | Var v' when v = v' -> Map.empty
  | _ ->
      if Set.mem v (ftv_typ ty) then
        Printf.sprintf "Occurs check failed for %s and %s" v (string_of_typ ty)
        |> failwith
      else Map.singleton v ty

let generalize ty ctx =
  let vars = Set.diff (ftv_typ ty) (ftv_ctx ctx) |> Set.to_seq in
  Scheme (List.of_seq vars, ty)

let istantiate = function
  | Scheme (vars, ty) ->
      let vars' = vars |> List.map (fun v -> new_var (Char.escaped v.[0])) in
      List.combine vars vars' |> List.to_seq |> Map.of_seq |> apply_typ ty

let unify ty1 ty2 =
  let rec unify = function
    | Var v, ty | ty, Var v -> bind_var ty v
    | Int, Int -> Map.empty
    | Bool, Bool -> Map.empty
    | Fun (p, r), Fun (p', r') ->
        let s = unify (p, p') in
        unify (apply_typ r s, apply_typ r' s) |> compose s
    | ty1, ty2 ->
        let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
        Printf.sprintf "Unification failed for %s and %s" ty1' ty2' |> failwith
  in
  unify (ty1, ty2)

let rec infer (exp : exp) ctx =
  match exp with
  | Var v -> (
      match Map.find_opt v ctx with
      | Some scheme -> (Map.empty, istantiate scheme)
      | None -> Printf.sprintf "Unbound variable %s" v |> failwith)
  | App (f, a) ->
      let s1, ty1 = infer f ctx in
      let s2, ty2 = infer a (apply_ctx ctx s1) in
      let ret = new_var "a" in
      let s3 = unify (apply_typ ty1 s2) (Fun (ty2, ret)) in
      (s3 ++ s2 ++ s1, apply_typ ret s3)
  | Fun (x, b) ->
      let ret = new_var "a" in
      let ctx' = Map.add x (Scheme ([], ret)) ctx in
      let s, ty = infer b ctx' in
      (s, Fun (apply_typ ret s, ty))
  | Let (x, v, b) ->
      let s1, ty1 = infer v ctx in
      let ctx' = Map.remove x ctx in
      let scheme = apply_ctx ctx' s1 |> generalize ty1 in
      let ctx' = Map.add x scheme ctx' in
      let s2, ty2 = infer b (apply_ctx ctx' s1) in
      (s2 ++ s1, ty2)
  | If (c, t, e) ->
      let s1, cond = infer c ctx in
      let s2, ty1 = infer t (apply_ctx ctx s1) in
      let s3, ty2 = infer e (apply_ctx ctx s2) in
      let s4 =
        unify (apply_typ cond s1) Bool
        ++ unify (apply_typ ty1 s3) (apply_typ ty2 s3)
      in
      (s4 ++ s3 ++ s2 ++ s1, apply_typ ty1 s4)
  | Lit (Bool _) -> (Map.empty, Bool)
  | Lit (Int _) -> (Map.empty, Int)

let infer_exp (exp : exp) (ctx : scheme Map.t) : typ =
  State.reset ();
  let s, ty = infer exp ctx in
  apply_typ ty s
