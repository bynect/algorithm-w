open Ast

type subst = typ Map.t

let rec ftv_typ = function
  | TVar v -> Set.singleton v
  | TInt | TBool | TUnit -> Set.empty
  | TFun (p, r) -> Set.union (ftv_typ p) (ftv_typ r)
  | TTup l ->
      List.fold_left (fun acc ty -> Set.union (ftv_typ ty) acc) Set.empty l

let rec apply_typ ty s =
  match ty with
  | TVar v -> ( match Map.find_opt v s with Some t -> t | None -> ty)
  | TInt | TBool | TUnit -> ty
  | TFun (p, r) -> TFun (apply_typ p s, apply_typ r s)
  | TTup l -> TTup (List.map (fun ty -> apply_typ ty s) l)

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

(* Left biased union *)
let compose s1 s2 =
  Map.map (fun t -> apply_typ t s1) s2 |> Map.union (fun _ x _ -> Some x) s1

let ( ++ ) = compose

let new_var pref = TVar (State.next () |> Printf.sprintf "%s%i" pref)

let bind_var ty v =
  match ty with
  | TVar v' when v = v' -> Map.empty
  | _ ->
      if Set.mem v (ftv_typ ty) then
        Printf.sprintf "Occurs check failed for %s in %s" v (string_of_typ ty)
        |> failwith
      else Map.singleton v ty

let generalize ty ctx =
  let vars = Set.diff (ftv_typ ty) (ftv_ctx ctx) |> Set.to_seq in
  Scheme (List.of_seq vars, ty)

let istantiate = function
  | Scheme (vars, ty) ->
      let vars' =
        (*let new_var' v = new_var Char.escaped v.[0] in*)
        let new_var' _ = new_var "a" in
        vars |> List.map new_var'
      in
      List.combine vars vars' |> List.to_seq |> Map.of_seq |> apply_typ ty

let unify ty1 ty2 =
  let unify_err ty1 ty2 =
    let ty1' = string_of_typ ty1 and ty2' = string_of_typ ty2 in
    Printf.sprintf "Unification failed for %s and %s" ty1' ty2' |> failwith
  in
  let rec unify = function
    | TVar v, ty | ty, TVar v -> bind_var ty v
    | TInt, TInt -> Map.empty
    | TBool, TBool -> Map.empty
    | TUnit, TUnit -> Map.empty
    | TFun (p, r), TFun (p', r') ->
        let s1 = unify (p, p') in
        let s2 = unify (apply_typ r s1, apply_typ r' s1) in
        s2 ++ s1
    | TTup l, TTup l' ->
        if List.length l != List.length l' then unify_err ty1 ty2
        else
          List.fold_left2
            (fun acc ty1 ty2 ->
              unify (apply_typ ty1 acc, apply_typ ty2 acc) ++ acc)
            Map.empty l l'
    | ty1, ty2 -> unify_err ty1 ty2
  in
  unify (ty1, ty2)

let rec infer (exp : exp) ctx =
  match exp with
  | Var v -> (
      match Map.find_opt v ctx with
      | Some scheme -> (Map.empty, istantiate scheme)
      | None -> Printf.sprintf "Unbound variable %s" v |> failwith)
  | App (f, a) ->
      let s1, ty = infer f ctx in
      let s2, p = infer a (apply_ctx ctx s1) in
      let r = new_var "a" in
      let s3 = unify (apply_typ ty s2) (TFun (p, r)) in
      (s3 ++ s2 ++ s1, apply_typ r s3)
  | Fun (x, b) ->
      let r = new_var "a" in
      let ctx' = Map.add x (Scheme ([], r)) ctx in
      let s, p = infer b ctx' in
      (s, TFun (apply_typ r s, p))
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
      let s3, ty2 = infer e (apply_ctx ctx s1) in
      let s4 = unify (apply_typ cond s1) TBool in
      let s5 = unify (apply_typ ty1 s3) (apply_typ ty2 s3) in
      (s5 ++ s4 ++ s3 ++ s2 ++ s1, apply_typ ty1 s4)
  | Tup [] -> (Map.empty, TUnit)
  | Tup l ->
      let s, tys =
        List.fold_left
          (fun (acc, tys) exp ->
            let s, ty = apply_ctx ctx acc |> infer exp in
            (s ++ acc, ty :: tys))
          (Map.empty, []) l
      in
      (s, apply_typ (TTup tys) s)
  | Lit (Bool _) -> (Map.empty, TBool)
  | Lit (Int _) -> (Map.empty, TInt)
  | Annot (e, ty) ->
      let s, ty' = infer e ctx in
      let s' = unify ty ty' ++ s in
      (s', apply_typ ty' s')

let infer_exp (exp : exp) (ctx : scheme Map.t) : typ =
  State.reset ();
  let s, ty = infer exp ctx in
  apply_typ ty s
