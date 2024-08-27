open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  let apply_int_operation (op : int -> int -> int) (div : bool) (e1 : exp) (e2: exp) : value =
    match (eval_expr e1 env, eval_expr e2 env) with
      | (Int_Val a, Int_Val b) -> 
          if not div then Int_Val (op a b) 
          else
            if (b = 0) then raise DivByZeroError
            else Int_Val (op a b)
      | _ -> raise TypeError
  in
  let apply_bool_operation (op : bool -> bool -> bool) (e1 : exp) (e2: exp) : value =
    match (eval_expr e1 env, eval_expr e2 env) with
      | (Bool_Val a, Bool_Val b) -> Bool_Val (op a b) 
      | _ -> raise TypeError
  in
  let apply_comp_operation (op : 'a -> 'a -> bool) (e1 : exp) (e2: exp) : value =
    match (eval_expr e1 env, eval_expr e2 env) with
      | (Int_Val a, Int_Val b) -> Bool_Val (op a b) 
      | _ -> raise TypeError
  in
  match e with
    | Var n -> if List.mem_assoc n env then List.assoc n env else raise UndefinedVar
    | Number n -> Int_Val n

    | Plus (e1, e2) -> apply_int_operation (+) false e1 e2
    | Minus (e1, e2) -> apply_int_operation (-) false e1 e2
    | Times (e1, e2) -> apply_int_operation ( * ) false e1 e2
    | Div (e1, e2) -> apply_int_operation (/) true e1 e2
    | Mod (e1, e2) -> apply_int_operation (mod) true e1 e2

    | Eq (e1, e2) -> 
      (match (eval_expr e1 env, eval_expr e2 env) with
      | (Int_Val a, Int_Val b) -> Bool_Val (a = b)
      | (Bool_Val a, Bool_Val b) -> Bool_Val (a = b)
      | _ -> raise TypeError)
    | Leq (e1, e2) -> apply_comp_operation (<=) e1 e2
    | Lt (e1, e2) -> apply_comp_operation (<) e1 e2
    | Not e ->     
      (match (eval_expr e env) with
        | (Bool_Val a) -> Bool_Val (not a) 
        | _ -> raise TypeError)
    | And (e1, e2) -> apply_bool_operation (&&) e1 e2
    | Or (e1, e2) -> apply_bool_operation (||) e1 e2
    | True -> Bool_Val true
    | False -> Bool_Val false
    | App (e1, e2) -> 
      (match (eval_expr e1 env) with
        | Closure (env', v, e) -> 
            let x = eval_expr e2 env in
            eval_expr e ((v, x)::env')
        | _ -> raise TypeError)
    | Fun (x, e) -> Closure (env, x, e)


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
    | While (e, c) -> 
      (match (eval_expr e env) with
        | (Bool_Val b) -> 
          if b then eval_command (While (e, c)) (eval_command c env)
          else env
        | _ -> raise TypeError)
    | For (e, c) -> 
      (match (eval_expr e env) with
        | (Int_Val n) -> 
          if n > 0 then eval_command (For (Number (n - 1), c)) (eval_command c env)
          else env
        | _ -> raise TypeError)
    | Cond (e, c1, c2) -> 
      (match (eval_expr e env) with
      | (Bool_Val b) -> eval_command (if b then c1 else c2) env
      | _ -> raise TypeError)
    | Comp (c1, c2) -> eval_command c2 (eval_command c1 env)
    | Assg (x, e) ->
      if List.mem_assoc x env then 
        match (List.assoc x env, eval_expr e env) with
          | (Int_Val _, Int_Val y) -> (x, Int_Val y) :: List.remove_assoc x env
          | (Bool_Val _, Bool_Val y) -> (x, Bool_Val y) :: List.remove_assoc x env
          | (Closure _, Closure (env',v,e)) -> (x, Closure (env', v, e)) :: List.remove_assoc x env
          | _ -> raise TypeError
      else raise UndefinedVar
    | Declare (t, x) ->
      (match t with
        | Int_Type -> (x, Int_Val 0) :: env
        | Bool_Type -> (x, Bool_Val false) :: env
        | Lambda_Type -> (x, Closure (env, x, Var "x")) :: env)
    | Skip -> env
