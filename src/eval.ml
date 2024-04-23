open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Int(num) -> Int(num)
  | String(string) -> String(string)
  | Bool(bool) -> Bool(bool)
  | ID(num) -> lookup env num
  | Not(exp) -> 
    let result = (eval_expr env exp) in
    (match result with
     | Bool(v) -> Bool(not v)
     | _ -> raise (TypeError "Expected type bool")
    )
  | Binop(op, e1, e2) -> 
    let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    if (op = Add || op = Sub || op = Mult || op = Div) then
      match op, val1, val2 with
      | Add, Int(v1), Int(v2) -> Int(v1 + v2)
      | Sub, Int(v1), Int(v2) -> Int(v1 - v2)
      | Mult, Int(v1), Int(v2) -> Int(v1 * v2)
      | Div, Int(v1), Int(v2) -> if (v2 != 0) then Int(v1 / v2) else raise DivByZeroError
      | _, _, _ -> raise (TypeError "Expected type int")
    else if (op = Greater || op = Less || op = GreaterEqual || op = LessEqual) then
      match op, val1, val2 with
      | Greater, Int(v1), Int(v2) -> Bool(v1 > v2)
      | Less, Int(v1), Int(v2) -> Bool(v1 < v2)
      | GreaterEqual, Int(v1), Int(v2) -> Bool(v1 >= v2)
      | LessEqual, Int(v1), Int(v2) -> Bool(v1 <= v2)
      | _ -> raise (TypeError "Expected type int")
    else if (op = Concat) then
      match op, val1, val2 with
      | Concat, String(v1), String(v2) -> String(v1 ^ v2)
      | _ -> raise (TypeError "Expected type string")
    else if (op = Equal || op = NotEqual) then
      match op, val1, val2 with
      | Equal, Int(v1), Int(v2) -> Bool(v1 = v2)
      | Equal, String(v1), String(v2) -> Bool(v1 = v2)
      | Equal, Bool(v1), Bool(v2) -> Bool(v1 = v2)
      | NotEqual, Int(v1), Int(v2) -> Bool(v1 <> v2)
      | NotEqual, String(v1), String(v2) -> Bool(v1 <> v2)
      | NotEqual, Bool(v1), Bool(v2) -> Bool(v1 <> v2)
      | _ -> raise (TypeError "Cannot compare types")
    else if (op = Or || op = And) then
      match op, val1, val2 with
      | Or, Bool(v1), Bool(v2) -> Bool(v1 || v2)
      | And, Bool(v1), Bool(v2) -> Bool(v1 && v2)
      | _ -> raise (TypeError "Expected type bool")
    else
      raise (TypeError "Type not found")
  | If(exp1, exp2, exp3) -> 
    let result = (eval_expr env exp1) in 
    (match result with
     | Bool(v1) -> if v1 then (eval_expr env exp2) else (eval_expr env exp3) 
     | _ -> raise (TypeError "Expected type bool")
    )
  | Let(name, is_rec, init_exp, body_exp) -> 
    if (not is_rec) then
      let result = (eval_expr env init_exp) in
      eval_expr (extend env name result) body_exp
    else 
      let tmp_env = extend_tmp env name in
      let result = (eval_expr tmp_env init_exp) in
      let _ = update tmp_env name result in
      eval_expr tmp_env body_exp
  | Fun(name, body_exp) -> Closure(env, name, body_exp)
  | App(expr1, expr2) -> 
    let c1 = eval_expr env expr1 in
    (match c1 with
     | Closure(env_A, name, e) -> 
       let e2 = eval_expr env expr2 in
       let env_A = extend env_A name e2 in
       eval_expr env_A e
     | _ -> raise (TypeError "Not a function")
    )
  | Record(fields) -> Record(fields)
  | Select(label, expr) -> 
    let record = eval_expr env expr in
    (match record with
     | Record(fields) ->
       let rec lookup_record record x =
         match record with
         | [] -> raise (SelectError ("Label not found"))
         | (var, value) :: t -> if x = var then value else lookup_record t x in
       lookup_record fields label
     | _ -> raise (TypeError "Not a record")
    )
  | _ -> raise (TypeError "Expression not found")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(name, expr) -> 
    let temp_env = extend_tmp env name in
    let result = eval_expr temp_env expr in
    let _ = update temp_env name result in
    (temp_env, Some result)
  | Expr(expr) -> (env, Some (eval_expr env expr))
  | NoOp -> (env, None)
  | _ -> raise (TypeError "Expression not found")
