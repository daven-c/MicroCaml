open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
    raise
      (InvalidInputException
         (Printf.sprintf "Expected %s from input %s, got %s"
            (string_of_token tok)
            (string_of_list string_of_token toks)
            (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let opt2tok tok =
  match tok with 
  | Some x -> x
  | _ -> failwith("InvalidOptTok")

let get_id tok = 
  let tok = opt2tok tok in 
  match tok with 
  | Tok_ID(v) -> v
  | _ -> failwith("InvalidID")

let rec parse_expr toks = 
  match lookahead toks with 
  | Some Tok_Let -> parse_let toks
  | Some Tok_Fun -> parse_fun toks
  | Some Tok_If -> parse_if toks
  | _ -> parse_or toks

and parse_let toks = 
  let toks = match_token toks Tok_Let in
  let is_rec = ((lookahead toks) = Some Tok_Rec) in
  let toks = if (is_rec) then match_token toks Tok_Rec else toks in
  let id = lookahead toks in
  let toks = match_token toks (opt2tok id) in
  let toks = match_token toks Tok_Equal in
  let (toks, exp1) = parse_expr toks in
  let toks = match_token toks Tok_In in
  let (toks, exp2) = parse_expr toks in
  (toks, Let(get_id id, is_rec, exp1, exp2))

and parse_fun toks = 
  let toks = match_token toks Tok_Fun in
  let id = lookahead toks in
  let toks = match_token toks (opt2tok id) in
  let toks = match_token toks Tok_Arrow in
  let (toks, exp1) = parse_expr toks in
  (toks, Fun(get_id id, exp1))

and parse_if toks = 
  let toks = match_token toks Tok_If in
  let (toks, exp1) = parse_expr toks in
  let toks = match_token toks Tok_Then in
  let (toks, exp2) = parse_expr toks in
  let toks = match_token toks Tok_Else in
  let (toks, exp3) = parse_expr toks in
  (toks, If(exp1, exp2, exp3))

and parse_or toks = 
  let (toks, exp1) = parse_and toks in
  if (lookahead toks = Some Tok_Or) then 
    let toks = match_token toks Tok_Or in
    let (toks, exp2) = parse_or toks in
    (toks, Binop(Or, exp1, exp2))
  else
    (toks, exp1)

and parse_and toks = 
  let (toks, exp1) = parse_equality toks in
  if (lookahead toks = Some Tok_And) then 
    let toks = match_token toks Tok_And in
    let (toks, exp2) = parse_and toks in
    (toks, Binop(And, exp1, exp2))
  else
    (toks, exp1)

and parse_equality toks = 
  let (toks, exp1) = parse_relational toks in
  let equality = lookahead toks in
  if (equality = Some Tok_Equal || equality = Some Tok_NotEqual) then 
    let toks = match_token toks (opt2tok equality) in
    let (toks, exp2) = parse_equality toks in
    (toks, Binop((if (equality = Some Tok_Equal) then Equal else NotEqual), exp1, exp2))
  else
    (toks, exp1)

and parse_relational toks = 
  let (toks, exp1) = parse_additive toks in
  let equality = lookahead toks in
  if (equality = Some Tok_Less || equality = Some Tok_Greater || equality = Some Tok_LessEqual || equality = Some Tok_GreaterEqual) then 
    let toks = match_token toks (opt2tok equality) in
    let (toks, exp2) = parse_equality toks in
    let equality = if (equality = Some Tok_Less) then Less else if (equality = Some Tok_Greater) then Greater else if (equality = Some Tok_LessEqual) then LessEqual else GreaterEqual in
    (toks, Binop(equality, exp1, exp2))
  else
    (toks, exp1)

and parse_additive toks = 
  let (toks, exp1) = parse_mulltiplicative toks in
  let operator = lookahead toks in
  if (operator = Some Tok_Add || operator = Some Tok_Sub) then 
    let toks = match_token toks (opt2tok operator) in
    let (toks, exp2) = parse_additive toks in
    let operator = if (operator = Some Tok_Add) then Add else Sub in
    (toks, Binop(operator, exp1, exp2))
  else
    (toks, exp1)

and parse_mulltiplicative toks = 
  let (toks, exp1) = parse_concat toks in
  let operator = lookahead toks in
  if (operator = Some Tok_Mult || operator = Some Tok_Div) then 
    let toks = match_token toks (opt2tok operator) in
    let (toks, exp2) = parse_mulltiplicative toks in
    let operator = if (operator = Some Tok_Mult) then Mult else Div in
    (toks, Binop(operator, exp1, exp2))
  else
    (toks, exp1)

and parse_concat toks = 
  let (toks, exp1) = parse_unary toks in
  if ((lookahead toks) = Some Tok_Concat) then 
    let toks = match_token toks Tok_Concat in
    let (toks, exp2) = parse_additive toks in
    (toks, Binop(Concat, exp1, exp2))
  else
    (toks, exp1)

and parse_unary toks = 
  if ((lookahead toks) = Some Tok_Not) then
    let toks = match_token toks Tok_Not in
    let (toks, exp2) = parse_unary toks in
    (toks, Not(exp2))
  else
    parse_app toks


and parse_app toks = 
  let (toks, exp1) = parse_select toks in
  let prim_type = lookahead toks in
  match prim_type with
  | Some (Tok_Int _) | Some (Tok_Bool _) | Some (Tok_String _) | Some (Tok_ID _) | Some Tok_LParen | Some Tok_LCurly ->
    let (toks, exp2) = parse_primary toks in
    (toks, App(exp1, exp2))
  |_ ->
    (toks, exp1)

and parse_select toks = 
  let (toks, exp1) = parse_primary toks in
  if ((lookahead toks) = Some Tok_Dot) then
    let toks = match_token toks Tok_Dot in
    let id = lookahead toks in
    let toks = match_token toks (opt2tok id) in
    (toks, Select(Lab(get_id id), exp1))
  else
    (toks, exp1)

and parse_primary toks = 
  let prim_type = lookahead toks in
  match prim_type with
  | Some Tok_Int(v) -> let toks = match_token toks (opt2tok prim_type) in (toks, Int(v))
  | Some Tok_Bool(v) -> let toks = match_token toks (opt2tok prim_type) in (toks, Bool(v))
  | Some Tok_String(v) -> let toks = match_token toks (opt2tok prim_type) in (toks, String(v))
  | Some Tok_ID(v) -> let toks = match_token toks (opt2tok prim_type) in (toks, ID(v))
  | Some Tok_LParen -> let toks = match_token toks Tok_LParen in
    let (toks, exp1) = parse_expr toks in
    let toks = match_token toks Tok_RParen in
    (toks, exp1)
  | Some Tok_LCurly -> parse_record toks
  | _ ->  raise (InvalidInputException "")

and parse_record toks = 
  let toks = match_token toks Tok_LCurly in
  if ((lookahead toks) = Some Tok_RCurly) then
    let toks = match_token toks Tok_RCurly in
    (toks, Record([]))
  else
    let (toks, exp1) = parse_record_body toks in
    let toks = match_token toks Tok_RCurly in
    (toks, exp1)

and parse_record_body toks = 
  let id = lookahead toks in
  let toks = match_token toks (opt2tok id) in
  let toks = match_token toks Tok_Equal in
  let (toks, exp1) = parse_expr toks in
  if ((lookahead toks) = Some Tok_Semi) then
    let toks = match_token toks Tok_Semi in
    let (toks, exp2) = parse_record_body toks in
    let rec_list = match exp2 with 
      | Record(li) -> li 
      | _ -> raise (InvalidInputException "") in
    (toks, Record((Lab(get_id id), exp1)::rec_list))
  else
    (toks, Record([(Lab(get_id id), exp1)]))

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_DoubleSemi ->
    let toks = match_token toks Tok_DoubleSemi in
    (toks, NoOp)
  | Some Tok_Def -> 
    let toks = match_token toks Tok_Def in
    let id = lookahead toks in
    let toks = match_many toks [opt2tok id; Tok_Equal] in
    let (toks, expr) = parse_expr toks in
    let toks = match_token toks Tok_DoubleSemi in
    (toks, Def(get_id id, expr))
  | _ -> 
    let (toks, expr) = parse_expr toks in
    let toks = match_token toks Tok_DoubleSemi in
    (toks, Expr(expr))

