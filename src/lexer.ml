open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec tokenize_matcher pos str =
    if (pos >= String.length str) then
      []
    else
    if (Str.string_match (Str.regexp "[ \t\n]+") str pos) then
      tokenize_matcher (pos + String.length (Str.matched_string str)) str
    else if (Str.string_match (Str.regexp "(-[0-9]+)") str pos) then
      let token = Str.matched_string str in
      let token_len =  String.length token in
      let sub_token = (String.sub token 1 (token_len - 2)) in
      (Tok_Int (int_of_string sub_token))::(tokenize_matcher (pos + token_len) str)

    else if (Str.string_match (Str.regexp "[0-9]+") str pos) then
      let token = Str.matched_string str in 
      (Tok_Int (int_of_string token))::(tokenize_matcher (pos + String.length token) str)

    else if (Str.string_match (Str.regexp "\"[^\"]*\"") str pos) then
      let token = Str.matched_string str in 
      let token_len =  String.length token in
      let sub_token = (String.sub token 1 (token_len - 2)) in
      (Tok_String sub_token)::(tokenize_matcher (pos + String.length token) str)

    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos) then
      let token = Str.matched_string str in 
      if (token = "not") then
        Tok_Not::(tokenize_matcher (pos + 3) str)
      else if (token = "if") then
        Tok_If::(tokenize_matcher (pos + 2) str)
      else if (token = "then") then
        Tok_Then::(tokenize_matcher (pos + 4) str) 
      else if (token = "else") then
        Tok_Else::(tokenize_matcher (pos + 4) str)
      else if (token = "let") then
        Tok_Let::(tokenize_matcher (pos + 3) str)
      else if (token = "def") then
        Tok_Def::(tokenize_matcher (pos + 3) str)
      else if (token = "in") then
        Tok_In::(tokenize_matcher (pos + 2) str)
      else if (token = "rec") then
        Tok_Rec::(tokenize_matcher (pos + 3) str)
      else if (token = "fun") then
        Tok_Fun::(tokenize_matcher (pos + 3) str)
      else if (token = "true") then
        (Tok_Bool true)::(tokenize_matcher (pos + 4) str)
      else if (token = "false") then
        (Tok_Bool false)::(tokenize_matcher (pos + 5) str)
      else
        (Tok_ID token)::(tokenize_matcher (pos + String.length token) str)

    else if (Str.string_match (Str.regexp "(") str pos) then
      Tok_LParen::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp ")") str pos) then
      Tok_RParen::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "{") str pos) then
      Tok_LCurly::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "}") str pos) then
      Tok_RCurly::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "\\.") str pos) then
      Tok_Dot::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "<>") str pos) then
      Tok_NotEqual::(tokenize_matcher (pos + 2) str)

    else if (Str.string_match (Str.regexp ">=") str pos) then
      Tok_GreaterEqual::(tokenize_matcher (pos + 2) str)

    else if (Str.string_match (Str.regexp "<=") str pos) then
      Tok_LessEqual::(tokenize_matcher (pos + 2) str)

    else if (Str.string_match (Str.regexp "->") str pos) then
      Tok_Arrow::(tokenize_matcher (pos + 2) str) 

    else if (Str.string_match (Str.regexp "=") str pos) then
      Tok_Equal::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp ">") str pos) then
      Tok_Greater::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "<") str pos) then
      Tok_Less::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "||") str pos) then
      Tok_Or::(tokenize_matcher (pos + 2) str) 

    else if (Str.string_match (Str.regexp "\\&\\&") str pos) then
      Tok_And::(tokenize_matcher (pos + 2) str)

    else if (Str.string_match (Str.regexp "+") str pos) then
      Tok_Add::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "-") str pos) then
      Tok_Sub::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "*") str pos) then
      Tok_Mult::(tokenize_matcher (pos + 1) str) 

    else if (Str.string_match (Str.regexp "/") str pos) then
      Tok_Div::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp "\\^") str pos) then
      Tok_Concat::(tokenize_matcher (pos + 1) str)

    else if (Str.string_match (Str.regexp ";;") str pos) then
      Tok_DoubleSemi::(tokenize_matcher (pos + 2) str)

    else if (Str.string_match (Str.regexp ";") str pos) then
      Tok_Semi::(tokenize_matcher (pos + 1) str) 

    else 
      []
  in
  tokenize_matcher 0 input
;;