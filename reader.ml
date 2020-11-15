
#use "pc.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader
: sig
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


let read_sexprs string = raise X_not_yet_implemented;;
end;; (* struct Reader *)

(*val make_paired : ('a -> 'b * 'c) -> ('d -> 'e * 'f) -> ('c -> 'g * 'd) -> 'a -> 'g * 'f = <fun>  *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (fun (_,d) -> d) in
  let nt = caten nt nt_right in
  pack nt (fun (d,_) -> d);;

(*3.2.1*)
(*val nt_whitespaces : char list -> char list * char list = <fun> *)
let nt_whitespaces = star(nt_whitespace);;

(*val make_spaced : (char list -> 'a * char list) -> char list -> 'a * char list = <fun> *)
(*The parser will skips whitespaces from left and right*)
let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;

(*3.2.2*)
(*char list -> sexpr * char list = <fun> *)
let nt_line_comment =
  let comment_start = (char ';') in
  let end_of_line = char (char_of_int 10) in
  let comment_end = disj end_of_line (pack nt_end_of_input (fun (d) -> 'd')) in
  let comment = star(diff nt_any comment_end) in
  let nt = caten comment_start comment in
  let nt = pack nt (fun (_,d)->d) in
  let nt = caten nt comment_end in
  let nt = pack nt (fun (_,d)->Nil) in
  make_spaced nt;;

(*3.3.1*)
(*char list -> sexpr * char list = <fun> *)
let nt_boolean = 
  let hashtag = (char '#') in
  let bool_true = (char_ci 't') in
  let bool_false = (char_ci 'f') in
  let hashtag_true = caten hashtag bool_true in
  let hashtag_false = caten hashtag bool_false in
  let bool_true = pack hashtag_true (fun (t) -> true) in
  let bool_false = pack hashtag_false (fun (f) -> false) in
  let nt = disj bool_true bool_false in
  let nt = pack nt (fun (x) -> (Bool x)) in
  make_spaced nt;;

(*3.3.2*)


(*3.3.4*)
let nt_string = 
  let quote = char (char_of_int 34) in
  let string_char = diff nt_any (disj (char (char_of_int 92)) (char (char_of_int 34))) in
  let meta_chars = disj_list [
    pack (word "\\r") (fun(_) -> (char_of_int 13));
    pack (word "\\n") (fun(_) -> (char_of_int 10));
    pack (word "\\t") (fun(_) -> (char_of_int 9));
    pack (word "\\f") (fun(_) -> (char_of_int 12));
    pack (word "\\\\") (fun(_) -> (char_of_int 92));
    pack (word "\\\"") (fun(_) -> (char_of_int 34));
  ] in
  let string = disj string_char meta_chars in
  let nt = caten quote (star(string)) in
  let nt = pack nt (fun (_,s) -> s) in
  let nt = caten nt quote in 
  let nt = pack nt (fun (s,_)-> (String(list_to_string s))) in 
   (* let nt = pack nt (fun(s))  *)
  make_spaced nt;;

(*char list -> sexpr * char list = <fun> *)
(*3.3.5*)
  let nt_char = 
    let char_start = caten (char '#') (char '\\') in
    let visible_char = const (fun ch -> (char_of_int 32) < ch) in
    let named_char = disj_list [
      pack (word_ci "nul") (fun(_) -> (char_of_int 0));
      pack (word_ci "newline") (fun(_) -> (char_of_int 10));
      pack (word_ci "return") (fun(_) -> (char_of_int 13));
      pack (word_ci "tab") (fun(_) -> (char_of_int 9));
      pack (word_ci "page") (fun(_) -> (char_of_int 12));
      pack (word_ci "space") (fun(_) -> (char_of_int 32));
    ] in
    let nt = caten char_start (disj named_char visible_char) in  
    let nt = pack nt (fun (_,ch) -> Char ch) in
    make_spaced nt;;




let test_string nt str = let (e, s) = (nt (string_to_list str)) in (e, (Printf.sprintf "->[%s]" (list_to_string s)));;

