#use "reader.ml";;
open Reader;;

#use "topfind";;
#use "pc.ml";;
open PC;;


(* let test_tag_raises_6 test_ctxt = assert_raises X_this_should_not_happen (fun _ -> (Reader.read_sexpr "(1 #{foo}=2 3 #{foo}=4)"));;
let test_tag_raises_7 test_ctxt = assert_raises X_this_should_not_happen (fun _ -> (Reader.read_sexpr "(1 (#{x}=5 6) 7 8 (#{x}=3 5) 9)"));; *)

Reader.read_sexprs "(1 (#{x}=5 6) 7 8 (#{x}=3 5) 9)"