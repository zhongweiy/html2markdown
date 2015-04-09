(* get the longest none tag string *)
(* TODO use pattern matching here? *)
fun get_longest_str(str) =
    if size(str) = 0 orelse String.sub(str, 0) = #"<" then ""
    else
        if size(str) = 1 then str
        else
            if String.sub(str, 1) = #"<" then String.str(String.sub(str, 0))
            else String.str(String.sub(str, 0)) ^ get_longest_str(String.extract(str, 1, NONE));

(* convert str to token list. *)
(* fun tokenize(str) = *)
(*     if size(str) = 0 then [] *)
(*     else *)
(*         if sub(str, 0) = "<" *)
(*         then substring(str, 0, 3)::tokenize(extract(str, 3)) (* 3 is only for tag <x> *) *)
(*         else *)
(*             let val s = get_longest_str(str); *)
                
                                       

(* convert html string to markdown string *)
(* fun html2markdown(input) = *)
(*     if size(input) = 0 then "" *)
(*     else if substring(input, 0, 3) = "<p>" then *)
(*         let val endi = end_tag(input) *)
(*         in html2markdown(substring(input, 3, endi)) end *)
(*     else input; *)

(* test *)
fun test_equal(s1, s2) =
    if s1 = s2 then print("s1 and s2 are same: "^s1^".\n")
    else print("s1: "^s1^" s2: "^s2^" are not same.\n");

test_equal(get_longest_str("ab<p>"), "ab");
test_equal(get_longest_str("a<p>"), "a");
test_equal(get_longest_str("<p>"), "");
test_equal(get_longest_str("a"), "a");
test_equal(get_longest_str("abc"), "abc");
