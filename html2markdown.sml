(* get the longest none tag string *)
(* TODO use pattern matching here *)
fun get_longest_str(str) =
    if size(str) = 0 orelse String.sub(str, 0) = #"<" then ""
    else
        if size(str) = 1 then str
        else
            if String.sub(str, 1) = #"<" then String.str(String.sub(str, 0))
            else String.str(String.sub(str, 0)) ^ get_longest_str(String.extract(str, 1, NONE));

(* convert str to token list. *)
fun tokenize(str) =
    if size(str) = 0 then []
    else
        if String.substring(str, 0, 3) = "<p>"
        then String.substring(str, 0, 3)::tokenize(String.extract(str, 3, NONE)) (* 3 is only for tag <x> *)
        else if String.substring(str, 0, 4) = "</p>"
        then String.substring(str, 0, 4)::tokenize(String.extract(str, 4, NONE)) (* 4 is only for tag </x> *)
        else
            let val s = get_longest_str(str)
            in s::tokenize(String.extract(str, size(s), NONE))
            end;

(* datatype status = START | TAG_START | TAG_END | STRING; *)
(* (* convert str to token list. *) *)
(* fun tokenize(str) = *)

(* convert html string to markdown string *)
(* fun html2markdown(input) = *)
(*     if size(input) = 0 then "" *)
(*     else if substring(input, 0, 3) = "<p>" then *)
(*         let val endi = end_tag(input) *)
(*         in html2markdown(substring(input, 3, endi)) end *)
(*     else input; *)

(* test utils*)
fun test_string_eq(s1, s2) =
    if s1 = s2 then print("s1 and s2 are same: "^s1^".\n")
    else print("s1: "^s1^" s2: "^s2^" are not same.\n");

fun test_list_eq(l1, l2) =
    if l1 = l2 then (print("l1 and l2 are equal: [");
                     print(String.concatWith ", " l1);
                     print("]\n"))
    else print("l1: ["^(String.concatWith ", " l1)^" l2: "^(String.concatWith ", " l2)^" are not same.\n");

(* test for get_longest_str *)
test_string_eq(get_longest_str("ab<p>"), "ab");
test_string_eq(get_longest_str("a<p>"), "a");
test_string_eq(get_longest_str("<p>"), "");
test_string_eq(get_longest_str("a"), "a");
test_string_eq(get_longest_str("abc"), "abc");

(* test for tokenize *)
test_list_eq(tokenize("<p>a</p>"), ["<p>", "a", "</p>"]);
test_list_eq(tokenize("<p>ab</p>"), ["<p>", "ab", "</p>"]);
test_list_eq(tokenize("<p><p>a</p>ad< b</p>"), ["<p>", "<p>", "a", "</p>", "ad< b", "</p>"]);
