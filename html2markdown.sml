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
fun tokenize_old(str) =
    if size(str) = 0 then []
    else
        if String.substring(str, 0, 3) = "<p>"
        then String.substring(str, 0, 3)::tokenize_old(String.extract(str, 3, NONE)) (* 3 is only for tag <x> *)
        else if String.substring(str, 0, 4) = "</p>"
        then String.substring(str, 0, 4)::tokenize_old(String.extract(str, 4, NONE)) (* 4 is only for tag </x> *)
        else
            let val s = get_longest_str(str)
            in s::tokenize_old(String.extract(str, size(s), NONE))
            end;

(* convert str to token list by automata *)
fun tokenize(s) =
    let val edges =
            vector[ (* < > * *)
                (* state 0 *)vector[0, 0, 0],
                (* state 1 *)vector[2, 0, 0],
                (* state 2 *)vector[2, 3, 2],
                (* state 3 *)vector[2, 4, 4],
                (* state 4 *)vector[5, 4, 4],
                (* state 5 *)vector[2, 3, 2]]
        val cs = String.explode(s)

        fun c2i (#"<") = 0
          | c2i (#">") = 1
          | c2i (_) = 2
        fun automata ([], _) = [] (* TODO use longest match rule here and remove state 5 *)
          | automata (n::ns, cur) =
            let val nx = Vector.sub(Vector.sub(edges, cur), c2i n)
                val it = automata(ns, nx)
            in case nx
                of 3 => String.str(n)::it
                 | 5 => ""::(String.str(n)^hd it::tl it) (* case like: "abc</p>" when n is "<" *)
                 | 0 => [""]
                 | _ => String.str(n)^hd it::tl it
            end
    in
        automata(cs, 1)
    end

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
    else print("l1: ["^(String.concatWith ", " l1)^"] l2: ["^(String.concatWith ", " l2)^"] are not same.\n");

(* test for get_longest_str *)
test_string_eq(get_longest_str("ab<p>"), "ab");
test_string_eq(get_longest_str("a<p>"), "a");
test_string_eq(get_longest_str("<p>"), "");
test_string_eq(get_longest_str("a"), "a");
test_string_eq(get_longest_str("abc"), "abc");

(* test for tokenize_old *)
test_list_eq(tokenize_old("<p>a</p>"), ["<p>", "a", "</p>"]);
test_list_eq(tokenize_old("<p>ab</p>"), ["<p>", "ab", "</p>"]);
(* test_list_eq(tokenize_old("<p><p>a</p>ad< b</p>"), ["<p>", "<p>", "a", "</p>", "ad< b", "</p>"]); *)

(* test for tokenize_old *)
test_list_eq(tokenize("<pa><a>"), ["<pa>","<a>"]);
test_list_eq(tokenize("<pa>a<pa>"), ["<pa>","a", "<pa>"]);
test_list_eq(tokenize("<paa>"), ["<paa>"]);
test_list_eq(tokenize("<p>a</p>"), ["<p>", "a", "</p>"]);
test_list_eq(tokenize("<p>ab</p>"), ["<p>", "ab", "</p>"]);
test_list_eq(tokenize("<p><p>a</p>ad< b</p>"), ["<p>", "<p>", "a", "</p>", "ad", "< b</p>"]);
