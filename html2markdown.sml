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
(* status abbrev: start -> s, tag start -> ts, string -> str, end -> e *)
(* | current | next | input         | *)
(* |---------+------+---------------| *)
(* | s       | ts   | <             | *)
(* | s       | str  | not <         | *)
(* | ts      | e    | EOF           | *)
(* | ts      | e    | >             | *)
(* | ts      | ts   | not > and EOF | *)
(* | str     | ts   | <             | *)
(* | str     | e    | EOF           | *)
(* | str     | str  | not <         | *)
(* | e       | ts   | <             | *)
(* | e       | str  | not <         | *)
(* EOF is "0" here *)
datatype status = START | TAG_START | END | STRING;
fun tokenize(s) =
    let fun automata (START, #"<") = TAG_START
          | automata (START, _) = STRING
          | automata (TAG_START, #">") = END
          | automata (TAG_START, #"0") = END
          | automata (TAG_START, _) = TAG_START
          | automata (STRING, #"<") = TAG_START
          | automata (STRING, #"0") = END
          | automata (STRING, _) = STRING
          | automata (END, #"<") = TAG_START
          | automata (END, _) = STRING
        val c = String.explode(s)@[#"0"] (* append character 0 to note EOF *)
        fun help ([], _) = [""]
          | help (n::ns, TAG_START) =
            let val nx = automata(TAG_START, n)
                val it = help(ns, nx)
            in 
                if nx = END then String.str(n)::it
                else String.str(n)^(hd it)::(tl it) 
            end
          | help (n::ns, STRING) =
            let val nx = automata(STRING, n)
                val it = help(ns, nx)
            in
                if nx = END orelse nx = TAG_START then String.str(n)::it
                else String.str(n)^(hd it)::(tl it) 
            end
          | help (n::ns, START) = 
            let val nx = automata(START, n)
                val it = help(ns, nx)
            in 
                String.str(n)^(hd it)::(tl it) 
            end
          | help (n::ns, END) = 
            let val nx = automata(END, n)
                val it = help(ns, nx)
            in 
                String.str(n)^(hd it)::(tl it) 
            end
    in 
        help(c, START)
    end

(* datatype status = START | TAG_START | TAG_END | STRING; *)
(* (* convert str to token list. *) *)
(* fun tokenize_old(str) = *)

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
test_list_eq(tokenize("<pa>a"), ["<pa>","a"]);
test_list_eq(tokenize("<paa>"), ["<paa>"]);
test_list_eq(tokenize("<p>a</p>"), ["<p>", "a", "</p>"]);
test_list_eq(tokenize("<p>ab</p>"), ["<p>", "ab", "</p>"]);
test_list_eq(tokenize("<p><p>a</p>ad< b</p>"), ["<p>", "<p>", "a", "</p>", "ad< b", "</p>"]);
