{
open CSS_EscapeParser
open Lexing

let unesc hexstr = let n = int_of_string ("0x" ^ hexstr) in
		match (n >= 65 && n < 127) || n == 45 with
			| true -> String.make 1 (char_of_int n)
			| false -> "\\" ^ hexstr
		;;
}

let	h	= ['0'-'9' 'a' - 'f' 'A' - 'F']
let	hh	= h h h h h h | h h h h h | h h h h | h h h | h h | h
let	wspend	= ('\r' '\n' | [' ' '\t' '\r' '\n' '\x0c'])?

rule token = parse
	| '\\' (hh as digits) wspend	{ STRING (unesc digits) }
	| '\\' 't'			{ STRING "\t"}
	| '\\' 'r'			{ STRING "\r"}
	| '\\' 'n'			{ STRING "\n"}
	| '\\' 'f'			{ STRING "\x0c"}
	| '\\' 				{ STRING (lexeme lexbuf)}
	| _ 				{ STRING (lexeme lexbuf)}
	| eof				{ EOF }

