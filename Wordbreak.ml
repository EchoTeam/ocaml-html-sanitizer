
(* Parsing states:
 * Initial: ordinary string
 * EntRef: entity reference, like &nbsp;, &copy; or &trade;.
 *)
type parsing_sate = Initial | EntRef of int;;

(* Extend our idea of entity reference length *)
let more_entref = function
	| Initial -> EntRef 1
	| EntRef i -> EntRef (1 + i);;

(* Return the length of entity reference *)
let entref_len = function
	| Initial -> 0
	| EntRef i -> i;;

let is_entref = function
	| Initial -> false
	| EntRef _ -> true;;

let internal_break_words b t =
	let add = Buffer.add_char b in
	(* 10 is to capture "&thetasym;", the longest entity reference *)
	let max_entref_length = 10 in
	let max_word_length = 10 in
	let rec proceed state n = match t with parser
		| [< '' ' >] -> add ' '; proceed Initial 1
		| [< ''\n' >] -> add '\n'; proceed Initial 1
		| [< ''\t' >] -> add '\t'; proceed Initial 1
		| [< ''\r' >] -> add '\r'; proceed Initial 1
		| [< ''&' >] ->
			let n = if n > max_word_length then begin
				Buffer.add_string b "<wbr></wbr>"; add '&'; 1
			end else begin
				add '&'; n
			end in
			proceed (EntRef 1) (n + 1)
		| [< 'sym when entref_len state >= max_entref_length >] ->
			Buffer.add_string b "<wbr></wbr>";
			add sym; proceed Initial 1
		| [< 'sym when is_entref state &&
				((sym >= 'A' && sym <= 'Z')
				|| (sym >= 'a' && sym <= 'z')
				|| (sym >= '0' && sym <= '9')
				|| sym = ';' || sym = '#')
				>] ->
			add sym;
			(* n+2: Putting more weight on splits near
			 * entity character references *)
			proceed (more_entref state) (n + 2)
		(* Don't break during UTF-8 sequence tail *)
		| [< 'sym when ((int_of_char sym >= 0x80) && (int_of_char sym < 0xc0) && (n < 16)) >] ->
			add sym; proceed state n
		| [< 'sym when n > max_word_length >] ->
			Buffer.add_string b "<wbr></wbr>";
			add sym; proceed Initial 1
		| [< 'sym >] -> add sym; proceed Initial (1 + n)
		| [< >] -> ()
	in proceed Initial 1;;

let html_break_words s =
	let b = Buffer.create 1024 in
	internal_break_words b (Stream.of_string s);
	Buffer.contents b;;

