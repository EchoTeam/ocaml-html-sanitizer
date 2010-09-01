(*s A CSS 2.1 lexer, closely matching one at http://www.w3.org/TR/CSS21/grammar.html\#scanner *)
{
open CSS_Parser
}

let	az		= ['a' - 'z' 'A' - 'Z']
let	h		= ['0'-'9' 'a' - 'f' 'A' - 'F']
let	stnrf		= [ ' ' '\t' '\n' '\r' '\x0c' ]

let	nl	= '\n'|'\r' '\n'|'\r'|'\x0c'

let	nonascii	= ['\x80' - '\xff']
let	unicode		= '\\' (h h h h h h | h h h h h | h h h h | h h h | h h | h) ('\r' '\n' | [' ' '\t' '\r' '\n' '\x0c'])?
let	escape		= unicode | '\\' [^ '\r' '\n' '\x0c' '0'-'9' 'a'-'f' 'A' - 'F']
let	nmstart		= '_' | az | nonascii | escape
let	nmchar		= '_' | az | '-' | ['0' - '9'] | nonascii | escape
let	notnrfsq	= [^ '\n' '\r' '\x0c' '\'']
let	notnrfdq	= [^ '\n' '\r' '\x0c' '"']
let	string1		= '"' (notnrfsq |'\\' nl | escape)* '"'
let	string2		= "'" (notnrfdq |'\\' nl | escape)* "'"
let	invalid1	= '"' (notnrfsq |'\\' nl | escape)*
let	invalid2	= "'" (notnrfdq |'\\' nl | escape)*

let	comment		= '/' '*' [^ '*']* '*'+ ([^'/''*'][^'*']*'*'+)* '/'
let	ident		= '-'? nmstart nmchar*
let	name		= nmchar+
let	num		= ['0'-'9']+|['0'-'9']*'.'['0'-'9']+
let	string		= string1|string2
let	invalid		= invalid1|invalid2
let	url		= (['!''#''$''%''&' '*'-'~']|nonascii|escape)*
let	url_label	= ['u''U'] ['r''R'] ['l''L']
let	s		= [' ' '\t' '\r' '\n' '\x0c']+
let	w		= s?

let	z40		= '\\' ('0' | "00" | "000" | "0000")
let	A		= 'a'|z40("41"|"61")('\r''\n'|stnrf)?
let	C		= 'c'|z40("43"|"63")('\r''\n'|stnrf)?
let	D		= 'd'|z40("44"|"64")('\r''\n'|stnrf)?
let	E		= 'e'|z40("45"|"65")('\r''\n'|stnrf)?
let	G		= 'g'|z40("47"|"67")('\r''\n'|stnrf)?|'\\''g'
let	H		= 'h'|z40("48"|"68")('\r''\n'|stnrf)?|'\\''h'
let	I		= 'i'|z40("49"|"69")('\r''\n'|stnrf)?|'\\''i'
let	K		= 'k'|z40("4b"|"6b")('\r''\n'|stnrf)?|'\\''k'
let	M		= 'm'|z40("4d"|"6d")('\r''\n'|stnrf)?|'\\''m'
let	N		= 'n'|z40("4e"|"6e")('\r''\n'|stnrf)?|'\\''n'
let	O		= 'o'|z40("51"|"71")('\r''\n'|stnrf)?|'\\''o'
let	P		= 'p'|z40("50"|"70")('\r''\n'|stnrf)?|'\\''p'
let	R		= 'r'|z40("52"|"72")('\r''\n'|stnrf)?|'\\''r'
let	S		= 's'|z40("53"|"73")('\r''\n'|stnrf)?|'\\''s'
let	T		= 't'|z40("54"|"74")('\r''\n'|stnrf)?|'\\''t'
let	X		= 'x'|z40("58"|"78")('\r''\n'|stnrf)?|'\\''x'
let	Z		= 'z'|z40("5a"|"7a")('\r''\n'|stnrf)?|'\\''z'

rule token = parse
	| s			{ S }

	| comment		{ token lexbuf (*c ignore comments c*)}
	| s+ comment		{ S }

	| "<!--"			{ CDO }
	| "-->"			{ CDC }
	| "~="			{ INCLUDES }
	| "|="			{ DASHMATCH }

	| w"{"			{ LBRACE }
	| w"+"			{ PLUS }
	| w">"			{ GREATER }
	| w","			{ COMMA }
	| ";"			{ SEMICOLON }
	| ":"			{ COLON }
	| "/"			{ SLASH }

	| string as lxm		{ STRING lxm }
	| invalid		{ INVALID }

	| ident as lxm		{ IDENT lxm }

	| ("#"name as lxm)	{ HASH lxm }

	| '@' I M P O R T	{IMPORT_SYM}
	| '@' P A G E		{PAGE_SYM}
	| '@' M E D I A		{MEDIA_SYM}
	| '@' C H A R S E T	{CHARSET_SYM}

	| "!"(w|comment)* I M P O R T A N T	{IMPORTANT_SYM}

(*i
	| num E M		{EMS lxm}
	| num E X		{EXS lxm}
	| num P X		{LENGTH lxm}
	| num C M		{LENGTH lxm}
	| num M M		{LENGTH lxm}
	| num I N		{LENGTH lxm}
	| num P T		{LENGTH lxm}
	| num P C		{LENGTH lxm}

	| num D E G		{ANGLE lxm}
	| num R A D		{ANGLE lxm}
	| num G R A D		{ANGLE lxm}
	| num M S		{TIME lxm}
	| num S			{TIME lxm}
	| num H Z		{FREQ lxm}
	| num K H Z		{FREQ lxm}
i*)

	| (num as lxm1) (ident as lxm2)		{DIMENSION (lxm1,lxm2)}

	| (num as lxm) '%'		{PERCENTAGE lxm}
	| (num as lxm)			{NUMBER lxm}

	| url_label"("w (string as lxm) w")"	{URI lxm}
	| url_label"("w (url as lxm) w")"	{URI lxm}
	| (ident as lxm)"("		{FUNCTION lxm}

	| _? as lxm		{CHAR lxm}
	| eof			{ EOF }

