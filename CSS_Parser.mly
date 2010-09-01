%{
open CSS_Types

let css_property_unescape s =
	let lexbuf = Lexing.from_string s in
	CSS_EscapeParser.parsed_escaping CSS_EscapeLexer.token lexbuf;;

%}
%token S CDO CDC INCLUDES DASHMATCH LBRACE PLUS GREATER COMMA COLON SEMICOLON SLASH
%token <string> STRING IDENT HASH NUMBER PERCENTAGE URI CHAR FUNCTION
%token <string * string> DIMENSION
%token INVALID
%token IMPORT_SYM IMPORTANT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM
%token EOF

%start css	/* the entry point */ 
%type <CSS_Types.rule list> css
%% 

css:
				{ [] }
	| declarations 		{ $1 };

declarations:
	Ss declaration more_declarations	{ $2 $3 }

more_declarations:					{ [] }
	| SEMICOLON Ss declaration more_declarations	{ $3 $4 }
	| SEMICOLON Ss more_declarations	{ $3 }

declaration:
	property COLON Ss expr opt_prio	{ fun x -> ($1, $4, $5) :: x };

property: IDENT Ss	{ css_property_unescape $1 };

opt_prio:			{ true }
	| IMPORTANT_SYM Ss	{ false };

expr: term opt_operatorterms	{ Term $1 :: $2 }

term:
	opt_unary_operator dimension Ss	{ Dimension ($1, $2) }
	| STRING Ss			{ String $1 }
	| IDENT Ss			{ Ident $1 }
	| URI Ss			{ URI $1 }
	| hexcolor			{ HexColor $1 }
	| css_function			{ Func $1 }
  ;

opt_unary_operator:		{ None }
	| "-"			{ Minus }
	| PLUS			{ Plus };

dimension:
	DIMENSION	{ $1 }
	| PERCENTAGE { ($1, "%") }
	| NUMBER { ($1, "") };

opt_operatorterms:		{ [] }
	| operator term opt_operatorterms	{ Operator $1 :: Term $2 :: $3 };


operator:
	 			{ NoOp }
	| SLASH Ss		{ Slash }
	| COMMA Ss		{ Comma };

hexcolor: HASH Ss		{ $1 };

css_function: FUNCTION Ss expr ")" Ss	{ $3 };

Ss:		{ () }
	| S Ss	{ () };

