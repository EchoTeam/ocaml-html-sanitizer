%{
%}

%token	<string>	STRING
%token	EOF

%start parsed_escaping
%type <string> parsed_escaping
%%

parsed_escaping:	{ "" }
	| STRING parsed_escaping { $1 ^ $2 };
