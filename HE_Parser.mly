%{

let convert f n = match n with
        | 38 -> "&amp;"
        | 60 -> "&lt;"
        | 62 -> "&gt;"
        | 160 -> "&nbsp;"
        | _ -> f n;;

let utf8_of_code c =
        convert (Netconversion.ustring_of_uchar `Enc_utf8) (int_of_string c);;

let utf8_of_hcode c =
        let hex_of_string c = Scanf.sscanf c "%X" (fun x -> x) in
        convert (Netconversion.ustring_of_uchar `Enc_utf8) (hex_of_string c);;
        
%}

%token ENTITY_AMP ENTITY_LT ENTITY_GT
%token <string> ENTITY_CODE CHAR
%token <string> ENTITY_HCODE CHAR
%token EOF

%start html
%type <string> html

%%

html:
        | symbols                                       { String.concat "" $1 }
        ;

symbols:
                                                        { [] }
        | symbol symbols                                { $1 :: $2 }
        ;

symbol:
        | ENTITY_AMP                                    { "&amp;" }
        | ENTITY_LT                                     { "&lt;" }
        | ENTITY_GT                                     { "&gt;" }
        | ENTITY_CODE                                   { utf8_of_code($1) }
        | ENTITY_HCODE                                  { utf8_of_hcode($1) }
        | CHAR                                          { $1 }
        ;
