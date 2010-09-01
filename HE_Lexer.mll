{
open HE_Parser
}

let     dec                 = ['0'-'9']+
let     hex                 = ['0'-'9' 'a' - 'f' 'A' - 'F']+
let     x                   = ['x' 'X']

rule token = parse
        | "&#" x (hex as lxm) ";"   { ENTITY_HCODE lxm }
        | "&#" (dec as lxm) ";"     { ENTITY_CODE lxm }
        | _? as lxm                 { CHAR lxm }
        | eof                       { EOF }
