
open CSS_Types

exception BadCssRule;;

let filter_and_rewrite f list =
    let handle_rewrite element acc = try ((f element) :: acc) with _ -> acc
    in List.fold_right handle_rewrite list [];;

let validate_expression = function
    (* External URLs are BAD: images can contain exploits *)
    | URI _ -> raise BadCssRule
    | term -> term

let sanitize_css_rule_permissive ((property, expression, important) as rule) =
    let prop = String.lowercase property in
    if not important then raise BadCssRule; (* Ignore !important *)
    match prop with
    | "font-size" -> raise BadCssRule
    | "position" -> raise BadCssRule
    | "z-index" -> raise BadCssRule
    | _ ->
        if prop.[0] = '-' then raise BadCssRule;    (* Ignore -moz-foo-bar. *)
        ignore(map_css_expression_terms validate_expression expression);
        rule
	;;

let sanitize_css_rule_aggressive ((property, expression, important) as rule) =
    let prop = String.lowercase property in
    if not important then raise BadCssRule; (* Ignore !important *)
    match prop with
    | "font-family"
    | "font-style"
    | "font-weight"
    | "text-decoration"
    | "text-align"
    | "border-color"
    | "border-style"
    | "clear"
    ->
        ignore(map_css_expression_terms validate_expression expression);
        rule
    | _ -> raise BadCssRule
	;;

let write_css buffer =
	let rule_to_str rule =
			Buffer.add_string buffer (rule2str rule);
			Buffer.add_char buffer ';'
	in List.iter rule_to_str;;

let sanitize ?(aggressive=false) =
    let outb = Buffer.create 256 in
    function s ->
        let lexbuf = Lexing.from_string s in
        let css = CSS_Parser.css CSS_Lexer.token lexbuf in
        let safe_css = filter_and_rewrite
                (if aggressive
                    then sanitize_css_rule_aggressive
                    else sanitize_css_rule_permissive)
                css in
        Buffer.reset outb;
        write_css outb safe_css;
        Buffer.contents outb
    ;;

