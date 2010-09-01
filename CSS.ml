
open CSS_Types

exception BadCssRule;;

let filter_and_rewrite f list =
	let handle_rewrite element acc = try ((f element) :: acc) with _ -> acc
	in List.fold_right handle_rewrite list [];;

let sanitize_css_rule ((property, expression, important) as rule) =
	let prop = String.lowercase property in
	match (prop, important) with
	| (_, false) -> raise BadCssRule	(* Ignore !important *)
	| ("font-size", _) -> raise BadCssRule
	| _ ->
                let validate = function
                        (* External URLs are BAD: images can contain exploits *)
                        | URI _ -> raise BadCssRule
                        | term -> term in
                ignore(map_css_expression_terms validate expression);
                rule
	;;

let write_css buffer =
	let rule_to_str rule =
			Buffer.add_string buffer (rule2str rule);
			Buffer.add_char buffer ';'
	in List.iter rule_to_str;;

let sanitize s = let lexbuf = Lexing.from_string s in
		let css = CSS_Parser.css CSS_Lexer.token lexbuf in
		let safe_css = filter_and_rewrite sanitize_css_rule css in
		let outb = Buffer.create 64 in
		write_css outb safe_css;
		Buffer.contents outb
