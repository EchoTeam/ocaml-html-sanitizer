

(*s The following types define a CSS declaration as described in CSS 2.1 grammar at http://www.w3.org/TR/CSS21/grammar.html *)
type unaryOperator = None | Plus | Minus;;
type dimension = unaryOperator * (string * string);;
type operator = NoOp | Slash | Comma;;

type term = Dimension of dimension
		| String of string
		| Ident of string
		| URI of string
		| HexColor of string
		| Func of expression
	and statement = Term of term | Operator of operator
	and expression = statement list
	;;

type rule = string * expression * bool;;

let rec map_css_expression_terms f list =
	let apply_f_to_statement = function
	    | Term (Func expr) -> Term (Func (map_css_expression_terms f expr))
	    | Term t -> Term (f t)
	    | Operator x -> Operator x in
	List.map apply_f_to_statement list


(*s Conver a given CSS rule into string representation *)
let rec rule2str (property, expr, important) =
		property ^ ": " ^ (expression2str expr) ^ match important with
			| false -> " !important"
			| true -> ""
	and expression2str statements =
		let collect f el acc = f el ^ acc in
		List.fold_right (collect statement2str) statements ""
	and statement2str = function
		| Term term -> term2str term
		| Operator NoOp -> ""
		| Operator Slash -> "/"
		| Operator Comma -> ","
	and term2str = function
		| Dimension (None, (n, munit)) -> n ^ munit
		| Dimension (Plus, (n, munit)) -> "+" ^ n ^ munit
		| Dimension (Minus, (n, munit)) -> "-" ^ n ^ munit
		| String s -> s
		| Ident s -> s
		| URI s -> "url(" ^ s ^ ")"
		| HexColor s -> s
		| Func expr -> "function(" ^ expression2str expr ^ ")"
	;;
