(* vim: set ts=4 sts=4 sw=4 et: *)
(*s Using ocamlnet's Netchannel and Nethtml modules for HTML parsing *)
open Netchannels
open Nethtml
open List

open Wordbreak

exception TagReplace of string * string;;
exception TagQuote;;
exception TagErase;;
exception TagEraseWithContents;;
exception UnexpectedNodeType;;

(*s Implement verification code for HTML attributes. *)
let re_url = Str.regexp_case_fold "^[ ]*\\(\\(\\(http\\|https\\|ftp\\|feed\\|news\\)://[a-z0-9_.-]+\\(/[^@]*\\)?\\)?\\)[ ]*$";;
let re_url_relative = Str.regexp_case_fold "[ ]*\\(/[^@]*\\)[ ]*$";;
let re_mailto = Str.regexp_case_fold "^[ ]*\\(mailto:[a-z0-9_.-]+@[a-z0-9_.-]+\\)?[ ]*$";;

let re_safeurl = Str.regexp_case_fold "^[ ]*\\(\\(\\(http\\|https\\|ftp\\|feed\\|news\\|mailto\\)://\\(www\\.\\)?\\(youtube\\.com\\|vimeo.com\\|download\\.macromedia\\.com\\)\\(/[^@]*\\)?\\)?\\)$";;
let re_safeurl_or_word = Str.regexp_case_fold "^[ ]*\\(\\(http?://\\(www\\.\\)?\\(youtube\\.com\\|vimeo.com\\|download\\.macromedia\\.com\\)\\(/[^@]*\\)?\\)\\|\\([0-9a-z_]+[ ]*\\)\\)$";;
let re_classid = Str.regexp_case_fold "^[ ]*clsid:d27cdb6e-ae6d-11cf-96b8-444553540000[ ]*$";;

(* Check value against a regular expression. Returns value or raises exception. *)
let check_re re s = match Str.string_match re s 0 with
		| true -> s
		| false -> raise Not_found;;

(* \textit{check\_href} is different from \textit{check\_re re\_url} in that it leaves <a href="">...</a> if something is terminally wrong with the URL, instead of leaving <a>...</a> *)
let rec check_href_helper s = function
    | [] -> ""
    | re::tail ->
        match Str.string_match re s 0 with
            | true -> Str.matched_group 1 s
            | false -> check_href_helper s tail
;;

let check_href s = check_href_helper s [re_url; re_mailto; re_url_relative];;

(* Check various kinds of HTML attribute value content. *)
let check_url = check_re re_url;;
let check_safeurl = check_re re_safeurl;;
let check_safeurl_or_word = check_re re_safeurl_or_word;;
let check_default s = s;;
let check_int s = string_of_int (int_of_string s);;
let check_classid = check_re re_classid;;
let check_css = CSS.sanitize;;
let (|?) f n = fun x -> try f x with Not_found -> n;;

let read_dtd = relaxed_html40_dtd;;
let write_dtd =
	(* We do <param ...></param> instead of just <param> like HTML mandates *)
	remove_assoc "param" relaxed_html40_dtd;;

(*s Define a list of permitted tags with their permitted unique attributes *)
let default_permitted_tags : (string * (string * (string -> string)) list) list = [
	("a", [("href", check_href); ("rel", check_default)])
	; ("param", [("name", check_default);
			("value", check_safeurl_or_word |? "")])
	; ("embed", [("src", check_safeurl)
			; ("type", check_default)
			; ("width", check_int)
			; ("height", check_int)
			])
	; ("object", [
			("classid", check_classid |? "")
			; ("codebase", check_safeurl)
			; ("data", check_safeurl)
			; ("width", check_int)
			; ("height", check_int)
			])
	; ("img", [("src", check_url); ("alt", check_default)])
	; ("div", [])
	; ("span", [])
	; ("p", [])
	; ("b", [])
	; ("i", [])
	; ("u", [])
	; ("sub", [])
	; ("sup", [])
	; ("em", [])
	; ("strong", [])
	; ("pre", [])
	; ("br", [])
	];;

(*s Add common attributes into the listed tags *)
let map_tags_styles =
	map (fun (tag, xs) -> (tag, ("style", check_css) :: xs));;

let get_attribute_checker = function
	|(_, "href")			-> check_href
	|(_, "width")|(_, "height")	-> check_int
	|(_, "classid")			-> check_classid |? ""
	|(_, "codebase")		-> check_safeurl
	|(_, "value")			-> check_safeurl_or_word |? ""
	|(_, "style")			-> check_css
	|("object", "data")		-> check_safeurl
	|("embed", "src")		-> check_safeurl
	|("img", "src")			-> check_url
	|("img", "dynsrc")		-> check_url
	|("img", "lowsrc")		-> check_url
	|(_, "src")				-> check_url
	|(_, "background")		-> check_url
	|_				-> check_default;;

let parse_permitted_tags =
	let re_sep = Str.regexp "[/,]+" in
	fold_left (fun acc rule ->
      match Str.split re_sep (String.lowercase rule) with
	  | [] -> acc
	  | (tag :: attrs) ->
        let acc0 = if mem_assoc tag acc then acc else (tag, []) :: acc in
        fold_left (fun acc1 attr ->
            let a_spec = (attr, get_attribute_checker (tag, attr)) in
            (tag, (a_spec :: assoc tag acc1)) :: remove_assoc tag acc1
        ) acc0 attrs
    ) [];; 

(*s To leave only permitted attributes we first check whether attribute in the permitted list (and throw if it is not), then use the checking function to verify the validity of the attribute's value. *)
let sanitize_attributes_white perm_attrs attrs = 
	let rewrite (attr, value) acc = try
				let check_fun = assoc attr perm_attrs in
				(attr, check_fun value) :: acc
			with _ -> acc
	in
	fold_right rewrite attrs [];;

let sanitize_attributes_black tag attrs =
	let rewrite (attr, value) acc = match attr with
            | "fscommand" -> acc
            | s when s.[0] = 'o' && s.[1] = 'n' -> acc (* onclick? *)
            | _ ->
                try let check_fun = get_attribute_checker (tag, attr) in
                    (attr, check_fun value) :: acc
                with _ -> acc
    in
    fold_right rewrite attrs [];;

let evil_tag = function
    | "script" | "iframe" | "layer" | "link" | "style" | "meta" -> true
    | "frame" | "frameset" -> true
    | "base" -> true
    | _ -> false

(*s While doing element sanitizing we check whether tags in a permitted list, and then leave only permitted and checked attributes. Since node children (if any) are handled elsewhere, we don't touch them here. *)
let element_sanitize_strict erase_script_contents erase_unallowed_tags permitted_tags_map dataF = function
	| Data text -> dataF text
	| Element (tag, attributes, children) ->
		try 
            let perm_attrs = assoc tag permitted_tags_map in
			Element (tag, sanitize_attributes_white perm_attrs attributes, children)
		with
            | Not_found when erase_script_contents && evil_tag tag ->
                raise TagEraseWithContents
            | Not_found when erase_unallowed_tags -> raise TagErase
            | Not_found -> raise TagQuote
	;;

let element_sanitize_permissive erase_script_contents erase_unallowed_tags dataF = function
    | Data text -> dataF text
    | Element (tag, _, _) when evil_tag tag ->
        (match erase_script_contents, erase_unallowed_tags with
            | true, _ -> raise TagEraseWithContents
            | false, true -> raise TagErase
            | false, false -> raise TagQuote)
    | Element (tag, attributes, children) ->
        Element (tag, sanitize_attributes_black tag attributes, children)
    ;;

(*s HTML stripping is removing all Element tags *)
let element_strip allow_breaks erase_script_contents dataF = function
	| Data text -> dataF text
	(*
		The following code is too simplistic:
		| (Element (_, _, _)) -> raise TagErase
		We need to strip all HTML tags. However, we should not just
		remove them, since strings like "foo.<p>bar</p>baz" will look ugly
		without a space: "foo.barbaz" instead of "foo. bar baz".
		Therefore, we replace certain elements with a space.
	*)
	| (Element ("br", _, _) as el) when allow_breaks -> el
	| Element ("br", _, _) -> raise (TagReplace ("\n", ""))
	| Element (tag, _, _) when erase_script_contents && evil_tag tag ->
        raise TagEraseWithContents
	| Element (tag, _, children) ->
		match fst (List.assoc tag html40_dtd) with
			| `Block when allow_breaks -> raise (TagReplace ("<br>", "<br>"))
			| `Block -> raise (TagReplace ("\n", "\n"))
			| `Essential_block when allow_breaks -> raise (TagReplace ("<br>", "<br>"))
			| `Essential_block -> raise (TagReplace ("\n", "\n"))
			| `Inline -> raise TagErase
			| `None -> raise TagErase
			| `Everywhere -> raise TagErase
	;;

(*s Transform every node of HTML tag soup one by one with a specified function. If there is an exception while applying a function to a node, we ``remove that tag`` by placing all children of this node into the current position instead of the failed element. *)
let rec map_document_nodes f = function 
	| x :: xs ->
	    let new_node = try
		let el = match f x with
		| (Data _) as x' -> x'
		| (Element (tag, attrs, children)) ->
			Element (tag, attrs, map_document_nodes f children)
		in
		el :: map_document_nodes f xs
	    with
		TagReplace (s1, s2) -> match x with
			| (Data _) -> [Data s1] @ (map_document_nodes f xs) @ [Data s2]
			| (Element (_, _, children)) ->
				[Data s1]
				@ map_document_nodes f children
				@ [Data s2]
				@ map_document_nodes f xs
			; ;
		| TagQuote -> match x with
			| (Element (tag, _, children)) ->
				[Data ("&lt;" ^ tag ^ "&gt;")]
				@ map_document_nodes f children
				@ [Data ("&lt;/" ^ tag ^ "&gt;")]
				@ map_document_nodes f xs
			| _ -> raise UnexpectedNodeType
			; ;
        | TagEraseWithContents -> map_document_nodes f xs
		| TagErase | _ ->
			let elements = match x with
				| (Data _) -> []
				| (Element (_, _, children)) ->
					map_document_nodes f children
			in elements @ map_document_nodes f xs
	    in new_node
	| [] -> []
	;;

(* The rest of the file defines an external API *)
let transform_string f src =
	let ich = new input_string src in
	let doc = with_in_obj_channel ich (parse ~dtd:read_dtd) in
	let buf = Buffer.create (10000 + String.length src) in
	let och = new output_buffer buf in
	write ~dtd:write_dtd och (f doc);
	och # close_out();
	Buffer.contents buf
	;;

type url_detection = Url of string | Text of string;;

let re_link = Str.regexp_case_fold "\\(http\\|https\\|ftp\\|feed\\|news\\|mailto\\)://[a-z0-9_.-]+\\(/\\([^ \t\r\n,.<>]\\|[,.][^ \r\n]\\)*\\)?";;

(* Detect links in the text and return a list of links and plain text *)
let rec detect_links s start =
	try
		let pos = Str.search_forward re_link s start in
		let matched = Str.matched_string s in
		Text (Str.string_after (Str.string_before s pos) start)
		:: Url matched
		:: detect_links s (pos + String.length matched)
	with Not_found -> [Text (Str.string_after s start)];;

let wordbreak s =
	let wbr = "<wbr></wbr>" in
    String.concat "" (List.map (function
			| Url url -> url ^ wbr
			| Text text when String.length text < 10 -> text
			| Text text -> html_break_words text ^ wbr
		) (detect_links s 0));;

type required_sanity_level =
    Transparent | Permissive | Aggressive | NanoHTML of allow_breaks
 and allow_breaks = bool

let sanity_levels_map = 
    ["transparent", Transparent;
     "permissive", Permissive;
     "agressive", Aggressive;
     "aggressive", Aggressive;
     "nanohtml", NanoHTML true;
     "nanotext", NanoHTML false];;
let sanity_levels = List.map fst
                    (List.remove_assoc "agressive" sanity_levels_map);;
let sanity_level_of_string s =
    try List.assoc s sanity_levels_map
    with Not_found -> raise (Failure ("Sanity level is none of "
        ^ String.concat "," sanity_levels));;

class html_sanitizer
    ?(break_long_words=false)   (* Insert HTML word breaks within long words *)
    ?(permit_common_attrs=true)
    ?(erase_unallowed_tags=true)
    ?(erase_script_contents=true)
    ?(force_utf8=true)         (* Replace entity references with UTF-8 *)
    ?(permitted_tags=[])       (* ["a/href"; "img/src,width,heigh"] *)
    (sanity_level:required_sanity_level) =

    let data_distiller = 
        let to_doc s = Data s in
        let from_doc = function (Data s) -> s | _ -> raise UnexpectedNodeType in
        let to_list a = [a] in
        let ( >>> ) f g = fun x -> g (f x) in
        let id x = x in
        let iff flag f = if flag then from_doc >>> f >>> to_doc else id in
        to_doc >>> to_list >>> (decode ~enc:`Enc_utf8) >>>
            (encode ~enc:`Enc_utf8 ~prefer_name:false) >>> hd
        >>> iff force_utf8 (Lexing.from_string >>>HE_Parser.html HE_Lexer.token)
        >>> iff break_long_words wordbreak in

    let permitted_tags_map =
        let tmap = match parse_permitted_tags permitted_tags with
            | [] -> default_permitted_tags
            | tagsMap -> tagsMap in
        if permit_common_attrs then map_tags_styles tmap else tmap
    in

    let element_distiller = match sanity_level with
        | NanoHTML allow_breaks -> element_strip allow_breaks erase_script_contents data_distiller
        | Aggressive -> element_sanitize_strict erase_script_contents erase_unallowed_tags permitted_tags_map data_distiller
        | Permissive -> element_sanitize_permissive erase_script_contents erase_unallowed_tags data_distiller
        | Transparent -> (function  | Data text -> data_distiller text
                                    | Element e -> Element e)
    in

    object (self)

    method sanitize_doc = map_document_nodes element_distiller
    method sanitize_string = transform_string self#sanitize_doc

    end;;

