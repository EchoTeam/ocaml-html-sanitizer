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

let re_safeurl = Str.regexp_case_fold "^[ ]*\\(\\(\\(\\(http\\|https\\|ftp\\|feed\\|news\\|mailto\\):\\)?//\\(www\\.\\)?\\(youtube\\(-nocookie\\)?\\.com\\|youtu\\.be\\|cdn\\.embedly\\.com\\|api\\.embed\\.ly\\|\\(player\\.\\)?vimeo\\.com\\|download\\.macromedia\\.com\\|abcnews\\.go\\.com\\|twitvid\\.com\\|embed\\.spotify\\.com\\|platform\\.twitter\\.com\\)\\(/[^@]*\\)?\\)?\\)$";;
let re_safeurl_or_word = Str.regexp_case_fold "^[ ]*\\(\\(https?://\\(www\\.\\)?\\(youtube\\(-nocookie\\)?\\.com\\|youtu\\.be\\|cdn\\.embedly\\.com\\|api\\.embed\\.ly\\|\\(player\\.\\)?vimeo\\.com\\|download\\.macromedia\\.com\\|abcnews\\.go\\.com\\|twitvid\\.com\\)\\(/[^@]*\\)?\\)\\|\\([0-9a-z_]+[ ]*\\)\\)$";;
let re_plainid = Str.regexp "^[@ a-zA-Z0-9/#.,;=+-]*$";;
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
let check_plainid = check_re re_plainid;;
let check_default s = s;;
let check_int_range l r s = match int_of_string s with
    | n when n < l -> string_of_int l
    | n when n > r -> string_of_int r
    | _ -> s;;
let check_dimension = check_int_range 0 2000;;
let check_classid = check_re re_classid;;
let check_css is_aggressive = CSS.sanitize ~aggressive:is_aggressive;;
let (|?) f n = fun x -> try f x with Not_found -> n;;

let read_dtd = relaxed_html40_dtd;;
let write_dtd =
	(* We do <param ...></param> instead of just <param> like HTML mandates *)
	remove_assoc "param" relaxed_html40_dtd;;

(*s Define a list of permitted tags with their permitted unique attributes *)
let default_permitted_tags : (string * (string * (string -> string)) list) list = [
	("a", [("href", check_href)
           ; ("rel", check_default)
           ; ("data-resolved", check_url)       (* Broken 3rd party data *)
           ; ("data-src-resolved", check_url)
           ])
	; ("param", [("name", check_plainid);
			("value", check_safeurl_or_word |? "")])
	; ("embed", [("src", check_safeurl)
			; ("type", check_plainid)
			; ("width", check_dimension)
			; ("height", check_dimension)
			; ("allowscriptaccess", check_plainid)
			; ("allowfullscreen", check_plainid)
			])
	; ("object", [
			("classid", check_classid |? "")
			; ("codebase", check_safeurl)
			; ("data", check_safeurl)
			; ("width", check_dimension)
			; ("height", check_dimension)
			])
	; ("img", [("src", check_url)
            ; ("alt", check_default)
            ; ("data-src-preview", check_url)
            ; ("data-src-web", check_url)
            ; ("data-src-full", check_url)
            ])
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
let add_style_attribute is_aggressive =
	map (fun (tag, xs) -> (tag, ("style", check_css is_aggressive) :: xs));;

let get_attribute_checker is_aggressive = function
	|(_, "href")			-> check_href
	|(_, "width")|(_, "height")	-> check_int_range 0 10000
	|(_, "classid")			-> check_classid |? ""
	|(_, "codebase")		-> check_safeurl
	|(_, "value")			-> check_safeurl_or_word |? ""
	|(_, "style")			-> check_css is_aggressive
	|("object", "data")		-> check_safeurl
	|("embed", "src")		-> check_safeurl
	|("img", "src")			-> check_url
	|("img", "dynsrc")		-> check_url
	|("img", "lowsrc")		-> check_url
	|(_, "src")				-> check_url
	|(_, "background")		-> check_url
	|_				-> check_default;;

let parse_permitted_tags is_aggressive =
	let re_sep = Str.regexp "[/,]+" in
	fold_left (fun acc rule ->
      match Str.split re_sep (String.lowercase rule) with
	  | [] -> acc
	  | (tag :: attrs) ->
        let acc0 = if mem_assoc tag acc then acc else (tag, []) :: acc in
        fold_left (fun acc1 attr ->
            let a_spec = (attr, get_attribute_checker is_aggressive (tag, attr)) in
            (tag, (a_spec :: assoc tag acc1)) :: remove_assoc tag acc1
        ) acc0 attrs
    ) [];; 

(*s To leave only permitted attributes we first check whether attribute in the permitted list (and throw if it is not), then use the checking function to verify the validity of the attribute's value. *)
let sanitize_attributes_white ?(raise_on_fail=None) perm_attrs attrs = 
    let rewrite (attr, value) acc =
        if mem_assoc attr perm_attrs
        then
            let check_fun = assoc attr perm_attrs in
            try (attr, check_fun value) :: acc
            with _ -> (match raise_on_fail with
                  | None -> acc
                  | Some exc -> raise exc)
        else acc
    in
    fold_right rewrite attrs [];;

let sanitize_attributes_black tag attrs =
    let rewrite (attr, value) acc = match attr with
            | "fscommand" -> acc
            | s when s.[0] = 'o' && s.[1] = 'n' -> acc (* onclick? *)
            | _ ->
                try let check_fun = get_attribute_checker false (tag, attr) in
                    (attr, check_fun value) :: acc
                with _ -> acc
    in
    fold_right rewrite attrs [];;

let evil_tag = function
    | "script" | "iframe" | "layer" | "link" | "style" | "meta" -> true
    | "frame" | "frameset" -> true
    | "base" -> true
    | _ -> false

let disallow_empty exc = function
    | [] -> raise exc
    | lst -> lst

let permit_safe_evil exc = function
    | Element ("iframe", attributes, [])
    | Element ("iframe", attributes, [Data ""]) ->
        Element ("iframe", sanitize_attributes_white 
                    [("src", check_safeurl)
                        ; ("width", check_dimension)
                        ; ("height", check_dimension)
                        ; ("frameborder", check_int_range 0 3)
                        ; ("allowfullscreen", check_plainid)] attributes
                , [])
    | Element ("script", attributes, [])
    | Element ("script", attributes, [Data ""]) ->
        Element ("script", disallow_empty exc (sanitize_attributes_white
                    [("src", check_safeurl)] attributes ~raise_on_fail:(Some exc))
                , [])
    | _ -> raise exc

(*s While doing element sanitizing we check whether tags in a permitted list, and then leave only permitted and checked attributes. Since node children (if any) are handled elsewhere, we don't touch them here. *)
let element_sanitize_strict erase_script_contents erase_unallowed_tags permitted_tags_map dataF parentTag = function
	| Data text -> dataF parentTag text
	| Element (tag, attributes, children) as elem ->
		try 
            let perm_attrs = assoc tag permitted_tags_map in
			Element (tag, sanitize_attributes_white perm_attrs attributes, children)
		with
            | Not_found when erase_script_contents && evil_tag tag ->
                permit_safe_evil TagEraseWithContents elem
            | Not_found when erase_unallowed_tags ->
                permit_safe_evil TagErase elem
            | Not_found ->
                permit_safe_evil TagQuote elem
	;;

let element_sanitize_permissive erase_script_contents erase_unallowed_tags dataF parentTag = function
    | Data text -> dataF parentTag text
    | Element (tag, attributes, children) as elem when evil_tag tag ->
        let exc = (match erase_script_contents, erase_unallowed_tags with
            | true, _ -> TagEraseWithContents
            | false, true -> TagErase
            | false, false -> TagQuote) in
        permit_safe_evil exc elem
    | Element (tag, attributes, children) ->
        Element (tag, sanitize_attributes_black tag attributes, children)
    ;;

(*s HTML stripping is removing all Element tags *)
let element_strip allow_breaks erase_script_contents dataF parentTag = function
	| Data text -> dataF parentTag text
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
let rec map_document_nodes parentTag (f: string -> Nethtml.document -> Nethtml.document) = function 
	| x :: xs ->
	    let new_node = try
		let el = match f parentTag x with
		| (Data _) as x' -> x'
		| (Element (tag, attrs, children)) ->
		   Element (tag, attrs, map_document_nodes tag f children)
		in
		el :: map_document_nodes parentTag f xs
	    with
		TagReplace (s1, s2) -> match x with
			| (Data _) -> [Data s1] @ (map_document_nodes parentTag f xs) @ [Data s2]
			| (Element (_, _, children)) ->
				[Data s1]
				@ map_document_nodes parentTag f children
				@ [Data s2]
				@ map_document_nodes parentTag f xs
			; ;
		| TagQuote -> match x with
			| (Element (tag, _, children)) ->
				[Data ("&lt;" ^ tag ^ "&gt;")]
				@ map_document_nodes parentTag f children
				@ [Data ("&lt;/" ^ tag ^ "&gt;")]
				@ map_document_nodes parentTag f xs
			| _ -> raise UnexpectedNodeType
			; ;
        | TagEraseWithContents -> map_document_nodes parentTag f xs
		| TagErase | _ ->
			let elements = match x with
				| (Data _) -> []
				| (Element (_, _, children)) ->
					map_document_nodes parentTag f children
			in elements @ map_document_nodes parentTag f xs
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

type word_break_method =    (* In the order of increasing breaking ability... *)
        | NoWordBreak       (* Disable word breaking *)
        | NoURLWordBreak    (* Break words everywhere except for URL strings *)
        | NoStandaloneURLWordBreak (* Break words everywhere except for
                                    * URL strings which are not surrounded
                                    * by <a>...</a> tags *)
        | DumbWordBreak     (* Wordbreak without respect for URLs *)

let wordbreak break_long_words : string -> Nethtml.document -> Nethtml.document =
        let wbr = "<wbr></wbr>" in
        let fmap f = function
                | (Data s) -> Data (f s)
                | _ -> raise UnexpectedNodeType in
        let with_detected_links f s =
                String.concat "" (List.map f (detect_links s 0)) in
        match break_long_words with
        | NoWordBreak -> (fun parentTag node -> node)
        | NoURLWordBreak -> fun parentTag ->
                fmap (with_detected_links (function
                | Url url -> url ^ wbr
                | Text text when String.length text < 10 -> text
                | Text text -> html_break_words text ^ wbr
                ))
        | NoStandaloneURLWordBreak -> fun parentTag ->
                fmap (with_detected_links (function
                | Url url when parentTag = "a" -> html_break_words url
                | Url url -> url ^ wbr
                | Text text when String.length text < 10 -> text
                | Text text -> html_break_words text ^ wbr
                ))
        | DumbWordBreak ->
                (fun parentTag node ->
                        fmap (fun s -> html_break_words s ^ wbr) node)
        ;;

class html_sanitizer
    ?(break_long_words=NoWordBreak)   (* Insert <wbr></wbr> into long words *)
    ?(permit_common_attrs=true)
    ?(erase_unallowed_tags=true)
    ?(erase_script_contents=true)
    ?(force_utf8=true)         (* Replace entity references with UTF-8 *)
    ?(permitted_tags=[])       (* ["a/href"; "img/src,width,heigh"] *)
    (sanity_level:required_sanity_level) =

    let data_distiller =
        let wordbreak = wordbreak break_long_words in
        let to_doc s = Data s in
        let from_doc = function (Data s) -> s | _ -> raise UnexpectedNodeType in
        let to_list a = [a] in
        let ( >>> ) f g = fun x -> g (f x) in
        let id x = x in
        let iff flag f = if flag then from_doc >>> f >>> to_doc else id in
        fun parentTag ->
          to_doc >>> to_list >>> (decode ~enc:`Enc_utf8) >>>
            (encode ~enc:`Enc_utf8 ~prefer_name:false) >>> hd
          >>> iff force_utf8 (Lexing.from_string
                              >>> HE_Parser.html HE_Lexer.token)
          >>> wordbreak parentTag in

    let is_aggressive = sanity_level = Aggressive in
    let permitted_tags_map () =
        let tmap = match parse_permitted_tags is_aggressive permitted_tags with
            | [] -> default_permitted_tags
            | tagsMap -> tagsMap in
        if permit_common_attrs
            then (add_style_attribute is_aggressive) tmap
            else tmap
    in

    let element_distiller : string -> Nethtml.document -> Nethtml.document = match sanity_level with
        | NanoHTML allow_breaks -> element_strip allow_breaks erase_script_contents data_distiller
        | Aggressive -> element_sanitize_strict erase_script_contents erase_unallowed_tags (permitted_tags_map ()) data_distiller
        | Permissive -> element_sanitize_permissive erase_script_contents erase_unallowed_tags data_distiller
        | Transparent -> (fun parentTag -> function
                        | Data text -> data_distiller parentTag text
                        | Element e -> Element e)
    in

    object (self)

    method sanitize_doc = map_document_nodes "" element_distiller
    method sanitize_string = transform_string self#sanitize_doc

    end;;

