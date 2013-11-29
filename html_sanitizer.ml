
open Nethtml
open Netchannels
open Sanitizer

(* Return the first line which does not start with '#' symbol. *)
let rec line_without_comments in_channel =
	let line = input_line in_channel in
	if String.length line > 0 && line.[0] <> '#' then line
	else line_without_comments in_channel

(* Compare a sanitized string with expected string *)
let compare_sanitized_with_expectation input sanityLevel expected =
        let san = new Sanitizer.html_sanitizer sanityLevel in
	let sanitized = san#sanitize_string input in
	if sanitized = expected then
		print_string ("Check OK:\n\t" ^ input
				^ "\n\t=>\n\t" ^ expected ^ "\n")
	else
		let () = print_string ("Check FAILED:"
				^ "\n\tLINE: " ^ input
				^ "\n\tWANT: " ^ expected
				^ "\n\tGOT:  " ^ sanitized ^ "\n") in
		raise (Failure "Test failed")

(* Sanitize the odd lines of the file and compare the result with even lines. *)
let check_test_channel in_channel = 
	let in_line = line_without_comments in_channel in
        let cmp = compare_sanitized_with_expectation in
        let expected_permissive = input_line in_channel in
        let expected_aggressive =
                match try input_line in_channel with _ -> "" with
                        "" -> expected_permissive | s -> s in
        cmp in_line Permissive expected_permissive;
        cmp in_line Aggressive expected_aggressive

(* Check all line pairs in the input file *)
let check_test_file filename =
	let in_channel = open_in filename in
	try while true do check_test_channel in_channel done with
		End_of_file -> close_in in_channel

(* Get the \emph{whole} channel contents and sanitize it, displaying the difference *)
let sanitize_channel san in_channel =
	let parser = parse ~dtd:read_dtd in
	let ich = new input_channel in_channel in
	let buf = Buffer.create 8000 in
	let och = new output_buffer buf in
	let doc = with_in_obj_channel ich parser in
	write ~dtd:write_dtd och doc;
	print_string "=== Parsed: ===\n";
	print_string (Buffer.contents buf);
	Buffer.clear buf;
	print_string "\n=== Sanitized: ===\n";
	write ~dtd:write_dtd och (san#sanitize_doc doc);
        let output = Buffer.contents buf in
	print_string output;
	och # close_out();
        output


type args = {
        mutable test_files: string list;
        mutable break_long_words: word_break_method;
        mutable permitted_tags: string list;
        mutable sanity_level: Sanitizer.required_sanity_level;
        mutable sanitize_string: string option;
        mutable sanitize_file: string option;
        mutable verify: string option;
        }

let mk_default_args () = {
                test_files = [];
                break_long_words = NoWordBreak;
                permitted_tags = [];
                sanity_level = Sanitizer.Aggressive;
                sanitize_string = None;
                sanitize_file = None;
                verify = None; }

let (args, arglist) =
    let args = mk_default_args () in
    let arglist = Arg.align [
        ("--test", Arg.Rest (fun s -> args.test_files <- args.test_files @ [s]),
        "<file>... Check sanitizing against a list of special test files");
        ("--break-long-words", Arg.Symbol (["false"; "url-aware"; "url-smart"; "dumb"], function
            | "false" -> args.break_long_words <- NoWordBreak
            | "url-aware" -> args.break_long_words <- NoURLWordBreak
            | "url-smart" -> args.break_long_words <- NoStandaloneURLWordBreak
            | "dumb" -> args.break_long_words <- DumbWordBreak
        ),
        "<bool>  Insert HTML word breaks within long runs of characters");
        ("--level", Arg.Symbol (Sanitizer.sanity_levels,
                fun s -> args.sanity_level <- sanity_level_of_string s),
        " Level of sanitizing rigor; default is \"aggressive\"");
        ("--permitted-tags", Arg.String (fun s ->
                let re_space = Str.regexp " " in
                args.permitted_tags <- Str.split re_space s),
        "<tagspecs>  Specify a list of permitted tags: \"a/href img/src,width,height\"");
        ("--string", Arg.String (fun s -> args.sanitize_string <- Some s),
        "<string>  Sanitize a given string");
        ("--file", Arg.String (fun s -> args.sanitize_file <- Some s),
        "<file>  Sanitize a given file");
        ("--verify", Arg.String (fun s -> args.verify <- Some s),
        "<string>  Verify sanitized output of --string or --file");
        ] in
    Arg.parse arglist (fun s -> ()) ("Usage:");
    args, arglist

let _ =
        let ifset var f = match var with
                | Some s -> f s
                | None -> () in
        let verify i = match args.verify with
                | Some o when i = o -> ()
                | Some o when i = (o ^ "\n") -> ()      (* Liberal *)
                | Some o ->
                        output_string stderr ("FAIL: expected \""
                                             ^ String.escaped o ^ "\"\n");
                        exit 1
                | None -> () in

        (* Check that --verify is accompanied by --string or --file *)
        if (args.verify <> None
        && (args.sanitize_string = None && args.sanitize_file = None))
                then (output_string stderr ("Error: --verify should be"
                        ^ " used with --string or --file\n"); exit 1);

        (* Check that --permitted-tags is accompanied by Aggressive *)
        if (args.permitted_tags <> [] && args.sanity_level <> Aggressive)
                then (output_string stderr ("Error: --permitted-tags should be"
                        ^ " used with --level aggressive\n"); exit 1);

        let san = new Sanitizer.html_sanitizer
                        ~break_long_words:args.break_long_words
                        ~erase_script_contents:false
                        ~permitted_tags:args.permitted_tags
                        args.sanity_level in

        (* --string *)
        ifset args.sanitize_string (fun i ->
                let sane = san#sanitize_string i in
                print_string (sane ^ "\n");
                verify sane);

        (* --file *)
        ifset args.sanitize_file (fun fname ->
                let ch = if fname = "-" then stdin else open_in fname in
                verify (sanitize_channel san ch);
                close_in ch);

        (* --test *)
        List.iter check_test_file args.test_files;

        (* No arguments? *)
        (if args = mk_default_args () then (Arg.usage arglist "Usage:"; exit 1))

