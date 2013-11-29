val read_dtd : Nethtml.simplified_dtd
val write_dtd :
  (string * (Nethtml.element_class * Nethtml.model_constraint)) list

type required_sanity_level =
        Transparent | Permissive | Aggressive | NanoHTML of allow_breaks
 and allow_breaks = bool

type word_break_method =    (* In the order of increasing breaking ability... *)
    | NoWordBreak       (* Disable word breaking *)
    | NoURLWordBreak    (* Break words everywhere except for URL strings *)
    | NoStandaloneURLWordBreak (* Break words everywhere except for
                                * URL strings which are not surrounded
                                * by <a>...</a> tags *)
    | DumbWordBreak     (* Wordbreak without respect for URLs *)

val sanity_levels : string list
val sanity_level_of_string : string -> required_sanity_level

val transform_string :
  (Nethtml.document list -> Nethtml.document list) -> string -> string
class html_sanitizer :
  ?break_long_words:word_break_method ->
  ?permit_common_attrs:bool ->
  ?erase_unallowed_tags:bool ->
  ?erase_script_contents:bool ->
  ?force_utf8:bool ->
  ?permitted_tags:string list ->
  required_sanity_level ->
  object
    method sanitize_doc : Nethtml.document list -> Nethtml.document list
    method sanitize_string : string -> string
  end
