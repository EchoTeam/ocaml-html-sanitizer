val read_dtd : Nethtml.simplified_dtd
val write_dtd :
  (string * (Nethtml.element_class * Nethtml.model_constraint)) list

type required_sanity_level =
        Transparent | Permissive | Aggressive | NanoHTML of allow_breaks
 and allow_breaks = bool

val sanity_levels : string list
val sanity_level_of_string : string -> required_sanity_level

val transform_string :
  (Nethtml.document list -> Nethtml.document list) -> string -> string
class html_sanitizer :
  ?break_long_words:bool ->
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
