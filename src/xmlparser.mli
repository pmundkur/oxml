(*
 * Simple Incremental XML Parser
 *)


type t

type xml_external_id =
  | XMLSystemId of string
  | XMLPublicId of string * string

type xml_parse_loc = int * int (* line, col *)

type xml_parse_error_info = xml_parse_loc * string 

exception XMLParseError of xml_parse_error_info

type xml_name = int * string * string (* namespace-id, namespace value, localname *)

class virtual xml_parser_client_interface :
  object
    method virtual xml_doctype_handler    : string -> xml_external_id option -> unit
    method virtual xml_proc_instr_handler : string -> string -> unit
    method virtual xml_start_handler      : xml_name -> (xml_name * string) list -> unit
    method virtual xml_end_handler        : xml_name -> unit
    method virtual xml_cdata_handler      : string -> unit
    method virtual xml_comment_handler    : string -> unit
  end

val create_parser : xml_parser_client_interface -> t

val end_parsing : t -> unit

val cur_line : t -> int

val cur_column : t -> int

val parse : t -> string -> bool -> unit
