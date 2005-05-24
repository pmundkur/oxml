(*
 * Simple Incremental XML Parser
 *)


type t

type external_id =
  | SystemId of string
  | PublicId of string * string

type parse_loc = int * int (* line, col *)

type parse_error_info = parse_loc * string

exception XMLParseError of parse_error_info

type name = int * string * string (* namespace-id, namespace value, localname *)

class virtual parser_client_interface :
  object
    method virtual xml_doctype_handler    : string -> external_id option -> unit
    method virtual xml_proc_instr_handler : string -> string -> unit
    method virtual xml_start_handler      : name -> (name * string) list -> unit
    method virtual xml_end_handler        : name -> unit
    method virtual xml_cdata_handler      : string -> unit
    method virtual xml_comment_handler    : string -> unit
  end

val create_parser : parser_client_interface -> t

val enable_parsing  : t -> unit

val disable_parsing : t -> unit

val cur_line : t -> int

val cur_column : t -> int

val parse : t -> string -> bool -> int
