type xml_external_id =
  | XMLSystemId of string
  | XMLPublicId of string * string

type xml_parse_loc = int * int

type xml_parse_error_info = xml_parse_loc * string

exception XMLParseError of xml_parse_error_info

class virtual xml_parser_client_interface =
  object
    method virtual xml_doctype_handler    : string -> xml_external_id option -> unit
    method virtual xml_proc_instr_handler : string -> string -> unit
    method virtual xml_start_handler      : string -> (string * string) list -> unit
    method virtual xml_end_handler        : string -> unit
    method virtual xml_cdata_handler      : string -> unit
    method virtual xml_comment_handler    : string -> unit
  end

(*
   Attribute values can contain special sequences of the type:
     		&#35;	represents ascii equivalent of 35 decimal
     		&#xBa;	represents ascii equivalent of 0xBA hexadecimal
     		&amp;	the '&' character
     		&apos;	the apostrophe (single-quote) character
     		&gt;	the '>' character
     		&lt;	the '<' character
     		&quot;	the double-quote character
   Otherwise, attribute values can contain any character except '<', '&', and
   the quote character used to delimit the attribute value.
*)

let get_entity_as_char = function
  | 'a' :: 'm' :: 'p' :: [] ->
      '&'
  | 'a' :: 'p' :: 'o' :: 's' :: [] ->
      '\''
  | 'g' :: 't' :: [] ->
      '>'
  | 'l' :: 't' :: [] ->
      '<'
  | 'q' :: 'u' :: 'o' :: 't' :: [] ->
      '"'
  | _ ->
      raise Not_found

let rev_clist_to_string clist =
  let n = List.length clist in
  let s = String.create n in
  let rec clist_to_string_rec i l =
    match l with
    | [] -> ()
    | h :: tl ->
	s.[i] <- h;
	clist_to_string_rec (i - 1) tl
  in
  clist_to_string_rec (n - 1) clist;
  s

type eol_state =
  | EOL_None
  | EOL_CR

type xml_attvalue =
  | XML_Attr_Value_Normal of char list
  | XML_Attr_Value_Ref of char list
  | XML_Attr_Value_CharRef of char list
  | XML_Attr_Value_HexCharRef of char list
  | XML_Attr_Value_DecCharRefCode of char list * int
  | XML_Attr_Value_HexCharRefCode of char list * int
  | XML_Attr_Value_EntityRefName of char list * char list

type xml_content =
  | XML_Content_Normal of char list
  | XML_Content_Ref of char list
  | XML_Content_CharRef of char list
  | XML_Content_HexCharRef of char list
  | XML_Content_DecCharRefCode of char list * int
  | XML_Content_HexCharRefCode of char list * int
  | XML_Content_EntityRefName of char list * char list
  | XML_Content_RBrack of char list
  | XML_Content_RBrack_RBrack of char list

type xml_parser_state =
  | XML_Parse_Initial
  | XML_Parse_Start
  | XML_Parse_Start_PI_Target of char list
  | XML_Parse_PI_Space of string
  | XML_Parse_PI of string * char list
  | XML_Parse_PI_Q of string * string
  | XML_Parse_Start_Bang
  | XML_Parse_Start_Bang_Dash
  | XML_Parse_Comment of char list
  | XML_Parse_Comment_Dash of char list
  | XML_Parse_Comment_Dash_Dash of char list
  | XML_Parse_DOCTYPE_D
  | XML_Parse_DOCTYPE_DO
  | XML_Parse_DOCTYPE_DOC
  | XML_Parse_DOCTYPE_DOCT
  | XML_Parse_DOCTYPE_DOCTY
  | XML_Parse_DOCTYPE_DOCTYP
  | XML_Parse_DOCTYPE_DOCTYPE
  | XML_Parse_DOCTYPE_Space
  | XML_Parse_DOCTYPE_Name of char list
  | XML_Parse_DOCTYPE_Name_Space
  | XML_Parse_DOCTYPE_SYSTEM_S
  | XML_Parse_DOCTYPE_SYSTEM_SY
  | XML_Parse_DOCTYPE_SYSTEM_SYS
  | XML_Parse_DOCTYPE_SYSTEM_SYST
  | XML_Parse_DOCTYPE_SYSTEM_SYSTE
  | XML_Parse_DOCTYPE_SYSTEM_SYSTEM
  | XML_Parse_DOCTYPE_SYSTEM_Space
  | XML_Parse_DOCTYPE_SysLiteral of char list
  | XML_Parse_DOCTYPE_PUBLIC_P
  | XML_Parse_DOCTYPE_PUBLIC_PU
  | XML_Parse_DOCTYPE_PUBLIC_PUB
  | XML_Parse_DOCTYPE_PUBLIC_PUBL
  | XML_Parse_DOCTYPE_PUBLIC_PUBLI
  | XML_Parse_DOCTYPE_PUBLIC_PUBLIC
  | XML_Parse_DOCTYPE_PUBLIC_Space
  | XML_Parse_DOCTYPE_PubidLiteral of char list
  | XML_Parse_DOCTYPE_PubidLiteral_End
  | XML_Parse_DOCTYPE_PubidLiteral_Space
  | XML_Parse_DOCTYPE_ExternalId_Space
  | XML_Parse_Start_Tag of char list
  | XML_Parse_Attr_Name of char list
  | XML_Parse_Attr_Name_Space of string
  | XML_Parse_Attr_Name_Eq of string
  | XML_Parse_Attr_Name_Eq_Space of string
  | XML_Parse_Attr_Value of string * xml_attvalue
  | XML_Parse_Start_Tag_Slash
  | XML_Parse_Tag_Content of xml_content
  | XML_Parse_End_Tag of char list
  | XML_Parse_End_Tag_Space of char list
  | XML_Parse_Start_CondSect
  | XML_Parse_Start_CDATA_C
  | XML_Parse_Start_CDATA_CD
  | XML_Parse_Start_CDATA_CDA
  | XML_Parse_Start_CDATA_CDAT
  | XML_Parse_Start_CDATA_CDATA
  | XML_Parse_CDATA of char list
  | XML_Parse_CDATA_RBrack of char list
  | XML_Parse_CDATA_RBrack_RBrack of char list

module StringSet = Set.Make (struct type t = string let compare = String.compare end);;

type t =
    { mutable line          : int;                          (* current line number of input *)
      mutable col           : int;                          (* current column number of input *)
      mutable eol           : eol_state;                    (* end-of-line handling *)
      mutable doc_name      : string option;                (* Doctype name *)
      mutable sys_literal   : string option;                (* Doctype system literal *)
      mutable pubid_literal : string option;                (* Doctype pubid literal *)
      mutable tag_stack     : string list;                  (* stack of entered tags *)
      mutable attr_stack    : (string * string) list;       (* stack of parsed attributes for the currently open tag *)
      mutable attr_set      : StringSet.t;                  (* set of attr names used to detect duplicates *)
      mutable quote_char    : char;                         (* quote char for attribute value, and system/public literal *)
      mutable parse_state   : xml_parser_state;             (* current parsing state *)
      mutable end_parsing   : bool;                         (* whether parsing is done, and no callbacks should be called *)
      mutable client        : xml_parser_client_interface;  (* client interface for parsing event handlers *)
    }

let create_parser client  =
  { line          = 1;
    col           = 0;
    eol           = EOL_None;
    doc_name      = None;
    sys_literal   = None;
    pubid_literal = None;
    tag_stack     = [];
    attr_stack    = [];
    attr_set      = StringSet.empty;
    quote_char    = '"';
    parse_state   = XML_Parse_Initial;
    end_parsing   = false;
    client        = client;
  }

let end_parsing p =
  p.end_parsing <- true

let cur_line p =
  p.line

let cur_column p =
  p.col

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let valid_first_name_char = function
  | ':' | '_' -> true
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let valid_name_char = function
  | '-' | '.' -> true
  | '0' .. '9' -> true
  | c -> if valid_first_name_char c then true else false

let valid_pubid_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
  | '-' | '\'' | '(' | ')' | '+' | ','
  | '.' | '/'  | ':' | '=' | '?' | ';'
  | '!' | '*'  | '#' | '@' | '$' | '_'
  | '%' | ' '  | '\r' | '\n'
    -> true
  | _ -> false

let print_state p =
  let doc_name =
    match p.doc_name with
    | Some s -> s
    | None -> ""
  and pubid =
    match p.pubid_literal with
    | Some s -> s
    | None -> ""
  and sysid =
    match p.sys_literal with
    | Some s -> s
    | None -> ""
  and loc = ((p.line, p.col) : xml_parse_loc)
  in match p.parse_state with
  | XML_Parse_Initial ->
      "Initial"
  | XML_Parse_Start ->
      "Start"
  | XML_Parse_Start_PI_Target clist ->
      Printf.sprintf "ProcInstrTarget(%s)" (rev_clist_to_string clist)
  | XML_Parse_PI_Space instr ->
      Printf.sprintf "ProcInstrSpace(%s)" instr
  | XML_Parse_PI (instr, clist) ->
      Printf.sprintf "ProcInstr(%s, %s)" instr (rev_clist_to_string clist)
  | XML_Parse_PI_Q (instr, text) ->
      Printf.sprintf "ProcInstrQ(%s, %s)" instr text
  | XML_Parse_Start_Bang ->
      "StartBang"
  | XML_Parse_Start_Bang_Dash ->
      "StartBangDash"
  | XML_Parse_Comment clist ->
      Printf.sprintf "Comment(%s)" (rev_clist_to_string clist)
  | XML_Parse_Comment_Dash clist ->
      Printf.sprintf "CommentDash(%s)" (rev_clist_to_string clist)
  | XML_Parse_Comment_Dash_Dash clist ->
      Printf.sprintf "CommentDashDash(%s)" (rev_clist_to_string clist)
  | XML_Parse_DOCTYPE_D ->
      "DOCTYPE(D)"
  | XML_Parse_DOCTYPE_DO ->
      "DOCTYPE(DO)"
  | XML_Parse_DOCTYPE_DOC ->
      "DOCTYPE(DOC)"
  | XML_Parse_DOCTYPE_DOCT ->
      "DOCTYPE(DOCT)"
  | XML_Parse_DOCTYPE_DOCTY ->
      "DOCTYPE(DOCTY)"
  | XML_Parse_DOCTYPE_DOCTYP ->
      "DOCTYPE(DOCTYP)"
  | XML_Parse_DOCTYPE_DOCTYPE ->
      "DOCTYPE(DOCTYPE)"
  | XML_Parse_DOCTYPE_Space ->
      "DOCTYPE(DOCTYPE )"
  | XML_Parse_DOCTYPE_Name clist ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s)" (rev_clist_to_string clist)
  | XML_Parse_DOCTYPE_Name_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_S ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s S)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_SY ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SY)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_SYS ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYS)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_SYST ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYST)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_SYSTE ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTE)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_SYSTEM ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM)" doc_name
  | XML_Parse_DOCTYPE_SYSTEM_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM )" doc_name
  | XML_Parse_DOCTYPE_SysLiteral clist ->
      (match p.pubid_literal with
      | Some _ ->
	  Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s %s)" doc_name pubid (rev_clist_to_string clist)
      | None ->
	  Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM %s)" doc_name (rev_clist_to_string clist)
      )
  | XML_Parse_DOCTYPE_PUBLIC_P ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s P)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_PU ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PU)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_PUB ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUB)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_PUBL ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBL)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_PUBLI ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLI)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_PUBLIC ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC)" doc_name
  | XML_Parse_DOCTYPE_PUBLIC_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC )" doc_name
  | XML_Parse_DOCTYPE_PubidLiteral clist ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s)" doc_name (rev_clist_to_string clist)
  | XML_Parse_DOCTYPE_PubidLiteral_End ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s)" doc_name pubid
  | XML_Parse_DOCTYPE_PubidLiteral_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s )" doc_name pubid
  | XML_Parse_DOCTYPE_ExternalId_Space ->
      (match (p.pubid_literal, p.sys_literal) with
      | (Some _, Some _) ->
	  Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s %s )" doc_name pubid sysid
      | (None, Some _) ->
	  Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM %s ) " doc_name sysid
      | (Some _, None) | (None, None) ->
	  assert false
      )
  | XML_Parse_Start_Tag clist ->
      Printf.sprintf "StartTag(%s)" (rev_clist_to_string clist)
  | XML_Parse_Attr_Name clist ->
      Printf.sprintf "AttrName(%s)" (rev_clist_to_string clist)
  | XML_Parse_Attr_Name_Space s ->
      Printf.sprintf "AttrNameSpace(%s)" s
  | XML_Parse_Attr_Name_Eq s ->
      Printf.sprintf "AttrNameEq(%s)" s
  | XML_Parse_Attr_Name_Eq_Space s ->
      Printf.sprintf "AttrNameEqSpace(%s)" s
  | XML_Parse_Attr_Value (n, aval) ->
      begin
	match aval with
	| XML_Attr_Value_Normal clist ->
	    Printf.sprintf "AttrValue(%s, %s)" n (rev_clist_to_string clist)
	| XML_Attr_Value_Ref clist ->
	    Printf.sprintf "AttrValue_Ref(%s, %s)" n (rev_clist_to_string clist)
	| XML_Attr_Value_CharRef clist ->
	    Printf.sprintf "AttrValue_CharRef(%s, %s)" n (rev_clist_to_string clist)
	| XML_Attr_Value_HexCharRef clist ->
	    Printf.sprintf "AttrValue_HexCharRef(%s, %s)" n (rev_clist_to_string clist)
	| XML_Attr_Value_DecCharRefCode (clist, ccode) ->
	    Printf.sprintf "AttrValue_DecCharRefCode(%s, %s, %d)" n (rev_clist_to_string clist) ccode
	| XML_Attr_Value_HexCharRefCode (clist, ccode) ->
	    Printf.sprintf "AttrValue_HexCharRefCode(%s, %s, %d)" n (rev_clist_to_string clist) ccode
	| XML_Attr_Value_EntityRefName (clist, erlist) ->
	    Printf.sprintf "AttrValue_EntityRef(%s, %s, %s)" n (rev_clist_to_string clist) (rev_clist_to_string erlist)
      end
  | XML_Parse_Start_Tag_Slash ->
      Printf.sprintf "StartTagSlash(%s)" (List.hd p.tag_stack)
  | XML_Parse_Tag_Content content ->
      begin
	match content with
	| XML_Content_Normal clist ->
	    Printf.sprintf "Content('%s')" (rev_clist_to_string clist)
	| XML_Content_Ref clist ->
	    Printf.sprintf "Content_Ref('%s')" (rev_clist_to_string clist)
	| XML_Content_CharRef clist ->
	    Printf.sprintf "Content_CharRef('%s')" (rev_clist_to_string clist)
	| XML_Content_HexCharRef clist ->
	    Printf.sprintf "Content_HexCharRef('%s')" (rev_clist_to_string clist)
	| XML_Content_DecCharRefCode (clist, ccode) ->
	    Printf.sprintf "Content_DecCharRefCode('%s', %d)" (rev_clist_to_string clist) ccode
	| XML_Content_HexCharRefCode (clist, ccode) ->
	    Printf.sprintf "Content_HexCharRefCode('%s', %d)" (rev_clist_to_string clist) ccode
	| XML_Content_EntityRefName (clist, erlist) ->
	    Printf.sprintf "Content_EntityRef('%s', '%s')" (rev_clist_to_string clist) (rev_clist_to_string erlist)
	| XML_Content_RBrack clist ->
	    Printf.sprintf "Content_RBrack('%s')" (rev_clist_to_string clist)
	| XML_Content_RBrack_RBrack clist ->
	    Printf.sprintf "Content_RBrack_RBrack('%s')" (rev_clist_to_string clist)
      end
  | XML_Parse_End_Tag clist ->
      Printf.sprintf "EndTag(%s)" (rev_clist_to_string clist)
  | XML_Parse_End_Tag_Space clist ->
      Printf.sprintf "EndTagSpace(%s)" (rev_clist_to_string clist)
  | XML_Parse_Start_CondSect ->
      "Start_CondSect"
  | XML_Parse_Start_CDATA_C ->
      "Start_CDATA_C"
  | XML_Parse_Start_CDATA_CD ->
      "Start_CDATA_CD"
  | XML_Parse_Start_CDATA_CDA ->
      "Start_CDATA_CDA"
  | XML_Parse_Start_CDATA_CDAT ->
      "Start_CDATA_CDAT"
  | XML_Parse_Start_CDATA_CDATA ->
      "Start_CDATA_CDATA"
  | XML_Parse_CDATA clist ->
      Printf.sprintf "CDATA(%s)" (rev_clist_to_string clist)
  | XML_Parse_CDATA_RBrack clist ->
      Printf.sprintf "CDATA](%s)" (rev_clist_to_string clist)
  | XML_Parse_CDATA_RBrack_RBrack clist ->
      Printf.sprintf "CDATA]](%s)" (rev_clist_to_string clist)

let parse_char p c =
(* Printf.printf "State = '%s' Input = '%c'\n" (print_state p) c; *)
  let loc = ((p.line, p.col) : xml_parse_loc) in
  let get_docname p =
    match p.doc_name with
    | Some s -> s
    | None -> assert false
  in
  match p.parse_state with
  | XML_Parse_Initial ->
      if not (is_space c) then
	begin
	  match c with
	  | '<' -> p.parse_state <- XML_Parse_Start
	  |  _  -> raise (XMLParseError (loc, "Unexpected char before a document tag"))
	end
  | XML_Parse_Start ->
      if c = '?' then
	p.parse_state <- XML_Parse_Start_PI_Target []
      else if c = '!' then
	p.parse_state <- XML_Parse_Start_Bang
      else if c = '/' then
	p.parse_state <- XML_Parse_End_Tag []
      else if valid_first_name_char c then
	p.parse_state <- XML_Parse_Start_Tag [ c ]
      else
	raise (XMLParseError (loc, "Invalid first character in start tag"))
  | XML_Parse_Start_PI_Target clist ->
      if clist = [] then
	if not (valid_first_name_char c) then
	  raise (XMLParseError (loc, "Invalid first character in processing instruction target"))
	else
	  p.parse_state <- XML_Parse_Start_PI_Target (c :: clist)
      else
	if is_space c then
	  p.parse_state <- XML_Parse_PI_Space (rev_clist_to_string clist)
	else if c = '?' then
	  p.parse_state <- XML_Parse_PI_Q ((rev_clist_to_string clist), "")
	else if valid_name_char c then
	  p.parse_state <- XML_Parse_Start_PI_Target (c :: clist)
	else
	  raise (XMLParseError (loc, "Invalid character in processing instruction target"))
  | XML_Parse_PI_Space instr ->
      if c = '?' then
	p.parse_state <- XML_Parse_PI_Q (instr, "")
      else if not (is_space c) then
	p.parse_state <- XML_Parse_PI (instr, [c])
  | XML_Parse_PI (instr, clist) ->
      if c = '?' then
	p.parse_state <- XML_Parse_PI_Q (instr, (rev_clist_to_string clist))
      else
	p.parse_state <- XML_Parse_PI (instr, c :: clist)
  | XML_Parse_PI_Q (instr, text) ->
      if c = '>' then
	begin
	  p.client#xml_proc_instr_handler instr text;
	  p.parse_state <-
	    if p.tag_stack = [] then
	      XML_Parse_Initial
	    else
	      XML_Parse_Tag_Content (XML_Content_Normal [])
	end
      else
	raise  (XMLParseError (loc, "Invalid character in processing instruction"))
  | XML_Parse_Start_Bang ->
      if c = '-' then
	p.parse_state <- XML_Parse_Start_Bang_Dash
      else if c = 'D' then
	(* The only XML declaration beginning with D is DOCTYPE, and
	   so we can check for its context right here instead of at the
	   end of the tag.  There can be only one Doctype declaration,
	   and it should be in the prolog (i.e. the tag stack should be
	   empty).
	 *)
	if p.doc_name = None && p.tag_stack = [] then
	  p.parse_state <- XML_Parse_DOCTYPE_D
	else
	  raise (XMLParseError (loc, "Invalid tag"))
      else if c = '[' then
	p.parse_state <- XML_Parse_Start_CondSect
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_Bang_Dash ->
      if c = '-' then
	p.parse_state <- XML_Parse_Comment []
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_Comment clist ->
      if c = '-' then
	p.parse_state <- XML_Parse_Comment_Dash clist
      else
	p.parse_state <- XML_Parse_Comment (c :: clist)
  | XML_Parse_Comment_Dash clist ->
      if c = '-' then
	p.parse_state <- XML_Parse_Comment_Dash_Dash clist
      else
	p.parse_state <- XML_Parse_Comment (c :: '-' :: clist)
  | XML_Parse_Comment_Dash_Dash clist ->
      if c = '>' then
	begin
	  p.client#xml_comment_handler (rev_clist_to_string clist);
	  p.parse_state <-
	    if p.tag_stack = [] then
	      XML_Parse_Initial
	    else
	      XML_Parse_Tag_Content (XML_Content_Normal [])
	end
      else
	raise (XMLParseError (loc, "Invalid comment"))
  | XML_Parse_DOCTYPE_D ->
      if c = 'O' then
	p.parse_state <- XML_Parse_DOCTYPE_DO
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DO ->
      if c = 'C' then
	p.parse_state <- XML_Parse_DOCTYPE_DOC
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DOC ->
      if c = 'T' then
	p.parse_state <- XML_Parse_DOCTYPE_DOCT
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DOCT ->
      if c = 'Y' then
	p.parse_state <- XML_Parse_DOCTYPE_DOCTY
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DOCTY ->
      if c = 'P' then
	p.parse_state <- XML_Parse_DOCTYPE_DOCTYP
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DOCTYP ->
      if c = 'E' then
	p.parse_state <- XML_Parse_DOCTYPE_DOCTYPE
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_DOCTYPE ->
      if is_space c then
	p.parse_state <- XML_Parse_DOCTYPE_Space
      else
	raise (XMLParseError (loc, "Invalid tag"))
  | XML_Parse_DOCTYPE_Space ->
      if is_space c then
	()
      else if valid_first_name_char c then
	p.parse_state <- XML_Parse_DOCTYPE_Name [ c ]
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_Name clist ->
      if is_space c then
	begin
	  p.doc_name <- (Some (rev_clist_to_string clist));
	  p.parse_state <- XML_Parse_DOCTYPE_Name_Space
	end
      else if valid_name_char c then
	p.parse_state <- XML_Parse_DOCTYPE_Name (c :: clist)
      else if c = '>' then
	let dn = rev_clist_to_string clist in
	begin
	  p.doc_name <- Some dn;
	  p.client#xml_doctype_handler dn None;
	  p.parse_state <- XML_Parse_Initial
	end
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_Name_Space ->
      if is_space c then
	()
      else if c = 'S' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_S
      else if c = 'P' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_P
      else if c = '>' then
	let dn = get_docname p in
	begin
	  p.client#xml_doctype_handler dn None;
	  p.parse_state <- XML_Parse_Initial
	end
      else if c = '[' then
	raise (XMLParseError (loc, "Unsupported Doctype (internal subset)"))
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_S ->
      if c = 'Y' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_SY
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_SY ->
      if c = 'S' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_SYS
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_SYS ->
      if c = 'T' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_SYST
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_SYST ->
      if c = 'E' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_SYSTE
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_SYSTE ->
      if c = 'M' then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_SYSTEM
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_SYSTEM ->
      if is_space c then
	p.parse_state <- XML_Parse_DOCTYPE_SYSTEM_Space
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SYSTEM_Space ->
      if is_space c then
	()
      else if c = '"' || c = '\'' then
	begin
	  p.quote_char <- c;
	  p.parse_state <- XML_Parse_DOCTYPE_SysLiteral []
	end
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_SysLiteral clist ->
      if c = p.quote_char then
	begin
	  p.sys_literal <- Some (rev_clist_to_string clist);
	  p.parse_state <- XML_Parse_DOCTYPE_ExternalId_Space
	end
      else
	p.parse_state <- XML_Parse_DOCTYPE_SysLiteral (c :: clist)
  | XML_Parse_DOCTYPE_PUBLIC_P ->
      if c = 'U' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_PU
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_PU ->
      if c = 'B' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_PUB
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_PUB ->
      if c = 'L' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_PUBL
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_PUBL ->
      if c = 'I' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_PUBLI
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_PUBLI ->
      if c = 'C' then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_PUBLIC
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_PUBLIC ->
      if is_space c then
	p.parse_state <- XML_Parse_DOCTYPE_PUBLIC_Space
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PUBLIC_Space ->
      if is_space c then
	()
      else if c = '"' || c = '\'' then
	begin
	  p.quote_char <- c;
	  p.parse_state <- XML_Parse_DOCTYPE_PubidLiteral []
	end
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_DOCTYPE_PubidLiteral clist ->
      if c = p.quote_char then
	begin
	  p.pubid_literal <- Some (rev_clist_to_string clist);
	  p.parse_state <- XML_Parse_DOCTYPE_PubidLiteral_End
	end
      else if valid_pubid_char c then
	p.parse_state <- XML_Parse_DOCTYPE_PubidLiteral (c :: clist)
      else
	raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | XML_Parse_DOCTYPE_PubidLiteral_End ->
      if is_space c then
	p.parse_state <- XML_Parse_DOCTYPE_PubidLiteral_Space
      else
	raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | XML_Parse_DOCTYPE_PubidLiteral_Space ->
      if is_space c then
	()
      else if c = '"' || c = '\'' then
	begin
	  p.quote_char <- c;
	  p.parse_state <- XML_Parse_DOCTYPE_SysLiteral []
	end
      else
	raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | XML_Parse_DOCTYPE_ExternalId_Space ->
      if is_space c then
	()
      else if c = '[' then
	raise (XMLParseError (loc, "Unsupported Doctype (internal subset)"))
      else if c = '>' then
	let extid =
	  match (p.pubid_literal, p.sys_literal) with
	  | (Some pubid, Some sysid) ->
	      Some (XMLPublicId (pubid, sysid))
	  | (None, Some sysid) ->
	      Some (XMLSystemId sysid)
	  | (Some _, None) | (None, None) ->
	      raise (XMLParseError (loc, "Bad Parser State while parsing DOCTYPE"))
	and dn = get_docname p in
	begin
	  p.client#xml_doctype_handler dn extid;
	  p.parse_state <- XML_Parse_Initial
	end
      else
	raise (XMLParseError (loc, "Invalid Doctype"))
  | XML_Parse_Start_Tag clist ->
      if is_space c then
	begin
	  p.tag_stack <- rev_clist_to_string clist :: p.tag_stack;
	  p.parse_state <- XML_Parse_Attr_Name []
	end
      else if c = '>' then
	let start_tag = rev_clist_to_string clist in
	begin
	  p.tag_stack <- start_tag :: p.tag_stack;
	  p.client#xml_start_handler start_tag [];
	  p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal [])
	end
      else if c = '/' then
	begin
	  p.tag_stack <- rev_clist_to_string clist :: p.tag_stack;
	  p.parse_state <- XML_Parse_Start_Tag_Slash
	end
      else if valid_name_char c then
	p.parse_state <- XML_Parse_Start_Tag (c :: clist)
      else
	raise (XMLParseError (loc, "Invalid character in start tag"))
  | XML_Parse_Attr_Name clist ->
      if clist = [] then
	if is_space c then
	  ()
	else if c = '/' then
	  p.parse_state <- XML_Parse_Start_Tag_Slash
	else if c = '>' then
	  begin
	    p.client#xml_start_handler (List.hd p.tag_stack) (List.rev p.attr_stack);
	    p.attr_stack <- [];
	    p.attr_set <- StringSet.empty;
	    p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal [])
	  end
	else if not (valid_first_name_char c) then
	  raise (XMLParseError (loc, "Invalid first attribute name character"))
	else
	  p.parse_state <- XML_Parse_Attr_Name (c :: clist)
      else if c = '=' then
	p.parse_state <- XML_Parse_Attr_Name_Eq (rev_clist_to_string clist)
      else if is_space c then
	p.parse_state <- XML_Parse_Attr_Name_Space (rev_clist_to_string clist)
      else if valid_name_char c then
	p.parse_state <- XML_Parse_Attr_Name (c :: clist)
      else
	raise (XMLParseError (loc, "Invalid attribute name character"))
  | XML_Parse_Attr_Name_Space attr_name ->
      if c = '=' then
	p.parse_state <- XML_Parse_Attr_Name_Eq attr_name
      else if is_space c then
	()
      else
	raise (XMLParseError (loc, "Invalid attribute specification"))
  | XML_Parse_Attr_Name_Eq attr_name ->
      if is_space c then
	p.parse_state <- XML_Parse_Attr_Name_Eq_Space attr_name
      else if c = '"' || c = '\'' then
	begin
	  p.quote_char <- c;
	  p.parse_state <- XML_Parse_Attr_Value (attr_name, (XML_Attr_Value_Normal []))
	end
      else
	raise (XMLParseError (loc, "Invalid attribute specification"))
  | XML_Parse_Attr_Name_Eq_Space attr_name ->
      if c = '"' || c = '\'' then
	begin
	  p.quote_char <- c;
	  p.parse_state <- XML_Parse_Attr_Value (attr_name, (XML_Attr_Value_Normal []))
	end
      else if is_space c then
	()
      else
	raise (XMLParseError (loc, "Invalid attribute specification"))
  | XML_Parse_Attr_Value (attr_name, attr_val) ->
      if c = p.quote_char then
	if StringSet.mem attr_name p.attr_set then
	  raise (XMLParseError (loc, "Non-unique attribute name"))
	else
	  begin
	    match attr_val with
	    | XML_Attr_Value_Normal clist ->
		p.attr_stack <- (attr_name, rev_clist_to_string clist) :: p.attr_stack;
		p.attr_set <- StringSet.add attr_name p.attr_set;
		p.parse_state <- XML_Parse_Attr_Name []
	    | XML_Attr_Value_Ref clist ->
		raise (XMLParseError (loc, "Invalid character in attribute value"))
	    | XML_Attr_Value_CharRef clist ->
		raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	    | XML_Attr_Value_EntityRefName (clist, erlist) ->
		raise (XMLParseError (loc, "Invalid attribute value (unterminated entity reference)"))
	    | XML_Attr_Value_DecCharRefCode (clist, ccode) ->
		raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	    | XML_Attr_Value_HexCharRef clist ->
		raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	    | XML_Attr_Value_HexCharRefCode (clist, ccode) ->
		raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	  end
      else if c = '<' then
	raise (XMLParseError (loc, "Invalid character in attribute value"))
      else
	begin
	  match attr_val with
	  | XML_Attr_Value_Normal clist ->
	      if c = '&' then
		p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_Ref clist)
	      else
		p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_Normal (c::clist))
	  | XML_Attr_Value_Ref clist ->
	      if c = '#' then
		p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_CharRef clist)
	      else if valid_first_name_char c then
		p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_EntityRefName (clist, [ c ]))
	      else
		raise (XMLParseError (loc, "Invalid character in attribute value"))
	  | XML_Attr_Value_CharRef clist ->
	      begin
		match c with
		| 'x' ->
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_HexCharRef clist)
		| '0' .. '9' ->
		    let ccode = int_of_char c - int_of_char '0' in
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_DecCharRefCode (clist, ccode))
		| _ ->
		    raise (XMLParseError (loc, "Invalid character reference"))
	      end
	  | XML_Attr_Value_HexCharRef clist ->
	      let ccode = match c with
	      | '0' .. '9' ->
		  int_of_char c - int_of_char '0'
	      | 'a' .. 'f' ->
		  int_of_char c - int_of_char 'a' + 10
	      | 'A' .. 'F' ->
		  int_of_char c - int_of_char 'A' + 10
	      | _ ->
		  raise (XMLParseError (loc, "Invalid character reference"))
	      in
	      p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_HexCharRefCode (clist, ccode))
	  | XML_Attr_Value_DecCharRefCode (clist, ccode) ->
	      begin
		match c with
		| '0' .. '9' ->
		    let ccode = 10 * ccode + int_of_char c - int_of_char '0' in
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_DecCharRefCode (clist, ccode))
		| ';' ->
		    if ccode > 255 then
		      raise (XMLParseError (loc, "Invalid character reference (not in supported range)"))
		    else
		      let ch = char_of_int ccode in
		      p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_Normal (ch :: clist))
		| _ ->
		    raise (XMLParseError (loc, "Invalid character reference"))
	      end
	  | XML_Attr_Value_HexCharRefCode (clist, ccode) ->
	      begin
		match c with
		| '0' .. '9' ->
		    let ccode = 16 * ccode + int_of_char c - int_of_char '0' in
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_DecCharRefCode (clist, ccode))
		| 'a' .. 'f' ->
		    let ccode = 16 * ccode + int_of_char c - int_of_char 'a' + 10 in
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_DecCharRefCode (clist, ccode))
		| 'A' .. 'F' ->
		    let ccode = 16 * ccode + int_of_char c - int_of_char 'A' + 10 in
		    p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_DecCharRefCode (clist, ccode))
		| ';' ->
		    if ccode > 255 then
		      raise (XMLParseError (loc, "Invalid character reference (not in supported range)"))
		    else
		      let ch = char_of_int ccode in
		      p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_Normal (ch :: clist))
		| _ ->
		    raise (XMLParseError (loc, "Invalid character reference"))
	      end
	  | XML_Attr_Value_EntityRefName (clist, ernlist) ->
	      if c = ';' then
		let ename = List.rev ernlist in
		try
		  let eval = get_entity_as_char ename in
		  p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_Normal (eval :: clist))
		with Not_found ->
		  raise (XMLParseError (loc, "Unsupported or unknown entity reference"))
	      else if valid_name_char c then
		p.parse_state <- XML_Parse_Attr_Value (attr_name, XML_Attr_Value_EntityRefName (clist, (c :: ernlist)))
	      else
		raise (XMLParseError (loc, "Invalid character in entity reference"))
	end
  | XML_Parse_Start_Tag_Slash ->
      if c = '>' then
	begin
	  p.client#xml_start_handler (List.hd p.tag_stack) (List.rev p.attr_stack);
	  p.attr_stack <- [];
	  p.attr_set <- StringSet.empty;
	  p.client#xml_end_handler (List.hd p.tag_stack);
	  p.tag_stack <- List.tl p.tag_stack;
	  p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal [])
	end
      else
	raise (XMLParseError (loc, "Invalid character in tag"))
  | XML_Parse_Tag_Content content ->
      if c = '<' then
	begin
	  match content with
	  | XML_Content_Normal [] ->
	      p.parse_state <- XML_Parse_Start
	  | XML_Content_Normal clist ->
	      p.client#xml_cdata_handler (rev_clist_to_string clist);
	      p.parse_state <- XML_Parse_Start
	  | XML_Content_RBrack clist ->
	      p.client#xml_cdata_handler (rev_clist_to_string (']' :: clist));
	      p.parse_state <- XML_Parse_Start
	  | XML_Content_RBrack_RBrack clist ->
	      p.client#xml_cdata_handler (rev_clist_to_string (']' :: ']' :: clist));
	      p.parse_state <- XML_Parse_Start
	  | XML_Content_Ref clist ->
	      raise (XMLParseError (loc, "Invalid character in element content"))
	  | XML_Content_CharRef clist ->
	      raise (XMLParseError (loc, "Invalid element content (unterminated character reference)"))
	  | XML_Content_EntityRefName (clist, erlist) ->
	      raise (XMLParseError (loc, "Invalid element content (unterminated entity reference)"))
	  | XML_Content_DecCharRefCode (clist, ccode) ->
	      raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	  | XML_Content_HexCharRef clist ->
	      raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
	  | XML_Content_HexCharRefCode (clist, ccode) ->
	      raise (XMLParseError (loc, "Invalid element content (unterminated character reference)"))
	end
      else
	begin
	  match content with
	  | XML_Content_Normal clist ->
	      if c = '&' then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Ref clist)
	      else if c = ']' then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_RBrack clist)
	      else
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal (c :: clist))
	  | XML_Content_RBrack clist ->
	      if c = '&' then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Ref (']' :: clist))
	      else
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Ref clist)
	  | XML_Content_RBrack_RBrack clist ->
	      if c = '>' then
		raise (XMLParseError (loc, "Invalid element content"))
	      else if c = '&' then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Ref (']' :: ']' :: clist))
	      else
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal (']' :: ']' :: clist))
	  | XML_Content_Ref clist ->
	      if c = '#' then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_CharRef clist)
	      else if valid_first_name_char c then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_EntityRefName (clist, [ c ]))
	      else
		raise (XMLParseError (loc, "Invalid element content"))
	  | XML_Content_CharRef clist ->
	      begin
		match c with
		| 'x' -> p.parse_state <- XML_Parse_Tag_Content (XML_Content_HexCharRef clist)
		| '0' .. '9' -> p.parse_state <-
		    XML_Parse_Tag_Content (XML_Content_DecCharRefCode
					     (clist, (int_of_char c - int_of_char '0')))
		| _ -> raise (XMLParseError (loc, "Invalid element content"))
	      end
	  | XML_Content_EntityRefName (clist, erlist) ->
	      if c = ';' then
		try
		  let ch = get_entity_as_char (List.rev erlist) in
		  p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal (ch :: clist))
		with
		  Not_found -> raise (XMLParseError (loc, "Unknown/invalid entity reference"))
	      else if valid_name_char c then
		p.parse_state <- XML_Parse_Tag_Content (XML_Content_EntityRefName
							  (clist, (c :: erlist)))
	      else
		raise (XMLParseError (loc, "Invalid element content"))
	  | XML_Content_DecCharRefCode (clist, ccode) ->
	      begin
		match c with
		|  ';' ->
		    if ccode > 255 then
		      raise (XMLParseError (loc, "Character reference out of supported range"))
		    else
		      let ch = char_of_int ccode in
		      p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal (ch :: clist))
		| '0' .. '9' ->
		    p.parse_state <- XML_Parse_Tag_Content (XML_Content_DecCharRefCode
							      (clist, (10 * ccode + int_of_char c - int_of_char '0')))
		| _ ->
		    raise (XMLParseError (loc, "Invalid character in character reference"))
	      end
	  | XML_Content_HexCharRef clist ->
	      let ccode = match c with
	      | '0' .. '9' ->
		  int_of_char c - int_of_char '0'
	      | 'a' .. 'f' ->
		  int_of_char c - int_of_char 'a' + 10
	      | 'A' .. 'F' ->
		  int_of_char c - int_of_char 'A' + 10
	      | _ -> raise (XMLParseError (loc, "Invalid character in character reference"))
	      in
	      p.parse_state <- XML_Parse_Tag_Content (XML_Content_HexCharRefCode (clist, ccode))
	  | XML_Content_HexCharRefCode (clist, ccode) ->
	      begin
		match c with
		| '0' .. '9' ->
		    p.parse_state <- XML_Parse_Tag_Content (XML_Content_HexCharRefCode
							      (clist, (16 * ccode + (int_of_char c - int_of_char '0'))))
		| 'a' .. 'f'->
		    p.parse_state <- XML_Parse_Tag_Content (XML_Content_HexCharRefCode
							      (clist, (16 * ccode + (int_of_char c - int_of_char 'a' + 10))))
		| 'A' .. 'F'->
		    p.parse_state <- XML_Parse_Tag_Content (XML_Content_HexCharRefCode
							      (clist, (16 * ccode + (int_of_char c - int_of_char 'A' + 10))))
		| ';' ->
		    if ccode > 255 then
		      raise (XMLParseError (loc, "Character reference out of supported range"))
		    else
		      let ch = char_of_int ccode in
		      p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal (ch :: clist))
		| _ ->
		    raise (XMLParseError (loc, "Invalid character in character reference"))
	      end
	end
  | XML_Parse_End_Tag clist ->
      if clist = [] then
	if not (valid_first_name_char c) then
	  raise (XMLParseError (loc, "Invalid first character in end tag"))
	else
	  p.parse_state <- XML_Parse_End_Tag (c :: clist)
      else if c = '>' then
	let end_tag = rev_clist_to_string clist in
	if p.tag_stack = [] || (List.hd p.tag_stack) <> end_tag then
	  raise (XMLParseError (loc, "Mismatched end tag"))
	else
	  begin
	    p.tag_stack <- List.tl p.tag_stack;
	    p.client#xml_end_handler end_tag;
	    p.parse_state <- XML_Parse_Tag_Content (XML_Content_Normal [])
	  end
      else if is_space c then
	p.parse_state <- XML_Parse_End_Tag_Space (c :: clist)
      else if valid_name_char c then
	p.parse_state <- XML_Parse_End_Tag (c :: clist)
      else
	raise (XMLParseError (loc, "Invalid character in end tag"))
  | XML_Parse_End_Tag_Space clist ->
      if c = '>' then
	let end_tag = rev_clist_to_string clist in
	if p.tag_stack = [] || (List.hd p.tag_stack) <> end_tag then
	  raise (XMLParseError (loc, "Mismatched end tag"))
	else
	  begin
	    p.tag_stack <- List.tl p.tag_stack;
	    p.client#xml_end_handler end_tag
	  end
      else if not (is_space c) then
	raise (XMLParseError (loc, "Invalid character in end tag"))
  | XML_Parse_Start_CondSect ->
      if c = 'C' then
	p.parse_state <- XML_Parse_Start_CDATA_C
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_CDATA_C ->
      if c = 'D' then
	p.parse_state <- XML_Parse_Start_CDATA_CD
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_CDATA_CD ->
      if c = 'A' then
	p.parse_state <- XML_Parse_Start_CDATA_CDA
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_CDATA_CDA ->
      if c = 'T' then
	p.parse_state <- XML_Parse_Start_CDATA_CDAT
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_CDATA_CDAT ->
      if c = 'A' then
	p.parse_state <- XML_Parse_Start_CDATA_CDATA
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_Start_CDATA_CDATA ->
      if c = '[' then
	p.parse_state <- XML_Parse_CDATA []
      else
	raise (XMLParseError (loc, "Unsupported XML"))
  | XML_Parse_CDATA clist ->
      if c = ']' then
	p.parse_state <- XML_Parse_CDATA_RBrack clist
      else
	p.parse_state <- XML_Parse_CDATA (c :: clist)
  | XML_Parse_CDATA_RBrack clist ->
      if c = ']' then
	p.parse_state <- XML_Parse_CDATA_RBrack_RBrack clist
      else
	p.parse_state <- XML_Parse_CDATA (c :: ']' :: clist)
  | XML_Parse_CDATA_RBrack_RBrack clist ->
      if c = '>' then
	begin
	  p.client#xml_cdata_handler (rev_clist_to_string clist);
	  p.parse_state <-
	    if p.tag_stack = [] then
	      XML_Parse_Initial
	    else
	      XML_Parse_Tag_Content (XML_Content_Normal [])
	end
      else
	p.parse_state <- XML_Parse_CDATA (c :: ']' :: ']' :: clist)

let parse p s is_last_buffer =
  let handle_eol c =
    match p.eol with
    | EOL_None ->
	if c = '\r' then p.eol <- EOL_CR else parse_char p c
    | EOL_CR ->
	if c = '\n' then
	  begin
	    parse_char p c;
	    p.eol <- EOL_None
	  end
	else
	  begin
	    parse_char p '\n';
	    parse_char p c;
	    p.eol <- EOL_None
	  end
  in
  let buflen = String.length s in
  let i = ref 0 in
  while !i < buflen && not p.end_parsing do
    let c = s.[!i] in
    handle_eol c;
    p.col <- p.col + 1;
    if c = '\n' then
      begin
	p.col <- 0;
	p.line <- p.line + 1;
      end;
    incr i
  done;
  if is_last_buffer && p.tag_stack <> [] then
    raise (XMLParseError (((p.line, p.col) : xml_parse_loc), "Unmatched start tags remain"))
