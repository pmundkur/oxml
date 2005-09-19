type external_id =
  | SystemId of string
  | PublicId of string * string

type parse_loc = int * int

type parse_error_info = parse_loc * string

exception XMLParseError of parse_error_info

type name = int * string * string

class virtual parser_client_interface =
  object
    method virtual xml_doctype_handler    : string -> external_id option -> unit
    method virtual xml_proc_instr_handler : string -> string -> unit
    method virtual xml_start_handler      : name -> (name * string) list -> unit
    method virtual xml_end_handler        : name -> unit
    method virtual xml_cdata_handler      : string -> unit
    method virtual xml_comment_handler    : string -> unit
  end

(*
   Attribute values can contain special sequences of the type:
                &#35;   represents ascii equivalent of 35 decimal
                &#xBa;  represents ascii equivalent of 0xBA hexadecimal
                &amp;   the '&' character
                &apos;  the apostrophe (single-quote) character
                &gt;    the '>' character
                &lt;    the '<' character
                &quot;  the double-quote character
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

type unresolved_name = string * string option

type resolved_name = int * string (* namespace-id, localname *)

type attvalue =
  | Attr_Value_Normal of char list
  | Attr_Value_Ref of char list
  | Attr_Value_CharRef of char list
  | Attr_Value_HexCharRef of char list
  | Attr_Value_DecCharRefCode of char list * int
  | Attr_Value_HexCharRefCode of char list * int
  | Attr_Value_EntityRefName of char list * char list

type content =
  | Content_Normal of char list
  | Content_Ref of char list
  | Content_CharRef of char list
  | Content_HexCharRef of char list
  | Content_DecCharRefCode of char list * int
  | Content_HexCharRefCode of char list * int
  | Content_EntityRefName of char list * char list
  | Content_RBrack of char list
  | Content_RBrack_RBrack of char list

type parser_state =
  | Parse_Initial
  | Parse_Start
  | Parse_Start_PI_Target of char list
  | Parse_PI_Space of string
  | Parse_PI of string * char list
  | Parse_PI_Q of string * string
  | Parse_XMLDecl_XML_Space
  | Parse_XMLDecl_Version_V
  | Parse_XMLDecl_Version_Ve
  | Parse_XMLDecl_Version_Ver
  | Parse_XMLDecl_Version_Vers
  | Parse_XMLDecl_Version_Versi
  | Parse_XMLDecl_Version_Versio
  | Parse_XMLDecl_Version_Version
  | Parse_XMLDecl_Version_eq
  | Parse_XMLDecl_Version of char list
  | Parse_XMLDecl_Version_End
  | Parse_XMLDecl_Version_Space
  | Parse_XMLDecl_Encoding_E
  | Parse_XMLDecl_Encoding_En
  | Parse_XMLDecl_Encoding_Enc
  | Parse_XMLDecl_Encoding_Enco
  | Parse_XMLDecl_Encoding_Encod
  | Parse_XMLDecl_Encoding_Encodi
  | Parse_XMLDecl_Encoding_Encodin
  | Parse_XMLDecl_Encoding_Encoding
  | Parse_XMLDecl_Encoding_eq
  | Parse_XMLDecl_Encoding of char list
  | Parse_XMLDecl_Encoding_End
  | Parse_XMLDecl_Encoding_Space
  | Parse_XMLDecl_Standalone_S
  | Parse_XMLDecl_Standalone_St
  | Parse_XMLDecl_Standalone_Sta
  | Parse_XMLDecl_Standalone_Stan
  | Parse_XMLDecl_Standalone_Stand
  | Parse_XMLDecl_Standalone_Standa
  | Parse_XMLDecl_Standalone_Standal
  | Parse_XMLDecl_Standalone_Standalo
  | Parse_XMLDecl_Standalone_Standalon
  | Parse_XMLDecl_Standalone_Standalone
  | Parse_XMLDecl_Standalone_eq
  | Parse_XMLDecl_Standalone of char list
  | Parse_XMLDecl_Standalone_End
  | Parse_XMLDecl_Standalone_Space
  | Parse_XMLDecl_Q
  | Parse_Start_Bang
  | Parse_Start_Bang_Dash
  | Parse_Comment of char list
  | Parse_Comment_Dash of char list
  | Parse_Comment_Dash_Dash of char list
  | Parse_DOCTYPE_D
  | Parse_DOCTYPE_DO
  | Parse_DOCTYPE_DOC
  | Parse_DOCTYPE_DOCT
  | Parse_DOCTYPE_DOCTY
  | Parse_DOCTYPE_DOCTYP
  | Parse_DOCTYPE_DOCTYPE
  | Parse_DOCTYPE_Space
  | Parse_DOCTYPE_Name of char list
  | Parse_DOCTYPE_Name_Space
  | Parse_DOCTYPE_SYSTEM_S
  | Parse_DOCTYPE_SYSTEM_SY
  | Parse_DOCTYPE_SYSTEM_SYS
  | Parse_DOCTYPE_SYSTEM_SYST
  | Parse_DOCTYPE_SYSTEM_SYSTE
  | Parse_DOCTYPE_SYSTEM_SYSTEM
  | Parse_DOCTYPE_SYSTEM_Space
  | Parse_DOCTYPE_SysLiteral of char list
  | Parse_DOCTYPE_PUBLIC_P
  | Parse_DOCTYPE_PUBLIC_PU
  | Parse_DOCTYPE_PUBLIC_PUB
  | Parse_DOCTYPE_PUBLIC_PUBL
  | Parse_DOCTYPE_PUBLIC_PUBLI
  | Parse_DOCTYPE_PUBLIC_PUBLIC
  | Parse_DOCTYPE_PUBLIC_Space
  | Parse_DOCTYPE_PubidLiteral of char list
  | Parse_DOCTYPE_PubidLiteral_End
  | Parse_DOCTYPE_PubidLiteral_Space
  | Parse_DOCTYPE_ExternalId_Space
  | Parse_Start_Tag of char list * string option (* localname, prefix option *)
  | Parse_Attr_Name of char list * string option (* localname, prefix option *)
  | Parse_Attr_Name_Space of unresolved_name
  | Parse_Attr_Name_Eq of unresolved_name
  | Parse_Attr_Value of unresolved_name * attvalue
  | Parse_Attr_Value_End
  | Parse_Attr_Value_End_Space
  | Parse_Start_Tag_Slash
  | Parse_Tag_Content of content
  | Parse_End_Tag of char list * string option   (* localname, prefix option *)
  | Parse_End_Tag_Space
  | Parse_Start_CondSect
  | Parse_Start_CDATA_C
  | Parse_Start_CDATA_CD
  | Parse_Start_CDATA_CDA
  | Parse_Start_CDATA_CDAT
  | Parse_Start_CDATA_CDATA
  | Parse_CDATA of char list
  | Parse_CDATA_RBrack of char list
  | Parse_CDATA_RBrack_RBrack of char list

module AttrSet = Set.Make (struct type t = resolved_name let compare = compare end)

module StringMap = Map.Make (struct type t = string let compare = compare end)

type t =
    { mutable line            : int;                             (* current line number of input *)
      mutable col             : int;                             (* current column number of input *)
      mutable eol             : eol_state;                       (* end-of-line handling *)
      mutable version         : string option;                   (* XML version *)
      mutable encoding        : string option;                   (* XML encoding *)
      mutable standalone      : bool option;                     (* Standalone declaration *)
      mutable doc_name        : string option;                   (* Doctype name *)
      mutable sys_literal     : string option;                   (* Doctype system literal *)
      mutable pubid_literal   : string option;                   (* Doctype pubid literal *)

      mutable next_nspace_id  : int;                             (* next namespace id *)
      mutable id_map          : string array;                    (* the id map {id -> namespace} *)
      mutable rev_id_map      : int StringMap.t;                 (* the reverse map {namespace -> id} *)

      mutable default_nspace  : int;                             (* id of current default namespace *)
      mutable prefix_map      : int StringMap.t;                 (* currently active prefix map
								    {prefix -> id} *)

      mutable nspace_stack    : (int StringMap.t * int) list;    (* stack of prefix maps and default
								    namespace ids that represent
								    namespace scopes corresponding
								    to the elem_stack*)

      mutable elem_stack      : name list;                       (* stack of entered elements *)

      mutable cur_elem        : unresolved_name;                 (* current unresolved element name *)
      mutable attr_list       : (unresolved_name * string) list; (* stack of unresolved parsed
								    attributes for the currently open
								    element *)

      mutable attr_set        : AttrSet.t;                       (* set of resolved attr names, used to
								    detect duplicates *)

      mutable quote_char      : char;                            (* quote char for attribute value,
								    and system/public literal *)
      mutable parse_state     : parser_state;                    (* current parsing state *)
      mutable expect_xmldecl  : bool;                            (* whether an XMLDecl is legal at
								    the current parsing location *)
      mutable in_epilog       : bool;                            (* whether the end of element tree
								    has been passed *)
      mutable parsing_enabled : bool;                            (* whether parsing is currently
								    enabled (i.e. whether any callbacks
								    should be called) *)
      mutable pending_end_tag : bool;                            (* whether an end_tag event is pending *)

      mutable client          : parser_client_interface;         (* event handler interface *)
    }

let create_parser client  =
  let id_map = Array.make 64 "" in
  let pmap = StringMap.empty in
  let pmap = StringMap.add "xml"   1 pmap in
  let pmap = StringMap.add "xmlns" 2 pmap in
  let rmap = StringMap.empty in
  let rmap = StringMap.add "http://www.w3.org/XML/1998/namespace" 1 rmap in
  let rmap = StringMap.add "http://www.w3.org/2000/xmlns/" 2 rmap in
  begin
    id_map.(1) <- "http://www.w3.org/XML/1998/namespace";
    id_map.(2) <- "http://www.w3.org/2000/xmlns/";
    { line            = 1;
      col             = 0;
      eol             = EOL_None;
      version         = None;
      encoding        = None;
      standalone      = None;
      doc_name        = None;
      sys_literal     = None;
      pubid_literal   = None;

      next_nspace_id  = 3;
      id_map          = id_map;
      rev_id_map      = rmap;

      default_nspace  = 0;
      prefix_map      = pmap;
      nspace_stack    = [];

      elem_stack      = [];

      cur_elem        = ("", None);
      attr_list       = [];

      attr_set        = AttrSet.empty;
      quote_char      = '"';
      parse_state     = Parse_Initial;
      expect_xmldecl  = true;
      in_epilog       = false;
      parsing_enabled = false;
      pending_end_tag = false;

      client          = client;
    }
  end

let enable_parsing p =
  p.parsing_enabled <- true

let disable_parsing p =
  p.parsing_enabled <- false

let cur_line p =
  p.line

let cur_column p =
  p.col

let is_restricted_char c =
  if  (('\x01' <= c && c <= '\x08')
    || ('\x0B' <= c && c <= '\x0C')
    || ('\x0E' <= c && c <= '\x1F')
    || ('\x7F' <= c && c <= '\x84')
    || ('\x86' <= c && c <= '\x9F')) then
    true
  else
    false

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_valid_version_char = function
  | '0' .. '9' -> true
  | '.' -> true
  | _ -> false

let is_valid_first_encname_char = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_valid_encname_char = function
  | '0' .. '9' -> true
  | '.' | '_' | '-' -> true
  | c -> is_valid_first_encname_char c

let is_valid_standalone_char = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_valid_first_name_char = function
  | '_' -> true
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_valid_name_char = function
  | '-' | '.' -> true
  | '0' .. '9' -> true
  | c -> is_valid_first_name_char c

let is_valid_pubid_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
  | '-' | '\'' | '(' | ')' | '+' | ','
  | '.' | '/'  | ':' | '=' | '?' | ';'
  | '!' | '*'  | '#' | '@' | '$' | '_'
  | '%' | ' '  | '\r' | '\n'
    -> true
  | _ -> false

let get_nspace_id s p =
  try
    StringMap.find s p.prefix_map
  with
  | Not_found ->
      let num_ids = Array.length p.id_map in
      if p.next_nspace_id = num_ids then
        begin
          let a = Array.make (2 * num_ids) "" in
          Array.blit p.id_map 0 a 0 num_ids;
          p.id_map <- a
        end;
      p.id_map.(p.next_nspace_id) <- s;
      p.prefix_map <- StringMap.add s p.next_nspace_id p.prefix_map;
      let id = p.next_nspace_id in
      p.next_nspace_id <- p.next_nspace_id + 1;
      id

let resolve_and_dispatch_start_tag p loc =
  let resolved_elem =
    match p.cur_elem with
    | lname, None ->
        (* default namespace *)
        (p.default_nspace, p.id_map.(p.default_nspace), lname)
    | lname, Some prefix ->
        (* explicit namespace *)
        try
          let nid = StringMap.find prefix p.prefix_map in
          let ns = p.id_map.(nid) in
          nid, ns, lname
        with
        | Not_found ->
            raise (XMLParseError (loc, "Undeclared namespace prefix in start tag"))
  in
  p.elem_stack <- resolved_elem :: p.elem_stack;
  (* build up a list of resolved attributes *)
  let resolved_attrs =
    List.fold_left
      (fun l (an, av) ->
        match an with
        | lname, None ->
            (* default namespaces do not apply to attributes *)
            if AttrSet.mem (0, lname) p.attr_set then
              raise (XMLParseError (loc, "Duplicate attribute name"))
            else
              begin
                p.attr_set <- AttrSet.add (0, lname) p.attr_set;
                ((0, p.id_map.(0), lname), av) :: l
              end
        | lname, Some prefix ->
            let nid =
              try
                StringMap.find prefix p.prefix_map
              with
              | Not_found ->
                  raise (XMLParseError (loc, "Unknown namespace prefix"))
            in
            if AttrSet.mem (nid, lname) p.attr_set then
              raise (XMLParseError (loc, "Duplicate attribute name"))
            else
              begin
                p.attr_set <- AttrSet.add (nid, lname) p.attr_set;
                ((nid, p.id_map.(nid), lname), av) :: l
              end
      )
      ([] : (name * string) list)
      p.attr_list
  in
  p.client#xml_start_handler resolved_elem resolved_attrs;
  p.attr_list <- [];
  p.attr_set <- AttrSet.empty

let resolve_and_dispatch_end_tag p loc =
  let resolved_elem =
    match p.cur_elem with
    | lname, None ->
        (* default namespace *)
        (p.default_nspace, p.id_map.(p.default_nspace), lname)
    | lname, Some prefix ->
        (* explicit namespace *)
        try
          let nid = StringMap.find prefix p.prefix_map in
          let ns = p.id_map.(nid) in
          (nid, ns, lname)
        with
        | Not_found ->
            raise (XMLParseError (loc, "Undeclared namespace prefix in end tag"))
  in
  if p.elem_stack = [] || List.hd p.elem_stack <> resolved_elem then
    raise (XMLParseError (loc, "Mismatched end tag"))
  else
    begin
      p.elem_stack <- List.tl p.elem_stack;
      p.client#xml_end_handler resolved_elem;
      p.in_epilog <- if p.elem_stack = [] then true else false;
      p.parse_state <- if p.in_epilog then Parse_Initial else Parse_Tag_Content (Content_Normal [])
    end

let dispatch_end_tag p =
  p.client#xml_end_handler (List.hd p.elem_stack);
  p.elem_stack <- List.tl p.elem_stack;
  p.in_epilog <- if p.elem_stack = [] then true else false;
  p.parse_state <- if p.in_epilog then Parse_Initial else Parse_Tag_Content (Content_Normal []);
  let (prev_pmap, prev_def_nspace) = List.hd p.nspace_stack in
  p.prefix_map <- prev_pmap;
  p.default_nspace <- prev_def_nspace

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
  and loc = ((p.line, p.col) : parse_loc)
  and format_prefix prefix =
      match prefix with
      | None -> ""
      | Some p -> p
  in match p.parse_state with
  | Parse_Initial ->
      "Initial"
  | Parse_Start ->
      "Start"
  | Parse_Start_PI_Target clist ->
      Printf.sprintf "ProcInstrTarget(%s)" (rev_clist_to_string clist)
  | Parse_PI_Space instr ->
      Printf.sprintf "ProcInstrSpace(%s)" instr
  | Parse_PI (instr, clist) ->
      Printf.sprintf "ProcInstr(%s, %s)" instr (rev_clist_to_string clist)
  | Parse_PI_Q (instr, text) ->
      Printf.sprintf "ProcInstrQ(%s, %s)" instr text
  | Parse_XMLDecl_XML_Space ->
      "XMLDecl"
  | Parse_XMLDecl_Version_V ->
      "XMLDecl(V)"
  | Parse_XMLDecl_Version_Ve ->
      "XMLDecl(Ve)"
  | Parse_XMLDecl_Version_Ver ->
      "XMLDecl(Ver)"
  | Parse_XMLDecl_Version_Vers ->
      "XMLDecl(Vers)"
  | Parse_XMLDecl_Version_Versi ->
      "XMLDecl(Versi)"
  | Parse_XMLDecl_Version_Versio ->
      "XMLDecl(Versio)"
  | Parse_XMLDecl_Version_Version ->
      "XMLDecl(Version)"
  | Parse_XMLDecl_Version_eq ->
      "XMLDecl(Version=)"
  | Parse_XMLDecl_Version clist ->
      Printf.sprintf "XMLDecl(Version=%s)" (rev_clist_to_string clist)
  | Parse_XMLDecl_Version_End ->
      "XMLDecl(VersionEnd)"
  | Parse_XMLDecl_Version_Space ->
      "XMLDecl(VersionSpace)"
  | Parse_XMLDecl_Encoding_E ->
      "XMLDecl(E)"
  | Parse_XMLDecl_Encoding_En ->
      "XMLDecl(En)"
  | Parse_XMLDecl_Encoding_Enc ->
      "XMLDecl(Enc)"
  | Parse_XMLDecl_Encoding_Enco ->
      "XMLDecl(Enco)"
  | Parse_XMLDecl_Encoding_Encod ->
      "XMLDecl(Encod)"
  | Parse_XMLDecl_Encoding_Encodi ->
      "XMLDecl(Encodi)"
  | Parse_XMLDecl_Encoding_Encodin ->
      "XMLDecl(Encodin)"
  | Parse_XMLDecl_Encoding_Encoding ->
      "XMLDecl(Encoding)"
  | Parse_XMLDecl_Encoding_eq ->
      "XMLDecl(Encoding=)"
  | Parse_XMLDecl_Encoding clist ->
      Printf.sprintf "XMLDecl(Encoding=%s)" (rev_clist_to_string clist)
  | Parse_XMLDecl_Encoding_End ->
      "XMLDecl(EncodingEnd)"
  | Parse_XMLDecl_Encoding_Space ->
      "XMLDecl(EncodingSpace)"
  | Parse_XMLDecl_Standalone_S ->
      "XMLDecl(S)"
  | Parse_XMLDecl_Standalone_St ->
      "XMLDecl(St)"
  | Parse_XMLDecl_Standalone_Sta ->
      "XMLDecl(Sta)"
  | Parse_XMLDecl_Standalone_Stan ->
      "XMLDecl(Stan)"
  | Parse_XMLDecl_Standalone_Stand ->
      "XMLDecl(Stand)"
  | Parse_XMLDecl_Standalone_Standa ->
      "XMLDecl(Standa)"
  | Parse_XMLDecl_Standalone_Standal ->
      "XMLDecl(Standal)"
  | Parse_XMLDecl_Standalone_Standalo ->
      "XMLDecl(Standalo)"
  | Parse_XMLDecl_Standalone_Standalon ->
      "XMLDecl(Standalon)"
  | Parse_XMLDecl_Standalone_Standalone ->
      "XMLDecl(Standalone)"
  | Parse_XMLDecl_Standalone_eq ->
      "XMLDecl(Standalone=)"
  | Parse_XMLDecl_Standalone clist ->
      Printf.sprintf "XMLDecl(Standalone=%s)" (rev_clist_to_string clist)
  | Parse_XMLDecl_Standalone_End ->
      "XMLDecl(StandaloneEnd)"
  | Parse_XMLDecl_Standalone_Space ->
      "XMLDecl(StandaloneSpace)"
  | Parse_XMLDecl_Q ->
      "XMLDecl_Q"
  | Parse_Start_Bang ->
      "StartBang"
  | Parse_Start_Bang_Dash ->
      "StartBangDash"
  | Parse_Comment clist ->
      Printf.sprintf "Comment(%s)" (rev_clist_to_string clist)
  | Parse_Comment_Dash clist ->
      Printf.sprintf "CommentDash(%s)" (rev_clist_to_string clist)
  | Parse_Comment_Dash_Dash clist ->
      Printf.sprintf "CommentDashDash(%s)" (rev_clist_to_string clist)
  | Parse_DOCTYPE_D ->
      "DOCTYPE(D)"
  | Parse_DOCTYPE_DO ->
      "DOCTYPE(DO)"
  | Parse_DOCTYPE_DOC ->
      "DOCTYPE(DOC)"
  | Parse_DOCTYPE_DOCT ->
      "DOCTYPE(DOCT)"
  | Parse_DOCTYPE_DOCTY ->
      "DOCTYPE(DOCTY)"
  | Parse_DOCTYPE_DOCTYP ->
      "DOCTYPE(DOCTYP)"
  | Parse_DOCTYPE_DOCTYPE ->
      "DOCTYPE(DOCTYPE)"
  | Parse_DOCTYPE_Space ->
      "DOCTYPE(DOCTYPE )"
  | Parse_DOCTYPE_Name clist ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s)" (rev_clist_to_string clist)
  | Parse_DOCTYPE_Name_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s )" doc_name
  | Parse_DOCTYPE_SYSTEM_S ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s S)" doc_name
  | Parse_DOCTYPE_SYSTEM_SY ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SY)" doc_name
  | Parse_DOCTYPE_SYSTEM_SYS ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYS)" doc_name
  | Parse_DOCTYPE_SYSTEM_SYST ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYST)" doc_name
  | Parse_DOCTYPE_SYSTEM_SYSTE ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTE)" doc_name
  | Parse_DOCTYPE_SYSTEM_SYSTEM ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM)" doc_name
  | Parse_DOCTYPE_SYSTEM_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM )" doc_name
  | Parse_DOCTYPE_SysLiteral clist ->
      (match p.pubid_literal with
      | Some _ ->
          Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s %s)"
	    doc_name pubid (rev_clist_to_string clist)
      | None ->
          Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM %s)"
	    doc_name (rev_clist_to_string clist)
      )
  | Parse_DOCTYPE_PUBLIC_P ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s P)" doc_name
  | Parse_DOCTYPE_PUBLIC_PU ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PU)" doc_name
  | Parse_DOCTYPE_PUBLIC_PUB ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUB)" doc_name
  | Parse_DOCTYPE_PUBLIC_PUBL ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBL)" doc_name
  | Parse_DOCTYPE_PUBLIC_PUBLI ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLI)" doc_name
  | Parse_DOCTYPE_PUBLIC_PUBLIC ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC)" doc_name
  | Parse_DOCTYPE_PUBLIC_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC )" doc_name
  | Parse_DOCTYPE_PubidLiteral clist ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s)" doc_name (rev_clist_to_string clist)
  | Parse_DOCTYPE_PubidLiteral_End ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s)" doc_name pubid
  | Parse_DOCTYPE_PubidLiteral_Space ->
      Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s )" doc_name pubid
  | Parse_DOCTYPE_ExternalId_Space ->
      (match (p.pubid_literal, p.sys_literal) with
      | (Some _, Some _) ->
          Printf.sprintf "DOCTYPE(DOCTYPE %s PUBLIC %s %s )" doc_name pubid sysid
      | (None, Some _) ->
          Printf.sprintf "DOCTYPE(DOCTYPE %s SYSTEM %s ) " doc_name sysid
      | (Some _, None) | (None, None) ->
          assert false
      )
  | Parse_Start_Tag (clist, prefix) ->
      let pre = format_prefix prefix in
      let lname = rev_clist_to_string clist in
      Printf.sprintf "StartTag(%s:%s)" pre lname
  | Parse_Attr_Name (clist, prefix) ->
      let pre = format_prefix prefix in
      let lname = rev_clist_to_string clist in
      Printf.sprintf "AttrName(%s:%s)" pre lname
  | Parse_Attr_Name_Space (lname, prefix) ->
      let pre = format_prefix prefix in
      Printf.sprintf "AttrNameSpace(%s:%s)" pre lname
  | Parse_Attr_Name_Eq (lname, prefix) ->
      let pre = format_prefix prefix in
      Printf.sprintf "AttrNameEq(%s:%s)" pre lname
  | Parse_Attr_Value ((lname, prefix), aval) ->
      let pre = format_prefix prefix in
      begin
        match aval with
        | Attr_Value_Normal clist ->
            Printf.sprintf "AttrValue(%s:%s, %s)"
	      pre lname (rev_clist_to_string clist)
        | Attr_Value_Ref clist ->
            Printf.sprintf "AttrValue_Ref(%s:%s, %s)"
	      pre lname (rev_clist_to_string clist)
        | Attr_Value_CharRef clist ->
            Printf.sprintf "AttrValue_CharRef(%s:%s, %s)"
	      pre lname (rev_clist_to_string clist)
        | Attr_Value_HexCharRef clist ->
            Printf.sprintf "AttrValue_HexCharRef(%s:%s, %s)"
	      pre lname (rev_clist_to_string clist)
        | Attr_Value_DecCharRefCode (clist, ccode) ->
            Printf.sprintf "AttrValue_DecCharRefCode(%s:%s, %s, %d)"
	      pre lname (rev_clist_to_string clist) ccode
        | Attr_Value_HexCharRefCode (clist, ccode) ->
            Printf.sprintf "AttrValue_HexCharRefCode(%s:%s, %s, %d)"
	      pre lname (rev_clist_to_string clist) ccode
        | Attr_Value_EntityRefName (clist, erlist) ->
            Printf.sprintf "AttrValue_EntityRef(%s:%s, %s, %s)"
	      pre lname (rev_clist_to_string clist) (rev_clist_to_string erlist)
      end
  | Parse_Attr_Value_End ->
      "AttrValueEnd"
  | Parse_Attr_Value_End_Space ->
      "AttrValueEndSpace"
  | Parse_Start_Tag_Slash ->
      let lname, prefix = p.cur_elem in
      let pre = match prefix with | None -> "" | Some pre -> pre in
      Printf.sprintf "StartTagSlash(%s:%s)" pre lname
  | Parse_Tag_Content content ->
      begin
        match content with
        | Content_Normal clist ->
            Printf.sprintf "Content('%s')"
	      (rev_clist_to_string clist)
        | Content_Ref clist ->
            Printf.sprintf "Content_Ref('%s')"
	      (rev_clist_to_string clist)
        | Content_CharRef clist ->
            Printf.sprintf "Content_CharRef('%s')"
	      (rev_clist_to_string clist)
        | Content_HexCharRef clist ->
            Printf.sprintf "Content_HexCharRef('%s')"
	      (rev_clist_to_string clist)
        | Content_DecCharRefCode (clist, ccode) ->
            Printf.sprintf "Content_DecCharRefCode('%s', %d)"
	      (rev_clist_to_string clist) ccode
        | Content_HexCharRefCode (clist, ccode) ->
            Printf.sprintf "Content_HexCharRefCode('%s', %d)"
	      (rev_clist_to_string clist) ccode
        | Content_EntityRefName (clist, erlist) ->
            Printf.sprintf "Content_EntityRef('%s', '%s')"
	      (rev_clist_to_string clist) (rev_clist_to_string erlist)
        | Content_RBrack clist ->
            Printf.sprintf "Content_RBrack('%s')"
	      (rev_clist_to_string clist)
        | Content_RBrack_RBrack clist ->
            Printf.sprintf "Content_RBrack_RBrack('%s')"
	      (rev_clist_to_string clist)
      end
  | Parse_End_Tag (clist, prefix) ->
      let pre = format_prefix prefix in
      Printf.sprintf "EndTag(%s:%s)" pre (rev_clist_to_string clist)
  | Parse_End_Tag_Space ->
      let nid, ns, lname = List.hd p.elem_stack in
      Printf.sprintf "EndTagSpace(%d:%s:%s)" nid ns lname
  | Parse_Start_CondSect ->
      "Start_CondSect"
  | Parse_Start_CDATA_C ->
      "Start_CDATA_C"
  | Parse_Start_CDATA_CD ->
      "Start_CDATA_CD"
  | Parse_Start_CDATA_CDA ->
      "Start_CDATA_CDA"
  | Parse_Start_CDATA_CDAT ->
      "Start_CDATA_CDAT"
  | Parse_Start_CDATA_CDATA ->
      "Start_CDATA_CDATA"
  | Parse_CDATA clist ->
      Printf.sprintf "CDATA(%s)" (rev_clist_to_string clist)
  | Parse_CDATA_RBrack clist ->
      Printf.sprintf "CDATA](%s)" (rev_clist_to_string clist)
  | Parse_CDATA_RBrack_RBrack clist ->
      Printf.sprintf "CDATA]](%s)" (rev_clist_to_string clist)

let parse_char p c =
  Printf.printf "State = '%s' Input = '%c'\n" (print_state p) c;
  let loc = ((p.line, p.col) : parse_loc) in
  let get_docname p =
    match p.doc_name with
    | Some s -> s
    | None -> assert false
  in
  match p.parse_state with
  | Parse_Initial ->
      if c = '<' then
        p.parse_state <- Parse_Start
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid char"))
  | Parse_Start ->
      if c = '?' then
        p.parse_state <- Parse_Start_PI_Target []
      else if c = '!' then
        begin
          p.expect_xmldecl <- false;
          p.parse_state <- Parse_Start_Bang
        end
      else if c = '/' then
        if p.in_epilog then
          raise (XMLParseError (loc, "Invalid epilog"))
        else
          p.parse_state <- Parse_End_Tag ([], None)
      else if is_valid_first_name_char c then
        if p.in_epilog then
          raise (XMLParseError (loc, "Invalid epilog"))
        else
          begin
            p.expect_xmldecl <- false;
            p.parse_state <- Parse_Start_Tag ([ c ], None)
          end
      else
        raise (XMLParseError (loc, "Invalid first character in start tag"))
  | Parse_XMLDecl_XML_Space ->
      if c = 'v' then
        p.parse_state <- Parse_XMLDecl_Version_V
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_V ->
      if c = 'e' then
        p.parse_state <- Parse_XMLDecl_Version_Ve
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Ve ->
      if c = 'r' then
        p.parse_state <- Parse_XMLDecl_Version_Ver
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Ver ->
      if c = 's' then
        p.parse_state <- Parse_XMLDecl_Version_Vers
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Vers ->
      if c = 'i' then
        p.parse_state <- Parse_XMLDecl_Version_Versi
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Versi ->
      if c = 'o' then
        p.parse_state <- Parse_XMLDecl_Version_Versio
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Versio ->
      if c = 'n' then
        p.parse_state <- Parse_XMLDecl_Version_Version
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Version ->
      if c = '=' then
        p.parse_state <- Parse_XMLDecl_Version_eq
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_eq ->
      if c = '\'' || c = '"' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_XMLDecl_Version []
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML version declaration"))
  | Parse_XMLDecl_Version clist ->
      if c = p.quote_char then
        let version = rev_clist_to_string clist in
        if version = "1.0" || version = "1.1" then
          begin
            p.version <- Some version;
            p.parse_state <- Parse_XMLDecl_Version_End
          end
        else
          raise (XMLParseError (loc, "Invalid XML version declaration"))
      else if c = '\'' || c = '"' then
        raise (XMLParseError (loc, "Invalid XML version declaration"))
      else if is_valid_version_char c then
        p.parse_state <- Parse_XMLDecl_Version (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid XML version declaration"))
  | Parse_XMLDecl_Version_End ->
      if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if is_space c then
        p.parse_state <- Parse_XMLDecl_Version_Space
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Version_Space ->
      if c = 'e' then
        p.parse_state <- Parse_XMLDecl_Encoding_E
      else if c = 's' then
        p.parse_state <- Parse_XMLDecl_Standalone_S
      else if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_E ->
      if c = 'n' then
        p.parse_state <- Parse_XMLDecl_Encoding_En
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_En ->
      if c = 'c' then
        p.parse_state <- Parse_XMLDecl_Encoding_Enc
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Enc ->
      if c = 'o' then
        p.parse_state <- Parse_XMLDecl_Encoding_Enco
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Enco ->
      if c = 'd' then
        p.parse_state <- Parse_XMLDecl_Encoding_Encod
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Encod ->
      if c = 'i' then
        p.parse_state <- Parse_XMLDecl_Encoding_Encodi
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Encodi ->
      if c = 'n' then
        p.parse_state <- Parse_XMLDecl_Encoding_Encodin
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Encodin ->
      if c = 'g' then
        p.parse_state <- Parse_XMLDecl_Encoding_Encoding
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Encoding ->
      if c = '=' then
        p.parse_state <- Parse_XMLDecl_Encoding_eq
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_eq ->
      if c = '\'' || c = '"' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_XMLDecl_Encoding []
        end
      else if not(is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding clist ->
      if c = p.quote_char then
        begin
          p.encoding <- Some (rev_clist_to_string clist);
          p.parse_state <- Parse_XMLDecl_Encoding_End
        end
      else if clist = [] && is_valid_first_encname_char c then
        p.parse_state <- Parse_XMLDecl_Encoding [ c ]
      else if clist <> [] && is_valid_encname_char c then
        p.parse_state <- Parse_XMLDecl_Encoding (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid XML encoding declaration"))
  | Parse_XMLDecl_Encoding_End ->
      if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if is_space c then
        p.parse_state <- Parse_XMLDecl_Encoding_Space
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Encoding_Space ->
      if c = 's' then
        p.parse_state <- Parse_XMLDecl_Standalone_S
      else if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_S ->
      if c = 't' then
        p.parse_state <- Parse_XMLDecl_Standalone_St
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_St ->
      if c = 'a' then
        p.parse_state <- Parse_XMLDecl_Standalone_Sta
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Sta ->
      if c = 'n' then
        p.parse_state <- Parse_XMLDecl_Standalone_Stan
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Stan ->
      if c = 'd' then
        p.parse_state <- Parse_XMLDecl_Standalone_Stand
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Stand ->
      if c = 'a' then
        p.parse_state <- Parse_XMLDecl_Standalone_Standa
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Standa ->
      if c = 'l' then
        p.parse_state <- Parse_XMLDecl_Standalone_Standal
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Standal ->
      if c = 'o' then
        p.parse_state <- Parse_XMLDecl_Standalone_Standalo
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Standalo ->
      if c = 'n' then
        p.parse_state <- Parse_XMLDecl_Standalone_Standalon
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Standalon ->
      if c = 'e' then
        p.parse_state <- Parse_XMLDecl_Standalone_Standalone
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Standalone ->
      if c = '=' then
        p.parse_state <- Parse_XMLDecl_Standalone_eq
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_eq ->
      if c = '\'' || c = '"' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_XMLDecl_Standalone []
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone clist ->
      if c = p.quote_char then
        let standalone = rev_clist_to_string clist in
        if standalone = "yes" then
          p.standalone <- Some true
        else if standalone = "no" then
          p.standalone <- Some false
        else
          raise (XMLParseError (loc, "Invalid XML standalone declaration"));
        p.parse_state <- Parse_XMLDecl_Standalone_End
      else if c = '\'' || c = '"' then
        raise (XMLParseError (loc, "Invalid XML standalone declaration"))
      else if is_valid_standalone_char c then
        p.parse_state <- Parse_XMLDecl_Standalone (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid XML standalone declaration"));
  | Parse_XMLDecl_Standalone_End ->
      if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if is_space c then
        p.parse_state <- Parse_XMLDecl_Standalone_Space
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Standalone_Space ->
      if c = '?' then
        p.parse_state <- Parse_XMLDecl_Q
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_XMLDecl_Q ->
      if c = '>' then
        p.parse_state <- Parse_Initial
      else
        raise (XMLParseError (loc, "Invalid XML declaration"))
  | Parse_Start_PI_Target clist ->
      if clist = [] then
        if is_valid_first_name_char c then
          p.parse_state <- Parse_Start_PI_Target (c :: clist)
        else
          raise (XMLParseError (loc, "Invalid first character in processing instruction target"))
      else
        if is_space c or c = '?' then
          let pi_target = rev_clist_to_string clist in
          match pi_target with
          | "xml" ->
              if p.expect_xmldecl && c <> '?' then
                begin
                  p.expect_xmldecl <- false;
                  p.parse_state <- Parse_XMLDecl_XML_Space
                end
              else if p.expect_xmldecl then
                raise (XMLParseError (loc, "Invalid XML declaration"))
              else
                raise (XMLParseError (loc, "Invalid location for an XML declaration"))
          | "xmL" | "xMl" | "xML" | "Xml" | "XmL" | "XMl" | "XML" ->
              raise (XMLParseError (loc, "Invalid processing instruction target"))
          | _ ->
              begin
                p.expect_xmldecl <- false;
                if c = '?' then
                  p.parse_state <- Parse_PI_Q (pi_target, "")
                else
                  p.parse_state <- Parse_PI_Space pi_target
              end
        else if is_valid_name_char c then
          p.parse_state <- Parse_Start_PI_Target (c :: clist)
        else
          raise (XMLParseError (loc, "Invalid character in processing instruction target"))
  | Parse_PI_Space instr ->
      if c = '?' then
        p.parse_state <- Parse_PI_Q (instr, "")
      else if is_space c then
        ()
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_PI (instr, [ c ])
      else
        raise (XMLParseError (loc, "Invalid character in processing instruction"))
  | Parse_PI (instr, clist) ->
      if c = '?' then
        p.parse_state <- Parse_PI_Q (instr, (rev_clist_to_string clist))
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_PI (instr, c :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in processing instruction"))
  | Parse_PI_Q (instr, text) ->
      if c = '>' then
        begin
          p.client#xml_proc_instr_handler instr text;
          p.parse_state <-
            if p.elem_stack = [] then
              Parse_Initial
            else
              Parse_Tag_Content (Content_Normal [])
        end
      else
        raise (XMLParseError (loc, "Invalid character in processing instruction"))
  | Parse_Start_Bang ->
      if c = '-' then
        p.parse_state <- Parse_Start_Bang_Dash
      else if c = 'D' then
        (* The only XML declaration beginning with D is DOCTYPE, and
           so we can check for its context right here instead of at the
           end of the tag.  There can be only one Doctype declaration,
           and it should be in the prolog (i.e. the tag stack should be
           empty).
         *)
        if p.doc_name = None && p.elem_stack = [] then
          p.parse_state <- Parse_DOCTYPE_D
        else
          raise (XMLParseError (loc, "Invalid tag"))
      else if c = '[' then
        if p.in_epilog then
          raise (XMLParseError (loc, "Invalid epilog"))
        else
          p.parse_state <- Parse_Start_CondSect
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_Bang_Dash ->
      if c = '-' then
        p.parse_state <- Parse_Comment []
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_Comment clist ->
      if c = '-' then
        p.parse_state <- Parse_Comment_Dash clist
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_Comment (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in comment"))
  | Parse_Comment_Dash clist ->
      if c = '-' then
        p.parse_state <- Parse_Comment_Dash_Dash clist
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_Comment (c :: '-' :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in comment"))
  | Parse_Comment_Dash_Dash clist ->
      if c = '>' then
        begin
          p.client#xml_comment_handler (rev_clist_to_string clist);
          p.parse_state <-
            if p.elem_stack = [] then
              Parse_Initial
            else
              Parse_Tag_Content (Content_Normal [])
        end
      else
        raise (XMLParseError (loc, "Invalid comment"))
  | Parse_DOCTYPE_D ->
      if c = 'O' then
        p.parse_state <- Parse_DOCTYPE_DO
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DO ->
      if c = 'C' then
        p.parse_state <- Parse_DOCTYPE_DOC
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DOC ->
      if c = 'T' then
        p.parse_state <- Parse_DOCTYPE_DOCT
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DOCT ->
      if c = 'Y' then
        p.parse_state <- Parse_DOCTYPE_DOCTY
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DOCTY ->
      if c = 'P' then
        p.parse_state <- Parse_DOCTYPE_DOCTYP
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DOCTYP ->
      if c = 'E' then
        p.parse_state <- Parse_DOCTYPE_DOCTYPE
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_DOCTYPE ->
      if is_space c then
        p.parse_state <- Parse_DOCTYPE_Space
      else
        raise (XMLParseError (loc, "Invalid tag"))
  | Parse_DOCTYPE_Space ->
      if is_space c then
        ()
      else if is_valid_first_name_char c then
        p.parse_state <- Parse_DOCTYPE_Name [ c ]
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_Name clist ->
      if is_space c then
        begin
          p.doc_name <- (Some (rev_clist_to_string clist));
          p.parse_state <- Parse_DOCTYPE_Name_Space
        end
      else if is_valid_name_char c then
        p.parse_state <- Parse_DOCTYPE_Name (c :: clist)
      else if c = '>' then
        let dn = rev_clist_to_string clist in
        begin
          p.doc_name <- Some dn;
          p.client#xml_doctype_handler dn None;
          p.parse_state <- Parse_Initial
        end
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_Name_Space ->
      if is_space c then
        ()
      else if c = 'S' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_S
      else if c = 'P' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_P
      else if c = '>' then
        let dn = get_docname p in
        p.client#xml_doctype_handler dn None;
        p.parse_state <- Parse_Initial
      else if c = '[' then
        raise (XMLParseError (loc, "Unsupported Doctype (internal subset)"))
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_S ->
      if c = 'Y' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_SY
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_SY ->
      if c = 'S' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_SYS
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_SYS ->
      if c = 'T' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_SYST
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_SYST ->
      if c = 'E' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_SYSTE
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_SYSTE ->
      if c = 'M' then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_SYSTEM
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_SYSTEM ->
      if is_space c then
        p.parse_state <- Parse_DOCTYPE_SYSTEM_Space
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SYSTEM_Space ->
      if c = '"' || c = '\'' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_DOCTYPE_SysLiteral []
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_SysLiteral clist ->
      if c = p.quote_char then
        begin
          p.sys_literal <- Some (rev_clist_to_string clist);
          p.parse_state <- Parse_DOCTYPE_ExternalId_Space
        end
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_DOCTYPE_SysLiteral (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in Doctype"))
  | Parse_DOCTYPE_PUBLIC_P ->
      if c = 'U' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_PU
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_PU ->
      if c = 'B' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_PUB
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_PUB ->
      if c = 'L' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_PUBL
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_PUBL ->
      if c = 'I' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_PUBLI
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_PUBLI ->
      if c = 'C' then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_PUBLIC
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_PUBLIC ->
      if is_space c then
        p.parse_state <- Parse_DOCTYPE_PUBLIC_Space
      else
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PUBLIC_Space ->
      if c = '"' || c = '\'' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_DOCTYPE_PubidLiteral []
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_DOCTYPE_PubidLiteral clist ->
      if c = p.quote_char then
        begin
          p.pubid_literal <- Some (rev_clist_to_string clist);
          p.parse_state <- Parse_DOCTYPE_PubidLiteral_End
        end
      else if is_valid_pubid_char c then
        p.parse_state <- Parse_DOCTYPE_PubidLiteral (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | Parse_DOCTYPE_PubidLiteral_End ->
      if is_space c then
        p.parse_state <- Parse_DOCTYPE_PubidLiteral_Space
      else
        raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | Parse_DOCTYPE_PubidLiteral_Space ->
      if c = '"' || c = '\'' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_DOCTYPE_SysLiteral []
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid character in Doctype PubidLiteral"))
  | Parse_DOCTYPE_ExternalId_Space ->
      if c = '[' then
        raise (XMLParseError (loc, "Unsupported Doctype (internal subset)"))
      else if c = '>' then
        let extid =
          match (p.pubid_literal, p.sys_literal) with
          | (Some pubid, Some sysid) ->
              Some (PublicId (pubid, sysid))
          | (None, Some sysid) ->
              Some (SystemId sysid)
          | (Some _, None) | (None, None) ->
              raise (XMLParseError (loc, "Bad Parser State while parsing DOCTYPE"))
        and dn = get_docname p in
        begin
          p.client#xml_doctype_handler dn extid;
          p.parse_state <- Parse_Initial
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid Doctype"))
  | Parse_Start_Tag (lname, prefix) ->
      if c = ':' then
        match prefix with
        | None -> p.parse_state <- Parse_Start_Tag ([], Some (rev_clist_to_string lname))
        | Some _ -> raise (XMLParseError (loc, "Invalid start tag"))
      else if is_space c then
        begin
          if lname = [] then
            raise (XMLParseError (loc, "Invalid start tag"));
          p.cur_elem <- (rev_clist_to_string lname, prefix);
          p.nspace_stack <- (p.prefix_map, p.default_nspace) :: p.nspace_stack;
          p.parse_state <- Parse_Attr_Name ([], None)
        end
      else if c = '>' then
        begin
          if lname = [] then
            raise (XMLParseError (loc, "Invalid start tag"));
          p.cur_elem <- (rev_clist_to_string lname, prefix);
          p.nspace_stack <- (p.prefix_map, p.default_nspace) :: p.nspace_stack;
          assert (p.attr_list = []);
          assert (p.attr_set = AttrSet.empty);
          resolve_and_dispatch_start_tag p loc;
          p.parse_state <- Parse_Tag_Content (Content_Normal [])
        end
      else if c = '/' then
        begin
          if lname = [] then
            raise (XMLParseError (loc, "Invalid start tag"));
          p.cur_elem <- (rev_clist_to_string lname, prefix);
          p.nspace_stack <- (p.prefix_map, p.default_nspace) :: p.nspace_stack;
          p.parse_state <- Parse_Start_Tag_Slash
        end
      else if is_valid_name_char c then
        p.parse_state <- Parse_Start_Tag ((c :: lname), prefix)
      else
        raise (XMLParseError (loc, "Invalid character in start tag"))
  | Parse_Attr_Name (lname, prefix) ->
      if c = ':' then
        match prefix with
        | None -> p.parse_state <- Parse_Attr_Name ([], Some (rev_clist_to_string lname))
        | Some _ -> raise (XMLParseError (loc, "Invalid attribute name"))
      else if c = '=' then
        if lname = [] then
          raise (XMLParseError (loc, "Invalid attribute"))
        else
          p.parse_state <- Parse_Attr_Name_Eq (rev_clist_to_string lname, prefix)
      else if is_space c then
        if lname = [] then
          raise (XMLParseError (loc, "Invalid attribute"))
        else
          p.parse_state <- Parse_Attr_Name_Space (rev_clist_to_string lname, prefix)
      else if is_valid_name_char c then
        p.parse_state <- Parse_Attr_Name ((c :: lname), prefix)
      else
        raise (XMLParseError (loc, "Invalid attribute name character"))
  | Parse_Attr_Name_Space attr_name ->
      if c = '=' then
        p.parse_state <- Parse_Attr_Name_Eq attr_name
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid attribute specification"))
  | Parse_Attr_Name_Eq attr_name ->
      if c = '"' || c = '\'' then
        begin
          p.quote_char <- c;
          p.parse_state <- Parse_Attr_Value (attr_name, (Attr_Value_Normal []))
        end
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid attribute specification"))
  | Parse_Attr_Value (attr_name, attr_val) ->
      if c = p.quote_char then
        match attr_val with
        | Attr_Value_Normal clist ->
            let xmlns = "xmlns" in
            let s = rev_clist_to_string clist in
            begin
              p.parse_state <- Parse_Attr_Value_End;
              match attr_name with
              | ("xmlns", None) ->
                  (* default namespace decl *)
                  if AttrSet.mem (0, xmlns) p.attr_set then
                    raise (XMLParseError (loc, "Redeclaration of default namespace"))
                  else
                    begin
                      p.attr_set <- AttrSet.add (0, xmlns) p.attr_set;
                      p.default_nspace <- if s = "" then 0 else get_nspace_id s p
                    end
              | (prefix_decl, Some "xmlns") ->
                  (* namespace decl *)
                  if AttrSet.mem (2, prefix_decl) p.attr_set then
                    raise (XMLParseError (loc, "Redeclaration of namespace"))
                  else
                    begin
                      match prefix_decl with
                      | "xmlns" ->
                          raise (XMLParseError (loc, "The xmlns namespace prefix cannot be assigned"))
                      | "xml" ->
                          if s <> p.id_map.(1) then
                            raise (XMLParseError (loc, "The xml namespace prefix cannot be changed"))
                          else
                            p.attr_set <- AttrSet.add (2, prefix_decl) p.attr_set
                      | _ ->
                          p.attr_set <- AttrSet.add (2, prefix_decl) p.attr_set;
                          if s = "" then
                            p.prefix_map <- StringMap.remove prefix_decl p.prefix_map
                          else
                            let nid = get_nspace_id s p in
                            p.prefix_map <- StringMap.add prefix_decl nid p.prefix_map
                    end
              | _ ->
                  p.attr_list <- (attr_name, s) :: p.attr_list
            end
        | Attr_Value_Ref clist ->
            raise (XMLParseError (loc, "Invalid character in attribute value"))
        | Attr_Value_CharRef clist ->
            raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
        | Attr_Value_EntityRefName (clist, erlist) ->
            raise (XMLParseError (loc, "Invalid attribute value (unterminated entity reference)"))
        | Attr_Value_DecCharRefCode (clist, ccode) ->
            raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
        | Attr_Value_HexCharRef clist ->
            raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
        | Attr_Value_HexCharRefCode (clist, ccode) ->
            raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
      else if c = '<' then
        raise (XMLParseError (loc, "Invalid character in attribute value"))
      else
        begin
          match attr_val with
          | Attr_Value_Normal clist ->
              if c = '&' then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Ref clist)
              else if c = '\t' || c = '\n' || c = '\r' then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (' ' :: clist))
              else if not (is_restricted_char c) && c <> '\x00' then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (c :: clist))
              else
                raise (XMLParseError (loc, "Invalid character in attribute value"))
          | Attr_Value_Ref clist ->
              if c = '#' then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_CharRef clist)
              else if is_valid_first_name_char c then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_EntityRefName (clist, [ c ]))
              else
                raise (XMLParseError (loc, "Invalid character in attribute value"))
          | Attr_Value_CharRef clist ->
              begin
                match c with
                | 'x' ->
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_HexCharRef clist)
                | '0' .. '9' ->
                    let ccode = int_of_char c - int_of_char '0' in
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_DecCharRefCode (clist, ccode))
                | _ ->
                    raise (XMLParseError (loc, "Invalid character reference"))
              end
          | Attr_Value_HexCharRef clist ->
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
              p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_HexCharRefCode (clist, ccode))
          | Attr_Value_DecCharRefCode (clist, ccode) ->
              begin
                match c with
                | '0' .. '9' ->
                    let ccode = 10 * ccode + int_of_char c - int_of_char '0' in
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_DecCharRefCode (clist, ccode))
                | ';' ->
                    if ccode > 255 then
                      raise (XMLParseError (loc, "Invalid character reference (not in supported range)"))
                    else if ccode = 0 then
                      raise (XMLParseError (loc, "Illegal character reference"))
                    else
                      let ch = char_of_int ccode in
                      if ch = '<' then
                        raise (XMLParseError (loc, "Illegal character < in attribute value"))
                      else if ch = '\t' || ch = '\n' || ch = '\r' then
                        p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (' ' :: clist))
                      else
                        p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (ch :: clist))
                | _ ->
                    raise (XMLParseError (loc, "Invalid character reference"))
              end
          | Attr_Value_HexCharRefCode (clist, ccode) ->
              begin
                match c with
                | '0' .. '9' ->
                    let ccode = 16 * ccode + int_of_char c - int_of_char '0' in
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_DecCharRefCode (clist, ccode))
                | 'a' .. 'f' ->
                    let ccode = 16 * ccode + int_of_char c - int_of_char 'a' + 10 in
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_DecCharRefCode (clist, ccode))
                | 'A' .. 'F' ->
                    let ccode = 16 * ccode + int_of_char c - int_of_char 'A' + 10 in
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_DecCharRefCode (clist, ccode))
                | ';' ->
                    if ccode > 255 then
                      raise (XMLParseError (loc, "Invalid character reference (not in supported range)"))
                    else if ccode = 0 then
                      raise (XMLParseError (loc, "Illegal character reference"))
                    else
                      let ch = char_of_int ccode in
                      if ch = '<' then
                        raise (XMLParseError (loc, "Illegal character < in attribute value"))
                      else if ch = '\t' || ch = '\n' || ch = '\r' then
                        p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (' ' :: clist))
                      else
                        p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (ch :: clist))
                | _ ->
                    raise (XMLParseError (loc, "Invalid character reference"))
              end
          | Attr_Value_EntityRefName (clist, ernlist) ->
              if c = ';' then
                let ename = List.rev ernlist in
                try
                  let ch = get_entity_as_char ename in
                  if ch = '<' then
                    raise (XMLParseError (loc, "Illegal character < in attribute value"))
                  else
                    p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_Normal (ch :: clist))
                with Not_found ->
                  raise (XMLParseError (loc, "Unsupported or unknown entity reference"))
              else if is_valid_name_char c then
                p.parse_state <- Parse_Attr_Value (attr_name, Attr_Value_EntityRefName (clist, (c :: ernlist)))
              else
                raise (XMLParseError (loc, "Invalid character in entity reference"))
        end
  | Parse_Attr_Value_End ->
      if c = '/' then
        p.parse_state <- Parse_Start_Tag_Slash
      else if c = '>' then
        begin
          resolve_and_dispatch_start_tag p loc;
          p.parse_state <- Parse_Tag_Content (Content_Normal [])
        end
      else if is_space c then
        p.parse_state <- Parse_Attr_Value_End_Space
      else
        raise (XMLParseError (loc, "Space expected after attribute value"))
  | Parse_Attr_Value_End_Space ->
      if c = '/' then
        p.parse_state <- Parse_Start_Tag_Slash
      else if c = '>' then
        begin
          resolve_and_dispatch_start_tag p loc;
          p.parse_state <- Parse_Tag_Content (Content_Normal [])
        end
      else if is_valid_first_name_char c then
        p.parse_state <- Parse_Attr_Name ([ c ], None)
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid first attribute name character"))
  | Parse_Start_Tag_Slash ->
      if c = '>' then
        begin
          resolve_and_dispatch_start_tag p loc;
	  if p.parsing_enabled then
            dispatch_end_tag p
	  else
	    p.pending_end_tag <- true
        end
      else
        raise (XMLParseError (loc, "Invalid character in tag"))
  | Parse_Tag_Content content ->
      if c = '<' then
        begin
          match content with
          | Content_Normal [] ->
              p.parse_state <- Parse_Start
          | Content_Normal clist ->
              p.client#xml_cdata_handler (rev_clist_to_string clist);
              p.parse_state <- Parse_Start
          | Content_RBrack clist ->
              p.client#xml_cdata_handler (rev_clist_to_string (']' :: clist));
              p.parse_state <- Parse_Start
          | Content_RBrack_RBrack clist ->
              p.client#xml_cdata_handler (rev_clist_to_string (']' :: ']' :: clist));
              p.parse_state <- Parse_Start
          | Content_Ref clist ->
              raise (XMLParseError (loc, "Invalid character in element content"))
          | Content_CharRef clist ->
              raise (XMLParseError (loc, "Invalid element content (unterminated character reference)"))
          | Content_EntityRefName (clist, erlist) ->
              raise (XMLParseError (loc, "Invalid element content (unterminated entity reference)"))
          | Content_DecCharRefCode (clist, ccode) ->
              raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
          | Content_HexCharRef clist ->
              raise (XMLParseError (loc, "Invalid attribute value (unterminated character reference)"))
          | Content_HexCharRefCode (clist, ccode) ->
              raise (XMLParseError (loc, "Invalid element content (unterminated character reference)"))
        end
      else
        begin
          match content with
          | Content_Normal clist ->
              if c = '&' then
                p.parse_state <- Parse_Tag_Content (Content_Ref clist)
              else if c = ']' then
                p.parse_state <- Parse_Tag_Content (Content_RBrack clist)
              else if not (is_restricted_char c) && c <> '\x00' then
                p.parse_state <- Parse_Tag_Content (Content_Normal (c :: clist))
              else
                raise (XMLParseError (loc, "Invalid character in element content"))
          | Content_RBrack clist ->
              if c = '&' then
                p.parse_state <- Parse_Tag_Content (Content_Ref (']' :: clist))
              else
                p.parse_state <- Parse_Tag_Content (Content_Ref clist)
          | Content_RBrack_RBrack clist ->
              if c = '>' then
                raise (XMLParseError (loc, "Invalid element content"))
              else if c = '&' then
                p.parse_state <- Parse_Tag_Content (Content_Ref (']' :: ']' :: clist))
              else
                p.parse_state <- Parse_Tag_Content (Content_Normal (']' :: ']' :: clist))
          | Content_Ref clist ->
              if c = '#' then
                p.parse_state <- Parse_Tag_Content (Content_CharRef clist)
              else if is_valid_first_name_char c then
                p.parse_state <- Parse_Tag_Content (Content_EntityRefName (clist, [ c ]))
              else
                raise (XMLParseError (loc, "Invalid element content"))
          | Content_CharRef clist ->
              begin
                match c with
                | 'x' -> p.parse_state <- Parse_Tag_Content (Content_HexCharRef clist)
                | '0' .. '9' -> p.parse_state <-
                    Parse_Tag_Content (Content_DecCharRefCode
                                             (clist, (int_of_char c - int_of_char '0')))
                | _ -> raise (XMLParseError (loc, "Invalid element content"))
              end
          | Content_EntityRefName (clist, erlist) ->
              if c = ';' then
                try
                  let ch = get_entity_as_char (List.rev erlist) in
                  p.parse_state <- Parse_Tag_Content (Content_Normal (ch :: clist))
                with
                  Not_found -> raise (XMLParseError (loc, "Unknown/invalid entity reference"))
              else if is_valid_name_char c then
                p.parse_state <- Parse_Tag_Content (Content_EntityRefName (clist, (c :: erlist)))
              else
                raise (XMLParseError (loc, "Invalid element content"))
          | Content_DecCharRefCode (clist, ccode) ->
              begin
                match c with
                |  ';' ->
                    if ccode > 255 then
                      raise (XMLParseError (loc, "Character reference out of supported range"))
                    else
                      let ch = char_of_int ccode in
                      p.parse_state <- Parse_Tag_Content (Content_Normal (ch :: clist))
                | '0' .. '9' ->
                    p.parse_state <- Parse_Tag_Content (Content_DecCharRefCode
                                                          (clist, (10 * ccode + int_of_char c - int_of_char '0')))
                | _ ->
                    raise (XMLParseError (loc, "Invalid character in character reference"))
              end
          | Content_HexCharRef clist ->
              let ccode = match c with
              | '0' .. '9' ->
                  int_of_char c - int_of_char '0'
              | 'a' .. 'f' ->
                  int_of_char c - int_of_char 'a' + 10
              | 'A' .. 'F' ->
                  int_of_char c - int_of_char 'A' + 10
              | _ -> raise (XMLParseError (loc, "Invalid character in character reference"))
              in
              p.parse_state <- Parse_Tag_Content (Content_HexCharRefCode (clist, ccode))
          | Content_HexCharRefCode (clist, ccode) ->
              begin
                match c with
                | '0' .. '9' ->
                    p.parse_state <- Parse_Tag_Content (Content_HexCharRefCode
                                                          (clist, (16 * ccode + (int_of_char c - int_of_char '0'))))
                | 'a' .. 'f'->
                    p.parse_state <- Parse_Tag_Content (Content_HexCharRefCode
                                                          (clist, (16 * ccode + (int_of_char c - int_of_char 'a' + 10))))
                | 'A' .. 'F'->
                    p.parse_state <- Parse_Tag_Content (Content_HexCharRefCode
                                                          (clist, (16 * ccode + (int_of_char c - int_of_char 'A' + 10))))
                | ';' ->
                    if ccode > 255 then
                      raise (XMLParseError (loc, "Character reference out of supported range"))
                    else
                      let ch = char_of_int ccode in
                      p.parse_state <- Parse_Tag_Content (Content_Normal (ch :: clist))
                | _ ->
                    raise (XMLParseError (loc, "Invalid character in character reference"))
              end
        end
  | Parse_End_Tag (lname, prefix) ->
      if lname = [] then
        if is_valid_first_name_char c then
          p.parse_state <- Parse_End_Tag ([ c ], prefix)
        else
          raise (XMLParseError (loc, "Invalid first character in end tag"))
      else if c = ':' then
        match prefix with
        | None -> p.parse_state <- Parse_End_Tag ([], Some (rev_clist_to_string lname))
        | Some _ -> raise (XMLParseError (loc, "Invalid end tag"))
      else if c = '>' then
        begin
          p.cur_elem <- rev_clist_to_string lname, prefix;
          resolve_and_dispatch_end_tag p loc
        end
      else if is_space c then
        begin
          p.cur_elem <- rev_clist_to_string lname, prefix;
          p.parse_state <- Parse_End_Tag_Space
        end
      else if is_valid_name_char c then
        p.parse_state <- Parse_End_Tag ((c :: lname), prefix)
      else
        raise (XMLParseError (loc, "Invalid character in end tag"))
  | Parse_End_Tag_Space ->
      if c = '>' then
        resolve_and_dispatch_end_tag p loc
      else if not (is_space c) then
        raise (XMLParseError (loc, "Invalid character in end tag"))
  | Parse_Start_CondSect ->
      if c = 'C' then
        p.parse_state <- Parse_Start_CDATA_C
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_CDATA_C ->
      if c = 'D' then
        p.parse_state <- Parse_Start_CDATA_CD
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_CDATA_CD ->
      if c = 'A' then
        p.parse_state <- Parse_Start_CDATA_CDA
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_CDATA_CDA ->
      if c = 'T' then
        p.parse_state <- Parse_Start_CDATA_CDAT
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_CDATA_CDAT ->
      if c = 'A' then
        p.parse_state <- Parse_Start_CDATA_CDATA
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_Start_CDATA_CDATA ->
      if c = '[' then
        (* CData sections can only appear as element content *)
        if p.elem_stack = [] then
          raise (XMLParseError (loc, "Invalid prolog"))
        else
          p.parse_state <- Parse_CDATA []
      else
        raise (XMLParseError (loc, "Unsupported XML"))
  | Parse_CDATA clist ->
      if c = ']' then
        p.parse_state <- Parse_CDATA_RBrack clist
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_CDATA (c :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in CDATA"))
  | Parse_CDATA_RBrack clist ->
      if c = ']' then
        p.parse_state <- Parse_CDATA_RBrack_RBrack clist
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_CDATA (c :: ']' :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in CDATA"))
  | Parse_CDATA_RBrack_RBrack clist ->
      if c = ']' then
        p.parse_state <- Parse_CDATA_RBrack_RBrack (c :: clist)
      else if c = '>' then
        begin
          p.client#xml_cdata_handler (rev_clist_to_string clist);
          p.parse_state <-
            if p.elem_stack = [] then
              Parse_Initial
            else
              Parse_Tag_Content (Content_Normal [])
        end
      else if not (is_restricted_char c) && c <> '\x00' then
        p.parse_state <- Parse_CDATA (c :: ']' :: ']' :: clist)
      else
        raise (XMLParseError (loc, "Invalid character in CDATA"))

let parse p s is_last_buffer =
  let handle_eol c =
    match p.eol with
    | EOL_None ->
        if c = '\x0D' then
          p.eol <- EOL_CR
        else if c = '\x85' then
          parse_char p '\x0A'
        else
          parse_char p c
    | EOL_CR ->
        if c = '\x0A' or c = '\x85' then
          begin
            p.eol <- EOL_None;
            parse_char p '\x0A'
          end
        else
          begin
            p.eol <- EOL_None;
            parse_char p '\x0A';
            parse_char p c
          end
  in
  if p.pending_end_tag then
    begin
      p.pending_end_tag <- false;
      dispatch_end_tag p
    end;
  let buflen = String.length s in
  let i = ref 0 in
  while !i < buflen && p.parsing_enabled do
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
  if is_last_buffer then
    if p.elem_stack <> [] then
      raise (XMLParseError (((p.line, p.col) : parse_loc), "Unmatched start tags remain"))
    else if p.parse_state <> Parse_Initial then
      raise (XMLParseError (((p.line, p.col) : parse_loc), "Unexpected document end"))
    else
      !i
  else
    !i
