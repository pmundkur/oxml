let print_usage argv0 =
  Printf.eprintf "Usage: %s file\n" argv0;
  exit 1;;

class xml_client =
  object
    inherit Xmlparser.parser_client_interface
    method xml_doctype_handler docname extid_opt =
      match extid_opt with
      |	None ->
	  Printf.printf "<!DOCTYPE %s>\n" docname
      | Some (Xmlparser.SystemId s) ->
	  Printf.printf "<!DOCTYPE %s SYSTEM \"%s\">\n" docname s
      | Some (Xmlparser.PublicId (p, s)) ->
	  Printf.printf "<!DOCTYPE %s PUBLIC \"%s\" \"%s\">\n" docname p s
    method xml_proc_instr_handler instr text =
      Printf.printf "<?%s %s?>\n" instr text
    method xml_start_handler elem attrs =
      let (nid, ns, lname) = elem in
      if attrs = [] then
	Printf.printf "<%d:%s:%s>\n" nid ns lname
      else
	begin
	  Printf.printf "<%d:%s:%s\n" nid ns lname;
	  List.iter (fun ((nid, ns, lname), v) -> Printf.printf "\t\t\t%d:%s:%s='%s'\n" nid ns lname v) attrs;
	  Printf.printf "\t>\n"
	end
    method xml_end_handler elem =
      let (nid, ns, lname) = elem in
      Printf.printf "</%d:%s:%s>\n" nid ns lname
    method xml_cdata_handler cdata = Printf.printf "CDATA(%s)\n" cdata
    method xml_comment_handler comm = Printf.printf "<!--%s-->\n" comm
  end;;

let cb = new xml_client;;

let main () =
  (* get filename from argv *)
  let argc = Array.length Sys.argv in
  try
    if argc != 2 then
      print_usage Sys.argv.(0)
    else
      let xp = Xmlparser.create_parser cb in
      let fl = open_in Sys.argv.(1) in
      let first_line = ref true in
      try
	while true do
	  let line = input_line fl in
	  Printf.printf "Parsing '%s'\n" line;
	  if not !first_line then
	    ignore (Xmlparser.parse xp "\n" false);
	  ignore (Xmlparser.parse xp line false);
	  first_line := false
	done
      with
      | End_of_file ->
	  begin
	    close_in fl;
	    ignore (Xmlparser.parse xp "" true)
	  end
      | _ as e ->
	  begin
	    close_in fl;
	    raise e
	  end
  with
  | Xmlparser.XMLParseError ((lin, col), msg) ->
      Printf.eprintf "Parse error on line %d, col %d of %s: %s\n" lin col Sys.argv.(1) msg;
      exit 1
  | Failure f ->
      Printf.eprintf "%s: %s" Sys.argv.(0) f;
      exit 1
in
  main ()
