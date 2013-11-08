(*
 * by Chris Greenhalgh (chris.greenhalgh@nottingham.ac.uk)
 *
 * Copyright (c) 2013, The University of Nottingham
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(* reflection (dyntype & Cow.Json)-based HTML form CRUD interface to DB *)

module CL = Cohttp_lwt_mirage
module C = Cohttp
open C.Code
open Cow
open Cow.Html
module DB = Persist.DB
module B = Baardskeerder
open Mime
open Dbcommon

let html_header = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" "^
  "\"http://www.w3c.org/TR/html4/loose.dtd\">\n"^
  "<html>\n"^
  "<head>"

let html_header2 = "</head><body>"

let html_trailer = "</body></html>"

let respond_html title html = 
  let headers = C.Header.init_with header_content_type content_type_html in
  let b = Buffer.create (1000) in
  Buffer.add_string b html_header; 
  (* ensure escaping *)
  Buffer.add_string b (Html.to_string (<:html< <title>$str:title$</title> >>)); 
  Buffer.add_string b html_header2; 
  Buffer.add_string b (Html.to_string html); 
  Buffer.add_string b html_trailer; 
  CL.Server.respond_string ~headers ~status:`OK ~body:(Buffer.contents b) () 

(* index *)
let return_index req typeinfos =
  (* top-level list *)
  let title = <:html< <h1>Dbforms index</h1> >> in
  let table h b = <:html< <table><thead>$h$</thead><tbody>$list:b$</tbody></table> >> in
  let tr x = <:html< <tr>$list:x$</tr> >> in
  let th x = <:html< <th>$x$</th> >> in
  let td x = <:html< <td>$x$</td> >> in
  let headings = tr ([th (html_of_string "Type"); 
    th (html_of_string "Parent"); 
    th (html_of_string "Actions")]) in
  let pname ti = match ti.tparent with 
   | None -> ""
   | Some pn -> pn in
  (* TODO parents *)
  let list_link ti = { text="List"; href="/db/"^ti.tname^"/?action=list" } in
  let add_link ti = { text="Add..."; href="/db/"^ti.tname^"/?action=addform" } in
  let row ti =
    let actions = 
      if ti.tparent=None then
        let list_link_html = html_of_link (list_link ti) in
        let add_link_html = html_of_link (add_link ti) in
	<:html< <td>$list_link_html$ $add_link_html$</td> >>
      else
        <:html< <td></td> >> in
    tr ([td (html_of_string ti.tname); 
      td (html_of_string (pname ti)); 
      actions ]) in
  let rows = List.map row typeinfos in
  let dump_link = html_of_link { text="Dump"; href="/db/dump.js" } in
  let restore_link = html_of_link { text="Restore..."; href="/db/restore.html" } in
  let html = List.flatten [title; table headings rows; <:html<<h2>Whole Database</h2>&>>; 
    <:html<<p>$dump_link$</p>&>>; <:html<<p>$restore_link$</p>&>> ] in
  respond_html "Dbforms index" html

let button_form label action = 
  <:html<<form method="POST" action="$str:action$"><input type="submit" value="$str:label$"></input></form>&>>

let return_list req path_elems typeinfos =
  (* check type/path *)
    let ti = get_typeinfo typeinfos path_elems in
    (* TODO check type parent(s) *)
    let key = get_key path_elems in 
    lwt db = Persist.get None in
    let key2 = String.copy key in
    let kmax = (String.length key)-1 in
    String.set key2 kmax (Char.chr (Char.code (String.get key2 kmax) + 1));
    lwt entries = DB.range_entries_latest db (Some key) true (Some key2) false None in
    OS.Console.log(Printf.sprintf "Found %d %s from %s-%s" (List.length entries) ti.tname key key2); 

    let titletext = if ti.tparent = None then "Dbforms list "^ti.tname 
      else "Dbforms list "^ti.tname^" of "^key in
    let title = <:html< <h1>$str:titletext$</h1> >> in
    let path = Uri.path (CL.Request.uri req) in
    let ppath = 
      let pix = String.rindex path '/' in
      let pix2 = String.rindex_from path (pix-1) '/' in
      String.sub path 0 pix2 in
    let back_uri = if ppath="/db" then "/db/" 
      else Uri.to_string(Uri.make ~path:ppath ~query:["action",["view"]] ()) in
    let back_text = if ppath="/db" then "Back to index" else "Back to parent" in
    let back_link = html_of_link { text=back_text; href=back_uri } in
    let table h b = <:html< <table><thead>$h$</thead><tbody>$list:b$</tbody></table> >> in
    let tr x = <:html< <tr>$list:x$</tr> >> in
    let th x = <:html< <th>$x$</th> >> in
    let td x = <:html< <td>$x$</td> >> in
    let cols (pn,_,_) = th (html_of_string pn) in
    let headings = match ti.ttype with 
    | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> tr((th (html_of_string "Actions")) :: List.map cols ps)
    | _ -> OS.Console.log("dyntype unsupported in list ("^ti.tname^" = "^(Dyntype.Type.to_string ti.ttype)^")"); [] in
    let row (key,sval) = 
      let jval = Json.of_string sval in
      let cols (tpn,_,_) =       
        let col value (vpn,pval) = if tpn=vpn then
            match pval with
            | Json.String s -> html_of_string s 
            | _ -> html_of_string "Unsupported"
          else value in
        let v = match jval with 
        | Json.Object ps -> List.fold_left col (html_of_string "Undefined") ps
        | _ -> begin OS.Console.log("Json type unsupported in list: "^sval); 
            html_of_string "Unsupported" 
          end in
        td( v )
      in
      let id = 
        let keylen = String.length key in
        try 
          let cix = String.rindex key ',' in
          String.sub key (cix+1) (keylen-cix-1)
        with 
          Not_found -> 
            let cix = String.index key ':' in
            String.sub key (cix+1) (keylen-cix-1)
      in
      let viewuri = Uri.to_string(Uri.make ~path:((Uri.path (CL.Request.uri req))^id) ~query:["action",["view"]] ()) in
      let actions = button_form "View" viewuri in
      match ti.ttype with 
      | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> tr((td actions) :: List.map cols ps)
      | _ -> [] 
    in
    let rows = List.map row entries in   
    let add_action = Uri.to_string(Uri.make ~path:(Uri.path (CL.Request.uri req)) ~query:["action",["addform"]] ()) in
    let add = button_form "Add..." add_action in
    let html = List.flatten [title; back_link; table headings rows; add] in
    respond_html titletext html

let return_addform req path_elems typeinfos =
  (* check type/path *)
    let ti = get_typeinfo typeinfos path_elems in
    (* TODO check type parent(s) *)
    let tname = html_of_string ti.tname in
    let title = <:html< <h1>Dbforms Add $tname$</h1> >> in
    let back_link = html_of_link { text="Back to index"; href="/db/" } in
    let table h b = <:html< <table><thead>$h$</thead><tbody>$list:b$</tbody></table> >> in
    let tr x = <:html< <tr>$list:x$</tr> >> in
    let th x = <:html< <th>$x$</th> >> in
    let td x = <:html< <td>$x$</td> >> in
    let headings = tr ([th (html_of_string "Property"); 
      th (html_of_string "Type"); 
      th (html_of_string "Value")]) in
    let row (pn,_,pdyntype) = begin
      let typename = match pdyntype with 
      | Dyntype.Type.String -> html_of_string "string"
      | _ -> html_of_string ("Unsupported: "^(Dyntype.Type.to_string pdyntype)) in
      let input = match pdyntype with
      | Dyntype.Type.String -> begin <:html<<input type="text" name="$str:pn$"></input>&>> end
      | _ -> html_of_string "Unsupported" in
      tr [td (html_of_string pn); 
          td typename;
          td input] end in
    let rows = match ti.ttype with 
    | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> List.map row ps
    | _ -> OS.Console.log("dyntype unsupported in add ("^ti.tname^" = "^(Dyntype.Type.to_string ti.ttype)^")"); [] in
    let submit = <:html<<input type="submit" value="Add"></input>&>> in
    let action = Uri.to_string (Uri.make ~path:(Uri.path (CL.Request.uri req)) ~query:["action",["add"]] ()) in
    let form xs = <:html<<form method="POST" action=$str:action$>$list:xs$</form>&>> in
    let html = List.flatten [title; back_link; form [table headings rows; submit]] in
    respond_html ("Dbforms Add "^ti.tname) html

exception Unimplemented of string

(* encode id to appear in store key, avoiding :, / or , *)
let encode_id id = 
  let len = String.length id in
  let b = Buffer.create len in
  for i=0 to len-1 do
    let c = String.get id i in
    match c with 
    | ',' | '/' | ':' | '%' -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))
    | c -> Buffer.add_char b c 
  done;
  Buffer.contents b

let return_add ?body req path_elems typeinfos =
    let ti = get_typeinfo typeinfos path_elems in
    lwt body = Cohttp_lwt_body.string_of_body body in 
    if not(CL.Request.is_form req) then 
      CL.Server.respond_error ~status:`Bad_request ~body:"Dbforms add is not form url-encoded" ()
    else begin
      let formvalues = Uri.query_of_encoded body in 
      (* build a json value *)
      let propval (pn,_,pdyntype) = begin
        let sval = Login.get_one_value formvalues pn in
        match pdyntype with 
        | Dyntype.Type.String -> (pn,Json.String sval)
        | _ -> raise(Unimplemented ("property dyntype: "^(Dyntype.Type.to_string pdyntype)))
      end in
      let propvals = match ti.ttype with 
      | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> List.map propval ps
      | _ -> raise(Unimplemented ("add dyntype ("^ti.tname^") "^(Dyntype.Type.to_string ti.ttype)))	 in
      let jval = Json.Object propvals in
      OS.Console.log("add "^ti.tname^" "^(Json.to_string jval));
      (* make persist key from type & id *)
      let key = get_key path_elems in 
      OS.Console.log("add key prefix = "^key);
      (* get/check id *)
      let id = match ti.pktype with
      | `User_defined -> begin
          let pk = List.fold_left (fun pk (pn,pv) -> 
            if ti.pkname=pn then begin match pv with
              | Json.String s -> s
              | _ -> raise (Unimplemented ("primary key (type) "^(Json.to_string pv)))
            end else pk) "" propvals in
          let id = encode_id pk in
          id
        end in
      let key = key^id in 
      (* check doesn't exist & add *)
      lwt db = Persist.get None in
      (* not convinced that lwt exceptions are caught properly in BS with_tx *)
      let doadd tx = 
        lwt chk = DB.get tx key in
        match chk with 
        | B.NOK _ -> (* ok *)
          lwt () = DB.set tx key (Json.to_string jval) in
          Lwt.return (B.OK "")
        | B.OK _ ->  OS.Console.log("add for existing "^ti.tname^" "^key); 
          Lwt.return (B.NOK "")
      in
      lwt res = DB.with_tx db doadd in 
      match res with
      | B.OK _ -> OS.Console.log("added "^ti.tname^" "^key);
        let path = Uri.path (CL.Request.uri req) in
	let redir = Uri.make ~path:(path^id) ~query:["action",["view"];"message",["Added"]] () in
        CL.Server.respond_redirect ~uri:redir ()
      | B.NOK _ -> 
        let path = Uri.path (CL.Request.uri req) in
	let redir = Uri.make ~path ~query:["action",["addform"];"message",["Already exists"]] () in
        CL.Server.respond_redirect ~uri:redir ()
        (*CL.Server.respond_error ~status:`Bad_request ~body:(ti.tname^" "^key^" already exists") ()*) 
    end

let return_view req path_elems typeinfos =
    let uri = CL.Request.uri req in
    let path = Uri.path uri in
    OS.Console.log("view "^path);
    let ti = get_typeinfo typeinfos path_elems in
    OS.Console.log("- type "^ti.tname);
    let key = get_key path_elems in
    OS.Console.log("- key "^key);
    lwt db = Persist.get None in 
    lwt res = DB.get_latest db key in
    let sval = match res with 
     | B.NOK _ -> raise (HttpError(`Not_found,("Key = "^key)))
     | B.OK sval -> sval in
    let jval = Json.of_string sval in
    (* table *)
    let tname = html_of_string ti.tname in
    let title = <:html< <h1>Dbforms View $tname$ $str:key$</h1> >> in
    let list_uri = 
      let ppath = 
        let pix = String.rindex path '/' in
        String.sub path 0 (pix+1) in
      Uri.to_string(Uri.make ~path:ppath ~query:["action",["list"]] ()) in
    let back_link = html_of_link { text="Back to list"; href=list_uri } in
    let table h b = <:html< <table><thead>$h$</thead><tbody>$list:b$</tbody></table> >> in
    let tr x = <:html< <tr>$list:x$</tr> >> in
    let th x = <:html< <th>$x$</th> >> in
    let td x = <:html< <td>$x$</td> >> in
    let headings = tr ([th (html_of_string "Property"); 
      th (html_of_string "Value")]) in
    let row (pn,pval) = begin
      let value = match pval with
      | Json.String s -> html_of_string s 
      | _ -> html_of_string "Unsupported" in
      tr [td (html_of_string pn); 
          td value] end in
    let rows = match jval with 
    | Json.Object ps -> List.map row ps
    | _ -> OS.Console.log("Json type unsupported in read ("^ti.tname^" = "^sval^")"); [] in
    let edit_action = Uri.to_string(Uri.make ~path ~query:["action",["editform"]] ()) in
    let edit = button_form "Edit..." edit_action in
    let delete_action = Uri.to_string(Uri.make ~path ~query:["action",["delete"]] ()) in
    let delete = button_form "Delete" delete_action in
    (* child types *)
    let chheadings = tr [th (html_of_string "Child type"); th (html_of_string "Actions")] in
    let chrow cti = begin if cti.tparent=Some ti.tname then 
        let list_action = Uri.to_string(Uri.make ~path:(path^"/"^cti.tname^"/") ~query:["action",["list"]] ()) in
        let listh = button_form "List" list_action in
        let add_action = Uri.to_string(Uri.make ~path:(path^"/"^cti.tname^"/") ~query:["action",["addform"]] ()) in
        let add = button_form "Add..." add_action in
        let actions = <:html< <td>$listh$ $add$</td> >> in
	<:html<<tr><td>$str:cti.tname$</td> $actions$</tr>&>>
      else html_of_string "" end in
    let chrows = List.map chrow typeinfos in
    let html = List.flatten [title; back_link; table headings rows; edit; delete; table chheadings chrows] in
    respond_html ("Dbforms View "^ti.tname^" "^key) html

let return_editform req path_elems typeinfos =
    let ti = get_typeinfo typeinfos path_elems in
    let key = get_key path_elems in
    lwt db = Persist.get None in 
    lwt res = DB.get_latest db key in
    let sval = match res with 
     | B.NOK _ -> raise(HttpError(`Not_found,("edit key "^key)))
     | B.OK sval -> sval in
    let jval = Json.of_string sval in
    (* table *)
    let tname = html_of_string ti.tname in
    let title = <:html< <h1>Dbforms Edit $tname$ $str:key$</h1> >> in
    let back_link = html_of_link { text="Back to index"; href="/db/" } in
    let table h b = <:html< <table><thead>$h$</thead><tbody>$list:b$</tbody></table> >> in
    let tr x = <:html< <tr>$list:x$</tr> >> in
    let th x = <:html< <th>$x$</th> >> in
    let td x = <:html< <td>$x$</td> >> in
    let headings = tr ([th (html_of_string "Property"); 
      th (html_of_string "Value")]) in
    let row (pn,pval) = begin
      let input = match pval with
      | Json.String s -> <:html<<input type="text" name="$str:pn$" value="$str:s$"></input>&>> 
      | _ -> html_of_string "Unsupported" in
      tr [td (html_of_string pn); 
          td input] end in
    let rows = match jval with 
    | Json.Object ps -> List.map row ps
    | _ -> OS.Console.log("Json type unsupported in edit ("^ti.tname^" = "^sval^")"); [] in
    let submit = <:html<<input type="submit" value="Update"></input>&>> in
    let action = Uri.to_string (Uri.make ~path:(Uri.path (CL.Request.uri req)) ~query:["action",["edit"]] ()) in
    let form xs = <:html<<form method="POST" action="$str:action$">$list:xs$</form>&>> in
    let html = List.flatten [title; back_link; form [table headings rows; submit]] in
    respond_html ("Dbforms Edit "^ti.tname^" "^key) html


let return_edit ?body req path_elems typeinfos =
    let ti = get_typeinfo typeinfos path_elems in
    let key = get_key path_elems in
    lwt body = Cohttp_lwt_body.string_of_body body in 
    if not(CL.Request.is_form req) then 
      CL.Server.respond_error ~status:`Bad_request ~body:"Dbforms edit is not form url-encoded" ()
    else begin
      let formvalues = Uri.query_of_encoded body in 
      (* build a json value *)
      let propval (pn,_,pdyntype) = begin
        let sval = Login.get_one_value formvalues pn in
        match pdyntype with 
        | Dyntype.Type.String -> (pn,Json.String sval)
        | _ -> raise(Unimplemented ("property dyntype: "^(Dyntype.Type.to_string pdyntype)))
      end in
      let propvals = match ti.ttype with 
      | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> List.map propval ps
      | _ -> raise(Unimplemented ("add dyntype ("^ti.tname^") "^(Dyntype.Type.to_string ti.ttype))) in
      let jval = Json.Object propvals in
      OS.Console.log("update "^key^" "^(Json.to_string jval));
      lwt db = Persist.get None in
      let doupdate tx = 
        lwt chk = DB.get tx key in
        match chk with 
        | B.OK _ -> (* ok *)
          lwt () = DB.set tx key (Json.to_string jval) in
          Lwt.return (B.OK "")
        | B.NOK _ ->  OS.Console.log("update for non-existing "^ti.tname^" "^key); 
          Lwt.return (B.NOK "")
      in
      lwt res = DB.with_tx db doupdate in 
      match res with
      | B.OK _ -> OS.Console.log("updated "^ti.tname^" "^key);
        let path = Uri.path (CL.Request.uri req) in
	let redir = Uri.make ~path ~query:["action",["view"];"message",["Updated"]] () in
        CL.Server.respond_redirect ~uri:redir ()
      | B.NOK _ -> 
        Lwt.fail Not_found
    end

let return_delete req path_elems typeinfos =
    let key = get_key path_elems in
    lwt db = Persist.get None in 
    let dodelete tx = begin
      lwt res = DB.delete tx key in
      (* delete children/descendents*)
      (* TODO *)
      Lwt.return res
    end in
    lwt res = DB.with_tx db dodelete in
    let ppath = 
      let path = Uri.path (CL.Request.uri req) in
      let pix = String.rindex path '/' in
      String.sub path 0 (pix+1) in
    match res with
    | B.OK _ -> OS.Console.log("Deleted "^key);
      let list_uri = Uri.make ~path:ppath ~query:["action",["list"];"message",["Deleted"]] () in
      CL.Server.respond_redirect ~uri:list_uri ()
    | B.NOK _ -> raise (HttpError(`Not_found,("delete key "^key)))

let return_dump typeinfos =
  lwt db = Persist.get None in
  lwt entries = DB.range_entries_latest db None true None false None in
  let b = Buffer.create 10 in 
  Buffer.add_string b "[";
  let write (k,v) = 
    if Buffer.length b > 1 then 
      Buffer.add_string b ", "
    else ();
    Buffer.add_string b "[";
    Json.to_buffer (Json.(String k)) b;
    Buffer.add_string b ",";
    Buffer.add_string b v;
    Buffer.add_string b "]"
  in let _ = List.map write entries in
  Buffer.add_string b "]";
  let body = Buffer.contents b in
  let headers = C.Header.init_with "Content-Type" "application/json" in
  CL.Server.respond_string ~status:`OK ~headers ~body () 

let return_restoreform req = 
  let form = <:html<<form method="POST" action="do.restore" enctype="multipart/form-data">
    <input type="file" name="file"></input>
    <input type="submit" value="Restore DB"></input></form>&>> in
  let heading = <:html<<h1>Restore database from dump</h1>&>> in
  respond_html "Restore database from dump" (List.flatten [heading; form])

let return_restore ?body req =
    lwt (parts,metas) = read_multipart_body ?body (CL.Request.headers req) in
    (*List.map (fun (pn,(pv::_)) -> OS.Console.log("multipart part "^pn^" = "^pv)) parts;*)
    let file = Login.get_one_value parts "file" in
    let jval = Json.of_string file in
    lwt db = Persist.get None in
    let ok = ref 0 and
      err = ref 0 in
    let output = Buffer.create 10 in
    let restore_fn tx = 
      match jval with 
      | Json.Array es -> begin
          let rec rset es = match es with 
          | (Json.Array ((Json.String k) :: v :: [])) :: es -> 
            lwt _ = DB.set tx k (Json.to_string v) in 
	    ok := !ok + 1;
            rset es
          | v :: es ->
            err := !err + 1;
            Buffer.add_string output "restore: ignore unsupported entry  ";
 	    Buffer.add_string output (Json.to_string v);
            Buffer.add_string output "\n";
            rset es
          | [] -> Lwt.return (B.OK "")
          in rset es
        end
      | v -> 
        err := !err + 1;
        Buffer.add_string output "restore: unsupported file type\n";
        Buffer.add_string output file;
        Buffer.add_string output "\n";
        Lwt.return (B.NOK "")
    in lwt _ = DB.with_tx db restore_fn in
    let log = Buffer.contents output in
    let msg = Printf.sprintf "Restore: OK %d; Error %d" !ok !err in
    OS.Console.log(msg);
    respond_html "Restore" <:html<<h1>Restore</h1><p>$str:msg$</p>$str:log$>>

(* handle HTTP request *)
let dispatch ?body req path_elems typeinfos = 
try_lwt
  match path_elems with
  | [""] | [] -> return_index req typeinfos
  | ["dump.js"] -> return_dump typeinfos
  | ["restore.html"] -> return_restoreform req
  | ["do.restore"] -> return_restore ?body req
  | _ ->
    (* check action *)
    let uri = CL.Request.uri req in
    let query = Uri.query uri in
    let action = Login.get_one_value query "action" in
    let meth = CL.Request.meth req in
    match action,meth with 
    | "list",_ -> 
        return_list req path_elems typeinfos
    | "addform",_ ->
      return_addform req path_elems typeinfos
    | "add",`POST -> 
      return_add ?body req path_elems typeinfos
    | "add",_ -> 
      raise (HttpError(`Method_not_allowed,"Dbforms action add requires POST"))
    | "view",_ -> 
      return_view req path_elems typeinfos
    | "editform",_ ->
      return_editform req path_elems typeinfos
    | "delete",`POST -> 
      return_delete req path_elems typeinfos
    | "delete",_ -> 
      raise (HttpError(`Method_not_allowed,"Dbforms action delete requires POST"))
    | "edit",`POST -> 
      return_edit ?body req path_elems typeinfos
    | "edit",_ -> 
      raise (HttpError(`Method_not_allowed,"Dbforms action edit requires POST"))
    | "",_ ->
      raise (HttpError(`Bad_request,"Dbforms action not specified"))
    | x,_ ->
      raise (HttpError(`Bad_request,("Dbforms action unknown: "^x)))
with 
    Invalid_content_type m -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Invalid content type: "^m) ()
  | Json.Runtime_error (m,v) ->
    CL.Server.respond_error ~status:`Bad_request ~body:("Problem with uploaded Json data: "^m) ()
  | HttpError (status,body) ->
    CL.Server.respond_error ~status ~body ()
  | Unimplemented x -> 
    CL.Server.respond_error ~status:`Not_implemented ~body:("Sorry, not implemented: "^x) ()
  | e -> 
    CL.Server.respond_error ~status:`Internal_server_error ~body:"Server error" ()
    
