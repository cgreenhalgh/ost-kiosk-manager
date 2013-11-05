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

type pktype = [ `User_defined ] (* TODO | `Auto_increment *)

type typeinfo = {
  tname : string;
  ttype : Dyntype.Type.t;
  tparent : string option;
  pkname : string;
  pktype : pktype
}

type typeinfos = typeinfo list

let header_content_type = "Content-Type"
let content_type_html = "text/html"

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
    let list_link_html = html_of_link (list_link ti) in
    let add_link_html = html_of_link (add_link ti) in
    tr ([td (html_of_string ti.tname); 
      td (html_of_string (pname ti)); 
      <:html< <td>$list_link_html$ $add_link_html$</td> >>]) in
  let rows = List.map row typeinfos in
  let html = List.flatten [title; table headings rows] in
  respond_html "Dbforms index" html

(* get type for path T1/id1/T2/id2/...; raises Not_found *)
let rec get_typeinfo typeinfos path_elems = match path_elems with
 | tn :: _ :: [] -> 
   let rec rget tis = begin match tis with 
     | [] -> raise Not_found
     | ti :: rest -> if (ti.tname=tn) then ti else rget rest
   end in
   rget typeinfos
 | rn :: _ :: rest -> get_typeinfo typeinfos rest
 | _ -> raise Not_found

let get_key path_elems =
  let keybuf = Buffer.create 100 in
  let keyidbuf = Buffer.create 100 in
  let rec build_key pels = match pels with
    | tn :: "" :: [] -> 
      Buffer.add_string keybuf "/";
      Buffer.add_string keybuf tn;
      if Buffer.length keyidbuf > 0 then Buffer.add_string keyidbuf ","
    | ptn :: pk :: rest -> 
      Buffer.add_string keybuf "/";
      Buffer.add_string keybuf ptn;
      if Buffer.length keyidbuf > 0 then Buffer.add_string keyidbuf ",";
        (* path element should be encoded already *)
        for i=0 to (String.length pk)-1 do
          let c = String.get pk i in 
          if c=',' || c='/' || c=':' || c='%' then raise Not_found
        done;
      Buffer.add_string keyidbuf pk
    | _ -> raise Not_found
  in build_key path_elems;
  Buffer.add_string keybuf ":";
  Buffer.add_buffer keybuf keyidbuf;
  Buffer.contents keybuf

let button_form label action = 
  <:html<<form method="POST" action="$str:action$"><input type="submit" value="$str:label$"></input></form>&>>

let return_list req path_elems typeinfos =
  (* check type/path *)
  try
    let ti = get_typeinfo typeinfos path_elems in
    (* TODO check type parent(s) *)
    let key = get_key path_elems in 
    lwt db = Persist.get None in
    let key2 = String.copy key in
    let kmax = (String.length key)-1 in
    String.set key2 kmax (Char.chr (Char.code (String.get key2 kmax) + 1));
    lwt entries = DB.range_entries_latest db (Some key) true (Some key2) false None in
    OS.Console.log(Printf.sprintf "Found %d %s from %s-%s" (List.length entries) ti.tname key key2); 

    let tname = html_of_string ti.tname in
    let title = <:html< <h1>Dbforms List $tname$</h1> >> in
    let back_link = html_of_link { text="Back to index"; href="/db/" } in
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
    respond_html ("Dbforms List "^ti.tname) html
  with Not_found -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms type") ()    

let return_addform req path_elems typeinfos =
  (* check type/path *)
  try
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
  with Not_found -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms type") ()    

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
  try_lwt
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
  with Not_found -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms type") ()    
  | Unimplemented f -> 
    CL.Server.respond_error ~status:`Not_implemented ~body:("Dbforms feature: "^f) () 

let return_view req path_elems typeinfos =
  try
    let ti = get_typeinfo typeinfos path_elems in
    let key = get_key path_elems in
    lwt db = Persist.get None in 
    lwt res = DB.get_latest db key in
    let sval = match res with 
     | B.NOK _ -> raise Not_found
     | B.OK sval -> sval in
    let jval = Json.of_string sval in
    (* table *)
    let tname = html_of_string ti.tname in
    let title = <:html< <h1>Dbforms View $tname$ $str:key$</h1> >> in
    let back_link = html_of_link { text="Back to index"; href="/db/" } in
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
    let edit_action = Uri.to_string(Uri.make ~path:(Uri.path (CL.Request.uri req)) ~query:["action",["editform"]] ()) in
    let edit = button_form "Edit..." edit_action in
    let delete_action = Uri.to_string(Uri.make ~path:(Uri.path (CL.Request.uri req)) ~query:["action",["delete"]] ()) in
    let delete = button_form "Delete" delete_action in
    let html = List.flatten [title; back_link; table headings rows; edit; delete] in
    respond_html ("Dbforms View "^ti.tname^" "^key) html
  with Not_found -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms type") ()    

let return_editform req path_elems typeinfos =
  try
    let ti = get_typeinfo typeinfos path_elems in
    let key = get_key path_elems in
    lwt db = Persist.get None in 
    lwt res = DB.get_latest db key in
    let sval = match res with 
     | B.NOK _ -> raise Not_found
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
  with Not_found -> 
    CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms type") ()    

(* handle HTTP request *)
let dispatch ?body req path_elems typeinfos = match path_elems with
  | [""] | [] -> return_index req typeinfos
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
      CL.Server.respond_error ~status:`Method_not_allowed ~body:"Dbforms action add requires POST" ()
    | "view",_ -> 
      return_view req path_elems typeinfos
    | "editform",_ ->
      return_editform req path_elems typeinfos
    | "delete",`POST -> 
      (* TODO *)
      CL.Server.respond_error ~status:`Not_implemented ~body:"Dbforms action delete" ()
    | "delete",_ -> 
      CL.Server.respond_error ~status:`Method_not_allowed ~body:"Dbforms action delete requires POST" ()
    | "edit",`POST -> 
      (* TODO *)
      CL.Server.respond_error ~status:`Not_implemented ~body:"Dbforms action edit" ()
    | "edit",_ -> 
      CL.Server.respond_error ~status:`Method_not_allowed ~body:"Dbforms action edit requires POST" ()
    | "",_ ->
      CL.Server.respond_error ~status:`Bad_request ~body:"Dbforms action not specified" ()
    | x,_ ->
      CL.Server.respond_error ~status:`Bad_request ~body:("Unknown Dbforms action "^x) ()
  
