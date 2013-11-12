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

(* Kiosk atom generation *)

open Model
open Persist
open Dbcommon
open Lwt
open Mime

module DB = Persist.DB
module B = Baardskeerder
open C.Code
open Cow
module CL = Cohttp_lwt_mirage
module C = Cohttp

let get_json db key =
  lwt usr = DB.get_latest db key in
  lwt us = match usr with
    | B.NOK _ -> fail(HttpError(`Not_found,("Key "^key)))
    | B.OK us -> return us in
  return (Json.of_string us)

let get_list db key of_json_fn =
  let key2 = String.copy key in
  let kmax = (String.length key)-1 in
  String.set key2 kmax (Char.chr (Char.code (String.get key2 kmax) + 1));
  lwt entries = DB.range_entries_latest db (Some key) true (Some key2) false None in
  let fn (k,v) =
    let jv = Json.of_string v in
    of_json_fn jv in
  return (List.map fn entries)

let get_user db uid = 
  lwt jv = get_json db ("/user:"^uid) in
  return (Model.user_of_json jv) 

let get_website db uid wid =
  let key = "/user/website:"^uid^","^wid in
  lwt jv = get_json db key in
  return (Model.website_of_json jv)

let get_group db uid gid =
  let key = "/user/group:"^uid^","^gid in
  lwt jv = get_json db key in
  return (Model.group_of_json jv)

let get_items db uid gid =
  let key = "/user/group/item:"^uid^","^gid^"," in
  get_list db key Model.item_of_json

let split s = 
  Re_str.split_delim (Re_str.regexp "[ \t]") s

let entry_of_item item =
  let visibility = if not item.ivisible then 
      <:xml< <category scheme="visibility" term="hidden"/> >>
    else [] in
  let requires = 
    let ds = split item.irequires in
    let fn d = <:xml< <category scheme="requires-device" term="$str:d$"></category> >> in
    List.flatten (List.map fn ds) in
  let supports = 
    let ss = split item.isupports in
    (* human-readable label? *)
    let fn s = <:xml< <category scheme="supports-mime-type" term="$str:s$"></category> >> in
    List.flatten (List.map fn ss) in
  (* we need icon mime type *)
  let iconmime = 
    try 
      guess_mime_type item.iiconurl
    with
      _ -> "image/unknown" in
  (* Todo: summary type, e.g. html handling *) 
  (* TODO: enclosure length *)

  <:xml< <entry>
     $visibility$
     $requires$
     $supports$
     <id>$str:item.iatomid$</id>
     <content>$str:item.isummary$</content>
     <title>$str:item.ititle$</title>
     <updated>$str:item.idate$</updated>
     <link rel="alternate" type="$str:iconmime$" href="$str:item.iiconurl$"></link>
     <link rel="enclosure" type="$str:item.ifilemime$" href="$str:item.ifileurl$"></link>
   </entry> >>

let dispatch uid wid =
try_lwt
  check_key_part uid;
  check_key_part wid;
  (* get website *)
  lwt db = Persist.get None in
  lwt user = get_user db uid in
  (* not encoded! *)
  let email = Model.(user.email) in
  OS.Console.log("Got user "^email);
  lwt website = get_website db email wid in
  OS.Console.log("Got website "^Model.(website.wid)^": "^Model.(website.wtitle));
  let watomid = Model.(website.watomid) in
  (* get groups and items *)
  let gids = Re_str.split_delim (Re_str.regexp "[ \t]") Model.(website.wgids) in
  lwt groups = 
    let rec rfn gps gids = match gids with
      | [] -> return gps
      | gid :: gids ->  
        lwt gp = get_group db uid gid in 
        rfn (gp :: gps) gids in
    rfn [] gids in
  OS.Console.log(Printf.sprintf "got %d groups" (List.length groups));
  lwt items = 
    let rec rfn items gids = match gids with
      | [] -> return items
      | gid :: gids ->  
        lwt is = get_items db uid gid in 
        rfn (is :: items) gids in
    rfn [] gids in
  let items = List.flatten items in
  OS.Console.log(Printf.sprintf "got %d items" (List.length items));
  (* check updated *)
  let updated = 
    let gfn upd group = 
      if upd = "" || group.gdate < upd then group.gdate else upd in
    let gupd = List.fold_left gfn "" groups in
    let ifn upd item = 
      if upd = "" || item.idate < upd then item.idate else upd in
    List.fold_left ifn gupd items in
  (* feed metadata *)
  let author = <:xml< <author><name>$str:email$</name><email>$str:email$</email></author> >> in
  (* TODO fix feed url 
  let feedurl = website.wgeturl in
    <link rel="self" type="application/atom+xml" href="$str:feedurl$"></link> *)
  let meta = <:xml< 
    $author$
    <id>$str:watomid$</id>
    <title>$str:Model.(website.wtitle)$</title> 
    <updated>$str:updated$</updated> 
   >> in
  (* items *)
  let entries = List.flatten (List.map entry_of_item items) in
  let feed = <:xml<<feed xmlns="http://www.w3.org/2005/Atom"> $meta$ $entries$ </feed> >> in
  let headers = C.Header.init_with header_content_type content_type_atom in
  let body = Xml.to_string ~decl:true feed in
  CL.Server.respond_string ~headers ~status:`OK ~body () 

with HttpError (status,body) ->
    CL.Server.respond_error ~status ~body ()
  | e -> 
    OS.Console.log("Internal server error in atomgen: "^(Printexc.to_string e));
    CL.Server.respond_error ~status:`Internal_server_error ~body:"Server error" ()

