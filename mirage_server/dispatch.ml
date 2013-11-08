(*
 * by Chris Greenhalgh (chris.greenhalgh@nottingham.ac.uk)
 * Skeleton based on mirage-www
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

open Lwt
open Printf

open Mime

module CL = Cohttp_lwt_mirage
module C = Cohttp

(* dbforms typeinfo *)
let dbtypeinfos = Dbcommon.([ 
  { tname="user"; ttype=Model.type_of_user; tparent=None; pkname="email"; pktype=`User_defined };
  { tname="group"; ttype=Model.type_of_group; tparent=Some "user"; pkname="gid"; pktype=`User_defined }
])

let _ = (* allow login against local accounts *)
  Login.add_authenticator Model.authenticator

module Resp = struct

  (* dynamic response *)
  let dyn ?(headers=[]) req body =
    printf "Dispatch: dynamic URL %s\n%!" (Uri.path (CL.Request.uri req));
    lwt body = body in
    let status = `OK in
    let headers = C.Header.of_list headers in
    CL.Server.respond_string ~headers ~status ~body ()

  (*let dyn_xhtml = dyn ~headers:Pages.content_type_xhtml*)

  (* dispatch non-file URLs *)
  let dispatch ?body req =
    function
      | [] | [""] -> CL.Server.respond_redirect ~uri:(Uri.make ~path:"/index.html" ()) ()
      | ["do.login"] -> Login.handle_login ?body req "/home.html" "/login.html"
      | ["do.logout"] -> Login.handle_logout ?body req "/login.html"
      | "db" :: path_elem -> 
        let user = Login.get_authenticated_user req in
        if user = Some "admin" then
          Dbforms.dispatch ?body req path_elem dbtypeinfos
        else if user = None then
          Login.respond_user_not_authorized req
	else
          Login.respond_user_forbidden req
      | "api" :: path_elem ->
        lwt user = Login.get_authenticated_api_user req in
        begin match user with 
          | None -> Login.respond_api_not_authorized req
          | Some user ->
            (* NB prefix user - this is all the security for now *)
            Dbrest.dispatch ?body req ("user" :: user :: path_elem) dbtypeinfos user
        end
      | x -> OS.Console.log("Not found: "^(List.fold_left (fun ss s -> ss^"/"^s) "" x));
        CL.Server.respond_not_found ~uri:(CL.Request.uri req) ()
end

let rec remove_empty_tail = function
  | [] | [""] -> []
  | hd::tl -> hd :: remove_empty_tail tl

(* main callback function; returns (Response.t * Cohttp_lwt_body.t) pair *)
let t conn_id ?body req =
  let path = Uri.path (CL.Request.uri req) in
  OS.Console.log("request: "^path);
  let path_elem =
    match (Re_str.split_delim (Re_str.regexp_string "/") path) with
    | ""::rest -> rest
    | x -> x 
  in
  lwt static =
    eprintf "finding the static kv_ro block device\n";
    OS.Devices.find_kv_ro "static" >>=
    function
    | None   -> Printf.printf "fatal error, static kv_ro not found\n%!"; exit 1
    | Some x -> return x in

  (* determine if it is static or dynamic content *)
  match_lwt static#read path with
  |Some body ->
     lwt body = Util.string_of_stream body in
     let headers =
       try 
         C.Header.init_with header_content_type (guess_mime_type path)
       with Not_found -> C.Header.init ()
     in
     CL.Server.respond_string ~headers ~status:`OK ~body ()
  |None -> begin
      match path_elem with 
      | "app" :: path_elem -> begin 
          (* app stuff... *)
          let apppath = String.sub path 4 ((String.length path) - 4) in
          lwt app = OS.Devices.find_kv_ro "app" >>=
            function
            | None   -> Printf.printf "fatal error, app kv_ro not found\n%!"; exit 1
            | Some x -> return x in
          match_lwt app#read apppath with
          | Some body ->
            lwt body = Util.string_of_stream body in
            let headers =
              try 
                C.Header.init_with header_content_type (guess_mime_type path)
              with Not_found -> C.Header.init ()
            in
              CL.Server.respond_string ~headers ~status:`OK ~body ()
          | None -> 
            CL.Server.respond_not_found ~uri:(CL.Request.uri req) ()
        end
      | _ ->
        Resp.dispatch ?body req path_elem
  end

