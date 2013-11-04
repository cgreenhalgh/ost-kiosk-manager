(* skeleton based on mirage-www *)
open Lwt
open Printf

module CL = Cohttp_lwt_mirage
module C = Cohttp

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
      | ["do.login"] -> Login.handle ?body req
      | x -> OS.Console.log("Not found: "^(List.fold_left (fun ss s -> ss^"/"^s) "" x));
        CL.Server.respond_not_found ~uri:(CL.Request.uri req) ()
end

let header_content_type = "Content-Type"

let content_type_html = "text/html"
(*["content-type","application/atom+xml; charset=UTF-8"]*)

let ext_regexp = Re_str.regexp "\\.\\([^./]+\\)$"

let get_file_ext filename = try
    let _ = Re_str.search_forward ext_regexp filename 0 in
    Re_str.matched_group 1 filename 
  with Not_found -> ""

module StringMap = Map.Make(String)

let file_ext_map =
  let (=>) f g = g f in
  let map = StringMap.empty =>
  StringMap.add "html" content_type_html in
  map

(* may throw Not_found *)
let rec guess_mime_type path = let ext = get_file_ext path in
    OS.Console.log("request ext = "^ext^" - path = "^path);
    StringMap.find ext file_ext_map

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
  |None ->
     Resp.dispatch ?body req path_elem
