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
  let dispatch req =
    function
      | x -> CL.Server.respond_not_found ~uri:(CL.Request.uri req) ()
end



let rec remove_empty_tail = function
  | [] | [""] -> []
  | hd::tl -> hd :: remove_empty_tail tl

(* main callback function *)
let t conn_id ?body req =
  let path = Uri.path (CL.Request.uri req) in
  let path_elem =
    remove_empty_tail (Re_str.split_delim (Re_str.regexp_string "/") path)
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
     CL.Server.respond_string ~status:`OK ~body ()
  |None ->
     Resp.dispatch req path_elem

