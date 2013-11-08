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

(* reflection (dyntype & Cow.Json)-based RESTful JSON interface to DB *)

module CL = Cohttp_lwt_mirage
module C = Cohttp
open C.Code
open Cow
open Cow.Html
module DB = Persist.DB
module B = Baardskeerder
open Mime
open Dbcommon

let do_list username ti key db =
  let key2 = String.copy key in
  let kmax = (String.length key)-1 in
  String.set key2 kmax (Char.chr (Char.code (String.get key2 kmax) + 1));
  (* Note: committing a read-only transaction is creates an error in BS *)
  lwt entries = DB.range_entries_latest db (Some key) true (Some key2) false None in
  let b = Buffer.create 10 in
  Buffer.add_string b "[";
  (* TODO filter, etc. *)
  let fn (k,v) = begin
    if Buffer.length b >1 then Buffer.add_string b ",";
    Buffer.add_string b v
  end in
  let _ = List.map fn entries in
  Buffer.add_string b "]";
  Lwt.return (B.OK (Buffer.contents b))

let do_read username ti key db =
  (* Note: committing a read-only transaction is creates an error in BS *)
  lwt res = DB.get_latest db key in
  lwt _ = match res with B.NOK _ -> Lwt.fail(HttpError(`Not_found,("Not found: "^key))) 
  | _ -> Lwt.return () in
  (* TODO filter, etc. *)
  Lwt.return res

(* match up Json object properties with Dyntypes *)
let match_json_with_dyntype (jval:Json.t) (dt:Dyntype.Type.t) : (string * (Json.t option) * Dyntype.Type.t * [`RW|`RO]) list =
  let jps = match jval with
    | Json.Object jps -> jps
    | x -> raise (HttpError(`Bad_request,("data was not a Json object: "^(Json.to_string x)))) in
  let dtps = match dt with
    | Dyntype.Type.Ext (_,Dyntype.Type.Dict (_, ps)) -> ps
    | x -> raise(HttpError(`Not_implemented, ("unsupported dyntype "^(Dyntype.Type.to_string x)))) in
  let (vs,jps) = 
    let rec rdt dtps vs jps = match dtps with
      | [] -> (vs,jps)
      | dtp :: dtps ->
        let rec rj (dtn,dtro,dtt) vs jps ujps = match jps with 
          | [] -> (((dtn,None,dtt,dtro)::vs),ujps)
          | (jn,jv) as jp :: jps -> 
            if jn=dtn then (* got it *)
              (((dtn,Some jv,dtt,dtro)::vs),(List.rev_append jps ujps))
            else
              rj (dtn,dtro,dtt) vs jps (jp::ujps)
        in let (vs,jps) = rj dtp vs jps [] in
        rdt dtps vs jps in
    rdt dtps [] jps in
  if jps = [] then vs 
  else 
    let tostr s (n,_) = s^" "^n in
    raise(HttpError(`Bad_request,("data had unknown property(s): "^(List.fold_left tostr "" jps))))

let do_create username ti key typeinfos path_elems data tx =
  let jval = Json.of_string data in
  let props = match_json_with_dyntype jval ti.ttype in
  (* primary key *)
  let id = match ti.pktype with
  | `User_defined -> begin
      let fn pkv (pn,jv,_,_) = if pn=ti.pkname then match jv with
          | Some Json.String s -> s
          | None -> raise(HttpError(`Bad_request,("Id property "^ti.pkname^" not provided for "^ti.tname)))
          | Some x -> raise (HttpError(`Not_implemented,("Primary key not a string: "^(Json.to_string x))))
        else pkv in
      List.fold_left fn "" props 
    end in
  let key = key^id in
  (* check *)
  lwt old = DB.get tx key in
  begin match old with 
    | B.NOK _ -> Lwt.return ()
    | B.OK v -> Lwt.fail(HttpError(`Conflict,("Cannot create "^key^": already exists"))) 
  end >>
  (* create *)
  lwt _ = DB.set tx key (Json.to_string jval) in
  Lwt.return (B.OK id)

let do_update username ti key data tx =
  lwt res = DB.get tx key in
  lwt oldval = match res with B.NOK _ -> Lwt.fail(HttpError(`Not_found,("Not found: "^key))) 
  | B.OK x -> Lwt.return x in
  let jval = Json.of_string data in
  let oldjval = Json.of_string oldval in
  let props = match_json_with_dyntype jval ti.ttype in
  (* override old values *)
  let oldjps = match oldjval with
    | Json.Object jps -> jps
    | x -> OS.Console.log("value of "^key^" was not a Json object: "^(Json.to_string x)); 
      []
  in let getojpv name =
    let fn ojpv (n,v) = if n=name then Some v 
      else ojpv in
    List.fold_left fn None oldjps 
  in let fn njps (pn,jpv,pdt,pro) =
    let opv = getojpv pn in
    match opv with
    | Some pv -> begin
        match jpv with
        | Some npv ->
          if pv = npv then
            (* unchanged *)
            ((pn,pv) :: njps)
          else begin
            (* check mutable or key *)
            if pn=ti.pkname then
              raise(HttpError(`Bad_request,("Cannot change primary key value "^pn)))
            else if pro=`RO then
              raise(HttpError(`Bad_request,("Cannot change immutable value "^pn)))
            else
              (* updated *)
              ((pn,npv) :: njps)   
          end
        | None -> (* not updated *) 
          ((pn,pv) :: njps)
      end
    | None -> begin
        match jpv with
        | Some npv -> if pro=`RO then
              raise(HttpError(`Bad_request,("Cannot set unset immutable value "^pn)))   
            else
              (* set *) 
              ((pn,npv) :: njps)
        | None -> (* default ?! *) njps
      end
  in let njps = List.fold_left fn [] props in
  let njv = Json.Object njps in
  let value = Json.to_string njv in
  lwt _ = DB.set tx key value in
  Lwt.return (B.OK "")

let do_delete username ti key typeinfos path_elems data tx =
  lwt res = DB.get tx key in
  lwt _ = match res with B.NOK _ -> Lwt.fail(HttpError(`Not_found,("Not found: "^key))) 
  | _ -> Lwt.return () in
  lwt res = DB.delete tx key in
  let res = match res with
  | B.OK _ -> B.OK ""
  | B.NOK _ -> raise(HttpError(`Internal_server_error,("Delete failed for "^key)))
  in Lwt.return res

let safe fn tx = 
  try_lwt
    fn tx
  with 
    HttpError(s,m) -> OS.Console.log("DbTx: HttpError "^(string_of_status s)^": "^m); 
      Lwt.return (B.NOK ((string_of_status s)^": "^m))
  | ex -> OS.Console.log("DbTx: Exception: "^(Printexc.to_string ex));
      Lwt.return (B.NOK ("Exception: "^(Printexc.to_string ex)))
let with_tx fn db = 
  DB.with_tx db fn

(* handle HTTP request *)
let dispatch ?body req path_elems typeinfos username = 
  try_lwt
    (* check action *)
    let uri = CL.Request.uri req in
    let path = Uri.path uri in
    let meth = CL.Request.meth req in
    OS.Console.log("dbrest "^(string_of_method meth)^" for "^path^" by "^username);
    lwt db = Persist.get None in
    lwt data = Cohttp_lwt_body.string_of_body body in
    let ti = get_typeinfo typeinfos path_elems in
    let key = get_key path_elems in
    let istype = 
      let rec rlast = function 
        | [] -> raise (HttpError (`Bad_request,"Top-level of API is not a method"))
        | x :: [] -> x=""
        | _ :: x -> rlast x in
      rlast path_elems in
    let fn = match (meth,istype) with
    | `GET,true -> do_list username ti key
    | `GET,false -> do_read username ti key
    | `PUT,false -> with_tx(safe(do_update username ti key data))
    | `POST,true -> with_tx(safe(do_create username ti key typeinfos path_elems data))
    | `DELETE,false -> with_tx(safe(do_delete username ti key typeinfos path_elems data))
    | m,_ -> raise (HttpError(`Bad_request,("No support for "^
                    (string_of_method m)^" on "^(if istype then "type" else "instance"))))
    in
    lwt res = fn db in
    let body = match res with 
    | B.NOK err -> raise (HttpError(`Bad_request,err))
    | B.OK body -> body in
    CL.Server.respond_string ~status:`OK ~body ()
  with 
    HttpError (status,body) -> 
    CL.Server.respond_error status body ()
  | ex ->
    CL.Server.respond_error `Internal_server_error "Internal error" ()

