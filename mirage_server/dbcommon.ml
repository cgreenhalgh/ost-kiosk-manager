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

(* Common DB interface types and functions *)

module CL = Cohttp_lwt_mirage
module C = Cohttp
open C.Code
open Cow
open Cow.Html
module DB = Persist.DB
module B = Baardskeerder
open Mime

type pktype = [ `User_defined ] (* TODO | `Auto_increment *)

type typeinfo = {
  tname : string;
  ttype : Dyntype.Type.t;
  tparent : string option;
  pkname : string;
  pktype : pktype
}

type typeinfos = typeinfo list

exception HttpError of (status_code * string) 

(* get type for path T1/id1/T2/id2/...; raises HttpError *)
let rec get_typeinfo typeinfos path_elems = match path_elems with
 | tn :: _ :: [] -> 
   let rec rget tis = begin match tis with 
     | [] -> raise (HttpError (`Not_found,("db type "^tn^" not found")))
     | ti :: rest -> if (ti.tname=tn) then ti else rget rest
   end in
   rget typeinfos
 | rn :: _ :: rest -> get_typeinfo typeinfos rest
 | _ -> raise (HttpError (`Bad_request,"odd path length"))


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
          if c=',' || c='/' || c=':' || c='%' then begin
            raise (HttpError (`Bad_request,("invlalid path id element "^pk)))
          end
        done;
      Buffer.add_string keyidbuf pk;
      begin match rest with 
      | [] -> ()
      | x -> build_key rest end
    | _ -> begin 
        raise (HttpError (`Bad_request,"odd path length (get_key)"))
      end
  in build_key path_elems;
  Buffer.add_string keybuf ":";
  Buffer.add_buffer keybuf keyidbuf;
  Buffer.contents keybuf


