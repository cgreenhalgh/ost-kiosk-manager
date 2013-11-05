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

(* http sessions based on cookies *)

module Store = Memorystore

(* is Random good enough? *)
let () = Random.self_init ()

(* 16 chars *)
let new_id () = 
  let bsize = 16 in
  let bstring = Buffer.create bsize in
  for i=0 to bsize-1 do
    let n = Random.int 62 in
    let c = if n<10 then char_of_int((int_of_char '0')+n) 
    else if n<(10+26) then char_of_int((int_of_char 'A')+n-10) 
    else char_of_int((int_of_char 'a')+n-10-26) in
    Buffer.add_char bstring c
  done;
  Buffer.contents bstring

let cookie_name = "MIRAGESESSIONID"
let header_host = "host"

let get_host headers =
  let ohost = Cohttp.Header.get headers header_host in match ohost with
  | None -> OS.Console.log("Host not found in request"); "localhost"
  | Some host -> 
    try 
      let ix = String.rindex host ':' in
      String.sub host 0 ix
    with 
      Not_found -> host 

let make_session_cookie headers id =
  let host = get_host headers in
  let t = Cohttp.Cookie.Set_cookie_hdr.make ~domain:host ~path:"/" (cookie_name,id) in
  Cohttp.Cookie.Set_cookie_hdr.serialize t

let get_session_id (headers:Cohttp.Header.t) =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
  let oid = List.fold_left
    (fun v (key,value) -> if key=cookie_name then Some value else v) None cookies in 
  oid

let session_store = Store.create ()

let store_key id key = id^":"^key

let get_session_value headers key = 
  let oid = get_session_id headers in
  match oid with 
  | None -> None
  | Some id -> Store.get session_store (store_key id key)

let set_session_value ?id headers key value timeout =
  let oid = match id with
  | Some id as oid -> oid  
  | None -> get_session_id headers in
  match oid with 
  | None -> 
    OS.Console.log("Warning: set_session_value with no session (key="^key^")")
  | Some id -> 
    Store.put session_store (store_key id key) value timeout;
    OS.Console.log("set_session_value "^key^"="^value^" in session "^id)

let clear_session_value headers key =
  let oid = get_session_id headers in
  match oid with 
  | None -> OS.Console.log("clear_session_value "^key^" outside session"); ()
  | Some id -> begin
      let skey = (store_key id key) in 
      let oldval = Store.get session_store skey in 
      begin match oldval with
        | Some value -> OS.Console.log("clear_session_value "^key^" (was "^value^") in session "^id);
          let _ = Store.remove session_store skey in ()
        | None -> OS.Console.log("clear_session_value "^key^" (was unset) in session "^id); 
          () 
      end
    end

