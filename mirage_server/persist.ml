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

(* ost kiosk persistence over (initially anyway) baardskeerder *)

open Lwt

module BS = Baardskeerder.Baardskeerder(Baardskeerder.Logs.Flog0)(Baardskeerder_mirage.Stores.Blkif)
module DB = BS

type t = BS.t

let default_id = "2049"

(* Blkfront will map xen block devs to Mirage blkif, e.g. sda1 -> 2049 (linux block linux bdev. no 8.1) *)
(*let init_xen () = 
  lwt () = Blkfront.register () in
  Lwt.return ()*)

(* unix-simple-blkdev over file *)
let init_unix () =
  Blkdev.add_provider default_id default_id

let inited = ref false

let init () = if (!inited) then return () else begin
    inited := true;
    (* should be in main ... *)
    OS.Console.log("WARNING: hard-coded dependency on simple-unix-blkdev in persist.ml (needs Mirari fix)");
    init_unix() 
  end

let bss = Hashtbl.create 10
  
let get oid = 
  let id = match oid with 
    | None -> default_id 
    | Some id -> id in
  lwt () = init() in
  lwt bs = if Hashtbl.mem bss id then 
    return (Hashtbl.find bss id)
  else begin 
    lwt () = try_lwt BS.init id
    with ex -> begin 
      OS.Console.log("BS.init error (may be initialised already)"); return () 
    end in
    lwt bs = BS.make id in
    Hashtbl.replace bss id bs;
    return bs
  end in
  return bs

let _ = init()
