(* ost kiosk persistence over (initially anyway) baardskeerder *)

open Lwt

module BS = Baardskeerder.Baardskeerder(Baardskeerder.Logs.Flog0)(Baardskeerder_mirage.Stores.Blkif)

type t = {bs:BS.t}

let default_id = "2049"

(* Blkfront will map xen block devs to Mirage blkif, e.g. sda1 -> 2049 (linux block linux bdev. no 8.1) *)
(*let init_xen () = 
  lwt () = Blkfront.register () in
  Lwt.return ()*)

(* unix-simple-blkdev over file *)
let init_unix () =
  Blkdev.add_provider default_id default_id

let inited = ref false
let 

let init () = if (!inited) then return () else begin
    inited := true;
    (* should be in main ... *)
    OS.Console.log("WARNING: hard-coded dependency on simple-unix-blkdev in persist.ml (needs Mirari fix)");
    init_unix() 
  end
  
let get oid = 
  let id = match None -> default_id | Some id -> id in
  lwt () = init() in
  lwt bs = BS.make id in
  return {bs}


