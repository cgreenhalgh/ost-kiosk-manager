(* http sessions based on cookies *)

(* is Random good enough? *)
let () = Random.self_init ()

(* 128 bits = 16 bytes *)
let new_id () = 
  let bsize = 16 in
  let bstring = Buffer.create bsize in
  for i=0 to bsize-1 do
    Buffer.add_char bstring (char_of_int(Random.int 256))
  done;
  Cohttp.Base64.encode (Buffer.contents bstring)

let cookie_name = "MIRAGESESSIONID"
let header_host = "host"

let get_host headers =
  let ohost = Cohttp.Header.get headers header_host in match ohost with
  | None -> OS.Console.log("Host not found in request"); "localhost"
  | Some host -> host 

let make_session_cookie headers id =
  let host = get_host headers in
  let t = Cohttp.Cookie.Set_cookie_hdr.make ~domain:host ~path:"/" (cookie_name,id) in
  Cohttp.Cookie.Set_cookie_hdr.serialize t

let get_session_id (headers:Cohttp.Header.t) =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
  let oid = List.fold_left
    (fun v (key,value) -> if key==cookie_name then Some value else v) None cookies in 
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

