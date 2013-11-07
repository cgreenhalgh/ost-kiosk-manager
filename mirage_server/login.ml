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

(* Login utilities. Sets up session cookie. On successful login stores
   authenticated username in transient server-side session state. *)

module CL = Cohttp_lwt_mirage
module C = Cohttp
open C.Code

let form_username = "username"
let form_password = "password"

let admin_username = "admin"
let admin_password = "password"

let session_username = "authenticatedusername"

let get_authenticated_user req =
  let headers = CL.Request.headers req in 
  let username = Session.get_session_value headers session_username in
  username

let check_user_and_pass username password = 
  (* built-in/bootstrap admin user *)
  if username=admin_username && password=admin_password then true 
  else begin
    (* TODO authenticate local user accounts *)
    false
  end

let get_authenticated_api_user req =
  (* let's try basic authentication for now *)
  let headers = CL.Request.headers req in 
  let auth = C.Header.get_authorization headers in
  match auth with 
  | Some C.Auth.Basic (username,password) -> 
    if check_user_and_pass username password then Some username
    else begin 
      OS.Console.log("authentication failure for api "^username);
      None
    end
  | _ -> None 
  
let respond_error req status message = 
  OS.Console.log("Return error "^(string_of_status status)^": "
    ^message^" for "^(Uri.path (CL.Request.uri req)));
  CL.Server.respond_error status message ()

let respond_user_not_authorized req =
  CL.Server.respond_redirect ~uri:(Uri.make ~path:"/login.html" ()) ()

let respond_user_forbidden req =
  CL.Server.respond_error ~status:`Forbidden ~body:"You do not have permission to access this" ()

let respond_api_not_authorized req =
  let headers = C.Header.init () in
  let realm = 
    let oh = Uri.host (CL.Request.uri req) in match oh with
    | Some h -> h
    | None -> "nonesuch" in
  let headers = C.Header.add_authorization_req headers C.Auth.(`Basic realm) in
  CL.Server.respond ~headers ~status:`Unauthorized ~body:(Cohttp_lwt_body.body_of_string "Not Authorized") ()

let respond_api_forbidden req =
  CL.Server.respond_error `Forbidden "Forbidden" ()


let get_one_value formvalues name = 
    let rec rget2 res n vs = if n=name then match vs with 
    | v :: vs -> if (String.length v)>0 then v else rget2 res n vs
    | _ -> res
    else res in
  let rec rget res fvs = match fvs with
  | (n,vs) :: fvs -> let res = rget2 res n vs in
    rget res fvs
  | _ -> res in
  rget "" formvalues 

(* handle login action URL *)
let handle_login ?body req success_path failure_path = 
  let meth = CL.Request.meth req in
  let headers = CL.Request.headers req in 
  match meth with 
  | `POST -> 
    Session.clear_session_value headers session_username;
    (* form: username, password *)
    lwt body = Cohttp_lwt_body.string_of_body body in 
    if not(CL.Request.is_form req) then 
      respond_error req `Bad_request "Login request is not form url-encoded"
    else begin
      let formvalues = Uri.query_of_encoded body in 
      let username = get_one_value formvalues form_username in
      let password = get_one_value formvalues form_password in
      if (String.length username)=0 then 
        respond_error req `Bad_request ("User name not specified in "^body)
      else if check_user_and_pass username password then begin
        (* built-in/bootstrap admin user *)
        let respheaders = ref (C.Header.init ()) in 
	let id = Session.get_session_id headers in
        let id = begin match id with 
        | None -> 
	  let id = Session.new_id () in
          (* set new cookie *)
          let hname,hvalue = Session.make_session_cookie headers id in
          OS.Console.log("set cookie "^hname^": "^hvalue);
          respheaders := C.Header.add !respheaders hname hvalue;
          id
        | Some id -> id
        end in
        Session.set_session_value ~id headers session_username username (Some 3600.);
	OS.Console.log("Successful login as "^username);
        let uri = Uri.make ~path:success_path () in
        OS.Console.log("Redirect to "^(Uri.to_string uri));
        CL.Server.respond_redirect ~headers:(!respheaders) ~uri ()
      end else begin
        OS.Console.log("User name / password combination incorrect or unknown for "^username);
        let uri = Uri.make ~path:failure_path () in
        CL.Server.respond_redirect ~headers ~uri ()
      end
    end
  | m -> 
    OS.Console.log("login attempt using method "^(string_of_method meth));
    respond_error req `Method_not_allowed "You must use POST to login"
    
(* handle logout action URL *)
let handle_logout ?body req redirect_path = 
  let headers = CL.Request.headers req in 
  Session.clear_session_value headers session_username;
  let uri = Uri.make ~path:redirect_path () in
  CL.Server.respond_redirect ~headers ~uri ()

