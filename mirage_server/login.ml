(* Login *)

module CL = Cohttp_lwt_mirage
module C = Cohttp
open C.Code

let form_username = "username"
let form_password = "password"

let admin_username = "admin"
let admin_password = "password"

let session_username = "username"

let respond_error req status message = 
  OS.Console.log("Return error "^(string_of_status status)^": "
    ^message^" for "^(Uri.path (CL.Request.uri req)));
  CL.Server.respond_error status message ()

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
let handle ?body req = 
  let meth = CL.Request.meth req in
  let headers = CL.Request.headers req in 
  match meth with 
  | `POST -> 
    OS.Console.log("login attempt...");
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
      else if username=admin_username && password=admin_password then begin
        (* built-in/bootstrap admin user *)
	let id = Session.new_id () in
        Session.set_session_value ~id headers session_username username (Some 3600.);
        (* cookie *)
        let hname,hvalue = Session.make_session_cookie headers id in
        OS.Console.log("set cookie "^hname^": "^hvalue);
        let headers = C.Header.init () in 
        let headers = C.Header.add headers hname hvalue in
        let uri = Uri.make ~path:"/login.html" () in
        OS.Console.log("Redirect to "^(Uri.to_string uri));
        CL.Server.respond_redirect ~headers ~uri ()
        (*let body = "Logged in as "^username in
        CL.Server.respond_string ~headers ~status:`OK ~body ()*)
      end else
        respond_error req `Unauthorized 
          ("User name / password combination incorrect or unknown for "^username)
    end
  | m -> 
    OS.Console.log("login attempt using method "^(string_of_method meth));
    respond_error req `Method_not_allowed "You must use POST to login"
    
