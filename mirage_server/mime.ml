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

module CL = Cohttp_lwt_mirage
module C = Cohttp

(* Mime-related constants and utilities *)
let header_content_type = "Content-Type"

let content_type_html = "text/html"
let content_type_javascript = "application/javascript"
let content_type_css = "text/css"
(*["content-type","application/atom+xml; charset=UTF-8"]*)

(* file extensions... *)
let ext_regexp = Re_str.regexp "\\.\\([^./]+\\)$"

let get_file_ext filename = try
    let _ = Re_str.search_forward ext_regexp filename 0 in
    Re_str.matched_group 1 filename 
  with Not_found -> ""

module StringMap = Map.Make(String)

let file_ext_map =
  let (=>) f g = g f in
  let map = StringMap.empty =>
  StringMap.add "html" content_type_html =>
  StringMap.add "css" content_type_css =>
  StringMap.add "js" content_type_javascript in
  map

(* may throw Not_found *)
let rec guess_mime_type path = let ext = get_file_ext path in
    OS.Console.log("request ext = "^ext^" - path = "^path);
    StringMap.find ext file_ext_map

(* Convert arbitrarily-divided string stream (e.g. from cohttp_lwt_body)
   to stream of HTTP/MIME-style text lines (i.e. ending with CR LF, which are not returned ) *)
let line_stream bstream = 
  lwt fbuf = Lwt_stream.get bstream in
  let ibuf = ref fbuf in
  let ipos = ref 0 in
  let b = Buffer.create 100 in
  let rec more () =
    match !ibuf with 
    | None -> begin
        if Buffer.length b = 0 then 
          Lwt.return None
        else begin 
          let res = Buffer.contents b in
          Buffer.clear b;
          Lwt.return (Some res)
        end
      end
    | Some s -> 
      let len = String.length s in
      try 
        let nlpos = (String.index_from s !ipos '\n')+1 in
        let cnt = nlpos - !ipos in
        Buffer.add_substring b s !ipos cnt;
        ipos := !ipos + cnt;
        if !ipos > 1 && Buffer.nth b (Buffer.length b - 2) = '\r' then begin
          let res = Buffer.sub b 0 (Buffer.length b - 2) in
          Buffer.clear b;
          Lwt.return (Some res) 
        end else
          more ()
      with Not_found ->
        let cnt = len - !ipos in
        if cnt > 0 then begin
          Buffer.add_substring b s !ipos cnt;
          ipos := !ipos + cnt
        end;
        lwt nbuf = Lwt_stream.get bstream in
        ibuf := nbuf;
        ipos := 0;
        more ()
  in Lwt.return (Lwt_stream.from more)

let regexp_semicolon = Re_str.regexp "[ \t]*;[ \t]*"
(* special characters from rfc822 *)
(* Note, there is an error in bracket in 
   lib/re_emacs.ml in ocaml-re - it does not handle
   escapes (including \]) inside a [..] section. 
   So \\] should be in the following and isn't *)
let regexp_value = Re_str.regexp "^[ \t]*\\([^()<>@,;:\\\".[ \t\n\r]+\\)[ \t\r\n]*$" (*"^[ \t]*\\([^()<>@,;:\\\".[\\]]+\\)[ \t\n\r]*$"*)
let regexp_parameter = Re_str.regexp "^[ \t]*\\([^()<>@,;:\\\".[ \t\n\r=]+\\)[ \t]*=[ \t]*\\(\\([^()<>@,;:\\\".[ \t\n\r]+\\)\\|\\([\"]\\(\\([\\].\\)\\|[^\"\\\r]\\)*[\"]\\)\\)"

exception Invalid_content_type of string

(* Header value format as used for Content-type: 
   "value; parameter=pvalue; ...".
   Note: this is not full rfc822-compliant, e.g. no comments *)
let parse_content_type s : string * (string * string) list =
  let parts = Re_str.split regexp_semicolon s in
  let value,pparts = match parts with 
    | [] -> raise (Invalid_content_type ("No parts found: "^s)) 
    | vp :: pparts ->
      if Re_str.string_match regexp_value vp 0 then
        (Re_str.matched_group 1 vp),pparts
      else
        raise (Invalid_content_type ("No valid value found in first part: "^vp))
  in
  let sp res ppart = 
    if Re_str.string_match regexp_parameter ppart 0 then begin
      (* NB only for valid quoted strings! *)
      let unescape_rfc822 s = 
        if s="" || String.get s 0 <> '"' then s else begin
    	  let l = String.length s in
          let b = Buffer.create l in
          let i = ref 1 in
          while !i < l-1 do
            let c = 
              let c = String.get s !i in
              if c<>'\\' then c else begin
                i := !i + 1;
                String.get s !i
              end in
            Buffer.add_char b c;
            i := !i + 1 
          done;
          Buffer.contents b
        end
      in
      let pname = Re_str.matched_group 1 ppart and
        pval = unescape_rfc822 (Re_str.matched_group 2 ppart) in
      (pname,pval) :: res
    end else
      raise (Invalid_content_type ("Not a valid parameter: "^ppart))
  in 
  let params = List.fold_left sp [] pparts in
  (value,params)

let regexp_header_name = Re_str.regexp "^[ \t]*\\([^()<>@,;:\\\".[ \t\n\r]+\\)[ \t]*:" 

let parse_header s : string * string * (string * string) list =
  if Re_str.string_match regexp_header_name s 0 then begin
    let name = Re_str.matched_group 1 s in
    let nxt = Re_str.match_end () in
    let rest = String.sub s nxt (String.length s - nxt) in
    let (value,params) = parse_content_type rest in
    (name, value, params)
  end else raise (Invalid_content_type ("Could not find header name: "^s))

(* read body as multipart/form-data, return (name,value list) list and (filepartname,filename,contenttype) list *)
let read_multipart_body ?body headers : ((string * string list) list * (string * string * string) list) Lwt.t =
  (* content type *)
  let cto = C.Header.get headers "content-type" in
  let (ctv,ctps) = match cto with 
  | None -> raise (Invalid_content_type "No content-type")
  | Some ct -> parse_content_type ct 
  in
  if ctv <> "multipart/form-data" then
    raise (Invalid_content_type ("Not multipart/form-data: "^ctv));
  (* boundary - from content type *)
  let rec getboundary ctps b = match ctps with
  | [] -> b
  | ("boundary",b) :: _-> b
  | (_,_) :: ctps -> getboundary ctps b
  in
  let boundary = getboundary ctps "" in
  if boundary = "" then
    raise (Invalid_content_type ("Not multipart/form-data: "^ctv));
  (* two leading dashes... *)
  let boundary = "--"^boundary in
  let lastboundary = boundary^"--" in
  (* body stream *)
  let bstream = Cohttp_lwt_body.stream_of_body body in
  lwt lstream = line_stream bstream in
  (* start with boundary *)
  lwt fl = Lwt_stream.get lstream in
  if fl <> Some boundary then
    raise (Invalid_content_type ("Body should start with boundary "^boundary^": "^(match fl with None -> "EOF" | Some b -> b)));
  (* parts... *)
  let rec rread_part parts metas =
    (* header *)
    let rec rdhdr found name filename contenttype = 
      lwt l = Lwt_stream.get lstream in
      match l with 
      | None -> if found then 
          Lwt.fail (Invalid_content_type ("No header separator in part "^name))
        else
          Lwt.return None
      | Some "" -> if found then 
          Lwt.return (Some(name,filename,contenttype))
        else
          Lwt.fail (Invalid_content_type ("Header separator with no header"))
      | Some hl ->
        let (hn,hv,hps) = parse_header hl in
        if hn = "Content-Disposition" && hv = "form-data" then begin
          if found then
            raise (Invalid_content_type ("More than one Content-Disposition header in part "^name));
          (*OS.Console.log("Found part "^hl);*)
          (* name , filename *)
          let rec chkps hps (name,fn) =
            match hps with 
            | [] -> (name,fn)
            | ("name",name) :: rest -> chkps rest (name,fn)
            | ("filename",fn) :: rest -> chkps rest (name,fn)
            | _ :: rest -> chkps rest (name,fn) 
          in let (name,filename) = chkps hps ("","") in
          rdhdr true name filename contenttype
        end else if hn = "Content-Type" then
          rdhdr found name filename hv
        else (* ignore *)
          rdhdr found name filename contenttype
    in lwt ohdr = rdhdr false "" "" "" in match ohdr with
    | None -> Lwt.return (parts,metas)
    | Some (name,filename,contenttype) -> begin
        (* body, boundary *)
        let b = Buffer.create 10 in
        let rec read () = 
          lwt l = Lwt_stream.get lstream in match l with 
          | None -> Lwt.fail (Invalid_content_type("No boundary after part "^name))
          | Some l when l = boundary -> 
            Lwt.return (false,(Buffer.contents b))
          | Some l when l = lastboundary -> (* Distinguished last boundary! *)
            Lwt.return (true,(Buffer.contents b))
          | Some l -> if Buffer.length b > 0 then 
              Buffer.add_string b "\r\n";
            Buffer.add_string b l;
            read ()
        in lwt (last,value) = read () in
        let parts = ((name,[value])::parts) and
          metas = ((name,filename,contenttype)::metas) in
        if last then
          Lwt.return (parts,metas)
        else
          rread_part parts metas
      end in 
  rread_part [] []



(* test *)
(* e.g.
Content-type: multipart/form-data; boundary=---------------------------92353215911568263661029728566

-----------------------------92353215911568263661029728566
Content-Disposition: form-data; name="file"; filename="dump.js"
Content-Type: application/javascript

[{ "k":"\/user:cmg@cs.nott.ac.uk", "v":{"email": "cmg@cs.nott.ac.uk", "utitle": "Chris", "pw": "pw"} }]
-----------------------------92353215911568263661029728566--
 *)

let test () = OS.Console.log("mime.ml...");
  let dump_ct ct = OS.Console.log("- content-type "^(fst ct)^" with "^
      (List.fold_left (fun r (n,v) -> r^n^" = "^v^" ") "" (snd ct))) in
  let dump_header (hn,v,ps) = OS.Console.log("- "^hn^" "^v^" with "^
      (List.fold_left (fun r (n,v) -> r^n^" = "^v^" ") "" ps)) in
  try 
    let ct = parse_content_type "multipart/form-data; boundary=---------------------------92353215911568263661029728566\r\n" in
    dump_ct ct;
    let ct = parse_header "Content-Disposition: form-data; name=\"file\"; filename=\"dump.js\"\r\n" in
    dump_header ct;
    let ct = parse_header "Content-Type: application/javascript\r\n" in
    dump_header ct;
  with Invalid_content_type s -> OS.Console.log("Mime type test error: "^s)

(* let _ = test() *)

