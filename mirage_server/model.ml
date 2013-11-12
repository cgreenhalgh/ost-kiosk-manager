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

open Dyntype
open Cow
module DB = Persist.DB
module B = Baardskeerder

(* user *)
type user = {
  email: string;
  mutable utitle: string;
  mutable pw: string
} with type_of, json

let authenticator = function (username, password) -> 
  lwt db = Persist.get None in
  let key = Dbcommon.get_key ["user"; username] in 
  lwt ruser = DB.get_latest db key in
  match ruser with
  | B.OK v -> begin try 
        let user = user_of_json (Json.of_string v) in
        Lwt.return (password = user.pw)
      with ex -> OS.Console.log("Error checking user "^username^" ("^v^"): "^(Printexc.to_string ex));
        Lwt.return false
    end
  | B.NOK _ -> Lwt.return false

(* group, child of user *)
type group = {
  gid : string;
  mutable gtitle : string;
  mutable gver: string;
  mutable gdate: string;
  mutable gpublic: bool
} with type_of, json

(* item, child of group *)
type item = {
  iid: string; 
  mutable iatomid: string;
  mutable ititle: string;
  mutable isummary: string;
  mutable iiconurl: string;
  mutable irequires: string;
  (* whitespace separated list *)
  mutable ifileurl: string;
  mutable ifilemime: string;
  mutable isupports: string;
  (* whitespace separated list *)
  mutable ivisible: bool;
  mutable ipublic: bool;
  mutable iver: string;
  mutable idate: string
} with type_of, json

(* webdeploy, child of user *)
type website = {
  wid: string;
  mutable watomid: string;
  mutable wtitle: string;
  mutable wgeturl: string;
  mutable wgids: string
  (* group(s), whitespace separated list *)
} with type_of, json

(* device, child of user *)
type device = {
  did: string;
  mutable dtitle: string;
  mutable dgids: string
  (* whitespace separated list *)
} with type_of, json

