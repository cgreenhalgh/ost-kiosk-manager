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

(** a simple soft-state data store. Initially holds a map of names to (value, 
    optional sub-store and timeout). 
 *)

let return x = x

(** item in the store *)
type item = {
	(** name (unique) *)
	name : string;
	(** option sub-store *)
	(*store : t;*)
	(** json-encoded *)
	jsonvalue : string; 
	(** unix time *)
	mutable expires : float option;
}

(** the store value type *)
type t = (string,item) Hashtbl.t

(** create store - may block *)
let create () = 
	return (Hashtbl.create 1)
	
(** put name/value in store; timeout is relative time in seconds - may block *)
let put s name value timeout = 
	let now = OS.Clock.time () in
	let expires = match timeout with
		  Some t -> Some (now+.t)
	  | None   -> None in
	let item = {name=name;jsonvalue=value;expires=expires} in
	return (Hashtbl.replace s name item)
	
let expired item now = match item.expires with
	  Some t -> t<=now
	| None   -> false
	
(** get item option for name - may block *)
let getitem s name =
	try 
		let item = Hashtbl.find s name in
	  let now = OS.Clock.time () in
		if (expired item now) then (Hashtbl.remove s name; return None) 
		else return (Some item)
	with Not_found -> return None

(* raises Not_found *)
let renew s name timeout =
	let now = OS.Clock.time () in
	let expires = match timeout with
		  Some t -> Some (now+.t)
	  | None   -> None in
	let item = getitem s name in
	match item with Some item -> item.expires <-expires
        | _ -> raise Not_found	

(** get value option for name - may block *)
let get s name =
	match getitem s name with
		| Some item -> return (Some item.jsonvalue)
		| None      -> return None

(** list values - no expire - may block *)
let list s =
	let get_names name item names =
		name :: names in
	return (Hashtbl.fold get_names s [])

(** remove any binding; return old value - may block *)
let remove s name =
	let value = get s name in
	Hashtbl.remove s name;
	return value

(** purge expired values; return next expiry time - may block *)
let get_expires s =
	let now = OS.Clock.time () in
	let find_expired name item namelist = 
		if (expired item now) then name :: namelist else namelist in
	let expired = Hashtbl.fold find_expired s [] in
	List.iter (fun name -> Hashtbl.remove s name) expired;
	let min_expires name item expires =
		match expires with
			| None -> item.expires 
			| Some m -> match item.expires with 
				| None -> Some m
				| Some t -> Some (min m t) 
	in
	return (Hashtbl.fold min_expires s None)
