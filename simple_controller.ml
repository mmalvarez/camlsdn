(** Simplified controller code.
	Delivers the same policy to all clients *)

open Sys;;
open Unix;;

let service_client  in_chan out_chan =
	(* Send policy *)


	(* Wait for client to acknowledge *)

	(* Hang out; wait for changes in policy OR client requests *)

let init () =
	let s = socket PF_INET SOCK_STREAM 0 in
	let (sfd, saddr) = accept s in
	let pol_map = parse_policy pol in 
	establish_server (s_handler pol_map) saddr;;