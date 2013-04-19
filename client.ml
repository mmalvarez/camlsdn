(** Simple implementation of client.
	Connects to server, gets policy, and prints it. *)

open Sys;;
open Unix;;
open Marshal;;
open UnixUtil;;
open NetCore;;

type simple_client_msg = 
	| ClientAck
	| ClientError;;

let portnum = 8988;;

(* Handle actions after connecting to server *)
let c_handler in_chan out_chan =
	(* Read policy; acknowledge receipt; then print policy *)
	prerr_endline "waiting for policy";
	let pol = (Marshal.from_channel in_chan  : policy) in

	(* TODO: Validate policy *)
	Marshal.to_channel out_chan ClientAck [];
	flush out_chan;
	print_endline ("Policy:\n" ^ (string_of_policy pol));;


let client () =
	if Array.length Sys.argv < 2 then begin
		prerr_endline "Usage: client <controller_host>";
		exit 2;
	end;
	let server_name = Sys.argv.(1) in
	let server_inet_addr =
		try (gethostbyname server_name).h_addr_list.(0)
		with not_found ->
			prerr_endline (server_name ^ ": Host not found!");
			exit 2 in

	let server_addr = ADDR_INET (server_inet_addr, portnum) in 
	let sock = socket PF_INET SOCK_STREAM 0 in
	let (in_chan, out_chan) = open_connection server_addr in
	c_handler in_chan out_chan;;
	
client ();;