(** controller.ml - implementation of the CamlSDN controller *)

open Sys;;
open Unix;;
open UnixUtil;;
open Marshal;;
open NetCore;;

type client_id = int;; (* Or something *)

type simple_client_msg = 
	| ClientAck
	| ClientError;;

let portnum = 8988;;

let simple_policy = Action ToAll;;

(** Handler function used by server.
		Needs a mapping of clients to policies, produced by parse_policy *)
let s_handler pol_map in_chan out_chan =
	prerr_endline "GOT TO s_handler";
	(* Write initial policy *)
	Marshal.to_channel out_chan pol_map [];
	flush out_chan;
	prerr_endline "waiting for response...";
	(* Wait for client to acknowledge policy *)
	let cliResponse = (Marshal.from_channel in_chan : simple_client_msg) in
	match cliResponse with
	| ClientAck -> print_endline "success!"
	| ClientError -> print_endline "something went wrong!";;

(* Server setup *)
let init pol = 
	(*let my_addr = ADDR_INET ((UnixUtil.get_my_addr ()), portnum) in *)
	let my_addr = ADDR_INET (inet_addr_any, portnum) in
	let server_handler = UnixUtil.treat_with_chan_handler (s_handler pol) in
	UnixUtil.tcp_server server_handler my_addr;;

let main () =
	init simple_policy;;

main ();;