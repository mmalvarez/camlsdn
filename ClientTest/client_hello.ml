(* Very simple test client *)
(* For sending a message to the server *)
open Sys;;
open Unix;;

let message = 
	if Array.length Sys.argv >= 4 then
		Sys.argv.(3)
  else "Hello, Server!!";;

let client () =
	if Array.length Sys.argv < 3 then begin
		prerr_endline "Usage: client <host> <port>";
		exit 2;
	end;
	let server_name = Sys.argv.(1)
	and port_number = int_of_string Sys.argv.(2) in
	let server_addr =
		try (gethostbyname server_name).h_addr_list.(0)
		with Not_found ->
			prerr_endline (server_name ^ ": Host not found");
			exit 2 in
	let sock = socket PF_INET SOCK_STREAM 0 in
	connect sock (ADDR_INET (server_addr, port_number));
	single_write sock message 0 (String.length message);;

client ();;