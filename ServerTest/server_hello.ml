(* Very simple test server*)
(* For echoing input from client*)
open Sys;;
open Unix;;


if Array.length Sys.argv < 2 then
begin
	print_endline "provide a port!!";
	exit 2;
end;;

let port = int_of_string Sys.argv.(1);;
let host = (gethostbyname(gethostname ())).h_addr_list.(0);; 
let addr = ADDR_INET (inet_addr_any, port);;

(* These are in the Misc module,*)
(* But I couldn't find the module. *)
let fork_treatment server service (client_sock, _ as client) =
  let treat () = match fork () with
    | 0 -> close server; service client; exit 0
    | k -> ()
  in
	treat ();;

let install_tcp_server_socket addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    bind s addr;
    listen s 10;
    s
  with z -> close s; raise z;;

let tcp_server treat_connection addr =
  ignore (signal sigpipe Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
      let client = accept server_sock in
      treat_connection server_sock client;
			print_endline "treated!";
  done;;

let treat sock (client_sock, client_addr as client) =
	begin match client_addr with
	| ADDR_INET (caller, _) ->
			prerr_endline("Connection from " ^ string_of_inet_addr caller);
	| ADDR_UNIX _ -> prerr_endline "Should be impossible";
	end;
	
	(* Handle the connected client *)
	let handle (sock, _) =
		(* Figure out how to loop this gracefully *)
		let buffer = String.create 1000 in
		let n = read sock buffer 0 1000 in
		let out = String.sub buffer 0 n in
		print_endline ("got " ^ out);
		
	in fork_treatment sock handle client;;

let main () = tcp_server treat addr;;
main ();;