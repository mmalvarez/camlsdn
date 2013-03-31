(** UnixUtil.ml*)
(** Collection of utility functions found in
		The OCaml UNIX tutorial's Misc module *)

open Sys;; 
open Unix;;

module UnixUtil =
struct

  (* "Finally" from other languages *)
  let try_finalize f x finally y =
    let res = try f x with exn -> finally y; raise exn in
    finally y;
    res;;
  
  (* Fork a new process to service a client *)
  let fork_treatment server service (client_sock, _ as client) =
    prerr_endline "FORK TREATMENT";
    let treat () = match fork () with
      | 0 -> close server; service client; exit 0
      | k -> ()
    in
  	treat ();;

  (* Set up socket for a TCP server on addr *)
  let install_tcp_server_socket addr =
    let s = socket PF_INET SOCK_STREAM 0 in
    try
      bind s addr;
      listen s 10; (* 10 = max # of pending reqs *)
      s
    with z -> close s; raise z;;

  (* Start a TCP server with the given handler and address *)
  let tcp_server treat_connection addr =
    ignore (signal sigpipe Signal_ignore);
    let server_sock = install_tcp_server_socket addr in
    while true do
      let client = accept server_sock in
      treat_connection server_sock client;
    done;;

  (** Convenience method for standard client treatment *)
  let treat_with_chan_handler handler server_sock (client_sock, client_addr as client) =
    begin match client_addr with
      | ADDR_INET (caller, _) ->
        prerr_endline("Connection from " ^ string_of_inet_addr caller);
      | ADDR_UNIX _ -> prerr_endline("Invalid socket!");
    end;

    let chan_handler sub_handler (client_sock, _ as client) = 
        (*let (in_chan, out_chan) = open_connection client_addr in*)
        let in_chan = in_channel_of_descr client_sock
        and out_chan = out_channel_of_descr client_sock in
        sub_handler in_chan out_chan;

    in fork_treatment server_sock (chan_handler handler) client;;

  (** Cool trick from O'Reilly oCaml book.
      Returns this machine's address *)
  let get_my_addr () =
    (gethostbyname(gethostname())).Unix.h_addr_list.(0);;
end;;