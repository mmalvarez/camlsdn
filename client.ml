(** Simple implementation of client.
    Connects to server, gets policy, and prints it. *)

open Sys;;
open Unix;;
open Marshal;;
open UnixUtil;;
open NetCore;;
open Token;;

type simple_client_msg = 
    | ClientAck
    | ClientError;;

let portnum = 8988;;
let conf_file_name = "ipt_config.sh";;

(* Handle actions after connecting to server *)
(* Takes integer switch ID, and communication channels *)
let c_handler id in_chan out_chan =
    (* Read policy; acknowledge receipt; then print policy *)
    prerr_endline "waiting for policy";
    let pol = (Marshal.from_channel in_chan  : NetCore.policy) in

    flush out_chan;
    print_endline "Compiling\n";
    NetCore.format_policy 0 pol;

    (* Compile policy *)
    (* TODO - better to do this on server or client? *)
    let commands = NetCore.tokens_of_policy pol in
    print_endline "Stage 1 of Compilation Complete. Commands:";
    List.map (fun c -> Token.format_command 0 c; print_endline "")
        commands;

    let conf_contents = Token.iptables_of_commands commands in
    print_endline "Stage 2 of Compilation Complete. iptables:\n";
    print_endline conf_contents;

    let conf_file_descr = openfile conf_file_name [O_RDWR; O_TRUNC] 0o666 in
    write conf_file_descr conf_contents 0 (String.length (conf_contents));
    print_endline "Wrote configuration to file.";

    system conf_file_name;
    print_endline "Executed config script. Iptables should be configured";

    Marshal.to_channel out_chan ClientAck [];;

let client () =
    if Array.length Sys.argv < 3 then begin
        prerr_endline "Usage: client <id_number> <controller_host>";
        exit 2;
    end;
    let server_name = Sys.argv.(1) in
    let server_inet_addr =
        try (gethostbyname server_name).h_addr_list.(0)
        with not_found ->
            prerr_endline (server_name ^ ": Host not found!");
            exit 2 in

    let switch_id_str = Sys.argv.(2) in
    let switch_id =
        try int_of_string switch_id_str
        with Failure _ ->
            prerr_endline (switch_id_str ^ ": invalid int");
            exit 2 in

    let server_addr = ADDR_INET (server_inet_addr, portnum) in 
    let sock = socket PF_INET SOCK_STREAM 0 in
    let (in_chan, out_chan) = open_connection server_addr in
    c_handler switch_id in_chan out_chan;;
    
client ();;