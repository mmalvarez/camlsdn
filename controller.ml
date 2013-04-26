(** controller.ml - implementation of the CamlSDN controller *)

open Sys;;
open Unix;;
open UnixUtil;;
open Marshal;;
open NetCore;;
open Hashtbl;;
open List;;

type simple_client_msg = 
    | ClientAck
    | ClientError;;

type simple_server_msg =
    | ServerAck
    | ServerError;;

let portnum = 8988;;

let sample_policies =
    [(0, Policy (Input, Drop,
        Or ((TcpSrcPort 80), (TcpDstPort 80))))
    ;(1, Policy (Input, Drop, 
        Not(Or (TcpDstPort 80,
                Or(
                    Or (TcpSrcPort 80, TcpDstPort 22),
                    Or (TcpSrcPort 8988, TcpDstPort 8988))))))];;

(** Convert a list of (switch id, policy) pairs into a hash table *)
let hashtbl_of_pol_sw_pairs pairs_list : (int, NetCore.policy) Hashtbl.t =
    let tbl = create (List.length pairs_list) in
    List.iter (fun pair -> add tbl (fst pair) (snd pair)) pairs_list;
    tbl;;

(** Handler function used by server.
        Needs a mapping of clients to policies, produced by parse_policy *)
let s_handler pol_map in_chan out_chan =
    prerr_endline "GOT TO s_handler";

    (* Wait for client to self-identify *)
    let cli_id = (Marshal.from_channel in_chan : int) in
    print_endline ("Client ID: " ^ string_of_int cli_id);

    (* Get policy TODO: handle exception if we can't find policy*)
    (*try*)
        let pol_for_switch = Hashtbl.find pol_map cli_id in

        (* Write policy *)
        Marshal.to_channel out_chan pol_for_switch [];
        flush out_chan;
        prerr_endline "waiting for response...";
        (* Wait for client to acknowledge policy *)
        (* TODO - don't block while waiting *)
        let cliResponse = (Marshal.from_channel in_chan : simple_client_msg) in
        match cliResponse with
        | ClientAck -> print_endline "success!"
        | ClientError -> print_endline "something went wrong!";;
    (*with*)


(* Server setup. Takes a mapping from switch IDs to policies *)
let init pol_map = 
    (*let my_addr = ADDR_INET ((UnixUtil.get_my_addr ()), portnum) in *)
    let my_addr = ADDR_INET (inet_addr_any, portnum) in
    let server_handler = UnixUtil.treat_with_chan_handler (s_handler pol_map) in
    UnixUtil.tcp_server server_handler my_addr;;

(* read config file given by first command-line arg *)
let main () =
    init (hashtbl_of_pol_sw_pairs sample_policies);;

main ();;