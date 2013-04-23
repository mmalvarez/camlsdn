(* Mario Alvarez - March 2013 *)
(* Implements data types and methods *)
(* Used internally in the compiler *)
(* Based on token language from HTables *)

(* lvl options are how deep into the tree of "boxes" you are*)
open Format;;
open MiscUtil;;

module Token = struct

    (** Identify switches*)
    type switch_id = int;;

    (** iptables chains *)
    type chain = Input | Output | User of string;; (* User = name of user chain *)

    let format_chain lvl ch =
        open_vbox lvl;
        print_space ();
        let out = match ch with
            | Input -> "Input"
            | Output -> "Output"
            | User name -> "User (" ^ name ^ ")"
        in print_string out;
        close_box ();
        print_newline ();;

    (** iptables actions.
        Third option = chain to jump to *)
    type action = Accept | Drop | Jump of chain;;

    let format_action lvl act =
        let next = lvl + 1 in
        open_vbox lvl;
        print_space ();
        match act with
            | Accept -> print_string "Accept";
            | Drop -> print_string "Drop";
            | Jump ch -> 
                print_string "Jump";
                print_space ();
                print_string "(";
                format_chain next ch;
                print_string ")";
        close_box ();
        print_newline ();;

    (** iptables predicates *)
    type predicate =
        | Srcip of Int32.t
        | NotSrcip of Int32.t
        | Dstip of Int32.t
        | NotDstip of Int32.t
        | Srceth of Int64.t   (* Ethernet addrs - 48 bit *)
        | NotSrceth of Int64.t
        | Protocol of int
        | NotProtocol of int
        | Srcport of int
        | NotSrcport of int
        | Dstport of int
        | NotDstport of int
        | Always
        | Never;;
 
    let format_predicate lvl pred =
        open_vbox lvl;
        print_space ();
        let out = match pred with
            | Srcip i -> "Srcip " ^ MiscUtil.string_of_ip i
            | NotSrcip i -> "NotSrcip " ^ MiscUtil.string_of_ip i
            | Dstip i -> "Dstip " ^ MiscUtil.string_of_ip i
            | NotDstip i -> "NotDstip " ^ MiscUtil.string_of_ip i
            | Srceth i -> "Srceth " ^ MiscUtil.string_of_eth i
            | NotSrceth i -> "NotSrceth " ^ Int64.to_string i
            | Protocol i -> "Protocol " ^ string_of_int i
            | NotProtocol i -> "NotProtocol " ^ string_of_int i
            | Srcport i -> "Srcport " ^ string_of_int i
            | Dstport i -> "Dstport " ^ string_of_int i
            | NotSrcport i -> "NotSrcport " ^ string_of_int i
            | NotDstport i -> "NotDstport " ^ string_of_int i
            | Always -> "Always"
            | Never -> "Never"
        in print_string out;
        close_box ();
        print_newline ();;

    (** iptables commands. Represent one line of iptables config. *)
    type command = 
        | ExtendChain of chain * action * predicate (* Add a new rule to a chain *)
        | CreateChain of string;; (* name of chain to create *)

    let format_command lvl cmd =
        open_vbox lvl;
        print_space ();
        let next = lvl + 1 in
        match cmd with
        | ExtendChain (c, a, p) ->
            print_string "ExtendChain";
            print_space ();
            print_string "(";
            format_chain next c;
            print_string ",";
            format_action next a;
            print_string ",";
            format_predicate next p;
            print_string ")";
        | CreateChain name ->
            print_string ("CreateChain (" ^ name ^ ")");
        close_box ();
        print_newline ();;

    type cmd_on_switch = CmdOnSwitch of command * NetCore.switchId;;

    (** Convert command structure to corresponding iptables code *)
    let iptables_of_command (CmdOnSwitch (com, sw)) =
        match com with
        | CreateChain chName -> "iptables -N " ^ chName
        | ExtendChain (c, a, p) ->

            let chainS = match c with
                | Input -> "INPUT"
                | Output -> "OUTPUT"
                | User s -> s

            and predS = match p with
                | Srcip ip -> "-s " ^ MiscUtil.string_of_ip ip
                | NotSrcip ip -> "-!s " ^ MiscUtil.string_of_ip ip
                | Dstip ip -> "-d " ^ MiscUtil.string_of_ip ip
                | NotDstip ip -> "!-d " ^ MiscUtil.string_of_ip ip
                | Srceth eth -> "--mac-source " ^ MiscUtil.string_of_eth eth
                | NotSrceth eth -> "!--mac-source " ^ MiscUtil.string_of_eth eth
                | Protocol pro -> "-p " ^ string_of_int pro
                | NotProtocol pro -> "!-p " ^ string_of_int pro
                | Srcport port -> "--sport " ^ string_of_int port
                | NotSrcport port -> "!--sport " ^ string_of_int port
                | Dstport port -> "--dport " ^ string_of_int port
                | NotDstport port -> "!--dport " ^ string_of_int port
                | Always -> ""
                | Never -> "-s 0.0.0.0" (*hacky*)

            and actionS = match a with
                | Accept -> "-j ACCEPT"
                | Drop -> "-j DROP"
                | Jump ch -> match ch with
                    | Input -> "INPUT"
                    | Output -> "OUTPUT"
                    | User s -> s

            in "iptables -A " ^ chainS ^ " " ^ predS ^ actionS ^ "\n";;

    let iptables_of_commands cmds =
        String.concat "" (List.map iptables_of_command cmds);;
        
end;;