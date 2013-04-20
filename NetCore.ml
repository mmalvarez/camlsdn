(* Mario Alvarez - March 2013 *)
(* Implementation of language for defining SDN settings *)

(* Modified from Frenetic/Desmoines' implementation of Netcore *)
(* open Packet
open OpenFlow0x04Types
open Platform0x04 *)

(* lvl options are how deep into the tree of "boxes" you are*)
open Format;;
open Token;;
open Hashtbl;;
open MiscUtil;;

module NetCore = struct
    type packet = int64;; (*gross; for now, to get it working *)
    type switchId = int;;
    type portId = int32;;
    type get_packet_handler = switchId -> portId -> packet -> unit;;

    (* Constant - prefix to chain names *)
    let chainPrefix = "camlsdn_ ";;

    (* Netcore, without "stacking" functionality *)
    type predicate =
        | And of predicate * predicate
        | Or of predicate * predicate
        | Not of predicate
        | All
        | NoPackets
        (*| Switch of switchId*) (*perhaps someday*)
        (* InPort?? *)
        | DlType of int (** 8 bits *)
        | DlSrc of Int64.t (** Match 48 bit Ethernet src addr *)
        (*| DlDst of Int64.t*) (** Match 48 bit Ethernet dest addr *)
        (* apparently iptables can't filter on destination mac *)
        | SrcIP of Int32.t
        | DstIP of Int32.t
        | TcpSrcPort of int (** 16 bit tcp source port *)
        | TcpDstPort of int;; (** 16 bit tcp dest port *)

    (** Pretty-print a predicate *)
    let rec format_predicate lvl pred =
        open_vbox lvl;
        print_space ();
        let next = lvl + 1 in
        match pred with
        | And (p1, p2) -> 
            print_string "And";
            print_space ();
            print_string "(";
            format_predicate next p1;
            print_string ",";
            format_predicate next p2;
            print_string ")";
        
        | Or (p1, p2) ->
            print_string "Or";
            print_space ();
            print_string "(";
            format_predicate next p1;
            print_string ",";
            format_predicate next p2;
            print_string ")";

        | Not p ->
            print_string "Not";
            print_space ();
            print_string "(";
            format_predicate next p;
            print_string ")";

        | All ->
            print_string "All";
        | NoPackets ->
            print_string "NoPackets";
        | DlType dlt ->
            print_string ("DlType (" ^ string_of_int dlt ^ ")");
        | DlSrc dls ->
            print_string ("DlSrc (" ^ Int64.to_string dls ^ ")");
        | SrcIP saddr ->
            print_string ("SrcIP (" ^ MiscUtil.string_of_ip saddr ^ ")");
        | DstIP daddr ->
            print_string ("DstIP (" ^ MiscUtil.string_of_ip daddr ^ ")");
        | TcpSrcPort sport ->
            print_string ("TcpSrcPort (" ^ string_of_int sport ^ ")");
        | TcpDstPort dport ->
            print_string ("TcpDstPort (" ^ string_of_int dport ^ ")");

        close_box ();
        print_newline ();;

    (*type action =
        | To of portId (** [To mods n] sends packets to port [n] *)
        | ToAll (** Flood packet out all ports *)
        | GetPacketHandler of get_packet_handler;; (** Call into provided handler *)
    *)
    

    (** Pretty-print an action *)
    (*let format_action lvl act =
        open_vbox lvl;
        print_space ();
        match act with
        | To pid ->
            print_string "To (" ^ string_of_int pid ^ ")";
        | ToAll -> 
            print_string "ToAll";
        | GetPacket _ -> 
            "GetPacket ([handler])";

        close_box ();
        print_newline ();
    *)

    (*type policy =
        | Action of action (* instead of pol *)
        | Par of policy * policy (** parallel composition *)
        | Restrict of policy * predicate;;
    *)

    (** Pretty-print a policy *)
    (*let rec format_policy lvl pol =
        open_vbox lvl;
        print_space ();
        let next = lvl + 1 in
        match pol Action
        | with act ->
            print_string "Action";
            print_space ();
            print_string "(";
            format_action next act;
            print_string ")";

        | Par (p1, p2) ->
            print_string "Par";
            print_space ();
            print_string "(";
            format_policy next p1;
            print_string ",";
            format_policy next p2;
            print_string ")"; 

        | Restrict (pol, pred) ->
            print_string "Restrict";
            print_space ();
            print_string "(";
            format_policy next pol;
            print_string ",";
            format_predicate next pred;
            print_string ")";

        close_box ();
        print_newline ();
    *)

    (** iptables chains. We do not expose the "user-chain" feature
        to the user of the language *)
    (** TODO - maybe add more chains? *)
    (** TODO - do not expose User *)
    type chain = Input | Output | User of string;;

    let format_chain lvl chn =
        open_vbox lvl;
        print_space ();
        match chn with
            | Input -> print_string "Input";
            | Output -> print_string "Output";
            | User s -> print_string ("User (" ^ s ^ ")");
        close_box ();
        print_newline ();;

    (* Simpler set of actions (HTables-style), to get things working *)
    (* TODO - add Jump as an action, but do not allow users to use it! *)
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

    (* Simplified policies, HTables-style *)
    type policy = Policy of chain * action * predicate * switchId;;

    let format_policy lvl (Policy (c, a, p, s)) =
        let next = lvl + 1 in
        open_vbox lvl;
        print_space ();
        print_string "Policy";
        print_space ();
        print_string "(";
        format_chain next c;
        print_string ",";
        format_action next a;
        print_string ",";
        format_predicate next p;
        print_string ",";
        print_string ("SwitchId (" ^ string_of_int s ^ ")"); 
        print_space ();
        print_string ")";
        close_box ();
        print_newline ();;

    (** Process to turn NetCore policy
        Into Tokens that can be then turned into iptables code 
        (see Token.ml)*)
    (** Recursive helper.
        We create user chains. next stores next
        available chain name. *)
    let rec _tokens_of_policy (Policy (c, a, p, s)) next =
        (* translate chain, action, switch *)
        let ctrans = match c with
            | Input -> Token.Input
            | Output -> Token.Output
            | User name -> Token.User name

        and atrans = match a with
            | Accept -> Token.Accept
            | Drop -> Token.Drop
            | Jump ch -> match ch with
                | Input -> Token.Jump Token.Input
                | Output -> Token.Jump Token.Output
                | User name -> Token.Jump (Token.User name) in
     
        match p with
        (** Sequence three entire chains, by creating user-chains
            and jumping between them. Only if all are met do we accept *)

        | And (p1, p2) -> 
            (* create chains *)
            let chainName1 = chainPrefix ^ string_of_int !next in
            let createChain1 = [Token.CreateChain chainName1] in
            incr next;
            let chainName2 = chainPrefix ^ string_of_int !next in
            let createChain2 = [Token.CreateChain chainName2] in
            incr next;
            
            (* "call into" first chain *)
            let jump = 
                [Token.ExtendChain 
                    (ctrans, Token.Jump (Token.User chainName1), Token.Always)] in
            
            (* first predicate goes into first chain; jump to second chain *)
            let chain1 = _tokens_of_policy 
                        (Policy (User chainName1, Jump (User chainName2), p1, s))
                        next in

            (* second predicate goes into second chain, triggers action  *)
            let chain2 = _tokens_of_policy
                        (Policy (User chainName2, a, p2, s))
                        next in

            createChain1 @ createChain2 @ jump @ chain1 @ chain2;

        (* Or is merely sequencing *)
        | Or (p1, p2) -> 
            let tokens1 = _tokens_of_policy (Policy (c, a, p1, s)) next in
            let tokens2 = _tokens_of_policy (Policy (c, a, p2, s)) next in
            tokens1 @ tokens2

        (* Cases for Not *)
        | Not p' ->
            begin
                match p' with
                (* DeMorgan *)
                | And (p1, p2) ->
                    _tokens_of_policy (Policy (c, a, (Or (Not p1, Not p2)), s)) next
                | Or (p1, p2) ->
                    _tokens_of_policy (Policy (c, a, (And (Not p1, Not p2)), s)) next
                (* cancellation *)
                | Not p'' ->
                    _tokens_of_policy (Policy (c, a, p'', s)) next
                (* Invert "primitives" *)
                | _ -> 
                    begin
                        let negated = match p' with
                            | All -> Token.Never
                            | NoPackets -> Token.Always
                            | SrcIP i -> Token.NotSrcip i
                            | DstIP i -> Token.NotDstip i
                            | DlType i -> Token.NotProtocol i
                            | DlSrc i -> Token.NotSrceth i
                            | TcpSrcPort i -> Token.NotSrcport i
                            | TcpDstPort i -> Token.NotDstport i
                            | _ -> raise MiscUtil.BadIncompletePattern
                        in [Token.ExtendChain (ctrans, atrans, negated)]
                    end
            end

        (* "primitives" *)
        | _ -> 
            begin
                let token = match p with
                    | All -> Token.Always
                    | NoPackets -> Token.Never
                    | SrcIP i -> Token.Srcip i
                    | DstIP i -> Token.Dstip i
                    | DlType i -> Token.Protocol i
                    | DlSrc i -> Token.Srceth i
                    | TcpSrcPort i -> Token.Srcport i
                    | TcpDstPort i -> Token.Dstport i
                    | _ -> raise MiscUtil.BadIncompletePattern
                in [Token.ExtendChain (ctrans, atrans, token)]
            end;;

    (* Called by client to convert policy to tokens *)
    let tokens_of_policy pol =
        match pol with
        (* having the chain number be mutable makes this not horrible *)
        | Policy (_, _, _, sw) ->
            (sw, _tokens_of_policy pol (ref 0));;

    (*module Make : functor (Platform : PLATFORM) -> sig
        val start_controller : (policy * policy * policy) Lwt_stream.t -> unit Lwt.t
    end;;*)
end;;