(* Mario Alvarez - March 2013 *)
(* Implementation of language for defining SDN settings *)

(* Based on Netcore *)
(* open Packet
open OpenFlow0x04Types
open Platform0x04 *)

type packet = int64 (*gross; for now, to get it working *)
type switchId = int64
type portId = int32
type get_packet_handler = switchId -> portId -> packet -> unit

(* Netcore, without "stacking" functionality *)
type predicate =
	| And of predicate * predicate
	| Or of predicate * predicate
	| Not of predicate
	| All
	| NoPackets
	| Switch of switchId
	| InPort of portId
	| DlType of int (** 8 bits *)
	| DlSrc of Int64.t (** Match 48 bit Ethernet src addr *)
	| DlDst of Int64.t (** Match 48 bit Ethernet dest addr *)
	| SrcIP of Int32.t
	| DstIP of Int32.t
	| TcpSrcPort of int (** 16 bit tcp source port *)
	| TcpDstPort of int;; (** 16 bit tcp dest port *)

type action =
	| To of portId (** [To mods n] sends packets to port [n] *)
	| ToAll (** Flood packet out all ports *)
	| GetPacket of get_packet_handler;; (** Call into provided handler *)

type policy =
	| Action of action (* instead of pol *)
	| Par of policy * policy (** parallel composition *)
	| Restrict of policy * predicate;;

(*let rec string_of_policy pol =
  match pol with
  | Action act = 
    "Action: " ^ string_of_action act
  | Par (pol1, pol2) = 
    "Parallel: " ^ string_of_policy pol1 ^
    "\nand " ^ string_of_policy pol2
  | Restrict (pol, pred) =
    "Restrict: Policy " ^ string_of_policy pol ^
    "\n By Predicate " ^ string_of_predicate pred;;
*)

let string_of_policy pol =
  match pol with
  | Action _ -> "Action!"
  | Par _ -> "Parallel!"
  | Restrict _ -> "Restrict!";;
	
(*module Make : functor (Platform : PLATFORM) -> sig
	val start_controller : (policy * policy * policy) Lwt_stream.t -> unit Lwt.t
end;;*)