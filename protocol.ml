(* Mario Alvarez - March 2013 *)
(* Implementation of client-server communication system *)

module Protocol = struct
	(* Types  *)
	type cli2servmsg = Hello | Foo of int;;
	type serv2climsg = Ack | Bar of int;;
	
	
		
	(* Hello/goodbye/status *)
	(* Message contents *)
	
	(* Utilities (e.g. marshalling) *)

end;;