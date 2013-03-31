(* Mario Alvarez - March 2013 *)
(* Implements data types and methods *)
(* Used internally in the compiler *)
(* Based on token language from HTables *)

module Token = struct
	type pred =
		| Srcip of Int32.t
		| NotSrcip of Int32.t
		| Dstip of Int32.t
		| NotDstip of Int32.t
		| Protocol of int
		| NotProtocol of int
		| Srcport of int
		| NotSrcport of int
		| Dstport of int
		| NotDstport of int
		| Always
		| Never;;

	type 

	(* ability to compile internal language down to iptables *)
	(* internal language is what actually gets marshalled *)
	val toIpTables : policy -> pred;;

	
	
		
end;;