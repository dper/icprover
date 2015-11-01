#use "parser.ml";;

(*
	Reads in a text file.
	Invariants: The file is in the current directory.
	@param filename The name of the file.
*)
let read_file (filename:string):string list =
	let lines = ref [] in
	let chan = open_in filename in
	try
		while true; do
			lines := input_line chan :: !lines
		done; !lines
	with End_of_file ->
		close_in chan;
		List.rev !lines;;

let failures_to_string failures =
	List.fold_right 
		(fun (q, _) s -> (s ^ "\n" ^ Question.to_string q)) failures ""

(*
	Returns true iff all questions in the file are proven.
	Invariants: The file must exist and be readable.
	@param search_type The search type.
	@param filename The file name.
*)
let prove search (search_type:Proof.search_type) (filename:string):bool =
	let lines = read_file filename in
	let questions = List.map parse_question lines in
	let results = List.map 
	  (fun q -> 
		let _ = print_endline ("Trying to prove " ^ Question.to_string q) in
	  (q, search search_type q)) questions
  in
	let total_count = List.length results in
	let failures = List.filter (fun (_, po) -> po = None) results in
	let success_count = (List.length results) - (List.length failures) in
	let _ =
		if failures = [] then () else
		print_endline (failures_to_string failures) in
	let _ = print_endline (filename ^ ": Proved: " ^ 
		(string_of_int success_count) ^ "/" ^ 
		(string_of_int total_count) ^ ".") in
	true
;;

(*
	Returns true iff none of the questions in the file are proven.
	Invariants: The file must exist and be readable.
	@param search_type The search type.
	@param filename The file name.
*)
let do_not_prove search (search_type:Proof.search_type) (filename:string):bool =
	let lines = read_file filename in
	let questions = List.map parse_question lines in
	let results = List.map 
	  (fun q -> 
		  let _ = print_endline ("Trying not to prove " ^ Question.to_string q) in
		  (q, search search_type q)) questions
		in
	let proven = List.filter (fun (_, po) -> po != None) results in
	let _ =
		if proven == [] then () else
		print_endline (failures_to_string proven) in
	let _ = print_endline (filename ^ ": Did not prove: " ^ 
		(string_of_int (List.length results - List.length proven)) ^ "/" ^ 
		(string_of_int (List.length results)) ^ ".") in
	true
;;

prove Proof.search Proof.Intuitionistic "positive-intuition.txt";;
prove Proof.search Proof.Classical "positive-classical.txt";;
do_not_prove Proof.search Proof.Intuitionistic "negative-intuition.txt";;
do_not_prove Proof.search Proof.Classical "negative-classical.txt";;
