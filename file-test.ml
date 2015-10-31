let failures_to_string failures =
	List.fold_right 
		(fun (q, _) s -> (s ^ "\n" ^ Question.to_string q)) failures ""

(* Returns true iff all questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
  @param search_type The search type.
*)
let prove search search_type file =
	let ql = Loader.load file in
	let results = List.map 
	  (fun q -> 
		let _ = print_endline ("Trying to prove " ^ Question.to_string q) in
	  (q, search search_type q)) ql 
  in
	let total_count = List.length results in
	let failures = List.filter (fun (_, po) -> po = None) results in
	let success_count = (List.length results) - (List.length failures) in
	let _ =
		if failures = [] then () else
		print_endline (failures_to_string failures) in
	let _ = print_endline (file ^ ": Proved: " ^ 
		(string_of_int success_count) ^ "/" ^ 
		(string_of_int total_count) ^ ".") in
	true
;;

(* Returns true iff none of the questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
	@param search_type The search type.
*)
let do_not_prove search search_type file =
	let ql = Loader.load file in
	let results = List.map 
	  (fun q -> 
		  let _ = print_endline ("Trying not to prove " ^ Question.to_string q) in
		  (q, search search_type q)) ql 
		in
	let proven = List.filter (fun (_, po) -> po != None) results in
	let _ =
		if proven == [] then () else
		print_endline (failures_to_string proven) in
	let _ = print_endline (file ^ ": Did not prove: " ^ 
		(string_of_int (List.length results - List.length proven)) ^ "/" ^ 
		(string_of_int (List.length results)) ^ ".") in
	true
;;


prove Proof.search Proof.Intuitionistic "positive-intuition.txt";;
prove Proof.search Proof.Classical "positive-classical.txt";;
do_not_prove Proof.search Proof.Intuitionistic "negative-intuition.txt";;
do_not_prove Proof.search Proof.Classical "negative-classical.txt";;
