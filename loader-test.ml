(* Returns true iff all questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
*)
let load_file (file:string) =
	let ql = Loader.load file in
	let qll = List.length ql in
	if (qll = 4) then print_endline "Success: Read 4 of 4."
	else print_endline ("Failure: Read " ^ (string_of_int qll) ^ " of 4.")
;;

load_file "questions.txt";;
