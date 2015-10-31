(** Loads files of questions into lists. *)

open Prover

(** Reads a text file and makes it a string list.
	- The file to load must be readable.
	@param filename The file to load.
*)
let file_to_string (filename:string):string list =
	let ch = open_in filename in
	let rec loop accum =
		let l = input_line ch in
		accum := l::(!accum);
		loop accum
	in
	let accum = ref [] in
	try
		loop accum
	with 
		End_of_file -> !accum

(** 
	@param filename The file to load.
*)
let load (filename:string):(Question.question list) =
	let s = String.concat "\n" (file_to_string filename) in
	let lexbuf = Lexing.from_string s in
	Parser.pqs Lexer.lexer lexbuf
