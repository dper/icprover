(* Tests the automated theorem prover. *)

#use "prover.ml";;
#load "str.cma";;

(*
	Parses a string of a question.
	The format should look like (A & B) |- G.

	Useful string functions documented here:
	http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
*)
module Parser = struct
	let parseFormula (s:string):Formula.formula =
		let s = String.trim s in
		print_endline s ;
		Formula.Bottom	

	let parseContext (s:string):Formula.formula list =
		let fs = Str.split (Str.regexp ",") s in
		List.map parseFormula fs

	let parseQuestion (s:string):Question.question =
		match Str.split (Str.regexp "|-") s with
		| c :: g :: [] -> (parseContext c, parseFormula g)
		| _ -> failwith ("Invalid question: " ^ s)
end
;;

Parser.parseQuestion "A, C |- B";;
print_endline "Printing.";;
