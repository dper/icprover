(* Tests the automated theorem prover. *)

#use "prover.ml";;
#load "str.cma";;

(*
	Parses a string of a question.  For example, (A & B), C |- D.

	Useful string functions documented here:
	http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
*)
module Parser = struct
	(*
		Parses a string and returns a formula.
		Invariants: The string must be a well-formed formula.
		@param s The string to be parsed.
	*)
	let parseFormula (s:string):Formula.formula =
		let s = String.trim s in
		print_endline s ;
		Formula.Bottom	

	(*
		Parses a context string and returns the formulae.
		Invariants: The string must be comma-separated formula strings.
		@param s The string to be parsed.
	*)
	let parseContext (s:string):Formula.formula list =
		let fs = Str.split (Str.regexp ",") s in
		List.map parseFormula fs

	(*
		Parses a string and returns a formula for it.
		Internally, we switch to single-character operators.
		White space is ignored.  
		Invariants: The string must be a well-formed formula.
		@param s The string to be parsed.
	*)
	let parseQuestion (s:string):Question.question =
		let s = Str.global_replace (Str.regexp " ") "" s in
		let s = Str.global_replace (Str.regexp "|-") "⊢" s in
		let s = Str.global_replace (Str.regexp "<->") "↔" s in
		let s = Str.global_replace (Str.regexp "->") "→" s in
		match Str.split (Str.regexp "⊢") s with
		| g :: [] -> ([], parseFormula g)
		| c :: g :: [] -> (parseContext c, parseFormula g)
		| _ -> failwith ("Invalid question: " ^ s)
end
;;

Parser.parseQuestion "A <-> D, F -> G, C |- B";;
print_endline "Printing.";;
