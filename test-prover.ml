(* Tests the automated theorem prover. *)

#use "prover.ml";;
#load "str.cma";;

(*
	Parses a string of a question.  For example, (A & B), C |- D.

	Useful string functions documented here:
	http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
	http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html
*)
module Parser = struct
	(*
		Returns Some p where p is the position of the main
		binary operator, or None if none exists.  A binary
		operator is the main operator if it's sitting anywhere
		in the formula not contained in parentheses.
	*)
	let findMainBinaryOperator (s:string):int option =
		(* TODO *)
	end

	(*
		Returns a string without extraneous outer parentheses.
		If such parentheses existed, they are removed.
		If they didn't exist, the original string is returned.
		Invariants: none.
		@param s The string to strip.
	*)
	let removeOuterParentheses (s:string):s = 
		(* TODO *)
	end

	(*
		Parses a string and returns a formula.
		1. Handle atomic formulae.
		2. If there's a top-level binary operator, handle it.
		3. Handle negations.
		Invariants: The string must be a well-formed formula.
		@param s The string to be parsed.
	*)
	let parseFormula (s:string):Formula.formula =
		print_endline s ;

		(* A single-character string is atomic. *)
		if String.length s == 1 then
			Formula.Atomic s.[0]
		else
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
