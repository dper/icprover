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
		Returns a list of chars for a string.
		Invariants: none.
		@param s The string to explode.
	*)
	let explode (s:string):char list =
		let rec exp i l =
			if i < 0 then l
			else exp (i - 1) (s.[i] :: l)
		in
			exp (String.length s - 1) []

	(*
		Returns true if the formula string has extra outer parentheses.
		Invariants: The string represents a formula.
		@param s The string to check.
	*)
	let extraOuterParentheses (s:string):bool = 
		(*
			If we find a closing parenthesis that matches the
			opening parenthesis, stop.  If it's the final
                        character, these two are extra.
		*)
		let rec scan (l:char list) (c:int):bool = 
			match l with
			| '(' :: t  -> scan t (c + 1)
			| ')' :: [] -> c == 0
			| ')' :: t  -> c > 1 && scan t (c - 1)
			| x :: t    -> scan t c
			| _         -> false
				
		in
		match (explode s) with
		| []       -> false
		| _ :: []  -> false
		| '(' :: t -> scan t 1
		| _        -> false

	(*
		Returns the formula string with extra outer parentheses removed.
		If none existed, the original string is returned.
		Invariants: The string represents a formula.
		@param s The string to strip.
	*)
	let removeExtraOuterParentheses (s:string):string =
		if extraOuterParentheses s then
			String.sub s 1 (String.length s - 2)
		else
			s

	(*
		Returns Some p where p is the position of the main
		binary operator, or None if none exists.  A binary
		operator is the main operator if it's sitting anywhere
		in the formula not contained in parentheses.
	*)
	let findMainBinaryOperator (s:string):int option =
		(* TODO *)
		None

	(*
		TODO
		- Find the main binary operator.
		- Split on it.
		- Handle negations.
	*)

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
