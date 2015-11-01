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
		Returns a string for a list of chars.
		Invariants: none.
		@param l The list to implode.
	*)
	let rec implode (l:char list):string =
		match l with
		| []     -> ""
		| c :: t -> (Char.escaped c) ^ (implode t)

	(*
		Returns true if the formula string has extra outer parentheses.
		Invariants: The string represents a formula.
		@param s The string to check.
	*)
	let extra_outer_parentheses (s:string):bool = 
		(*
			If we find a closing parenthesis that matches the
			opening parenthesis, stop.  If it's the final
                        character, these two are extra.
			@param l The remaining characters to examine.
			@param d The depth of the parentheses.
		*)
		let rec scan (l:char list) (d:int):bool = 
			match l with
			| '(' :: t  -> scan t (d + 1)
			| ')' :: [] -> d == 0
			| ')' :: t  -> d > 1 && scan t (d - 1)
			| x :: t    -> scan t d
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
	let remove_extra_outer_parentheses (s:string):string =
		if extra_outer_parentheses s then
			String.sub s 1 (String.length s - 2)
		else
			s

	(*
		Returns Some p where p is the position of the main
		binary operator, or None if none exists.  A binary
		operator is the main operator if it's sitting anywhere
		in the formula not contained in parentheses.
	*)
	let find_main_binary_operator (s:string):int option =
		(*
			If we find a binary symbol at top level, return its index.
			@param l The remaining characters.
			@param d The depth of the parentheses.
			@param p The current index.
		*)
		let rec scan (l:char list) (d:int) (p:int) =
			match l with
			| []       -> None
			| '(' :: t -> scan t (d + 1) (p + 1)
			| ')' :: t -> scan t (d - 1) (p + 1)
			| '&' :: t -> if d == 0 then Some p else scan t d (p + 1)
			| 'v' :: t -> if d == 0 then Some p else scan t d (p + 1)
			| '=' :: t -> if d == 0 then Some p else scan t d (p + 1)
			| '>' :: t -> if d == 0 then Some p else scan t d (p + 1)
			| _ :: t   -> scan t d (p + 1)
		in
		match (explode s) with
		| []      -> None
		| _ :: [] -> None
		| l       -> scan l 0 0

	(*
		Parses a string and returns a formula.
		1. Handle atomic formulae.
		2. If there's a top-level binary operator, handle it.
		3. Handle negations.
		Invariants: The string must be a well-formed formula.
		@param s The string to be parsed.
	*)
	let rec parse_formula (s:string):Formula.formula =
		(* Strip unnecessary outer parentheses. *)
		let s = remove_extra_outer_parentheses s in

		(* A single-character string is atomic. *)
		if String.length s == 1 then
			match s.[0] with
			| 'A' .. 'Z' as c -> Formula.Atomic c
			| _               -> failwith ("Expecting a capital letter: " ^ s)
		else
			match find_main_binary_operator s with
			| Some p -> parse_binary s p
			| None -> parse_negation s

	(*
		Parses a string of a formula where the negation is the main operator.
		Invariants: s is a formula string with a leading negation.
		@param s The string to be parsed.
	*)
	and parse_negation (s:string):Formula.formula =
		match (explode s) with
		| '~' :: t -> Formula.Negation (parse_formula (implode t))
		| _        -> failwith ("Expecting a negation: " ^ s)

	(*
		Parses a string where the main operator is binary and returns a formula.
		Invariants: s is a formula string with a main binary operator at p.
		@param s The string to be parsed.
		@param p The offset of the operator.
	*)
	and parse_binary (s:string) (p:int):Formula.formula =
		Formula.Bottom

	(*
		Parses a context string and returns the formulae.
		Invariants: The string must be comma-separated formula strings.
		@param s The string to be parsed.
	*)
	let parse_context (s:string):Formula.formula list =
		let fs = Str.split (Str.regexp ",") s in
		List.map parse_formula fs

	(*
		Parses a string and returns a formula for it.
		Internally, we switch to single-character operators.
		White space is ignored.  
		Invariants: The string must be a well-formed formula.
		@param s The string to be parsed.
	*)
	let parse_question (s:string):Question.question =
		let s = Str.global_replace (Str.regexp " ") "" s in
		let s = Str.global_replace (Str.regexp "|-") "⊢" s in
		let s = Str.global_replace (Str.regexp "<->") "=" s in
		let s = Str.global_replace (Str.regexp "->") ">" s in
		match Str.split (Str.regexp "⊢") s with
		| g :: [] -> ([], parse_formula g)
		| c :: g :: [] -> (parse_context c, parse_formula g)
		| _ -> failwith ("Invalid question: " ^ s)
end
;;

print_endline (Question.to_string (Parser.parse_question "~~A <-> B |- C"));;
