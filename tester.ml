(* Tests the automated theorem prover. *)

#use "prover.ml";;
#load "str.cma";;

(* Parses a string of a question.
   The format should look like (A & B) |- G. *)
module Parser = struct

	let splitQuestion (s:string) =
		Str.split (Str.regexp "|-") s

	let parse (s:string):Question.question =
		let context = [] in
		let goal = Formula.Bottom in
			(context, goal)

end

print_endline "What?"
