(*
 * Copyright 2007, 2008, Douglas Perkins.
 * 
 * This file is part of ICProver.
 * 
 * ICProver is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * ICProver is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with ICProver.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* A simple implementation of formulas. *) 
module Formula = struct
	type formula =
	| Bottom
	| Atomic of char
	| Negation of formula
	| Conjunction of formula * formula
	| Disjunction of formula * formula
	| Implication of formula * formula
	| Biconditional of formula * formula
	;;

	let rec equals (f1:formula) (f2:formula):bool =
		match (f1,f2) with
		| (Bottom, Bottom) -> true
		| (Atomic f1l, Atomic f2l) -> f1l= f2l
		| (Negation f1s, Negation f2s) -> equals f1s f2s
		| (Conjunction (f1l, f1r), Conjunction (f2l, f2r)) ->
			equals f1l f2l && equals f1r f2r 
		| (Disjunction (f1l, f1r), Disjunction (f2l, f2r)) ->
			equals f1l f2l && equals f1r f2r 
		| (Implication (f1l, f1r), Implication (f2l, f2r)) ->
			equals f1l f2l && equals f1r f2r
		| (Biconditional (f1l, f1r), Biconditional (f2l, f2r)) ->
			equals f1l f2l && equals f1r f2r
		| _ -> false
  
	let rec to_string (f:formula):string =
	  match f with
	  | Bottom               -> "_|_"
	  | Atomic s             -> Char.escaped s
	  | Negation s           -> "~" ^ (to_string s)
	  | Conjunction (l, r)   -> "(" ^ (to_string l) ^ "&" ^ (to_string r) ^ ")"
	  | Disjunction (l, r)   -> "(" ^ (to_string l) ^ "|" ^ (to_string r) ^ ")"
	  | Implication (l, r)   -> "(" ^ (to_string l) ^ "->" ^ (to_string r) ^ ")"
	  | Biconditional (l, r) -> "(" ^ (to_string l) ^ "<->" ^ (to_string r) ^ ")"
end

(* The context for a step in a proof search. *)
module Context = struct
	type context = Formula.formula list;;

	let empty ():context = []

	(**
		@param c The context to extend.
		@param n The formula to add.
	*)
	let extend (c:context) (n:Formula.formula):context =
		if (List.exists (fun f -> (Formula.equals n f)) c) then c else n::c

	let join (c1:context) (c2:context) =
		List.fold_right (fun f c -> extend c f) c2 c1

	let exists (c:context) (f:Formula.formula):bool =
		List.exists (Formula.equals f) c

	(**
		@param p The parent context.
		@param c The sub-context.
	*)
	let subcontext (p:context) (c:context):bool = 
		not (List.exists (fun x -> not (exists c x)) p)

	let to_string (c:context):string = 
		match c with
		| []    -> "[]"
		| f::[] -> "[" ^ (Formula.to_string f) ^ "]"
		| f::l  -> "[" ^ (Formula.to_string f) ^
			(List.fold_right (fun f s -> s ^ ", " ^ Formula.to_string f) l "") ^ "]"
end

(* Inference rules. *)
module Rule = struct
	type rule = 
  	| Pr 
	| Ex 
	| Ca 
	| Ai 
	| Vl 
	| Vr 
	| Ii 
	| Bi 
	| Ni 
	| Cn 
	| Bo 
	| Ef 
	;;

	let to_string (r:rule):string =
		match r with
		| Pr -> " Pre"
		| Ex -> " Ext"
		| Ca -> "  vE"
		| Ai -> "  &I"
		| Vl -> " vIL"
		| Vr -> " vIR"
		| Ii -> " ->I"
		| Bi -> "<->I"
		| Ni -> "  ~I"
		| Cn -> "  ~C"
		| Bo -> " _|_"
  		| Ef -> " EFQ"
end

(* An extraction branch.
   A series of goal-directed forward eliminations. *)
module Extraction = struct
	open Formula;;

	type step = L of formula | R of formula | End of formula
	type branch = step list

	(**
		@param p The premise of the extraction.
		@param g The goal of the extraction.
	*)
	let find_normal_branches (p:formula) (g:formula):(branch list) =
		if (Formula.equals g Bottom) then [] else
  	    	let rec find_extractions_local (c:formula) (e:branch):(branch list) =
			if (Formula.equals c g) then [List.rev (End g::e)]
			else
				match c with
				| Bottom -> []
				| Atomic _ -> []
				| Negation _ -> []
				| Implication (_, r) -> find_extractions_local r (R c::e)
				| Conjunction (l, r) -> 
					(find_extractions_local l (L c::e)) @ 
					(find_extractions_local r (R c::e))
				| Disjunction (l, r) -> 
					(find_extractions_local l (L c::e)) @ 
					(find_extractions_local r (R c::e))
				| Biconditional (l, r) -> 
					(find_extractions_local l (L c::e)) @
					(find_extractions_local r (R c::e))
		in
			find_extractions_local p []

	(**
		@param p The premise of the extraction.
	*)
	let find_cases_branches (p:formula):(branch list) =
		let rec find_cases_local (c:formula) (e:branch):(branch list) =
			match c with
			| Disjunction _      -> [List.rev (End c::e)]
			| Bottom             -> []
			| Atomic _           -> []
			| Negation _         -> []
			| Implication (_, r) -> find_cases_local r (R c::e)
			| Conjunction (l, r) -> 
				(find_cases_local l (L c::e)) @
				(find_cases_local r (R c::e))
			| Biconditional (l, r) -> 
				(find_cases_local l (L c::e)) @
				(find_cases_local r (R c::e))
		in
			find_cases_local p []

	(**
		@param p The premise of the extraction.
	*)
	let find_bottom_branches (p:formula):(branch list) =
		let rec find_contra_local (c:formula) (e:branch):(branch list) =
			match c with
			| Negation _         -> [List.rev (End c::e)]
			| Bottom             -> []
			| Atomic _           -> []
			| Implication (_, r) -> find_contra_local r (R c::e)
			| Conjunction (l, r) -> 
				(find_contra_local l (L c::e)) @
				(find_contra_local r (R c::e))
			| Disjunction (l, r) -> 
				(find_contra_local l (L c::e)) @
				(find_contra_local r (R c::e))
			| Biconditional (l, r) ->
				(find_contra_local l (L c::e)) @
				(find_contra_local r (R c::e))
  	in
  		find_contra_local p []

	let find_normal_questions (e:branch):(Context.context * formula) list =
		if e = [] then failwith "Invalid branch: []." else
		let goal = match List.nth e ((List.length e) - 1) with
			| End f -> f
			| _ -> failwith "Branch must end in End f."
		in
		let rec find_questions_in_context c e =
			match e with
			| (End _)::[] -> []
			| (L (Conjunction _))::el -> 
				find_questions_in_context c el
			| (R (Conjunction _))::el -> 
				find_questions_in_context c el
			| (L (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_r, goal)::(find_questions_in_context c_l el) 
			| (R (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_l, goal)::(find_questions_in_context c_r el)
			| (R (Implication (l, _)))::el -> 
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| (L (Biconditional (_, r)))::el ->
				let c_new = Context.extend c r in
				(c, r)::(find_questions_in_context c_new el)
			| (R (Biconditional (l, _)))::el ->
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| _ -> failwith "Invalid branch."
		in
			find_questions_in_context (Context.empty ()) e

	(**
		@param e The branch, ending in a falsum.
	*)
	let find_bottom_questions (e:branch):(Context.context * formula) list =
		if e = [] then failwith "Invalid branch: []." else
		let goal = match List.nth e ((List.length e) - 1) with
			| End f -> f
			| _ -> failwith "Branch must end in End f."
		in
		let rec find_questions_in_context c e =
			match e with
			| (End Negation f)::[] -> [([], f)]
			| (L (Conjunction _))::el -> 
				find_questions_in_context c el
			| (R (Conjunction _))::el -> 
				find_questions_in_context c el
			| (L (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_r, goal)::(find_questions_in_context c_l el) 
			| (R (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_l, goal)::(find_questions_in_context c_r el)
			| (R (Implication (l, _)))::el -> 
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| (L (Biconditional (_, r)))::el ->
				let c_new = Context.extend c r in
				(c, r)::(find_questions_in_context c_new el)
			| (R (Biconditional (l, _)))::el ->
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| _ -> failwith "Invalid branch."
		in
		find_questions_in_context (Context.empty ()) e

	(**
		@param g The goal of the extraction.
		@param e The branch, ending in g.
	*)
	let find_cases_questions g e =
		if e = [] then failwith "Invalid branch: []." else
		let rec find_questions_in_context c e =
			match e with
			| (End (Disjunction (l, r)))::[] -> 
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				[(c_l, g); (c_r, g)]
			| (L (Conjunction _))::el -> 
				find_questions_in_context c el
			| (R (Conjunction _))::el -> 
				find_questions_in_context c el
			| (L (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_r, g)::(find_questions_in_context c_l el) 
			| (R (Disjunction (l, r)))::el ->
				let c_l = Context.extend c l in
				let c_r = Context.extend c r in
				(c_l, g)::(find_questions_in_context c_r el)
			| (R (Implication (l, _)))::el -> 
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| (L (Biconditional (_, r)))::el -> 
				let c_new = Context.extend c r in
				(c, r)::(find_questions_in_context c_new el)	
			| (R (Biconditional (l, _)))::el -> 
				let c_new = Context.extend c l in
				(c, l)::(find_questions_in_context c_new el)
			| _ -> failwith "Invalid branch."
		in
		find_questions_in_context (Context.empty ()) e

	let to_string (e:branch):string =
	  	let rec to_string_local e =
			match e with 
			| End x::[] -> "E(" ^ (Formula.to_string x) ^ ")"
			| L x::l -> "L(" ^ (Formula.to_string x) ^ "), " ^ (to_string_local l)
			| R x::l -> "R(" ^ (Formula.to_string x) ^ "), " ^ (to_string_local l)
			| _ -> failwith "Illegal extraction; can't display."
		in
			"<" ^ to_string_local e ^ ">"
end

(* A question in a proof search.
   That is, the context and a goal to be proven. *)
module Question = struct
	open Formula;;

	type question = Context.context * Formula.formula;;
	type dependencies = question list * Rule.rule;;

	let to_string ((c,g):question):string = 
		(Context.to_string c) ^ " |- " ^ (Formula.to_string g)

	(* Returns the result of applying conjunction introduction.  Returns [] 
	   if the rule cannot be applied.
	   Invariants: none.
	   @param c The premises.
	   @param g The goal.
	*)
	let conjunction_intro ((c, g):question):(dependencies list) =
		match g with
		| Conjunction(l, r) -> [([(c, l); (c, r)], Rule.Ai)]
		| _ -> []

	(* Returns the results of applying disjunction introduction left and right.
	   Returns [] if the rule cannot be applied.
	   Invariants: none.
	   @param c The premises.
	   @param g The goal.
	*)
	let disjunction_intro ((c, g):question):(dependencies list) =
		match g with
  		| Disjunction(l, r) ->
    			let left:dependencies = ([(c, r)], Rule.Vl) in
    			let right:dependencies = ([(c, l)], Rule.Vr) in
    			[left; right]
  		| _ -> []

	(* Returns the result of applying implication introduction.  Returns
	   [] if the rule cannot be applied.
	   Invariants: none.
	   @param c The premises.
	   @param g The goal.
	*)
	let implication_intro ((c, g):question):(dependencies list) =
		match g with
		| Implication(l ,r) -> [([(Context.extend c l, r)], Rule.Ii)]
		| _ -> []

	(* Returns the result of applying biconditional introduction.  Returns
	   [] if the rule cannot be applied.
	   Invariants: none.
	   @param c The premises.
	   @param g The goal.
	*)
	let biconditional_intro ((c, g):question):(dependencies list) =
		match g with
		| Biconditional(l, r) ->
			[([(Context.extend c l, r); (Context.extend c r, l)], Rule.Bi)]
		| _ -> []

	(* Extends the context of dependencies.  This is needed for extraction, 
	   where the open questions initially have context information only from 
	   the branch itself.  This merges the search context (dc) with the 
	   extraction context. *)
	let extend_dep_context ((ql, r):dependencies) (dc:Context.context):dependencies =
		(List.map (fun (c,f) -> (Context.join dc c, f)) ql, r)
 
	(* Returns the extractions possible from this question.  If the
	   goal is a falsum, extraction cannot be used, so [] is returned.
	   Invariants: none.
	   @param c The premises.
	   @param g The goal.
 	*)
	let extraction ((c, g):question):dependencies list =
		if g = Formula.Bottom then [] else 
  		let branch_extraction (eb:Extraction.branch):dependencies =
			let qs = Extraction.find_normal_questions eb in
				extend_dep_context (qs, Rule.Ex) c
			in
			let premise_extraction (p:formula):dependencies list =
				let el = Extraction.find_normal_branches p g in
				List.fold_right (fun el dl -> (branch_extraction el)::dl) el []
		in
			List.fold_right (fun p dl -> (premise_extraction p) @ dl) c []

	(* Returns the cases possible from this question.
	   Invariants: none.
	   @param c The context.
	   @param g The goal.
	*)
	let cases ((c, g):question):(dependencies list) =
  		let branch_cases (eb:Extraction.branch):dependencies =
			let qs = Extraction.find_cases_questions g eb in
			    	extend_dep_context (qs, Rule.Ca) c
		  	in
		  	let premise_cases (p:formula):dependencies list =
			    	let cs = Extraction.find_cases_branches p in
			    	List.fold_right (fun el dl -> (branch_cases el)::dl) cs []
  		in
  			List.fold_right (fun p dl -> (premise_cases p) @ dl) c []

	(* Returns dependencies for the contradictory pairs.  Expands 
	   their extraction branches.  
	   Invariants: none.
	   @param c The context.
	   @param g The goal.
	*)
	let bottom ((c, g):question):dependencies list =
		if (not (Formula.equals Bottom g)) then [] else
  		let branch_questions (eb:Extraction.branch):dependencies = 
			let qs = Extraction.find_bottom_questions eb in
			extend_dep_context (qs, Rule.Ex) c
		in
		let premise_bottom (p:formula):dependencies list =
  		let cs = Extraction.find_bottom_branches p in
  		List.fold_right (fun el dl -> (branch_questions el)::dl) cs []
		in
		List.fold_right (fun p dl -> (premise_bottom p) @ dl) c []

	(* Returns the result of applying classical negation.  Cannot be applied
	   if the goal is a falsum; returns [] in this case.
	   Invariants: none.
	   @param c The context.
	   @param g The goal.
	*)
	let classical_negation ((c, g):question):(dependencies list) =
	  	if (Formula.equals Bottom g) then [] else
  		let c_new = Context.extend c (Negation g) in 
		    	[([(c_new, Bottom)], Rule.Cn)]

	(* Returns the result of applying ex falso quodlibet.  Cannot be applied
	   if the goal is a falsum; returns [] in this case.
	   Invariants: none.
	   @param c The context.
	   @param g The goal.
	*)
	let ex_falso_quodlibet ((c, g):question):(dependencies list) =
		if (Formula.equals Bottom g) then [] else
		[([(c, Bottom)], Rule.Ef)]

	(* Returns the result of applying negation introduction.  Returns
	   [] if the rule cannot be applied.
	   Invariants: none.
	   @param c The context.
	   @param g The goal.
	*)
	let negation_intro ((c, g):question):(dependencies list) =
		match g with
		| Negation s -> let c_new = Context.extend c s in
	               	[([(c_new, Bottom)], Rule.Ni)]
		| _ -> []

	(* Returns a list of all possible rule applications on q except
	   for classical rules or ex falso quodlibet. 
	   Invariants: none.
	   @param q The question to be proven.
	*)
	let expand_basic (q:question):dependencies list =
		(extraction q) @
		(conjunction_intro q) @
		(disjunction_intro q) @
		(implication_intro q) @
		(biconditional_intro q) @
		(negation_intro q) @
		(bottom q) @
		(cases q) 

	let expand_classical (q:question):dependencies list = 
 		(expand_basic q) @ (classical_negation q)

	let expand_intuition (q:question):dependencies list =
		(expand_basic q) @ (ex_falso_quodlibet q)
end

(* A thread of a proof search. *)
module Thread = struct
	type thread = (Question.question * (Rule.rule option)) list;;

	(* 
	   @param t The thread.
	*)
	let repeat_question (t:thread):bool =
		match t with
		| [] -> failwith "Threads should not be empty."
		| _::[] -> false
		| ((c, g), _)::l -> List.exists (fun ((bc, bg), _) -> 
			(Formula.equals bg g) && (Context.subcontext c bc)) l

	(* 
	   @param t The thread.
	*)
	let extra_ex_falso_quodlibet (t:thread):bool =
		match t with
		| [] -> failwith "Threads should not be empty."
		| ((cc, _), Some Rule.Ef)::((co, _), _)::_ -> Context.subcontext cc co
		| _ -> false

	(* One should not use ~C on the positive half of a contradictory pair.
 	   It's not obvious how much checking for this would affect proof search
  	   speed, though in cases of backtracking on falsums, it would probably
  	   be significant.  In programmatic terms, if the head of a thread is shown
  	   by ~C and the second element is shown by _|_I, then that thread
  	   should not be pursued.  The current strategy of using just threads
  	   that are question lists does not appear effective for this.  I can either
  	   be creative in backtrack checking or extend threads some.  But even
  	   adding the rule application to threads isn't quite enough -- some
  	   contextual knowledge of, say, which premise an element is, is also 
  	   necessary.

  	   TODO Write method to be used right after repeat_question that filters
	   out such rule applications.
	*)
end

(* A proof.*)
module Proof = struct
	type search_type = Classical | Intuitionistic;;

	(* A proof.  The question, its dependencies, and the rule justifying it. *)
	type proof = { q: Question.question;
               	a: proof list;
               	r: Rule.rule };;

	(* Returns Some p where p is a proof of q, or None if no proof can be found.
		@param expand The expansion function -- applies inference rules.
		@param t The thread; initially [] and built as rules are applied.
		@param q The question to be proven.
	*)
	let rec thread_search expand t ((c, g) as q):proof option =
  	if (Context.exists c g) then Some { q = (c, g) ; a = [] ; r = Rule.Pr } else
  	if (Thread.repeat_question t) then None else 
  	if (Thread.extra_ex_falso_quodlibet t) then None else
  	(* Add other context-specific filters here. *)
  
  	(* Tries to prove all questions.  If this is possible, returns
  	Some pl where pl is the list of the subproofs, and if not
  	returns None.  Returns Some [] when given [] questions. *)
  	let rec search_questions (questions, rule) answers: proof list option =
    	match questions with
    	| [] -> Some answers
    	| question::question_tail ->
      	(match thread_search expand ((question, Some rule)::t) question with
      	| None -> None
      	| Some p -> 
        	let answers = p::answers in
        	search_questions (question_tail, rule) answers)
  	in
  	let search_question ((questions, rule):Question.dependencies):proof option =
   		match search_questions (questions, rule) [] with
    	| None -> None
    	| Some answers -> Some { q = q ; a = answers ; r = rule }
  	in

  	(* Tries to prove the dependencies.  Returns Some p where p is a proof of
  	a dependecies if one can be found, or None if none can be found.  Stops as
  	soon as one is found. *)
  	let rec search_expansion expansion:proof option = 
    	match expansion with
    	| [] -> None
    	| dependencies::expansion_tail ->
      	(match search_question dependencies with
      	| None -> search_expansion expansion_tail 
      	| Some p -> Some p)
  	in
  	search_expansion (expand q)

	(* Searches for a proof of q using search type st.
  	@param st The search type.
  	@param q The question.
	*)
	let search (st:search_type) (q:Question.question):proof option =
	match st with
	| Intuitionistic -> thread_search Question.expand_intuition [(q, None)] q
	| Classical -> thread_search Question.expand_classical [(q, None)] q 

	(* Returns a string of the proof.
   	@param p The proof.
	*)
	let to_string (p:proof):string = 
  	let rec to_string_i p i =
    	let sp = String.make i ' ' in
    	let qs = Question.to_string p.q in
    	let rs = "(" ^ Rule.to_string p.r ^ ")" in
    	let line = rs ^ " " ^ sp ^ qs in
    	List.fold_right (fun p s -> (to_string_i p (i + 2)) ^ "\n" ^ s) p.a line 
  	in
  	to_string_i p 0
end
