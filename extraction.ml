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
