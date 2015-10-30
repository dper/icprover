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
  | _          -> []

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
