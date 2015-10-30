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

(** An extraction branch.  A series of eliminations from a premise
	to a conclusion.  The conclusion must be a strict positive 
	subformula of the premise for such a thing to exist. *)

(** The steps in an extraction.  Indicates what path to choose in
	traversing the formula, as well as what the subformula is at each 
	step in the traversal.  The front of the extraction list is the 
	premise, and the tail is the goal.  Each step describes the 
	current formula and which side of it (or none) is taken. *)
type step = 
	| L of Formula.formula (** The left of the formula. *)
	| R of Formula.formula (** The right of the formula. *)
	| End of Formula.formula;; (** The final formula. *)

(** The extraction branch.  It starts with the premise and ends with
	End g where g is the goal.  Each element in the middle is L f or
	R f, depending which side of the current formula is obtained from
	elimination. *)
type branch

(** @return the extractions from p to g.  
	If g is not strictly positively embedded in p, an empty list is 
	returned.  One cannot extract to falsums; there are variants of 
	extractions that can do so and aren't part of this function, 
	however, such as cases extractions. *)
val find_normal_branches : Formula.formula -> Formula.formula -> branch list

(** @return the extractions from p to any disjunctions.
	In the resulting extraction lists, the front is the premise and 
	the tail is the disjunction. *)
val find_cases_branches : Formula.formula -> branch list

(** @return extractions to any strictly positively embedded negations
	in the formula.  The front of the list is the 
	negation itself. *)
val find_bottom_branches : Formula.formula -> branch list

(** @return the questions raised by the branch b.  That is, each
	element in the list is a context c and a formula f where f is
	an open question from b and c is its local context.  The local
	context starts empty and is filled in two ways:
		1. For both branches of disjunction elimination.
		2. For the consequent of implication elimination. *)
val find_normal_questions : branch -> (Context.context * Formula.formula) list

(** @return the questions raised by the bottom branch b.  That is, each
	element in the list is a context c and a formula f where f is
	an open question from b and c is its local context.  The local
	context starts empty and is filled in two ways:
		1. For both branches of disjunction elimination.
		2. For the consequent of implication elimination. 
	The unnegated part of the contradictory pair is one of these questions. *)
val find_bottom_questions : branch -> (Context.context * Formula.formula) list

(** @return the questions raised by the cases branch b and goal g.  
 That is, each element in the list is a context c and a formula f 
 where f is an open question from b and c is its local context.  
 The local context starts empty and is filled in two ways:
		1. For both branches of disjunction elimination.
		2. For the consequent of implication elimination.
	The final disjunction elimination is one of these questions. *)
val find_cases_questions : Formula.formula -> branch -> (Context.context * Formula.formula) list

(** @return a string of the extraction. *)
val to_string : branch -> string
