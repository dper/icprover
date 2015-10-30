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

(** A question to be proven (or the attempt made). *)

(** A question is a list of formulas, the premises, and a 
 	single formula, the goal. *)
type question = Context.context * Formula.formula;;

(** The dependencies for a question.  To show that a question is a 
  theorem (provable), one applies inference rules.  However, 
  applying inference rules can result in new questions (open questions) 
  that must be demonstrated in turn.  The open questions produced by 
  the application of an inference rule are the question list here, 
  and the inference rule that produced them is the rule here.
*)
type dependencies = question list * Rule.rule;;

(** @return a string of the question. *)
val to_string : question -> string 

(** @return a list of all the possible rule applications on q using
  classical propositional logic. *)
val expand_classical : question -> dependencies list 

(** @return a list of all possible rule applications on q using
  intuitionistic logic. *)
val expand_intuition : question -> dependencies list
