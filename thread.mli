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

(** A thread in a proof. *)

(* A thread in a proof search tree.   The front of the list is the farthest up
	in the search tree.  The back of the list is the original question.  Each
	element in the list is a pair -- the last element in the list, the goal,
	should have None as its Rule.rule option; all other elements should
	have Some r where r is the rule that has been applied to the question.
*)
type thread = (Question.question * (Rule.rule option)) list;;

(** @return true iff the question is a repeat in the thread. *)
val repeat_question: thread -> bool

(** @return true iff ex falso quodlibet has been used and not as the 
	last rule in the proof or a subproof.  In these cases, the current
	thread should fail. *)
val extra_ex_falso_quodlibet: thread -> bool
