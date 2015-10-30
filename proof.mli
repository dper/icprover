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

(** A proof.*)

(* The different types of searches. *)
type search_type = 
| Classical (* Classical logic. *)
| Intuitionistic (*Propositional logic. *)
;;

(** A proof.  The question, its dependencies, and the rule used to
  justify it. *)
type proof 

(** @return Some p where p is a proof of q, or None if 
  no proof is found. *)
val search : search_type -> Question.question -> proof option

(** @return a string of the proof. *)
val to_string : proof -> string 
