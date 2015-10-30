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

(** Formulas and their basic operations.  Propositional logic. *)

(** A formula is defined recursively. *)
type formula =
| Bottom (** Contradiction. *) 
| Atomic of char (** Atomic formula. *)
| Negation of formula (** Negation. *)
| Conjunction of formula * formula (** Conjunction. *)
| Disjunction of formula * formula (** Disjunction. *)
| Implication of formula * formula (** Implication. *)
| Biconditional of formula * formula (** Biconditional *)
;;

(** @return true iff the two formulas are equal. *)
val equals : formula -> formula -> bool
  
(** @return the string representation of f. *)
val to_string : formula -> string
