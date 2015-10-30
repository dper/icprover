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

(** A context (also called a scope).  That is, some
	premises and assumptions. *)

(** A context. *)
type context = Formula.formula list;;

(** @return an empty context. *)
val empty : unit -> context

(** @return context c extended with formula n.  If formula n is already in c, 
then the context stays the same. *)
val extend : context -> Formula.formula -> context

(** Joins two contexts. *)
val join: context -> context -> context

(** @return true iff the context contains the formula. *)
val exists : context -> Formula.formula -> bool

(** @return true iff c is a possibly-equal subcontext of p.	 A context c 
is a subcontext of p if c has all of the formulas (and
maybe more) of p. *)
val subcontext : context -> context -> bool

(** @return a string of the context. *)
val to_string : context -> string
