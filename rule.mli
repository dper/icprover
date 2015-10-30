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

(** Inference rules. *)

(** Inference rules. *)
type rule = 
  Pr (** Is a premise. *)
| Ex (** Extraction. *)
| Ca (** Cases. *)
| Ai (** Conjunction introduction. *)
| Vl (** Disjunction introduction left. *)
| Vr (** Disjunction introduction right. *)
| Ii (** Implication introduction. *)
| Bi (** Biconditional introduction. *)
| Ni (** Negation introduction. *)
| Cn (** Classical negation. *)
| Bo (** Bottom introduction. *)
| Ef (** Ex falso quodlibet. *)
;;

(** @return a string of the rule. *)
val to_string : rule -> string
