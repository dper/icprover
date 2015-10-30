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
  | Bottom        -> "_|_"
  | Atomic s      -> Char.escaped s
  | Negation s    -> "~" ^ (to_string s)
  | Conjunction (l, r) -> "(" ^ (to_string l) ^ "&" ^ (to_string r) ^ ")"
  | Disjunction (l, r) -> "(" ^ (to_string l) ^ "|" ^ (to_string r) ^ ")"
  | Implication (l, r) -> "(" ^ (to_string l) ^ "->" ^ (to_string r) ^ ")"
  | Biconditional (l, r) -> "(" ^ (to_string l) ^ "<->" ^ (to_string r) ^ ")"
