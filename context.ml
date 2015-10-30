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

type context = Formula.formula list;;

let empty ():context = []

(**
	@param c The context to extend.
	@param n The formula to add.
*)
let extend (c:context) (n:Formula.formula):context =
	if (List.exists (fun f -> (Formula.equals n f)) c) then c else n::c

let join (c1:context) (c2:context) =
	List.fold_right (fun f c -> extend c f) c2 c1

let exists (c:context) (f:Formula.formula):bool =
	List.exists (Formula.equals f) c

(**
	@param p The parent context.
	@param c The sub-context.
*)
let subcontext (p:context) (c:context):bool = 
	not (List.exists (fun x -> not (exists c x)) p)

let to_string (c:context):string = 
	match c with
	| []    -> "[]"
	| f::[] -> "[" ^ (Formula.to_string f) ^ "]"
	| f::l  -> "[" ^ (Formula.to_string f) ^
		(List.fold_right (fun f s -> s ^ ", " ^ Formula.to_string f) l "") ^ "]"
