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

type thread = (Question.question * (Rule.rule option)) list;;

(* 
  @param t The thread.
*)
let repeat_question (t:thread):bool =
  match t with
  | [] -> failwith "Threads should not be empty."
  | _::[] -> false
  | ((c, g), _)::l -> List.exists (fun ((bc, bg), _) -> 
		(Formula.equals bg g) && (Context.subcontext c bc)) l

(* 
	@param t The thread.
*)
let extra_ex_falso_quodlibet (t:thread):bool =
	match t with
	| [] -> failwith "Threads should not be empty."
	| ((cc, _), Some Rule.Ef)::((co, _), _)::_ -> Context.subcontext cc co
	| _ -> false

(* One should not use ~C on the positive half of a contradictory pair.
   It's not obvious how much checking for this would affect proof search
   speed, though in cases of backtracking on falsums, it would probably
   be significant.  In programmatic terms, if the head of a thread is shown
   by ~C and the second element is shown by _|_I, then that thread
   should not be pursued.  The current strategy of using just threads
   that are question lists does not appear effective for this.  I can either
   be creative in backtrack checking or extend threads some.  But even
   adding the rule application to threads isn't quite enough -- some
   contextual knowledge of, say, which premise an element is, is also 
   necessary.

   TODO Write method to be used right after repeat_question that filters
   out such rule applications.
*)
