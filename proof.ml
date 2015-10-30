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

(* A proof.*)

type search_type = Classical | Intuitionistic;;

(* A proof.  The question, its dependencies, and the rule justifying it. *)
type proof = { q: Question.question;
               a: proof list;
               r: Rule.rule };;

(* Returns Some p where p is a proof of q, or None if no proof can be found.
	@param expand The expansion function -- applies inference rules.
	@param t The thread; initially [] and built as rules are applied.
	@param q The question to be proven.
*)
let rec thread_search expand t ((c, g) as q):proof option =
  if (Context.exists c g) then Some { q = (c, g) ; a = [] ; r = Rule.Pr } else
  if (Thread.repeat_question t) then None else 
  if (Thread.extra_ex_falso_quodlibet t) then None else
  (* Add other context-specific filters here. *)
  
  (* Tries to prove all questions.  If this is possible, returns
  Some pl where pl is the list of the subproofs, and if not
  returns None.  Returns Some [] when given [] questions. *)
  let rec search_questions (questions, rule) answers: proof list option =
    match questions with
    | [] -> Some answers
    | question::question_tail ->
      (match thread_search expand ((question, Some rule)::t) question with
      | None -> None
      | Some p -> 
        let answers = p::answers in
        search_questions (question_tail, rule) answers)
  in
  let search_question ((questions, rule):Question.dependencies):proof option =
   	match search_questions (questions, rule) [] with
    | None -> None
    | Some answers -> Some { q = q ; a = answers ; r = rule }
  in

  (* Tries to prove the dependencies.  Returns Some p where p is a proof of
  a dependecies if one can be found, or None if none can be found.  Stops as
  soon as one is found. *)
  let rec search_expansion expansion:proof option = 
    match expansion with
    | [] -> None
    | dependencies::expansion_tail ->
      (match search_question dependencies with
      | None -> search_expansion expansion_tail 
      | Some p -> Some p)
  in
  search_expansion (expand q)

(* Searches for a proof of q using search type st.
  @param st The search type.
  @param q The question.
*)
let search (st:search_type) (q:Question.question):proof option =
match st with
| Intuitionistic -> thread_search Question.expand_intuition [(q, None)] q
| Classical -> thread_search Question.expand_classical [(q, None)] q 

(* Returns a string of the proof.
   @param p The proof.
*)
let to_string (p:proof):string = 
  let rec to_string_i p i =
    let sp = String.make i ' ' in
    let qs = Question.to_string p.q in
    let rs = "(" ^ Rule.to_string p.r ^ ")" in
    let line = rs ^ " " ^ sp ^ qs in
    List.fold_right (fun p s -> (to_string_i p (i + 2)) ^ "\n" ^ s) p.a line 
  in
  to_string_i p 0
