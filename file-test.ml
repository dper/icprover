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

let failures_to_string failures =
	List.fold_right 
		(fun (q, _) s -> (s ^ "\n" ^ Question.to_string q)) failures ""

(* Returns true iff all questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
  @param search_type The search type.
*)
let prove search search_type file =
	let ql = Loader.load file in
	let results = List.map 
	  (fun q -> 
		let _ = print_endline ("Trying to prove " ^ Question.to_string q) in
	  (q, search search_type q)) ql 
  in
	let total_count = List.length results in
	let failures = List.filter (fun (_, po) -> po = None) results in
	let success_count = (List.length results) - (List.length failures) in
	let _ =
		if failures = [] then () else
		print_endline (failures_to_string failures) in
	let _ = print_endline (file ^ ": Proved: " ^ 
		(string_of_int success_count) ^ "/" ^ 
		(string_of_int total_count) ^ ".") in
	true
;;

(* Returns true iff none of the questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
	@param search_type The search type.
*)
let do_not_prove search search_type file =
	let ql = Loader.load file in
	let results = List.map 
	  (fun q -> 
		  let _ = print_endline ("Trying not to prove " ^ Question.to_string q) in
		  (q, search search_type q)) ql 
		in
	let proven = List.filter (fun (_, po) -> po != None) results in
	let _ =
		if proven == [] then () else
		print_endline (failures_to_string proven) in
	let _ = print_endline (file ^ ": Did not prove: " ^ 
		(string_of_int (List.length results - List.length proven)) ^ "/" ^ 
		(string_of_int (List.length results)) ^ ".") in
	true
;;


prove Proof.search Proof.Intuitionistic "positive-intuition.txt";;
prove Proof.search Proof.Classical "positive-classical.txt";;
do_not_prove Proof.search Proof.Intuitionistic "negative-intuition.txt";;
do_not_prove Proof.search Proof.Classical "negative-classical.txt";;
