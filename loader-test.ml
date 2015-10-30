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

(* Returns true iff all questions in the file are proven.
	- The file must exist and be readable.
	@param file The file name.
*)
let load_file (file:string) =
	let ql = Loader.load file in
	let qll = List.length ql in
	if (qll = 4) then print_endline "Success: Read 4 of 4."
	else print_endline ("Failure: Read " ^ (string_of_int qll) ^ " of 4.")
;;

load_file "questions.txt";;
