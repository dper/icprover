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

(** Loads files of questions into lists. *)

(** Reads a text file and makes it a string list.
	- The file to load must be readable.
	@param filename The file to load.
*)
let file_to_string (filename:string):string list =
	let ch = open_in filename in
	let rec loop accum =
		let l = input_line ch in
		accum := l::(!accum);
		loop accum
	in
	let accum = ref [] in
	try
		loop accum
	with 
		End_of_file -> !accum

(** 
	@param filename The file to load.
*)
let load (filename:string):(Question.question list) =
	let s = String.concat "\n" (file_to_string filename) in
	let lexbuf = Lexing.from_string s in
	Parser.pqs Lexer.lexer lexbuf
