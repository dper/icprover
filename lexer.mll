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

(** Lexes question files.  Question files consist of text files with one
question on each line.  For each line, on the left are the premises, and
on the right is the conclusion.  There may be zero or more premises, and 
there is one goal.  The premises and goal are separated by the turnstile,
"|-".
*)

{
open Parser
let line=ref 1
}

rule lexer = parse
	(* Eat blank characters. *)
	[' ' '\t'] {lexer lexbuf}
	| '\n' {incr line; TEOL}
	| ['A'-'Z'] {TAtomic (String.get (Lexing.lexeme lexbuf) 0)}
	| "v." {TDisjunction}
	| "v" {TDisjunction}
	| '|' {TDisjunction}
	| "<->" {TBiconditional}
	| "->" {TImplication}
	| '&' {TConjunction}
	| '^' {TConjunction}
	| '~' {TNegation}
	| '!' {TNegation}
	| '(' {TLPar}
	| ')' {TRPar}
	| "|-" {TEntails}
	| ',' {TComma}
	| eof {TEOF}
	| _ {failwith((Lexing.lexeme lexbuf) ^
		": mistake at line " ^ string_of_int !line)}
