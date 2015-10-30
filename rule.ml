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

type rule = 
  Pr 
| Ex 
| Ca 
| Ai 
| Vl 
| Vr 
| Ii 
| Bi 
| Ni 
| Cn 
| Bo 
| Ef 
;;

let to_string (r:rule):string =
  match r with
  | Pr -> " Pre"
  | Ex -> " Ext"
  | Ca -> "  vE"
  | Ai -> "  &I"
  | Vl -> " vIL"
  | Vr -> " vIR"
  | Ii -> " ->I"
	| Bi -> "<->I"
  | Ni -> "  ~I"
  | Cn -> "  ~C"
  | Bo -> " _|_"
  | Ef -> " EFQ"
