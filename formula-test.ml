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

open Formula;;
    
print_endline "Testing Formula.equals.";;
let a:formula = Atomic 'A';;
let b:formula = Atomic 'B';;
let aOrB:formula = Disjunction(a, b);;
let aImpB:formula = Implication(a, b);;
equals a a || failwith "Should be equal (1).";;
equals aOrB aOrB || failwith "Should be equal (2).";;
not (equals a aImpB) || failwith "Should not be equal (3).";; 
not (equals a b) || failwith "Should not be equal (4).";;

print_endline "Testing Formula.compare.";;
0 = (compare a a) || failwith "Should be equal (1).";;
0 != (compare a aOrB) || failwith "Should not be equal (2).";;
