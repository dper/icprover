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

open Context;;
    
let a:Formula.formula = Formula.Atomic 'A';;
let b:Formula.formula = Formula.Atomic 'B';;
let c:Formula.formula = Formula.Atomic 'C';;
let c0:context = empty ();;
let c1:context = Context.extend c0 a;;
let c2:context = Context.extend c0 b;;
let c3:context = Context.join (Context.join c0 c1) c2;;

print_endline "Testing Context.exists.";;
not (exists c0 a) || failwith "Context should be empty (1).";;
not (exists c1 b) || failwith "Context should not contain this (2).";;
exists c1 a || failwith "Context should contain this (3).";;
exists c3 a || failwith "Context should contain this (4).";;
exists c3 b || failwith "Context should contain this (5).";;
not (exists c3 c) || failwith "Context should not contain this (6).";;

print_endline "Testing Context.subcontext.";;
subcontext c0 c3 || failwith "Should be a subcontext (1).";;
subcontext c1 c3 || failwith "Should be a subcontext (2).";;
subcontext c2 c3 || failwith "Should be a subcontext (3).";;
subcontext c0 c1 || failwith "Should be a subcontext (4).";;
not (subcontext c3 c0) || failwith "Should not be a subcontext (5).";;
not (subcontext c1 c0) || failwith "Should not be a subcontext (6).";;
not (subcontext c2 c1) || failwith "Should not be a subcontext (7).";;
not (subcontext c1 c2) || failwith "Should not be a subcontext (8).";;
not (subcontext c3 c1) || failwith "Should not be a subcontext (9).";;
not (subcontext c3 c2) || failwith "Should not be a subcontext (10).";;
