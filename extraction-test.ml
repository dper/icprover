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

open Prover;;
open Extraction;;
open Formula;;
  
print_endline "Testing Extraction.find_normal_branches.";;
let a:formula = Atomic 'A';;
let b:formula = Atomic 'B';;
let c:formula = Atomic 'C';;
let aImpB:formula = Implication (a, b);;
let cImpAImpB:formula = Implication (c, aImpB);;
let aAndB:formula = Conjunction (a, b);;
let aAndBAndB:formula = Conjunction (aAndB, b);;
let negA:formula = Negation (a);;
let negB:formula = Negation (b);;
let aOrB:formula = Disjunction (a, b);;
let negA_Or_NegB:formula = Disjunction (negA, negB);;
let neg_AOrB:formula = Negation aOrB;;
let negNeg_AOrB:formula = Negation neg_AOrB;;
[] = (find_normal_branches aAndB c) || failwith "Should not be extractions (2).";;
[] != (find_normal_branches aAndB b) || failwith "Should be extractions (3).";;
[] != (find_normal_branches aAndB a) || failwith "Should be extractions (4).";;
2 = List.length (find_normal_branches aAndBAndB b) || failwith "Should be extractions (5).";;
1 = List.length (find_normal_branches aAndB a) || failwith "Should be extractions (6).";;
[] = (find_normal_branches negNeg_AOrB Bottom) || failwith "Should not be extractions (9).";;
[] = (find_normal_branches negA_Or_NegB Bottom) || failwith "Should not be extractions (10).";;
let ell2 = find_normal_branches negA_Or_NegB negA;;
let el2 = List.nth ell2 0;;
  
print_endline "Testing Extraction.find_cases_branches.";;
let aOrB_Imp_C:formula = Implication (aOrB, c);;
let c_Imp_AOrB:formula = Implication (c, aOrB);;
let aOrC:formula = Disjunction (a, c);;
let aOrBAndAOrC:formula = Conjunction (aOrB, aOrC);;
[] = (find_cases_branches aAndBAndB) || failwith "Should not be cases (1).";;
[] = (find_cases_branches aOrB_Imp_C) || failwith "Should not be cases (2).";;
[] != (find_cases_branches c_Imp_AOrB) || failwith "Should be cases (3).";;
[] != (find_cases_branches aOrB) || failwith "Should be cases (4).";;
2 = List.length (find_cases_branches aOrBAndAOrC) || failwith "Should be cases (5).";;
[] = (find_normal_branches negNeg_AOrB Bottom) || failwith "Should not be cases (6).";;
let cll2 = find_cases_branches c_Imp_AOrB;;
let cl2 = List.nth cll2 0;;

print_endline "Testing Extraction.find_contradiction.";;
let b_And_NegA:formula = Conjunction (b, negA);;
let a_Imp_NegB:formula = Implication (a, negB);;
[] = find_bottom_branches a || failwith "Should not be contradictions (1).";;
let cll1 = find_bottom_branches negA;;
[] != cll1 || failwith "Should be contradictions (2).";; 
let cll2 = find_bottom_branches b_And_NegA;;
[] != cll2 || failwith "Should be contradictions (4).";;
let cll3 = find_bottom_branches negA_Or_NegB;;
List.length cll3 = 2 || failwith "Should be two contradictions (6).";;
