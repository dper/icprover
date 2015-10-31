#use "prover.ml"

open Question;;
open Formula;;

let a:formula = Atomic 'A';;
let b:formula = Atomic 'B';;
let c:formula = Atomic 'C';;
let aAndB:formula = Conjunction (a, b);;
let aOrB:formula = Disjunction(a, b);;
let aOrBAndAOrB:formula = Conjunction (aOrB, aOrB);;
let aImpB:formula = Implication (a, b);;
let negA:formula = Negation a;;
let negB:formula = Negation b;;
let negA_Or_NegB:formula = Disjunction (negA, negB);;
let neg_AOrB:formula = Negation aOrB;;
let negNeg_AOrB:formula = Negation neg_AOrB;;
let aOrC:formula = Disjunction (a, c);;
let aOrBAndAOrC = Conjunction (aOrB, aOrC);;
let negC:formula = Negation c;;
let aAndNegC:formula = Conjunction (a, negC);;
let q1:question = ([aAndB], a);;
let q2:question = ([aAndB; aImpB], b);;
let q3:question = ([negNeg_AOrB; negA_Or_NegB], Bottom);;
let q4:question = ([negA_Or_NegB], negA);;
let q5:question = ([aOrBAndAOrC], c);;
let q6:question = ([], negC);;
let q7:question = ([negC], Bottom);;
let q8:question = ([aAndNegC], Bottom);;

print_endline "Testing Question.expand.";;
[] != expand_classical q1 || failwith "Should be a dependency (1).";;
[] != expand_classical q2 || failwith "Should be a dependency (2).";;
[] != expand_classical q3 || failwith "Should be a dependency (3).";;
[] != expand_classical q4 || failwith "Should be a dependency (4).";;
[] != expand_classical q5 || failwith "Should be a dependency (5).";;
[] != expand_classical q6 || failwith "Should be a depencency (6).";;
[] != expand_classical q7 || failwith "Should be a dependency (7).";;
[] != expand_classical q8 || failwith "Should be a dependency (8).";;
