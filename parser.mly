%{
(** Parses files of questions on separate lines and returns a list of these. *)

(** Displays information on a parsing error.
Arguments:
	s - Error information.
*)
let parse_error s =
	print_endline ("parser.mly: Parse error.");
	print_endline s;
	flush stdout;;

%}

/* Token declarations. */
%token TEOL
%token TEOF
%token <char> TAtomic
%token TBiconditional
%token TImplication
%token TDisjunction
%token TConjunction
%token TNegation
%token TLPar
%token TRPar
%token TEntails
%token TComma

/* Non-terminal declarations. */
%type <Question.question list> pqs
%start pqs

%%

pqs:
| pq TEOL pqs {$1 :: $3}
| pq TEOF {[$1]}
| TEOF {[]}
;

pq:
| pc TEntails pf {($1, $3)}
| pf TEntails pf {([$1], $3)}
| TEntails pf {([], $2)}
;

pc:
| pf TComma pf {$1::[$3]}
| pf TComma pc {$1::$3}
;

pf:
| TAtomic {Formula.Atomic $1}
| TNegation TLPar pf TRPar {Formula.Negation $3}
| TNegation pf {Formula.Negation $2}  
| TLPar pf TBiconditional pf TRPar {Formula.Biconditional ($2, $4)}
| TLPar pf TImplication pf TRPar {Formula.Implication ($2, $4)}
| TLPar pf TConjunction pf TRPar {Formula.Conjunction ($2, $4)}
| TLPar pf TDisjunction pf TRPar {Formula.Disjunction ($2, $4)}
;

%%
