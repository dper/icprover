# Copyright 2007, 2008, Douglas Perkins.
# 
# This file is part of ICProver.
# 
# ICProver is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# ICProver is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with ICProver.  If not, see <http://www.gnu.org/licenses/>.

all: prover.cmo \
	parser.ml lexer.ml loader.cmo loader.test \
	proveformula.cmo \
	file-test.cmo file.test \
	doc

doc: *.ml *.mli
	mkdir -p doc
	ocamldoc -colorize-code -all-params -html -d doc -keep-code \
		formula.ml formula.mli context.ml context.mli \
		extraction.ml extraction.mli question.ml question.mli \
		thread.ml thread.mli proof.ml proof.mli \
		loader.ml loader.mli

clean:
	rm -f *.cmo *.cmi *.test
	rm -f parser.ml lexer.ml
	rm -rf doc
	rm -f parser.mli
	rm -f proveformula

question.cmo: extraction.cmo rule.cmo question.mli question.ml
	ocamlc -c question.mli question.ml

thread.cmo: question.cmo thread.mli thread.ml
	ocamlc -c thread.mli thread.ml

proof.cmo: thread.cmo proof.mli proof.ml
	ocamlc -c proof.mli proof.ml

parser.ml:
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer.ml:
	ocamllex lexer.mll
	ocamlc -c lexer.ml

loader.cmo:
	ocamlc -c loader.mli loader.ml 

proveformula.cmo: proof.cmo thread.cmo question.cmo parser.cmo lexer.cmo proveformula.ml 
	ocamlc -c proveformula.ml
	ocamlc -o proveformula \
		rule.cmo formula.cmo context.cmo extraction.cmo \
		question.cmo thread.cmo proof.cmo \
		parser.cmo lexer.cmo \
		proveformula.cmo

loader.test:
	ocamlc -c loader-test.ml
	ocamlc -o loader.test \
		rule.cmo formula.cmo context.cmo extraction.cmo question.cmo \
		lexer.cmo parser.cmo loader.cmo \
		loader-test.cmo

file-test.cmo: proof.cmo file-test.ml context-test.ml
	ocamlc -c formula-test.ml
	ocamlc -c context-test.ml
	ocamlc -c extraction-test.ml
	ocamlc -c question-test.ml
	ocamlc -c file-test.ml

file.test: file-test.cmo
	ocamlc -o formula.test \
		formula.cmo formula-test.cmo
	ocamlc -o context.test \
		formula.cmo context.cmo context-test.cmo
	ocamlc -o extraction.test \
		formula.cmo context.cmo extraction.cmo extraction-test.cmo
	ocamlc -o question.test \
		formula.cmo context.cmo extraction.cmo question.cmo \
		question-test.cmo
	ocamlc -o file.test \
		rule.cmo formula.cmo context.cmo extraction.cmo question.cmo \
		thread.cmo proof.cmo lexer.cmo parser.cmo loader.cmo \
		file-test.cmo
