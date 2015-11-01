ICProver
========

ICProver is an automated theorem prover.  It is written in OCaml and implements the intercalation calculus.  More information about this type of proof search can be found in my [master's thesis](https://archive.org/details/thesis_201502), in research by [Wilfried Sieg](http://www.hss.cmu.edu/philosophy/faculty-sieg.php), and on [Wikipedia](http://en.wikipedia.org/wiki/Natural_deduction).

Current versions of this program should be available on [github](https://github.com/dper/icprover).


Use
===

To run the test suite, use the following command.

```
ocaml test-lists.ml
```

Modify the `list` files as desired to try proving other questions.

The proof search code is in `prover.ml`.  The parser, which takes strings, parses them, and returns formulae and questions and such, is in `parser.ml`.


Contact
=======

* [dper@microca.st](https://microca.st/dper)
* [dpp0@twitter.com](https://twitter.com/dpp0)
* [dperkins.org](https://dperkins.org)


License
=======

Copyright 2008, 2015 Douglas Perkins

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
