.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f music.zip
	zip -r music.zip . -x@exclude.lst

clean:
	dune clean
	rm -f music.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage