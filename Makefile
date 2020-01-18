OPTS := texi ft
SRC := build.lisp
SBCL := sbcl
CCL := ccl
ECL := ecl
CLISP := clisp
LISP := sbcl

.PHONY: sbcl ccl ecl clisp
all: yadfa reference2

sbcl: $(SRC)
	$(SBCL) --script $< $(OPTS)

ccl: $(SRC)
	$(CCL) -b -l $< -- $(OPTS)

ecl: $(SRC)
	$(ECL) --shell $< $(OPTS)

clisp: $(SRC)
	$(CLISP) $< $(OPTS)

yadfa: $(SRC)
	$(MAKE) $(LISP)

docs/reference/yadfa-reference.texi: yadfa
	./$< texi

reference: docs/reference/yadfa-reference.texi
	$(MAKE) -C docs/reference

reference2: yadfa
	$(MAKE) -C docs/reference

install-reference:
	$(MAKE) -C docs/reference install-docs

clean:
	rm -f yadfa docs/reference/yadfa-reference.texi
	$(MAKE) -C docs/reference clean
