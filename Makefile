.PHONY: all
all: yadfa doc
IMPL:=sbcl

50-yadfa.conf:
	echo '(:tree "$(PWD)")' > 50-yadfa.conf
	install -dm755 ~/.config/common-lisp/source-registry.conf.d/
	install -Dm644 50-yadfa.conf ~/.config/common-lisp/source-registry.conf.d/


doc: 50-yadfa.conf
	sbcl --script run.lisp texi
	makeinfo --html --no-split yadfa.texi
	makeinfo --no-split yadfa.texi
	makeinfo --no-split  --pdf yadfa.texi

yadfa: 50-yadfa.conf
	ros -p clim-listener -p ltk-mw -sp yadfa -L $(IMPL) dump --disable-compression executable yadfa.ros
yadfa-repl: 50-yadfa.conf
	sbcl --load run.lisp --eval '(in-package :yadfa)' --eval '(sb-ext:save-lisp-and-die "yadfa" :executable t)'
