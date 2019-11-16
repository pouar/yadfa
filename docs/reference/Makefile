SRC := yadfa-reference.texi
export MAKEINFO := makeinfo
export TEX := pdftex --output-format=dvi
export PDFTEX := xetex
TEXOBJS := yadfa-reference.ps yadfa-reference.dvi yadfa-reference.pdf
OBJS := yadfa-reference.info yadfa-reference.html yadfa-reference.xml
.PHONY: maketex
all: $(OBJS) maketex

yadfa-reference.info: $(SRC)
	$(MAKEINFO) --no-split $<

yadfa-reference.dvi: $(SRC)
	$(MAKEINFO) --no-split --dvi $<

yadfa-reference.html: $(SRC)
	$(MAKEINFO) --css-include=style-common.css --css-include=style-single.css --no-split --html $<

yadfa-reference.pdf: $(SRC)
	$(MAKEINFO) --no-split --pdf $<

yadfa-reference.ps: $(SRC)
	$(MAKEINFO) --no-split --ps $<

yadfa-reference.xml: $(SRC)
	$(MAKEINFO) --no-split --docbook $<

maketex: $(SRC)
	$(MAKE) -j1 $(TEXOBJS)

clean:
	rm -f $(OBJS) $(TEXOBJS) yadfa-reference.aux yadfa-reference.log yadfa-reference.tmp yadfa-reference.toc yadfa-reference.cps yadfa-reference.cp yadfa-reference.fns yadfa-reference.fn yadfa-reference.tps yadfa-reference.tp yadfa-reference.vrs yadfa-reference.vr
