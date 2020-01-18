SRC := yadfa-reference.texi
export MAKEINFO := makeinfo
export TEX := pdftex --output-format=dvi
export PDFTEX := xetex
TEXOBJS := yadfa-reference.ps yadfa-reference.dvi yadfa-reference.pdf
OBJS := yadfa-reference.info yadfa-reference.html
INSTALL := install
INSTALL_DATA := $(INSTALL) -m644
INSTALL_DIR := $(INSTALL) -Ddm755
.PHONY: maketex install-info install-pdf install-html install-ps install-dvi install-texi install-docs dvi pdf info ps html
prefix := /usr/local
datarootdir := $(prefix)/share
docdir := $(datarootdir)/doc/yadfa
infodir := $(datarootdir)/info
SHELL := /bin/sh
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

maketex: $(SRC)
	$(MAKE) -j1 $(TEXOBJS)

$(DESTDIR)$(docdir) $(DESTDIR)$(infodir): %:
	$(INSTALL_DIR) -Ddm755 $@

install-info: yadfa-reference.info $(DESTDIR)$(infodir)
	$(INSTALL_DATA) yadfa-reference.info $(DESTDIR)$(infodir)/
	if $(SHELL) -c 'install-info --version' \
                     >/dev/null 2>&1; then \
                    install-info --dir-file="$(DESTDIR)$(infodir)/dir" \
                                 "$(DESTDIR)$(infodir)/yadfa-reference.info"; \
                  else true; fi

install-pdf: yadfa-reference.pdf $(DESTDIR)$(docdir)
	$(INSTALL_DATA) yadfa-reference.pdf $(DESTDIR)$(docdir)/

install-html: yadfa-reference.html $(DESTDIR)$(docdir)
	$(INSTALL_DATA) yadfa-reference.html $(DESTDIR)$(docdir)/

install-ps: yadfa-reference.ps $(DESTDIR)$(docdir)
	$(INSTALL_DATA) yadfa-reference.ps $(DESTDIR)$(docdir)/

install-dvi: yadfa-reference.dvi $(DESTDIR)$(docdir)
	$(INSTALL_DATA) yadfa-reference.dvi $(DESTDIR)$(docdir)/

install-texi: yadfa-reference.texi $(DESTDIR)$(docdir)
	$(INSTALL_DATA) yadfa-reference.texi $(DESTDIR)$(docdir)/

install-docs: install-info install-pdf install-html install-ps install-dvi

clean:
	rm -f $(OBJS) $(TEXOBJS) yadfa-reference.aux yadfa-reference.log yadfa-reference.tmp yadfa-reference.toc yadfa-reference.cps yadfa-reference.cp yadfa-reference.fns yadfa-reference.fn yadfa-reference.tps yadfa-reference.tp yadfa-reference.vrs yadfa-reference.vr

dvi: yadfa-reference.dvi

pdf: yadfa-reference.pdf

info: yadfa-reference.info

ps: yadfa-reference.ps

html: yadfa-reference.html
