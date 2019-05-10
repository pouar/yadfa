SRC := yadfa-guide.texi
export MAKEINFO := makeinfo
export TEX := pdftex  --output-format=dvi
export PDFTEX := xetex
TEXOBJS := yadfa-guide.ps yadfa-guide.pdf yadfa-guide.dvi
OBJS := yadfa-guide.info yadfa-guide.html
SVGS := $(wildcard yadfa-guide-figures.in/*.svg)
PNGS := $(wildcard yadfa-guide-figures.in/*.png)
SVGPNGS := $(patsubst yadfa-guide-figures.in/%.svg, yadfa-guide-figures/%.png, $(SVGS))
SVGEPSS := $(patsubst yadfa-guide-figures.in/%.svg, yadfa-guide-figures/%.eps, $(SVGS))
SVGPDFS := $(patsubst yadfa-guide-figures.in/%.svg, yadfa-guide-figures/%.pdf, $(SVGS))
PNGEPSS := $(patsubst yadfa-guide-figures.in/%.png, yadfa-guide-figures/%.eps, $(PNGS))
PNGPNGS := $(patsubst yadfa-guide-figures.in/%.png, yadfa-guide-figures/%.png, $(PNGS))
.PHONY: maketex install-info install-pdf install-html install-ps install-dvi install-texi install-docs dvi pdf info ps html
INSTALL := install
INSTALL_DATA := $(INSTALL) -m644
INSTALL_DIR := $(INSTALL) -Ddm755
prefix := /usr/local
datarootdir := $(prefix)/share
docdir := $(datarootdir)/doc/yadfa
infodir := $(datarootdir)/info
SHELL := /bin/sh
all: $(OBJS) maketex

yadfa-guide-figures:
	mkdir -p $@

references.texi: pouar.bib.texi
	cat $^ > $@
$(SVGEPSS): yadfa-guide-figures/%.eps: yadfa-guide-figures.in/%.svg | yadfa-guide-figures
	rsvg-convert -f eps -o $@ $<

$(SVGPNGS): yadfa-guide-figures/%.png: yadfa-guide-figures.in/%.svg | yadfa-guide-figures
	rsvg-convert -f png -o $@ $<
	ect -4 -strip $@

$(SVGPDFS): yadfa-guide-figures/%.pdf: yadfa-guide-figures.in/%.svg | yadfa-guide-figures
	rsvg-convert -f pdf -o $@ $<

$(PNGEPSS): yadfa-guide-figures/%.eps: yadfa-guide-figures.in/%.png | yadfa-guide-figures
	convert $< -flatten EPS3:$@

$(PNGPNGS): yadfa-guide-figures/%.png: yadfa-guide-figures.in/%.png | yadfa-guide-figures
	install -Dm644 $< $@

yadfa-guide.info: $(SRC) $(SVGPNGS) $(PNGPNGS) references.texi
	$(MAKEINFO) --no-split $<

yadfa-guide.dvi: $(SRC) $(SVGEPSS) $(PNGEPSS) references.texi
	PDFTEX=$(PDFTEX) $(MAKEINFO) --no-split --dvi $<

yadfa-guide.html: $(SRC) $(SVGPNGS) $(PNGPNGS) references.texi
	$(MAKEINFO) --no-split --html $<

yadfa-guide.pdf: $(SRC) $(SVGPDFS) $(PNGPNGS) references.texi
	$(MAKEINFO) --no-split --pdf $<

yadfa-guide.ps: $(SRC) $(SVGEPSS) $(PNGEPSS) references.texi
	$(MAKEINFO) --no-split --ps $<

maketex: $(SRC)
	$(MAKE) -j1 $(TEXOBJS)


$(DESTDIR)$(docdir)/yadfa-guide-figures $(DESTDIR)$(infodir)/yadfa-guide-figures: %:
	$(INSTALL_DIR) -Ddm755 $@

install-info: yadfa-guide.info $(SVGPNGS) $(PNGPNGS) $(DESTDIR)$(infodir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.info $(DESTDIR)$(infodir)/
	$(INSTALL_DATA) $(SVGPNGS) $(PNGPNGS) $(DESTDIR)$(infodir)/yadfa-guide-figures/
	if $(SHELL) -c 'install-info --version' \
                     >/dev/null 2>&1; then \
                    install-info --dir-file="$(DESTDIR)$(infodir)/dir" \
                                 "$(DESTDIR)$(infodir)/yadfa-guide.info"; \
                  else true; fi

install-pdf: yadfa-guide.pdf $(SVGPDFS) $(PNGPNGS) $(DESTDIR)$(docdir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.pdf $(DESTDIR)$(docdir)/
	$(INSTALL_DATA) $(SVGPDFS) $(PNGPNGS) $(DESTDIR)$(docdir)/yadfa-guide-figures/

install-html: yadfa-guide.html $(SVGPNGS) $(PNGPNGS) $(DESTDIR)$(docdir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.html $(DESTDIR)$(docdir)/
	$(INSTALL_DATA) $(SVGPNGS) $(PNGPNGS) $(DESTDIR)$(docdir)/yadfa-guide-figures/

install-ps: yadfa-guide.ps $(SVGEPSS) $(PNGEPSS) $(DESTDIR)$(docdir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.ps $(DESTDIR)$(docdir)/
	$(INSTALL_DATA) $(SVGEPSS) $(PNGEPSS) $(DESTDIR)$(docdir)/yadfa-guide-figures/

install-dvi: yadfa-guide.dvi $(SVGEPSS) $(PNGEPSS) $(DESTDIR)$(docdir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.dvi $(DESTDIR)$(docdir)/
	$(INSTALL_DATA) $(SVGEPSS) $(PNGEPSS) $(DESTDIR)$(docdir)/yadfa-guide-figures/

install-texi: yadfa-guide.texi $(DESTDIR)$(docdir)/yadfa-guide-figures
	$(INSTALL_DATA) yadfa-guide.texi $(DESTDIR)$(docdir)/

install-docs: install-info install-pdf install-html install-ps install-dvi

clean:
	rm -f $(OBJS) $(TEXOBJS) $(SVGPNGS) $(SVGEPSS) $(SVGPDFS) $(PNGPNGS) $(PNGEPSS) yadfa-guide.aux yadfa-guide.log yadfa-guide.tmp yadfa-guide.toc

dvi: yadfa-guide.dvi

pdf: yadfa-guide.pdf

info: yadfa-guide.info

ps: yadfa-guide.ps

html: yadfa-guide.html
