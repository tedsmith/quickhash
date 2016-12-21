# Makefile for Linux

PROJECT = project1
LAZBUILD := lazbuild
OPTIONS ?=
ifeq ($(V),1)
OPTIONS += --verbose
endif


all: $(PROJECT)

clean:
	rm -rf lib/
	rm -f $(BIN) $(PROJECT)

distclean: clean
	rm -f $(PROJECT).lpi $(PROJECT).ico $(PROJECT).res

$(PROJECT): $(PROJECT).lpi $(PROJECT).ico
	$(LAZBUILD) $(OPTIONS) $<

$(PROJECT).lpi: $(PROJECT)_linux.lpi
	cp $< $@

$(PROJECT).ico: QuickHash.ico
	cp $< $@

install:

