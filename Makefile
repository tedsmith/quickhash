# Makefile for Linux

PREFIX ?= /usr
BIN = quickhash
PROJECT = project1
LAZBUILD := lazbuild
OPTIONS ?=
# use a local temporary config directory to not register
# dcpcrypt_laz.lpk permanently and globally
OPTIONS += --pcp=lazarus_cfg  HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk


all: $(BIN)

clean:
	rm -rf lib/ lazarus_cfg/
	rm -rf HashLib4Pascal/HashLib/src/Packages/FPC/lib/
	rm -f $(BIN) $(PROJECT)

distclean: clean
	rm -f $(PROJECT).lpi $(PROJECT).ico $(PROJECT).res

$(BIN): $(PROJECT).lpi $(PROJECT).ico
	$(LAZBUILD) $(OPTIONS) $<

$(PROJECT).lpi: $(PROJECT)_linux.lpi
	cp $< $@

$(PROJECT).ico: misc/QuickHash.ico
	cp $< $@

define \n


endef

install:
	install -d -m 755 $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(BIN) $(DESTDIR)$(PREFIX)/bin
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/applications
	install -m 644 misc/quickhash.desktop $(DESTDIR)$(PREFIX)/share/applications
	$(foreach SIZE,16 24 32 48 64 96 128,\
	  install -d -m 755 $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps ; ${\n}\
	  install -m 644 misc/quickhash_$(SIZE).png $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps/quickhash.png ; ${\n})


