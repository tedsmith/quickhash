# Makefile for Linux

PREFIX ?= /usr
BIN = quickhash
PROJECT = project1
LAZBUILD := lazbuild
LAZRES := lazres
RESFILES = udisplaygrid.lrs unit2.lrs
OPTIONS ?=
# use a local temporary config directory to not register
# the used package(s) permanently and globally
OPTIONS += --pcp=lazarus_cfg  HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk

define \n


endef


all: $(BIN)

clean:
	rm -rf lib/ lazarus_cfg/
	rm -rf HashLib4Pascal/HashLib/src/Packages/FPC/lib/
	rm -f $(BIN) $(PROJECT)
	$(foreach FILE,$(RESFILES),\
	  test ! -f $(FILE).backup || mv -f $(FILE).backup $(FILE) ; ${\n})

distclean: clean
	rm -f $(PROJECT).lpi $(PROJECT).ico $(PROJECT).res

$(BIN): $(PROJECT).lpi $(PROJECT).ico
	$(foreach FILE,$(RESFILES),\
	  test -f $(FILE).backup || cp $(FILE) $(FILE).backup ; ${\n}\
	  $(LAZRES) $(FILE) $(FILE:.lrs=.lfm) ; ${\n})
	$(LAZBUILD) $(OPTIONS) $<

$(PROJECT).lpi: $(PROJECT)_linux.lpi
	cp $< $@

$(PROJECT).ico: misc/QuickHash.ico
	cp $< $@

install:
	install -d -m 755 $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(BIN) $(DESTDIR)$(PREFIX)/bin
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/applications
	install -m 644 misc/quickhash.desktop $(DESTDIR)$(PREFIX)/share/applications
	$(foreach SIZE,16 24 32 48 64 96 128,\
	  install -d -m 755 $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps ; ${\n}\
	  install -m 644 misc/quickhash_$(SIZE).png $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps/quickhash.png ; ${\n})


