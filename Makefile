# Makefile for Linux
# https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6.4/

PREFIX ?= /usr
BIN = quickhash
LAZBUILD := /usr/share/lazarus/1.6.4/lazbuild
LAZRES := /usr/share/lazarus/1.6.4/tools/lazres
RESFILES = dbases_sqlite.lrs frmaboutunit.lrs udisplaygrid.lrs unit2.lrs
OPTIONS ?=
# use a local temporary config directory to not register
# the used package(s) permanently and globally
OPTIONS += --pcp=lazarus_cfg
OPTIONS += HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk
OPTIONS += DateTimePicker/zvdatetimectrls.lpk
OPTIONS += /usr/share/lazarus/1.6.4/components/dbexport/lazdbexport.lpk

define \n


endef


all: $(BIN)

clean:
	rm -rf lazarus_cfg/ DateTimePicker/lib/ HashLib4Pascal/HashLib/src/Packages/FPC/lib/
	rm -f $(BIN) quickhash_linux.ico *.o *.or *.ppu *.res *.compiled
	rm -f DateTimePicker/zvdatetimectrls.pas HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.pas
	$(foreach FILE,$(RESFILES),\
	  test ! -f $(FILE).backup || mv -f $(FILE).backup $(FILE) ; ${\n})

distclean: clean

$(BIN):
	$(foreach FILE,$(RESFILES),\
	  test -f $(FILE).backup || cp $(FILE) $(FILE).backup ; ${\n}\
	  $(LAZRES) $(FILE) $(FILE:.lrs=.lfm) ; ${\n})
	cp -f quickhash.ico quickhash_linux.ico
	$(LAZBUILD) $(OPTIONS) quickhash_linux.lpi

install:
	install -d -m 755 $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(BIN) $(DESTDIR)$(PREFIX)/bin
	install -d -m 755 $(DESTDIR)$(PREFIX)/share/applications
	install -m 644 misc/quickhash.desktop $(DESTDIR)$(PREFIX)/share/applications
	$(foreach SIZE,16 24 32 48 64 96 128,\
	  install -d -m 755 $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps ; ${\n}\
	  install -m 644 misc/quickhash_$(SIZE).png $(DESTDIR)$(PREFIX)/share/icons/hicolor/$(SIZE)x$(SIZE)/apps/quickhash.png ; ${\n})


