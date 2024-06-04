PREFIX=/usr/local
CC=tcc

bb:
	ol -x c bb.scm | $(CC) -x c - -o bb
install: bb
	cp -v bb $(PREFIX)/bin/bb
uninstall: bb
	rm -v $(PREFIX)/bin/bb
clean:
	rm bb
