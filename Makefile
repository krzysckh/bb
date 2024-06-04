PREFIX=/usr/local

bb:
	ol -O2 -x c bb.scm | $(CC) -x c - -o bb
install: bb
	cp -v bb $(PREFIX)/bin/bb
uninstall: bb
	rm -v $(PREFIX)/bin/bb
clean:
	rm bb
