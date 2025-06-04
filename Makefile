PREFIX=/usr/local
CC=tcc

bb: bb.scm
	ol -i lib/robusta -x c bb.scm | $(CC) -x c - -o bb -lsqlite3
install: bb
	cp -v bb $(PREFIX)/bin/bb
uninstall: bb
	rm -v $(PREFIX)/bin/bb
clean:
	rm -f bb
