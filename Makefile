bb:
	ol -O2 -x c bb.scm | $(CC) -x c - -o bb
