all:
	dmd -O -of=fuck src/*.d core/*.d

install: all
	cp fuck /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/fuck

