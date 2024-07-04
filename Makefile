all:
	dmd -O -of=fuck src/*.d core/*.d -L-ldl

install: all
	cp -f fuck /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/fuck

