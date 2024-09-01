all:
	dmd -O -of=fuck src/*.d core/*.d -L-ldl

install: all
	install fuck /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/fuck

