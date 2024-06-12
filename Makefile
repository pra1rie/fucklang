
all:
	dmd -of=fuck src/*.d core/*.d

install: all
	cp fuck ~/bin/
