COLLECTIONS=minecraft

all: setup

clean:
	rm -rf compiled doc

setup:
	raco setup $(COLLECTIONS)

link:
	raco pkg install --link $$(pwd)

unlink:
	raco pkg remove $$(basename $$(pwd))
