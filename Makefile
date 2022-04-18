TARGETS = main.exe

all: build link

build:
		dune build bin/main.exe

link: $(TARGETS)

%.exe:
		if [ ! -f $@ ]; then ln -s _build/default/bin/$@ . ; fi

clean:
		rm -rf _build $(TARGETS)