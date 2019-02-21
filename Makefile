.PHONY: all build clean 

all: build

build:
	dune build --profile release

install:
	dune install

clean:
	rm -rf _build *.install
