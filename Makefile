
all: press-test

press-test: PressTest.hs Text/Press Text
	ghc --make PressTest -o press-test

test: press-test
	./press-test

clean:
	- rm -rf ./test 

