
all: parsetest

parsetest: ParseTest.hs Press
	ghc --make ParseTest.hs -o parsetest 

test: parsetest
	./parsetest

clean:
	- rm -rf ./main
