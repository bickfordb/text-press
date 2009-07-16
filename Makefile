
all: parsetest runtest

parsetest: ParseTest.hs Press
	ghc --make ParseTest.hs -o parsetest 

runtest: RunTest.hs Press
	ghc --make RunTest.hs -o runtest

test: parsetest runtest
	./parsetest
	./runtest

clean:
	- rm -rf ./main
