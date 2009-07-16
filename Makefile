
all: parsetest runtest

parsetest: ParseTest.hs Text/Press
	ghc --make ParseTest.hs -o parsetest 

runtest: RunTest.hs Text/Press
	ghc --make RunTest.hs -o runtest

test: parsetest runtest
	./parsetest
	./runtest

clean:
	- rm -rf ./runtest ./parsetest

