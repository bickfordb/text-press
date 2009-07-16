
all: parsetest runtest

parsetest: ParseTest.hs Text
	ghc --make ParseTest.hs -o parsetest 

runtest: RunTest.hs Text
	ghc --make RunTest.hs -o runtest

test: parsetest runtest
	./parsetest
	./runtest

clean:
	- rm -rf ./main
