
all: parsetest

parsetest: ParseTest.hs Press
	ghc --make ParseTest.hs -o parsetest 

clean:
	- rm -rf ./main
