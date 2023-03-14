# build
make:
	cabal build --with-ghc ghc-9.2.5

# delete output dir and build dir
clean:
	rm -rf ./out/* && rm -rf dist-newstyle

# dataset 1
test0:
	cat ./settings/s0.txt | cabal run

# dataset 1 with only two features
test0_2D:
	cat ./settings/s0_2d.txt | cabal run

# dataset 2
test1:
	cat ./settings/s1.txt | cabal run

# dataset 3
test2:
	cat ./settings/s2.txt | cabal run

# dataset 4
test3:
	cat ./settings/s3.txt | cabal run

# dataset 5
test4:
	cat ./settings/s4.txt | cabal run

# dataset 6
test5:
	cat ./settings/s5.txt | cabal run

# aks for parameters via stdin
standalone:
	cabal run

# show python visualization
vis:
	python visualisation.py