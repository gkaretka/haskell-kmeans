# build
make:
	cabal build

# dataset 1
test0:
	cat ./settings/s0.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest0:
	cat ./settings/s0.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

# dataset 2
test1:
	cat ./settings/s1.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest1:
	cat ./settings/s1.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

# dataset 3
test2:
	cat ./settings/s2.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest2:
	cat ./settings/s2.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

# dataset 4
test3:
	cat ./settings/s3.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest3:
	cat ./settings/s3.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

# dataset 5
test4:
	cat ./settings/s4.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest4:
	cat ./settings/s4.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

# dataset 6
test5:
	cat ./settings/s5.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8
# 920,19s user
# 362,93s system
# 638% cpu
# 3:20,90 total

stest5:
	cat ./settings/s5.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1
# 119,82s user
# 4,60s system
# 100% cpu
# 2:04,39 total

# dataset 7
test6:
	cat ./settings/s6.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N8

stest6:
	cat ./settings/s6.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/kmeans-0.1.0.0/x/kmeans/build/kmeans/kmeans +RTS -N1

vis:
	python visualisation.py