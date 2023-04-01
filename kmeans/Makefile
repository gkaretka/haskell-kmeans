# build
VERSION = --with-ghc ghc-9.2.5
SHELL = /bin/bash
PROGRAM = ./main.out # replace with cabal run if you prefer running it with cabal

make:
	cabal build $(VERSION)

penv:
	python3 -m venv venv && \
	source ./venv/bin/activate && \
	python --version && \
	pip install -r requirements.txt

# without cabal
ghc_standalone:
	rm -rf build
	mkdir build
	ghc src/*.hs -odir build -hidir build -Wall -o main.out

# remove all generated stuff (including venv)
fresh:
	$(MAKE) clean && \
	rm -rf venv

# delete output dir and build dir
clean:
	rm -rf ./out/* dist-newstyle ./src/*.hi ./src/*.o build main.out

# dataset 1
test0:
	cat ./settings/s0.txt | $(PROGRAM) $(VERSION)

# dataset 1 with only two features
test0_2D:
	cat ./settings/s0_2d.txt | $(PROGRAM)$(VERSION)

# dataset 2
test1:
	cat ./settings/s1.txt | $(PROGRAM) $(VERSION)

# dataset 3
test2:
	cat ./settings/s2.txt | $(PROGRAM) $(VERSION)

# dataset 4
test3:
	cat ./settings/s3.txt | $(PROGRAM) $(VERSION)

# dataset 5
test4:
	cat ./settings/s4.txt | $(PROGRAM) $(VERSION)

# dataset 6
test5:
	cat ./settings/s5.txt | $(PROGRAM) $(VERSION)

# aks for parameters via stdin
standalone:
	$(PROGRAM) $(VERSION)

# show python visualization
vis:
	source ./venv/bin/activate && \
	python visualisation.py

visd:
	python visualisation.py