make clean
quiet make -j 10 -f Makefile test|tee /tmp/x1
make clean
quiet make -j 10 -f Makefile.i test|tee /tmp/x2
