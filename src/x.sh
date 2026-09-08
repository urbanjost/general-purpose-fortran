(
exec 2>&1
make -f Makefile.ifx -j 8 -k |tee /tmp/x.out
)|tee x.out
exit
                              -coarray  
