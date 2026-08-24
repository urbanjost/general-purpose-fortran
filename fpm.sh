#!/bin/bash
(
exec 2>&1
fpm test  --compiler ifx      --flag=-coarray         --profile=debug --verbose --c-compiler=cc
fpm test  --compiler gfortran --flag=-fcoarray=single --profile=debug --verbose
)|tee /tmp/test.log
exit
