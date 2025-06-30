#!/bin/bash
(
exec 2>&1
fpm build --compiler ifx      --flag=-coarray         --profile=debug --verbose
fpm build --compiler gfortran --flag=-fcoarray=single --profile=debug --verbose
fpm test  --compiler ifx      --flag=-coarray         --profile=debug --verbose
fpm test  --compiler gfortran --flag=-fcoarray=single --profile=debug --verbose
)|tee /tmp/test.log
exit
