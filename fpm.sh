#!/bin/bash
###############################################################################
(
exec 2>&1
fpm test  --compiler ifx      --flag=-coarray         --profile=debug --verbose --c-compiler=cc
)|tee /tmp/test_ifx.log
###############################################################################
(
exec 2>&1
fpm test  --compiler gfortran --flag=-fcoarray=single --profile=debug --verbose
)|tee /tmp/test_gfortran.log
###############################################################################
(
exec 2>&1
fpm test  --compiler flang  --profile=debug --verbose
)|tee /tmp/test_flang.log
###############################################################################
(
exec 2>&1
export LFORTRAN_LINKER=gcc
export LFORTRAN_LINKER=clang
export OSTYPE=Windows-NT
export OS=Windows-NT
#export OSTYPE=cygwin
#export OS=cygwin
fpm --version --verbose

fpm build --verbose --compiler=lfortran --flag='--no-style-suggestions --realloc-lhs-arrays --print-leading-space --std f23 --use-loop-variable-after-loop --cpp --legacy-array-sections --linker-path=/usr/bin/link'
#Tip: If there is a linker issue, switch the linker using --linker=<CC>
#option or create an environment variable `export LFORTRAN_LINKER=<CC>`,
#where CC is clang or gcc
#Also, if required use --linker-path=<PATH>, where PATH has location to
#look for the linker executable
)|tee /tmp/test_lfortran.log
###############################################################################
exit
###############################################################################
