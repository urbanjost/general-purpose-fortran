#!/bin/bash
# @(#) If compiler is found in path run and log "fpm.test"

case "$(uname -a)" in
*WSL*) X11=nox11 ;;
*) X11=x11 ;;
esac

ulimit -s unlimited
git branch -a
###############################################################################
if which ifx 2>&1 >/dev/null;then
   git switch ifx-posix-${X11}
   (
   exec 2>&1
   fpm test --compiler ifx --flag=-coarray --profile=debug --verbose --c-compiler=cc
   )|tee /tmp/test_ifx.log
fi
###############################################################################
if which gfortran 2>&1 >/dev/null;then
   git switch gfortran-posix-${X11}
   (
   exec 2>&1
   fpm test  --compiler gfortran --flag=-fcoarray=single --profile=debug --verbose
   )|tee /tmp/test_gfortran.log
fi
###############################################################################
if which flang 2>&1 >/dev/null;then
   git switch flang-posix-${X11}
   (
   exec 2>&1
   fpm test  --compiler flang  --profile=debug --verbose
   )|tee /tmp/test_flang.log
fi
###############################################################################
if which lfortran 2>&1 >/dev/null;then
   git switch lfortran-posix-${X11}
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
fi
###############################################################################
git switch master
exit
###############################################################################
# NOTES # NOTES # NOTES # NOTES # NOTES # NOTES # NOTES # NOTES # NOTES # NOTES
###############################################################################
The difference between X11 Windows and no X11 Windows:

      diff --git a/fpm.toml b/fpm.toml
      -   link = ["curl", "ncurses", "sqlite3", "readline", "X11"]
      +   link = ["curl", "ncurses", "sqlite3", "readline"]
      
      rename from src/C-X11.c to src/C-X11.c.off
      
      diff --git a/src/C-drivers.c b/src/C-drivers.c
      -#define TEK
      -#define XTEK
      -#define X11
      +#undef TEK
      +#undef XTEK
      +#undef X11
###############################################################################
