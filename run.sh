quiet fpm test --verbose                                  |tee x.out
quiet fpm test --compiler ifx --flag '-coarray' --verbose |tee -a x.out
quiet fpm test --compiler ifort --flag -coarray --verbose |tee -a x.out
