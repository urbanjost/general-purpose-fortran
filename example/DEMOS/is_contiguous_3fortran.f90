          program demo_is_contiguous
          implicit none
          intrinsic is_contiguous
          REAL, DIMENSION (1000, 1000), TARGET :: A
          REAL, DIMENSION (:, :), POINTER       :: IN, OUT
          IN => A              ! Associate IN with target A
          OUT => A(1:1000:2,:) ! Associate OUT with subset of target A
          !
          write(*,*)'IN is ',IS_CONTIGUOUS(IN)
          write(*,*)'OUT is ',IS_CONTIGUOUS(OUT)
          !
          end program demo_is_contiguous
