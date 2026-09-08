      program demo_in
      use M_math, only : in
      implicit none
         write(*, *)in(1, 3, 10)
         write(*, *)in(1, -30, 10)
         write(*, *)in(1, 30, 10)
         write(*, *)in(-11.11, 5.5, 9.999)
      end program demo_in
