      program demo_print
      implicit none
      real :: a=11.11, s=sqrt(12.0)
      integer :: j=753210
      character(len=*),parameter :: commas='(*(g0:,","))'

       ! List-directed output is frequently specified
        PRINT *, A, S

       ! a format may be placed on the print(7f) statement
        PRINT '(*(g0,1x))', A, S, J

       ! the format may be in a character variable
        print commas, a, s, j

       ! or may be in a labeled format statement
        PRINT 10, A, S, J
        10 FORMAT (2E16.3,1x,I0)

      end program demo_print
