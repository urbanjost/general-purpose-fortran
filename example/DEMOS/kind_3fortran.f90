      program demo_kind
      implicit none
      integer,parameter :: dc = kind(' ')
      integer,parameter :: dl = kind(.true.)

         print *, "The default character kind is ", dc
         print *, "The default logical kind is ", dl

      end program demo_kind
