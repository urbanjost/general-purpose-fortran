      program demo_fmt
      use :: M_attr, only : fmt
      implicit none
      character(len=:),allocatable :: output

         output=fmt(10,"'[',i0,']'")
         write(*,*)'result is ',output

         output=fmt(10.0/3.0,"'[',g0.5,']'")
         write(*,*)'result is ',output

         output=fmt(.true.,"'The final answer is [',g0,']'")
         write(*,*)'result is ',output

    end program demo_fmt
