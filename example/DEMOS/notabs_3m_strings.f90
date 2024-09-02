     program demo_notabs

     !  test filter to remove tabs and trailing white space from input
     !  on files up to 1024 characters wide
     use M_strings, only : notabs
     character(len=1024) :: in,out
     integer             :: iostat,iout
        do
           read(*,'(A)',iostat=iostat)in
           if(iostat /= 0) exit
           call notabs(in,out,iout)
           write(*,'(a)')out(:iout)
        enddo
     end program demo_notabs
