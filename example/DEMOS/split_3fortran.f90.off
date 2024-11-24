      program demo_split
         !use m_strings, only: split=>split2020
         implicit none
         character (len=:), allocatable :: input
         integer :: position, istart, iend
         input = "one,last example,,x,, ,,"
         position = 0
         ! write a number line
         write(*,'(t3,a)') repeat('1234567890',6)
         ! display the input line
         write(*,'(t3,a)') input
         ! step through the input string locating the bounds of the
         ! next token and printing it
         do while (position < len(input))
            istart = position + 1
            call split (input, set=', ', pos=position)
            iend = position - 1
            if(iend >= istart)then
               print '(t3,a,1x,i0,1x,i0)', input (istart:iend),istart,iend
            else
               ! maybe ignore null fields, maybe not ...
               write(*,'(t3,*(g0))')'null between ',iend,' and ',istart
            endif
         end do
      end program demo_split
