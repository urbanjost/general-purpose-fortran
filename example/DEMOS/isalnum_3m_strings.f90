          program demo_isdigit

           use M_strings, only : isdigit, isspace, switch
           implicit none
           character(len=10),allocatable :: string(:)
           integer                       :: i
              string=[&
              & '1 2 3 4 5 ' ,&
              & 'letters   ' ,&
              & '1234567890' ,&
              & 'both 8787 ' ]
              ! if string is nothing but digits and whitespace return .true.
              do i=1,size(string)
                 write(*,'(a)',advance='no')'For string['//string(i)//']'
                 write(*,*) &
                 all(isdigit(switch(string(i))) .or. &
                 & isspace(switch(string(i))))
              enddo

           end program demo_isdigit
