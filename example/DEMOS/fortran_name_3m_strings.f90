            program demo_fortran_name
            use M_strings, only : fortran_name
            implicit none
            character(len=*),parameter :: names(*)=[character(len=20) ::  &
             & '_name',         'long_variable_name', 'name_',         &
             & '12L',           'a__b__c  ',          'PropertyOfGas', &
             & '3%3',           '$NAME',              ' ',             &
             & 'Variable-name', 'A',                  'x@x' ]
            integer :: i
               write(*,'(i3,1x,a20,1x,l1)')&
               & (i,names(i),fortran_name(names(i)),i=1,size(names))
            end program demo_fortran_name
