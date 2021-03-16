          program demo_iachar
          implicit none
          ! create function to convert uppercase letters to lowercase
             write(*,'(a)')lower('abcdefg ABCDEFG')
          contains
          !
          elemental pure function lower(str) result (string)
          ! Changes a string to lowercase
          character(*), intent(In)     :: str
          character(len(str))          :: string
          integer                      :: i
             string = str
             ! step thru each letter in the string in specified range
             do i = 1, len(str)
                select case (str(i:i))
                case ('A':'Z') ! change letter to miniscule
                   string(i:i) = char(iachar(str(i:i))+32)
                case default
                end select
             end do
          end function lower
          !
          end program demo_iachar
