      program demo_iachar
      implicit none
         ! basic usage
          ! just does a string one character long
          write(*,*)iachar('A')
          ! elemental: can do an array of letters
          write(*,*)iachar(['A','Z','a','z'])

         ! convert all characters to lowercase
          write(*,'(a)')lower('abcdefg ABCDEFG')
      contains
      !
      pure elemental function lower(str) result (string)
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
