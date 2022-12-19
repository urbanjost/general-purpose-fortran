      program demo_char
      implicit none
      integer, parameter :: ascii =  selected_char_kind ("ascii")
      character(len=1, kind=ascii ) :: c
      integer :: i
        ! basic
         i=74
         c=char(i)
         write(*,*)'ASCII character ',i,'is ',c
        !
         print *, 'a selection of ASCII characters (shows hex if not printable)'
         do i=0,127,10
            c = char(i,kind=ascii)
            select case(i)
            case(32:126)
               write(*,'(i3,1x,a)')i,c
            case(0:31,127)
               ! print hexadecimal value for unprintable characters
               write(*,'(i3,1x,z2.2)')i,c
            case default
               write(*,'(i3,1x,a,1x,a)')i,c,'non-standard ASCII'
            end select
         enddo

      end program demo_char
