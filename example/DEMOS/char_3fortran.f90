      program demo_char
      implicit none
      integer, parameter :: ascii =  selected_char_kind ("ascii")
      character(len=1, kind=ascii ) :: c, esc
      integer :: i
        ! basic
         i=74
         c=char(i)
         write(*,*)'ASCII character ',i,'is ',c
         write(*,'(*(g0))')'Uppercase ASCII: ',(char(i),i=65,90)
         write(*,'(*(g0))')'lowercase ASCII: ',(char(i),i=97,122)
         esc=char(27)
         write(*,'(*(g0))')'Elemental: ',char([65,97,90,122])
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
