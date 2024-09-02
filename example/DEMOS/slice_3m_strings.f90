      program demo_slice
      use M_strings, only: slice
      implicit none
      integer                    :: i
      character(len=*),parameter :: &
      & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
      integer,allocatable        :: ibegin(:), iend(:) ! output arrays of positions
      character(len=*),parameter :: title='(80("="),t1,a)'
         write(*,*)'INPUT LINE:['//line//']'
         !
         write(*,title)'typical call: '
         call slice(line,ibegin,iend)
         call printme()
         !
         write(*,title)'custom list of delimiters=":|" : '
         call slice(line,ibegin,iend,delimiters=':|',nulls='ignore')
         call printme()
         !
         write(*,title)'delimiters=":|", and count null fields: '
         call slice(line,ibegin,iend,delimiters=':|',nulls='return')
         call printme()
         !
         write(*,title)'default delimiters and return null fields: '
         call slice(line,ibegin,iend,delimiters='',nulls='return')
         call printme()
      contains
      subroutine printme()
         write(*,'((*(:/,3x,"[",g0,"]")))')&
                 & (line(ibegin(i):iend(i)),i=1,size(ibegin))
         write(*,'(*(g0,1x))')'SIZE:',size(ibegin)
      end subroutine printme
      end program demo_slice
