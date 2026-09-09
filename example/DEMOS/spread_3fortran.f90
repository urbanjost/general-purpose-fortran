      program demo_spread
      implicit none

      integer a1(4,3), a2(3,4), v(4), s

         write(*,'(a)' ) &
         'TEST SPREAD(3)                                      ', &
         '  SPREAD(3) is a FORTRAN90 function which replicates', &
         '  an array by adding a dimension.                   ', &
         ' '

         s = 99
         call printi('suppose we have a scalar S',s)

         write(*,*) 'to add a new dimension (1) of extent 4 call'
         call printi('spread( s, dim=1, ncopies=4 )',spread ( s, 1, 4 ))

         v = [ 1, 2, 3, 4 ]
         call printi(' first we will set V to',v)

         write(*,'(a)')' and then do "spread ( v, dim=2, ncopies=3 )"'
         a1 = spread ( v, dim=2, ncopies=3 )
         call printi('uses v as a column and makes 3 columns',a1)

         a2 = spread ( v, 1, 3 )
         call printi(' spread(v,1,3) uses v as a row and makes 3 rows',a2)

      contains
      ! CONVENIENCE ROUTINE; NOT DIRECTLY CONNECTED TO SPREAD(3)
      subroutine printi(title,a)
      use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,&
       & stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
      implicit none

      !@(#) print small 2d integer scalar, vector, matrix in row-column format

      character(len=*),parameter   :: all='(" ",*(g0,1x))'
      character(len=*),intent(in)  :: title
      character(len=20)            :: row
      integer,intent(in)           :: a(..)
      integer                      :: i

         write(*,all,advance='no')trim(title)
         ! select rank of input
         select rank(a)
         rank (0); write(*,'(a)')' (a scalar)'
            write(*,'(" > [ ",i0," ]")')a
         rank (1); write(*,'(a)')' (a vector)'
            ! find how many characters to use for integers
            write(row,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(a))))))+2
            ! use this format to write a row
            row='(" > [",*(i'//trim(row)//':,","))'
            do i=1,size(a)
               write(*,fmt=row,advance='no')a(i)
               write(*,'(" ]")')
            enddo
         rank (2); write(*,'(a)')' (a matrix) '
            ! find how many characters to use for integers
            write(row,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(a))))))+2
            ! use this format to write a row
            row='(" > [",*(i'//trim(row)//':,","))'
            do i=1,size(a,dim=1)
               write(*,fmt=row,advance='no')a(i,:)
               write(*,'(" ]")')
            enddo
         rank default
            write(stderr,*)'*printi* did not expect rank=', rank(a), &
             & 'shape=', shape(a),'size=',size(a)
            stop '*printi* unexpected rank'
         end select
         write(*,all) '>shape=',shape(a),',rank=',rank(a),',size=',size(a)
         write(*,*)

      end subroutine printi

      end program demo_spread
