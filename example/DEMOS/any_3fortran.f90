      program demo_any
      implicit none
      logical,parameter :: T=.true., F=.false.
      integer           :: a(2,3), b(2,3)
      logical           :: bool
        ! basic usage
         bool = any([F,F,T,F])
         print *,bool
         bool = any([F,F,F,F])
         print *,bool
        ! fill two integer arrays with values for testing
         a = 1
         b = 1
         b(:,2) = 2
         b(:,3) = 3
        ! using any(3) with logical expressions you can compare two arrays
        ! in a myriad of ways
         ! first, print where elements of b are bigger than in a
         call printl( 'first print b > a             ', b > a         )
         ! now use any() to test
         call printl( 'any true values?  any(b > a)  ', any(b > a )   )
         call printl( 'again by columns? any(b > a,1)', any(b > a, 1) )
         call printl( 'again by rows?    any(b > a,2)', any(b > a, 2) )
      contains
      ! CONVENIENCE ROUTINE. this is not specific to ANY()
      subroutine printl(title,a)
      use, intrinsic :: iso_fortran_env, only : &
       & stderr=>ERROR_UNIT,&
       & stdin=>INPUT_UNIT,&
       & stdout=>OUTPUT_UNIT
      implicit none

      !@(#) print small 2d logical scalar, vector, or matrix

      character(len=*),parameter   :: all='(*(g0,1x))'
      character(len=*),parameter   :: row='(" > [ ",*(l1:,","))'
      character(len=*),intent(in)  :: title
      logical,intent(in)           :: a(..)
      integer                      :: i
         write(*,*)
         write(*,all,advance='no')trim(title),&
          & ' : shape=',shape(a),',rank=',rank(a),',size=',size(a)
         ! get size and shape of input
         select rank(a)
         rank (0); write(*,'(a)')'(a scalar)'
            write(*,fmt=row,advance='no')a
            write(*,'(" ]")')
         rank (1); write(*,'(a)')'(a vector)'
            do i=1,size(a)
               write(*,fmt=row,advance='no')a(i)
               write(*,'(" ]")')
            enddo
         rank (2); write(*,'(a)')'(a matrix) '
            do i=1,size(a,dim=1)
               write(*,fmt=row,advance='no')a(i,:)
               write(*,'(" ]")')
            enddo
         rank default
            write(stderr,*)'*printl* did not expect rank=', rank(a), &
             & 'shape=', shape(a),'size=',size(a)
            stop '*printl* unexpected rank'
         end select

      end subroutine printl

      end program demo_any
