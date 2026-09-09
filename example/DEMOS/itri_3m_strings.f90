      program demo_itri
      use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use M_strings, only : itri
      implicit none
      integer                      :: i
      integer(kind=int64)          :: ival64
      character(len=*),parameter   :: braces='(*(:"[",g0,"]",1x))'
      character(len=*),parameter   :: brace='(:"[",g0,"]")'
         ival64=1
         ! scalars are returned trimmed of spaces
         do i=1,19
            write(*,braces)itri(ival64),itri(-ival64)
            ival64=ival64*10+mod(i+1,10)
         enddo
         ! arrays are all returned right-justified
         ! and long enough to fit values of that kind
         write(*,brace) itri([10_int64, 123456890_int64, -huge(0_int64)])
         write(*,brace) itri([10, 123456890, -huge(0)])
         write(*,brace) itri([10_int16, 12345_int16, -huge(0_int16)])
         write(*,brace) itri([10_int8, 123_int8, -huge(0_int8)])

         ival64=-huge(0_int64)
         write(*,brace) &
         & itri(ival64,separator=' '),  &
         & itri(ival64,separator=char(int(z'B7'))) !  CenterDot 183  U+B7
      end program demo_itri
