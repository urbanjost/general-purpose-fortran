      program demo_achar
      use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
      implicit none
      integer :: i
         i=65
         write(*,'("decimal     =",i0)')i
         write(*,'("character   =",a1)')achar(i)
         write(*,'("binary      =",b0)')i
         write(*,'("octal       =",o0)')i
         write(*,'("hexadecimal =",z0)')i

         write(*,'(8(i3,1x,a,1x))')(i,achar(i), i=32,126)

         write(*,'(a)')upper('Mixed Case')
         !
         !Shows how to place a non-advancing status counter...
         !
            do i=0,100,10
               write(*,fmt="(A1,A,t21,F6.2,A)",advance="NO") achar(13), &
               & "Percent Complete: ", real(i), "%"
               call system_usleep(1000000) !give a delay in microseconds
            enddo
            write(*,*)
      contains
      ! a classic use of achar(3) is to convert the case of a string

      pure elemental function upper(str) result (string)
      !
      !$@(#) upper(3): function to return a trimmed uppercase-only string
      !
      ! input string to convert to all uppercase
      character(*), intent(in)      :: str
      ! output string that contains no miniscule letters
      character(len(str))           :: string
      integer                       :: i, iend
      integer,parameter             :: toupper = iachar('A')-iachar('a')
         iend=len_trim(str)
         ! initialize output string to trimmed input string
         string = str(:iend)
         ! process each letter in the string
         do concurrent (i = 1:iend)
             select case (str(i:i))
             ! located miniscule letter
             case ('a':'z')
                ! change miniscule to majuscule letter
                string(i:i) = achar(iachar(str(i:i))+toupper)
             end select
         enddo
      end function upper

      subroutine system_usleep(microseconds)
      use,intrinsic       :: iso_c_binding, only: c_int
      integer,intent(in)  :: microseconds
      integer(kind=c_int) :: status
      interface
         function c_usleep(mseconds) bind (c,name="usleep")
            import
            ! should be unsigned int (not available in Fortran).
            ! OK until highest bit gets set.
            integer(c_int)       :: c_usleep
            integer(c_int), intent(in), value :: mseconds
         end function c_usleep
      end interface
         if(microseconds > 0)then
            status=c_usleep(int(microseconds,kind=c_int))
         endif
      end subroutine system_usleep

      end program demo_achar
