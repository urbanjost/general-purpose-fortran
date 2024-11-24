      program demo_achar
      use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
      implicit none
      integer :: i
         i=65
         write(*,'("decimal     =",i0)')i
         write(*,'("character   =",a1)')achar(i)
         write(*,'("binary      =",b0)')achar(i)
         write(*,'("octal       =",o0)')achar(i)
         write(*,'("hexadecimal =",z0)')achar(i)

         write(*,'(8(i3,1x,a,1x))')(i,achar(i), i=32,126)

         write(*,'(a)')upper('Mixed Case')
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
      end program demo_achar
