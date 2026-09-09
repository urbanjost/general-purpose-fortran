      program demo_len_trim
      implicit none
      character(len=:),allocatable :: string
      integer :: i
      ! basic usage
         string=" how long is this string?     "
         write(*,*) string
         write(*,*)'UNTRIMMED LENGTH=',len(string)
         write(*,*)'TRIMMED LENGTH=',len_trim(string)

         ! print string, then print substring of string
         string='xxxxx   '
         write(*,*)string,string,string
         i=len_trim(string)
         write(*,*)string(:i),string(:i),string(:i)
         !
        ! elemental example
         ELE:block
         ! an array of strings may be used
         character(len=:),allocatable :: tablet(:)
         tablet=[character(len=256) :: &
         & ' how long is this string?     ',&
         & 'and this one?']
            write(*,*)'UNTRIMMED LENGTH=  ',len(tablet)
            write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
            write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
         endblock ELE
         !
      end program demo_len_trim
