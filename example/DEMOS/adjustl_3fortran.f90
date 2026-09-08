      program demo_adjustl
      implicit none
      character(len=20)            :: str
      character(len=:),allocatable :: astr
      character(len=*),parameter   :: au= '(a,"[",a,"]")'
      integer :: istart, iend

        ! basic use
          str='   sample string  '
          write(*,au) 'original: ',str

        ! note the allocated string stays the same length
        ! and is not trimmed by just an adjustl(3) call.
          astr=adjustl(str)
          write(*,au) 'adjusted: ',astr

        ! a fixed-length string can be printed cropped
        ! combining adjustl(3) with trim(3)
          write(*,au) 'trimmed:  ',trim(adjustl(str))

        ! or even printed without adjusting the string a
        ! cropped substring can be printed
          iend=len_trim(str)
          istart= verify(str, ' ') ! first non-blank character
          write(*,au) 'substring:',str(istart:iend)

        ! to generate an actually trimmed allocated variable
          astr = trim(adjustl(str))
          write(*,au) 'trimmed:  ',astr

      end program demo_adjustl
