      program demo_adjustl
      implicit none
      character(len=20) :: str = '   sample string'
      character(len=:),allocatable :: astr
      integer :: length

         ! basic use
          write(*,'(a,"[",a,"]")') 'original: ',str
          str=adjustl(str)
          write(*,'(a,"[",a,"]")') 'adjusted: ',str

          ! a fixed-length string can be printed
          ! trimmed using trim(3f) or len_trim(3f)
          write(*,'(a,"[",a,"]")') 'trimmed:  ',trim(str)
          length=len_trim(str)
          write(*,'(a,"[",a,"]")') 'substring:',str(:length)

          ! note an allocatable string stays the same length too
          ! and is not trimmed by just an adjustl(3f) call.
          astr='    allocatable string   '
          write(*,'(a,"[",a,"]")') 'original:',astr
          astr = adjustl(astr)
          write(*,'(a,"[",a,"]")') 'adjusted:',astr
          ! trim(3f) can be used to change the length
          astr = trim(astr)
          write(*,'(a,"[",a,"]")') 'trimmed: ',astr

      end program demo_adjustl
