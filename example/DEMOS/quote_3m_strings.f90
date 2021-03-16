          program demo_quote
          use M_strings, only : quote
          implicit none
          character(len=:),allocatable :: str
          character(len=1024)          :: msg
          integer                      :: ios
          character(len=80)            :: inline
             do
                write(*,'(a)',advance='no')'Enter test string:'
                read(*,'(a)',iostat=ios,iomsg=msg)inline
                if(ios.ne.0)then
                   write(*,*)trim(inline)
                   exit
                endif

                ! the original string
                write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'

                ! the string processed by quote(3f)
                str=quote(inline)
                write(*,'(a)')'QUOTED     ['//str//']'

                ! write the string list-directed to compare the results
                write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
                write(*,*,iostat=ios,iomsg=msg,delim='none') inline
                write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
                write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
             enddo
          end program demo_quote
