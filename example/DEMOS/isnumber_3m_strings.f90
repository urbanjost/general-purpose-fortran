           program demo_isnumber
           use M_strings, only : isnumber
           implicit none
           character(len=256) :: line
           real               :: value
           integer            :: ios
           integer            :: answer
           character(len=256) :: message
           character(len=:),allocatable :: description
              write(*,*)'Begin entering values, one per line'
              do
                 read(*,'(a)',iostat=ios)line
                 !
                 ! try string as number using list-directed input
                 line=''
                 read(line,*,iostat=ios,iomsg=message) value
                 if(ios.eq.0)then
                    write(*,*)'VALUE=',value
                 elseif( is_iostat_end(ios) ) then
                    stop 'end of file'
                 else
                    write(*,*)'ERROR:',ios,trim(message)
                 endif
                 !
                 ! try string using isnumber(3f)
                 answer=isnumber(line,msg=description)
                 if(answer.gt.0)then
                    write(*,*) &
                    & ' for ',trim(line),' ',answer,':',description
                 else
                    write(*,*) &
                    & ' ERROR for ',trim(line),' ',answer,':',description
                 endif
                 !
              enddo
           end program demo_isnumber
