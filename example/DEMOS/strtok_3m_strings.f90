           program demo_strtok
           use M_strings, only : strtok
           implicit none
           character(len=264)          :: inline
           character(len=*),parameter  :: delimiters=' ;,'
           integer                     :: ios, itoken, istart, iend
              do ! read lines from stdin until end-of-file or error
                 read (unit=*,fmt="(a)",iostat=ios) inline
                 if(ios.ne.0)stop
                 ! must set ITOKEN=0 before looping on strtok(3f)
                 ! on a new string.
                 itoken=0
                 do while &
                 &( strtok(inline,itoken,istart,iend,delimiters) )
                    print *, itoken,&
                    & 'TOKEN=['//(inline(istart:iend))//']',istart,iend
                 enddo
              enddo
           end program demo_strtok
