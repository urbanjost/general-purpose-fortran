      program demo_iostat
      implicit none
      real               :: value
      integer            :: ios
      character(len=256) :: message
         write(*,*)'Begin entering numeric values, one per line'
         do
            read(*,*,iostat=ios,iomsg=message)value
            if(ios.eq.0)then
               write(*,*)'VALUE=',value
            elseif( is_iostat_end(ios) ) then
               stop 'end of file. Goodbye!'
            else
               write(*,*)'ERROR:',ios,trim(message)
               exit
            endif
            !
         enddo
      end program demo_iostat
