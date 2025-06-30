         program demo_system_putchar
         use M_system,      only : system_putchar
         implicit none
         integer :: i, j
         integer :: iostat
         j=0
         do i=32,126 ! printable ASCII characters
            iostat=system_putchar(achar(i))
            if(iostat.lt.0)stop '<ERROR> *main* character '//achar(i)
            j=j+1
            if(j.ge.19)then
               iostat=system_putchar(new_line('a'))
               j=0
            endif
         enddo
         iostat=system_putchar(new_line('a'))
         end program demo_system_putchar
