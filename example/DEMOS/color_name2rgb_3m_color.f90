           program demo_color_name2rgb
           use M_color, only : hue, color_name2rgb
           implicit none
           !
           ! list colors known to colorname2rgb(3f) & corresponding RGB values
           !
           character(len=20) :: name
           character(len=20) :: echoname
           real              :: red,green,blue
           integer           :: i
           TRYALL: do i=1,10000
              ! weird little thing where the color names have aliases
              ! that are numeric strings
              write(name,'(i0)')i
              ! get the RGB values and English name of the color
              call color_name2rgb(name,red,green,blue,echoname)
              ! the last color name is "Unknown" so the loop should exit
              if(echoname.eq.'Unknown')exit TRYALL
              ! display the English name and RGB values for the name
              write(*,*)echoname,int([red,green,blue])
           enddo TRYALL
           !write(*,*)'Number of colors found is ',i-1
           end program demo_color_name2rgb
