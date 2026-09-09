      program demo_alert
      use M_attr, only : alert, attr, attr_mode
      implicit none
      real X
       call attr_mode(manner='plain')
       call attr_mode(manner='color')
       call alert("error",&
                 "Say you didn't!")
       call alert("warn", &
                 "I wouldn't if I were you, Will Robinson.")
       call alert("info", &
                 "I fixed that for you, but it was a bad idea.")
       call alert("debug", &
                 "Who knows what is happening now?.")
       call alert("???    ",  "not today you don't")
       ! call to just update the macros
       call alert()
       ! conventional call to ATTR(3f) using the ALERT(3f)-defined macros
       write(*,*)attr(&
               '<bo>The year was <g><YE></g>, the month was <g><MO></g>')
       ! optional arguments
       X=211.3
       call alert('error',&
               'allowed range of X is 0 <lt> X <lt> 100, X=<r>',X)
       ! up to twenty values are allowed of intrinsic type
       call alert('info','values are<g>',10,234.567,&
               cmplx(11.0,22.0),123.456d0,'</g>today')
      end program demo_alert
