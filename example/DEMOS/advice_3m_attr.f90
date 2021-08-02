           program demo_advice
           use M_attr, only : advice, attr
           implicit none
              call advice("error", "Say you didn't!")
              call advice("warn",  "I wouldn't if I were you, Will Robinson.")
              call advice("info",  "I fixed that for you, but it was a bad idea.")
              call advice("???    ",  "not today you don't")
              ! call to just update the macros
              call advice()
              ! conventional call to ATTR(3f) using the ADVICE(3f)-defined macros
              write(*,*)attr('<bo>The year was <g><YE></g>, the month was <g><MO></g>')
           end program demo_advice
