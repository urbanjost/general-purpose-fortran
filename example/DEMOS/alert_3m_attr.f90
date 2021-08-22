           program demo_alert
           use M_attr, only : alert, attr
           implicit none
              call alert("error", "Say you didn't!")
              call alert("warn",  "I wouldn't if I were you, Will Robinson.")
              call alert("info",  "I fixed that for you, but it was a bad idea.")
              call alert("debug", "Who knows what is happening now?.")
              call alert("???    ",  "not today you don't")
              ! call to just update the macros
              call alert()
              ! conventional call to ATTR(3f) using the ALERT(3f)-defined macros
              write(*,*)attr('<bo>The year was <g><YE></g>, the month was <g><MO></g>')
           end program demo_alert
