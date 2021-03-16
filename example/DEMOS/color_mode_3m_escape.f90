          program demo_color_mode
             use M_escape, only : color, color_mode, &
            ! FOREGROUND COLORS
               & fg_red, fg_cyan, fg_magenta, fg_blue, &
               & fg_green, fg_yellow, fg_white, fg_ebony, &
               & fg_default, &
            ! BACKGROUND COLORS
               & bg_red, bg_cyan, bg_magenta, bg_blue, &
               & bg_green, bg_yellow, bg_white, bg_ebony, &
               & bg_default, &
            ! ATTRIBUTES
               & bold, italic, inverse, underline,  &
               & unbold, unitalic, uninverse, ununderline,  &
               & reset, &
            ! DISPLAY
               & clear
             implicit none
               write(*,'(*(g0))')fg_red,bg_green,bold,' Hello! ',reset

               write(*,'(a)')color(' Hello! ',&
                & fg=fg_white,bg=bg_red,style=italic//bold)
               call color_mode(.false.)
               write(*,'(a)')color(' Hello! ',&
                & fg=fg_red,bg=bg_red,style=italic//bold)
          end program demo_color_mode
