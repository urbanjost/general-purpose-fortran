           program demo_M_xterm
           use M_xterm, only : xterm_colors, xterm_font
           use M_xterm, only : xterm_geometry, xterm_width, xterm_position
           use M_xterm, only : xterm_clear, xterm_keywords, xterm_labels
           call xterm_colors('background','black')
           call xterm_colors('foreground','white')
           call xterm_colors('cursor','red')
           call xterm_colors('mouse_foreground','red')
           call xterm_colors('mouse_background','white')
           call xterm_font('5')
           call xterm_geometry(cols=132,rows=36)
           call xterm_position(right=100,down=200)
           call xterm_keywords('raise')
           end program demo_M_xterm
