            program demo_draw_interpret
            use M_drawplus, only : draw_interpret
            character(len=:),allocatable :: draw_cmds(:)

            ! $FILTER variable -varname DRAW_CMDS

            draw_cmds=[ character(len=128) ::                                         &
            'set N=11; prefsize 600 200; vinit ;page -15 15 -5 5                    ',&
            'textsize .3 .3; linewidth 150/3; color 0; clear                        ',&
            'color  1;  spirograph  -10  0  N  1  N  5  1000  0  0  0               ',&
            'polyhatch 1; hatchang 45.0 ; hatchpitch 0.3 # turn on polygon hatching ',&
            'color  2;  spirograph   10  0  N  1  N  5  1000  0  0  2               ',&
            '                                                                       ',&
            'vflush; getkey ; vexit                                                 ',&
            '']

            ! $FILTER END

            call draw_interpret(draw_cmds,delimiters=';')
            end program demo_draw_interpret
