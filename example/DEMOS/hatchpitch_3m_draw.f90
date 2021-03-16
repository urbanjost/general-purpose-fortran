          program demo_hatchpitch
          use M_drawplus, only : draw_interpret
          character(len=:),allocatable :: draw_cmds(:)

          DRAW_CMDS=[ CHARACTER(LEN=128) :: &
          'prefsize 1000 200; vinit                                                    ',&
          'set b=.1; page -25-b 25+b -5-b 5+b;color 0;clear                            ',&
          'textsize .5 .6;font futura.l; leftjustify                                   ',&
          'circleprecision 3                                                           ',&
          '# draw circles with hatching                                                ',&
          'linewidth 150; polyhatch .true.; hatchang 30                                ',&
          'hatchpitch 1/3 ; color 7; circle X=-20 0 5; move2 X-4.9 -4.9;color 7;drawstr 1/3',&
          'hatchpitch 1/2 ; color 2; circle X=-10 0 5; move2 X-4.9 -4.9;color 7;drawstr 1/2',&
          'hatchpitch  1  ; color 6; circle X=-0  0 5; move2 X-4.9 -4.9;color 7;drawstr 1',&
          'hatchpitch  2  ; color 5; circle X=10  0 5; move2 X-4.9 -4.9;color 7;drawstr 2',&
          'hatchpitch  3  ; color 4; circle X=20  0 5; move2 X-4.9 -4.9;color 7;drawstr 3',&
          'getkey                                                                      ',&
          'vexit                                                                       ',&
          '']

          call draw_interpret(DRAW_CMDS,delimiters=';')
          end program demo_hatchpitch
