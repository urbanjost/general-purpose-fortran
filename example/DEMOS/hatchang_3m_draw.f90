          program demo_hatchang
          use M_drawplus, only : draw_interpret
          character(len=:),allocatable :: draw_cmds(:)

          DRAW_CMDS=[ CHARACTER(LEN=128) :: &
          'prefsize 1000 200; vinit                                     ',&
          'set b=.4; page -25-b 25+b -5-b 5+b; color 0;clear            ',&
          'textsize .6 .7;font futura.l;centertext .true.               ',&
          'leftjustify; linewidth 50; polyhatch .true.; hatchpitch 1/2  ',&
          '# draw circles with hatching                                 ',&
          'linewidth 90;hatchang  90.1; color  7;  circle X=-20  Y=0  5 ',&
          'move2  X-4.9 Y=-4.9;color 7;linewidth 60;drawstr  90 degrees ',&
          'linewidth 90;hatchang  45  ; color  2;  circle X=-10  Y=0  5 ',&
          'move2  X-4.9 Y=-4.9;color 7;linewidth 60;drawstr  45 degrees ',&
          'linewidth 90;hatchang   0  ; color  6;  circle X=-0   Y=0  5 ',&
          'move2  X-4.9 Y=-4.9;color 7;linewidth 60;drawstr   0 degrees ',&
          'linewidth 90;hatchang -45  ; color  5;  circle X=10   Y=0  5 ',&
          'move2  X-4.9 Y=-4.9;color 7;linewidth 60;drawstr -45 degrees ',&
          'linewidth 90;hatchang -90  ; color  4;  circle X=20   Y=0  5 ',&
          'move2  X-4.9 Y=-4.9;color 7;linewidth 60;drawstr -90 degrees ',&
          'linewidth 130                                                ',&
          'move2 0 0;draw2 -5 0                                         ',&
          'move2 -5 0;draw2 -4.4  0.3                                   ',&
          'move2 -5 0;draw2 -4.4 -0.3                                   ',&
          'rightjustify                                                 ',&
          'linewidth 60                                                 ',&
          'move2 -5 0;drawstr 0 degrees                                 ',&
          'getkey                                                       ',&
          'vexit                                                        ',&
          '']

          call draw_interpret(DRAW_CMDS,delimiters=';')
          end program demo_hatchang
