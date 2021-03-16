          program demo_scale
          use M_drawplus, only : draw_interpret
          character(len=:),allocatable :: draw_cmds(:)
          draw_cmds=[ character(len=128) ::                                      &
          '# set up display                                                    ',&
          'prefsize 300 300;prefposition 200 10;vinit X11;                     ',&
          'set SIZE=1.2                                                        ',&
          'color 3;clear;color 2; ortho2 -SIZE SIZE -SIZE SIZE                 ',&
          'set X=-0.75 Y=0.75                                                  ',&
          '# create an object to repeatedly draw                               ',&
          'makeobj 1                                                           ',&
          'polyfill .true.;color 1; rect 0 0 X Y                               ',&
          'polyfill .false.;linewidth 200;color 2 ;rect 0 0 X Y                ',&
          'closeobj                                                            ',&
          '# draw object, rotating coordinate system between instantiations   ' ,&
          'pushmatrix                                                          ',&
          'scale 1.1 2.0                                                       ',&
          'callobj 1                                                           ',&
          'scale 0.5 0.5                                                       ',&
          'callobj 1                                                           ',&
          'circle 0 0 X/3                                                      ',&
          'popmatrix                                                           ',&
          'color 5;circle 0 0 X/3                                              ',&
          'getkey;vexit                                                        ']
          write(*,'(a)')draw_cmds
          call draw_interpret(draw_cmds,delimiters=';')
          end program demo_scale
