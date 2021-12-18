     program demo_help_command
     use M_help, only : help_command
     character(len=:),allocatable :: help_text(:)
     integer                      :: position(2)
     position=[0,23]
     help_text=[character(len=80) :: &
     '==============================================',&
     '   A sample help text file.                   ',&
     '   Note the first line MUST start with "==="  ',&
     '==============================================',&
     'SUMMARY                                       ',&
     '  This is usually a crib sheet                ',&
     '==============================================',&
     'SECTION1                                      ',&
     'topic1                                        ',&
     '   A description of topic 1                   ',&
     '                                              ',&
     '   and any general text you want              ',&
     '                                              ',&
     'topic2  A description of topic 2              ',&
     'topic3                                        ',&
     '   A description of topic 3                   ',&
     '   more  description of topic 3               ',&
     '   and more description of topic 3 a          ',&
     '   and more description of topic 3 b          ',&
     '   and more description of topic 3 c          ',&
     '   and more description of topic 3 d          ',&
     '   and more description of topic 3 e          ',&
     '   and more description of topic 3 f          ',&
     '   and more description of topic 3 g          ',&
     '   and more description of topic 3 h          ',&
     '   and more description of topic 3 i          ',&
     '   and more description of topic 3 j          ',&
     '   and more description of topic 3 k          ',&
     '   and more description of topic 3 l          ',&
     '   and more description of topic 3 m          ',&
     '   and more description of topic 3 n          ',&
     '   and more description of topic 3 o          ',&
     '   and more description of topic 3 p          ',&
     '   and more description of topic 3 q          ',&
     '   and more description of topic 3 r          ',&
     '   and more description of topic 3 s          ',&
     '   and more description of topic 3 t          ',&
     '   and more description of topic 3 u          ',&
     '   and more description of topic 3 v          ',&
     '   and more description of topic 3 w          ',&
     '   and more description of topic 3 x          ',&
     '   and more description of topic 3 y          ',&
     '   and more description of topic 3 z          ',&
     '==============================================',&
     'SECTION2                                      ',&
     'topic4  A description of topic 4              ',&
     '   this is the last part of SECTION1          ',&
     'topic5                                        ',&
     '  This is all about the fifth topic and is    ',&
     '  just displayed as-is. The text cannot start ',&
     '  in column one or it will be seen as the     ',&
     '  beginning of a topic.                       ',&
     '==============================================',&
     '                                              ' ]

     write(*,*)'>>>>>'
     call help_command(help_text,'',position)
     write(*,*)'>>>>>topic1'
     call help_command(help_text,'topic1',position)
     write(*,*)'>>>>>topics'
     call help_command(help_text,'topics',position)
     write(*,*)'>>>>>manual'
     call help_command(help_text,'manual',position)
     end program demo_help_command
