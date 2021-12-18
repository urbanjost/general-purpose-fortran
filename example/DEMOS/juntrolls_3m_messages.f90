     program demo_juntrolls
     use M_messages, only : juntrolls
        call juntrolls('s',[         &
           'Please ...           ',  &
           "   don't feed        ",  &
           '   the               ',  &
           '   TROLLS!           '   &
           ])
     end program demo_juntrolls
