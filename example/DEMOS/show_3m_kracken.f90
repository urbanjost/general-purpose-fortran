           program demo_show
           use M_kracken, only : kracken, show
           implicit none

           call kracken('demo', ' default keyword -i 10 -j 20.20 -value my default string')
           call show('demo',.false.,0)

           end program demo_show
