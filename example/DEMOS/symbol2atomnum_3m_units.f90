     program demo_symbol2atomnum
     use M_units, only :  symbol2atomnum
     implicit none
     integer           :: atomnum
     character(len=2)  :: name
     name='Ne'
     call symbol2atomnum(name,atomnum)
     write(*,*)atomnum,name
     end program demo_symbol2atomnum
