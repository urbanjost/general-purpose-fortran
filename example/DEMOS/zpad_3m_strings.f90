       program demo_zpad
        use M_strings, only : zpad
        implicit none
        character(len=*),parameter :: boxed='("[",a,"]",*(g0,1x))'
        integer :: lun, i
           print boxed, zpad( '111', 5),'basic use'
           print boxed, zpad( valuein=42 , length=7),'by argument name'
           print boxed, zpad( '  34567  ', 7),'cropped before padding'
           print boxed, zpad( '123456789', 5),'input longer than length'
           print boxed, zpad( '  +34567  ', 7),'starts with plus sign'
           print boxed, zpad( '  -34567  ', 7),'starts with minus sign'
           print boxed, zpad(1234),'some integers instead of strings'
           print boxed, zpad(-1234)
           print boxed, zpad(1234,8)
           print boxed, zpad(-1234,8)
           print boxed, zpad(''),'a null gets you nothing'
           print boxed, zpad('0'),'but blanks are used for default length'
           print boxed, zpad('0    ')
           print boxed, zpad('     ')
           print *, 'input value may be an array:'
           print '("[",a,"]")', zpad([1,10,100,1000,10000,100000],8)

           ! example usage:
           ! open output_00085.dat
           i=85
           open(newunit=lun,file='output_'//zpad(i,5)//'.dat')
           close(unit=lun,status='delete')

       end program demo_zpad
