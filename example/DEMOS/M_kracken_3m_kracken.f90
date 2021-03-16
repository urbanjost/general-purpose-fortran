            program demo_M_kracken
            use M_kracken, only : kracken
            ! define and crack command line arguments
            call kracken('cmd',' DEFAULT STRING -x 123 -y 456 ')
            call showstring()
            call showvalue()
            contains

            subroutine showstring()
            use M_kracken, only : sget
            character(len=:),allocatable :: string
            ! get value of string before any switch
            string=trim(sget('cmd_oo'))
            write(*,*)'string is ',string
            end subroutine showstring

            subroutine showvalue()
            use M_kracken, only : rget
            ! show values for -x and -y parameters
            x=rget('cmd_x')
            y=rget('cmd_y')
            write(*,*)' X and Y are ',x,y
            end subroutine showvalue

            end program demo_M_kracken
