        program demo_kracken

        use M_kracken
        ! define command arguments, default values and crack command line
        call kracken('cmd',              &
           &   '-int 20                  &
           &   -real 10e3                &
           &   -file input               &
           &   -dble 4.11223344556677d0  &
           &   -help    .false.          &
           &   -version .false.         '&
           &   )
        ! that's it. You defined your command arguments and their default
        ! values and parsed the user-supplied command line arguments.

        ! Now you can just retrieve the values as strings using
        ! names of the form VERB_SWITCHNAME anywhere in your program.
        ! Note that the special name "VERB_oo"  is for the string
        ! before any switch.
           if(lget('cmd_help'))then ! was -help specified?
              write(*,*)'The help text'
              stop
           endif
           if(lget('cmd_version'))then ! was -version specified?
              write(*,*)'version 1.0 20161030'
              stop
           endif
           ! convert all the remaining options to scalar values
           ! and call a procedure with the values
           call mymain(                  &
           & sget('cmd_file'),           &
           & rget('cmd_real'),           &
           & dget('cmd_dble'),           &
           & iget('cmd_int')             &
           & )
        contains
        subroutine mymain(filename,value1,value2,ivalue3)
        ! this routine is using conventional values and does
        ! not use M_kracken(3fm) module at all
        implicit none
        character(len=*),intent(in) :: filename
        real,intent(in)             :: value1
        doubleprecision,intent(in)  :: value2
        integer,intent(in)          :: ivalue3
           ! just to show the command arguments have
           ! been processed echo the values
           print *, 'filename=',trim(filename)
           print *, 'values=',value1,value2,ivalue3
        end subroutine mymain
        end program demo_kracken
