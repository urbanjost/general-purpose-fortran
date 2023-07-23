     program demo_set_mode
     use M_CLI2,  only : set_args, lget, set_mode
     implicit none
     character(len=*),parameter :: all='(*(g0))'
        !
        ! enable use of response files
        call set_mode('response_file')
        !
        ! Any dash in a keyword is treated as an underscore
        call set_mode('underdash')
        !
        ! The case of long keywords are ignored.
        ! Values and short names remain case-sensitive
        call set_mode('ignorelongcase')
        ! The case of short and long keywords are ignored
        call set_mode('ignoreallcase')
        !
        ! short single-character boolean keys may be bundled
        ! but it is required that a single dash is used for
        ! short keys and a double dash for long keywords.
        call set_mode('strict')
        !
        call set_args(' --switch_X:X F --switch-Y:Y F --ox:O F -t F -x F -o F')
        !
        ! show the results
        print all,'--switch_X or -X ... ',lget('switch_X')
        print all,'--switch_Y or -Y ... ',lget('switch_Y')
        print all,'--ox or -O       ... ',lget('ox')
        print all,'-o               ... ',lget('o')
        print all,'-x               ... ',lget('x')
        print all,'-t               ... ',lget('t')
     end program demo_set_mode
