     program demo_expand
        ! demonstrate filter to expand C-like escape sequences in input lines
        use M_strings, only : expand
        integer,parameter     :: iwidth=1024
        integer               :: i
        character(len=iwidth),parameter :: input(*)=[ character(len=iwidth) :: &
           '\e[H\e[2J',&   ! home cursor and clear screen on ANSI terminals
           '\tABC\tabc',&  ! write some tabs in the output
           '\tA\a',&       ! ring bell at end if supported
           '\nONE\nTWO\nTHREE',&  ! place one word per line
           '\\']
           write(*,'(a)')(trim(expand(input(i))),i=1,size(input))
     end program demo_expand
