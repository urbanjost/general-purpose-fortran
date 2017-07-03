NAME
    M_journal(3fm) - [M_journal]write program messages to stdout and/or a log file
SYNOPSIS
    use, M_journal , only : journal
DESCRIPTION

    For interactive programs in particular it is useful if all messages go thru the JOURNAL(3f) routine. This makes it easy to
    write messages to a log file as well as standard output; to toggle time prefixes on and off; to turn on and off debug-mode
    messages; control output paging and create replayable input journals.

    The primary use of JOURNAL(3f) is to create journal files for interactive programs that can be replayed and/or be used to
    verify program executions. Typically, you would echo what the user typed to the trail file as-is, and write output you write to
    stdout as comments to the trail file so that the trail file can easily be read back in (by ignoring comments). So usually
    things that are read from user input are using output with WHERE='T' and output that usually goes to stdout is written with
    WHERE='SC'.

         >      :
         >      :
         > character(len=256) userline, output
         > call journal('O','my_trail_file')  ! open trail file
         >      :
         >      :
         > do
         >    read(*,'(a)',iostat=ios) userline  ! read user input
         >    if(ios.ne.0)exit
         >    ! echo user input to trail file
         >    call journal('T',userline)
         >    ! assume user input causes values i1, i2, and i3 to be calculated
         >    write(output,'(i0,1x,i0,1x)')i1,i2,i3 ! build an output line
         >    ! write output to stdout and as comment to trail file
         >    call journal('SC',output)
         > enddo

    In this example an output line was built with an internal write; but calls to journal(3f) with numeric values with and without
    advancing I/O turned on are often used for simpler output:

             I=10
             R=20.3
             ! write to stdout and trail file without advancing I/O
             call journal('+SC','I=',i)
             call journal('SC','AND R=',r)

    writes to the trail file are ignored unless a trail file was opened with

      CALL JOURNAL('O',filename)


    So that routines that do their output via JOURNAL(3f) can be used with and without programs generating trail files. That is,
    destinations 'T' and 'C' are ignored unless a trail file has been requested.

EXAMPLES

    program testit
    use M_journal,only : journal
    implicit none
       call journal('a single string A -should be in S')

       ! add time prefix to output
       ! turn on time prefix
       call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
       call journal('a single string B -should be in S with prefix')
       call journal('%','CPU_TIME: %c:CALLS: %C: %b')  ! change time prefix
       call journal('a single string B-1 -should be in S with prefix')
       call journal('a single string B-2 -should be in S with prefix')
       call journal('a single string B-3 -should be in S with prefix')
       !  Other useful time formats:
       !     %E -- Unix Epoch time
       !     %e -- integer value of Unix Epoch time
       !     %C -- number of times this format is used
       !     %c -- CPU_time(3f) output
       !     %S -- seconds since last use of this format
       !     %k -- CPU time in seconds from system_clock
       call journal('%','') ! turn off time prefix

       call journal('a single string C -should be in S')

       call journal('O','aaa.out') ! turn on trail file
       call journal('a single string D -should be in SC')
       call journal('a single string E -should be in SC')
       call journal('a single string F -should be in SC')
       call journal('O','') ! turn off trail file

       call journal('a single string G -should be in S')
       call journal('a single string H -should be in S')
       call journal('a single string I -should be in S')

       ! build one line of output with intrinsic scalar values added
       call journal('+sc','APPEND:')
       call journal('+sc',' integer',         1234)
       call journal('+sc',' and real',        1234.5678)
       call journal('+sc',' and double',1234567890.123456d0)
       call journal('+sc',' and logical',    .true.)
       call journal('sc','')

    end program testit

