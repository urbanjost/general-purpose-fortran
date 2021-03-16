          program demo_M_process
           use M_process ,only : process_open_write, process_writeline
           use M_process ,only : streampointer, process_close
           implicit none
           ! C file pointer returned by process_open()
           type(streampointer) :: fp
           ! check status of calls to process module routines
           integer :: ierr
           character(len=:),allocatable :: text(:)

           ! open process to write to (ie. start gnuplot(1) program)
           !!call process_open_write('cat',fp,ierr)
           ! open process to write to (ie. start gnuplot(1) program)
           call process_open_write('bash',fp,ierr)

           text=[character(len=128) :: &
           "rm -f sqlite1.db", &
           "sqlite3 sqlite1.db <<\EOF", &
           "-- ***********************************************",&
           "CREATE TABLE IF NOT EXISTS animals(               ",&
           "   name        TEXT   NOT NULL   PRIMARY KEY ,    ",&
           "   hair        INT    NOT NULL   ,                ",&
           "   mobility    INT    NOT NULL   ,                ",&
           "   vision      INT    NOT NULL   );               ",&
           "-- ***********************************************",&
           "INSERT INTO animals(&
           &name,hair,mobility,vision) VALUES('kittens',4,5,1);",&
           "INSERT INTO animals(&
           &name,hair,mobility,vision) VALUES('mice'   ,6,7,2);",&
           "INSERT INTO animals(&
           &name,hair,mobility,vision) VALUES('rats'   ,2,3,3);",&
           "-- ***********************************************",&
           ".quit", &
           "EOF", &
           "##################################################",&
           "sqlite3 -header -column sqlite1.db  'select * from animals'",&
           "sqlite3 sqlite1.db  &
           &'select name, hair, mobility, vision from animals'",&
           "##################################################",&
           "gnuplot --persist <<\EOF                          ",&
           "########################################          ",&
           "#set terminal gif                                 ",&
           "#set output 'M_process.3.gif'                     ",&
           "########################################          ",&
           "#set terminal png                                 ",&
           "#set output 'bar.png'                             ",&
           "########################################          ",&
           "#set terminal pdf enhanced                        ",&
           "#set output 'bar.pdf'                             ",&
           "########################################          ",&
           "#set style data lines                             ",&
           "########################################          ",&
           "set datafile separator ""|""                      ",&
           "set style data histogram                          ",&
           "set style histogram cluster gap 1                 ",&
           "set style fill solid border rgb ""black""         ",&
           "set auto x                                        ",&
           "set yrange [0:*]                                  ",&
           "plot ""< sqlite3 sqlite1.db  &
           &'select name, hair, mobility, vision  from animals'"" \  ", &
           "      using 2:xtic(1) title ""hair"",  \          ",&
           "   '' using 4:xtic(1) title ""vision"", \         ",&
           "   '' using 3:xtic(1) title ""mobility""          ",&
           "quit                                              ",&
           "EOF                                               ",&
           " "]

              !!write(*,'(a)')text
              call process_writeline(text,fp,ierr)
              call process_close(fp,ierr)
              write(*,'(a)')'CLOSED THE PROCESS. RETURNING TO PROGRAM'

              end program demo_M_process
