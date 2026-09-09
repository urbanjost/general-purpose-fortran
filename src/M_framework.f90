module M_framework
use M_framework__journal
use M_framework__msg
use M_framework__verify
use M_framework__approx
use M_framework__utility
use M_framework__timing
!use M_framework__attr
implicit none(type,external)
!private
! public ...
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_framework(3f) - [M_framework::INTRO] unit test framework
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!! M_framework(3f) is an aggregate of Fortran modules brought together to
!! provide unit testing for Fortran; and is particularly well suited for
!! use with the Fortran Package Manager (fpm).
!!
!! The support modules are useful for creating terminal messages, comparing
!! expected values to results, writing logfiles and playback journals
!! independent of unit testing as well.
!!
!! Typically unit testing can be integrated seamlessly into the fpm(1) subcommand
!! "test" making unit testing from github/gitlab CD/CI utilities as simple as
!! "fpm test" in many cases.
!!
!!
!!  BASIC TIMING IS INCLUDED
!! As well as the expected **pass/fail/skipped unit testing** report.
!! basic **timing information** can be produced by the unit testing module
!! M_framework__verify(3f).
!!
!!  HOOKS ARE PROVIDED TO EXTERNAL LOCAL APPLICATIONS
!!
!! M_framework(3f) comes with a unique hook that allows calling your own
!! programs to integrate with local logging tools, report generators,
!! spreadsheets or other local infrastructure. The example program
!! "bookkeeper" is included that uses the hook to write example report
!! data files:
!!
!!  + [CSV](https://urbanjost.github.io/M_framework/bookkeeper.csv),
!!  + [HTML](https://urbanjost.github.io/M_framework/bookkeeper.html)
!!  + [NAMELIST](https://urbanjost.github.io/M_framework/bookkeeper.nml)
!!
!! The hook can call any local program with an interface similar to
!! "bookkeeper". This allows modifying the program for local needs such
!! as sending e-mail alerts and so on without having to change the tests.
!!
!!   MESSAGES CAN BE COMPOSED ALMOST LIKE LIST-DIRECTED I/O
!! Messages can be composed of up to twenty scalar intrinsic variables,
!! allowing freedom in writing messages akin to that of list-directed I/O;
!! but simultaneously to a list of output units. This easily lets messages
!! go to stdout or stderr and a file of the user's choosing as well. Output
!! can even be sent to a scratch file, essentially creating a quiet mode.
!!
!!    DESIGNED FOR INTEGRATION WITH FPM(1)
!! In conjunction with fpm(1) it is easy to run the tests with the --runner
!! option, allowing for integration with other utilities as well such as
!! the GNU debugger gdb(1), valgrind(1), and other tools.
!!
!!    PROGRAMS INCLUDED TO GENERATE SKELETON TEST PROGRAM
!! Example programs are provided to create unit test skeleton programs to
!! ease usage.
!!
!!    EASILY USED WITH GITHUB CD/CI
!! Example CD/CI scripts that can be used with github are in the .github/
!! directory that assume your tests can by run by using "fpm test".
!!
!!    MOTIVATION
!! Including **unit testing** is strongly encouraged for any software package,
!! but particularly when it is to be used in programming environments the
!! package was not developed in.
!!
!! This is the typical case for public fpm(1) packages on github.
!!
!! Additionally the proposed rules for registered fpm(1) package repositories
!! include requiring package candidates themselves only have external
!! dependencies that are also registered repository packages ( of course
!! wrappers of C libraries or other existing libraries cannot easily be
!! conformed to this).
!!
!! As a result -- this project concentrates on tools for a procedural unit
!! testing framework based on nothing but standard Fortran that specifically
!! can be used as a remote fpm(1) dependency in fpm(1) packages that may
!! reside in public github repositories.
!!
!! That is, it is designed to be integrated with the fpm(1) "test" subcommand
!! as an external dependency. It is intended to have little or no other
!! dependencies and ultimately become a registered fpm(1) package to aid in
!! the development of the fpm(1) repository.
!!
!! That being said, it can be used standalone as well.
!!
!!##DESCRIPTION
!!
!! Unit testing allows you to automatically confirm changes are acceptable
!! so you can quickly and confidently make and release changes. But many of
!! the operations required for testing are useful generically. So this
!! project is broken down into small general-purpose modules and
!! one unit-testing-specific one.
!!
!! M_framework(3f) is composed of the following individual modules:
!!
!!  + **M\_framework__msg** is a small module that in particular
!!    can convert a list of variables of any of the common default types
!!    to a string.
!!
!!    It performs low-level operations that are often used by other larger
!!    modules so it is its own module to prevent circular dependencies.
!!
!!  + **M\_framework__journal__** allows for creating log and journal files
!!
!!  + **M\_framework__approx** contains routines for rounding and comparing
!!    floating-point values.
!!
!!  + **M\_framework__help** provides for creating a standard simple
!!    interactive help facility
!!
!!  + **M\_framework__verify** is at the heart of the collection and
!!    handles virtually all the unit-test-specific operations.
!!
!! The procedures can be used in a variety of ways. After using them for
!! a long time my typical usage has changed completely several times.
!!
!! As part of private software there was little problem calling the
!! procedures directly from within the modules where they resided; and even
!! auto-detecting the routines (a naming convention of "test\_suite\_$NAME"
!! was used). The routines were pre-installed on virtually all platforms
!! so using them was no more a burden than say, calling an intrinsic.
!!
!! Put autodetecting tools (at least the ones employed) are not generally
!! portable; and with public packages there is a strong incentive to
!! minimize the amount of infrastructure and external dependencies. It is
!! generally imperative to make package use as simple and generic as possible.
!!
!! So the suggested scheme is to create a small test program for each
!! procedure or closely related procedures in the fpm test/ directory.
!!
!!  + make a subroutine for each component to be tested
!!  + in each test procedure start it with a call to UNIT\_TEST\_START(3f)
!!  + end each test procedure it with a call to UNIT\_TEST\_END(3f)
!!  + make multiple calls to UNIT\_TEST(3f) in-between to generate
!!    test results
!!  + call each of those test routines from the main program
!!  + end the main program with a call to UNIT\_TEST\_STOP(3f)
!!
!! Optionally, before starting set preferred non-default modes.
!!
!! Use the 'fpm test' command to run specific tests; all tests (the default);
!! a list of tests or test names using simple globbing.
!!
!! As desired, command line options can be used to control various test
!! behaviors.
!!
!! As an example, we can create a skeleton program to test a few
!! routines. Using
!!
!!      fpm test_suite sqrt cos sin > test/test_suite.f90
!!
!! to create the skeleton program and then adding a few actual
!! calls to unit\_test\_test(3f) results in
!!
!!   Sample program:
!!
!!    program M_test_suite_M_intrinsics
!!    use,intrinsic :: iso_fortran_env, only : &
!!    & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
!!    !
!!    use M_framework, only : unit_test_start,unit_test,unit_test_end, &
!!                     unit_test_mode, unit_test_level, unit_test_stop
!!    use M_framework, only : CHECK_PREFIX ! change column one of messages
!!    !
!!    !use M_mymodule ! load any modules you will be testing
!!    implicit none
!!    double precision,allocatable :: expected(:), answers(:), input(:)
!!    double precision,parameter :: PI=atan(1.0d0)*4
!!    !! setup
!!       !---------------------------------------------------
!!       !OPTIONAL:
!!       !  values used in prefix column for various messages
!!       !  the default is to set them all to the basename of
!!       !  the executable running tests, but they can be
!!       !  altered. For example:
!!       CHECK_PREFIX=prefix(                  &
!!        test_MSG    =  'test_msg:   ', &
!!        test        =  'test:      ',  &
!!        test_START  =  'test_start: ', &
!!        test_STOP   =  'test_stop:  ', &
!!        test_END    =  'test_end:   '  &
!!       )
!!       !---------------------------------------------------
!!       !OPTIONAL:
!!       ! the options available at run-time on the command
!!       ! line can have their defaults selected. See the
!!       ! man-page for the procedure for details.
!!       call unit_test_mode(
!!         ( keep_going=.true. ,
!!         flags=[character(len=0) ::],
!!         luns=[stdout],
!!         command, &
!!         brief=.false. ,
!!         interactive=.false. ,
!!         CMDLINE='',
!!         debug=.false. ,
!!         match
!!         )
!!       !---------------------------------------------------
!!    !! test each subroutine
!!       call test_sqrt()
!!       call test_cos()
!!       call test_sin()
!!    !! teardown
!!       call unit_test_stop()
!!    contains
!!    subroutine test_sqrt()
!!    integer :: i
!!       call unit_test_start('sqrt',msg='calculate the square root')
!!       input   =[1.0d0,4.0d0,9.0d0]
!!       expected=[1.0d0,2.0d0,3.0d0]
!!       answers=[(sqrt(input(i)),i=1,size(input))]
!!       call unit_test('sqrt', all(expected.eq.answers),&
!!          & 'check table of values')
!!       call unit_test('sqrt', sqrt(25.0d0).eq.5.0d0,&
!!          & 'got',sqrt(25.0d0),'expected',5.0d0)
!!       call unit_test_end('sqrt',msg='')
!!    end subroutine test_sqrt
!!
!!    subroutine test_sin()
!!       call unit_test_start('sin',msg='calculate the sine of a value')
!!       call unit_test_end('sin',msg='')
!!    end subroutine test_sin
!!
!!    subroutine test_cos()
!!       call unit_test_start('cos',msg='calculate the cosine of a value')
!!       call unit_test_end('cos',msg='')
!!    end subroutine test_cos
!!
!!    end program M_test_suite_M_intrinsics
!!
!! The output now looks like this (note if no calls to unit\_test
!! routines are made between unit_test_start(3f) and unit_test_end(3f)
!! the procedure gets an "UNTESTED" entry to remind you to make
!! some tests ..).
!!
!!    > test:       sqrt   SUCCESS : check table of values
!!    > test:       sqrt   SUCCESS : got 5.0000000000000000 expected 5.0000000000000000
!!    > test_end:   sqrt   PASSED  : GOOD:        2 BAD:        0 DURATION:00000000012000:
!!    > test_end:   cos    UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
!!    > test_end:   sin    UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
!!    > test_stop:  TALLY  PASSED  : GOOD:        2 BAD:        0 DURATION:00000000000000
!!    > STOP 0
!!
!! this is a model that works particularly well for basic numeric procedures.
!!
!! Now it is just a matter of adding more calls to unit\_test(3f). This is
!! where procedures from the other modules become useful, as they provide
!! methods for comparing float values, for example. Since Fortran has
!! many powerful masking intrinsics usually just the unit\_test(3f) procedure
!! is required. In particular, be familiar with the ALL(3f), ANY(3f),
!! and PACK(3f) procedures.
!!
!! There are options to call a system command and use the initial string as
!! options, to interactively pause after each check, and to change options
!! like which output file to write on, what error level to use, and other
!! things I will hopefully solidify and document here.
!!
!!    RECOMMENDED BASIC USAGE
!!
!! So for an fpm(1) user a recommended process is to create and/or enter the
!! test/ directory and use the "unit\_test" program. It will make a test
!! program called "unit\_test\_$NAME.f90" for each name given on the command
!! line.
!!
!! There is a case made for closely related groups of procedures to share
!! a single test file that will be made a little bit later. For that, the
!! command "test\_suite" is used instead of "unit\_test".
!!
!! For now lets assume we have or plan to have the procedures
!! "a","b", and "c" in our package, and that we want to create a test for
!! each ( and that unit\_test(1) has been installed in your path):
!!
!!     cd $TOP_OF_FPM_PACKAGE
!!     cd test
!!     unit_test a b c
!!
!! If you then run "fpm test" the skeleton should run indicating the
!! procedures are not tested. Change the routines to actually call the
!! "unit\_test" procedure and you have the beginnings of a unit test for
!! your procedures.
!!
!! The "unit\_test(3f)" procedure in its simplest form takes a string that
!! is usually the procedure name and a logical expression, along with up
!! to twenty completely optional intrinsic scalar values which will be used
!! to create an optional message.
!!
!! The example programs also contain a placeholder call to unit\_test\_mode(3f).
!!
!!    PROCEDURAL AND COMMAND LINE MODE OPTIONS
!! The dummy skeleton routines all start with a call to unix\_test\_mode(3f).
!! Its documentation describes a few default modes you can change with the
!! routine. Essentially the same options are available on the command line
!! of the test program(s) as well.
!!
!!     # options may be specified in NAMELIST input format with no
!!     # extraneous spaces on the command line
!!     fpm test -- flags=100,200,300 keep_going=T
!!     # a little more Unix-like or MSWindows-like syntax is allowed, as
!!     # these are equivalent to the original command ...
!!     fpm test -- --flags=100,200,300 --keep_going
!!     fpm test -- /flags=100,200,300 /keep_going=T
!!
!! M_framework(3f) uses a built-in command line parser instead of
!! packages like M\_CLI or M\_CLI2 to minimize the number of dependencies
!! required.
!!
!!    SUGGEST ONE TEST PER PROGRAM
!!
!! There are advantages to each procedure being tested with a separate
!! program.
!!
!! A large number of individual test programs works well with fpm(1).
!! fpm defaults to running all the tests, but can execute subgroups
!! easily because it can execute a list of tests and the names can
!! use simple globbing.
!!
!! If you autogenerate a list of procedure names to test and call the
!! sample program "unit_test" with the names of the procedures it will
!! generate new names but skip existing files; allowing for a simple form
!! of auto-generation assuming you have a way of listing all the procedure
!! names to test.
!!
!! individual procedure tests can be deleted or added or moved easily when
!! each is in its own file.
!!
!! It is easier to test with debuggers and other tools like gdb and valgrind
!! on small units.
!!
!! This is true with other tools that you can use with --runner as well.
!! See "fpm help runner" or "fpm manual \>manual.txt" for more information.
!!
!!     TESTING MANY PROCEDURES IN A SINGLE FILE
!!
!! If it is preferred, one program can test multiple procedures. The
!! main disadvantage is that the complete test suite is always run unless
!! conditional coding is added. One of the uses of the unit\_test\_flags(:)
!! array is to allow integer values to be passed at execution time that
!! can be tested to provide such conditional testing. When many tests are
!! in one file the unit_test_start(3f) procedure includes a "matched" argument
!! which can detect if a simple globbing expression that can be given on the
!! command line matches the string composed of the test name and message.
!! This allows you to optionally select specific groups of tests from a set.
!!
!!##UNIQUE FEATURES
!!
!!    CALLING AN EXTERNAL PROGRAM
!!
!! A unique feature of the M\_framework(3f) test harness is a hook to call
!! a custom program. You can ignore it is there, but it is a very powerful
!! feature if you want to do bookkeeping on the test results or enter
!! the results into a tracking tool. This allows you a way to sent alerts
!! if something fails in automated tests, to create spreadsheets with the
!! test results, to retain results in a database or SQLite3 file, or make
!! a custom tool to convert the data to something else like TAP (Test Anywhere
!! Protocol) reports.
!!
!! An example program called "bookkeeper" is included that shows how to
!! parse the information passed to the program that generates several files:
!!
!!    + bookkeeper.csv
!!    + bookkeeper.html
!!    + bookkeeper.nml
!!    + bookkeeper\_clicks.csv
!!
!! To use the defaults you simply enter
!!
!!     fpm test -- command=bookkeeper
!!
!! The CSV files can typically be read directly into a spreadsheet program.
!!
!! They can also be read using the SQLite3 tool. This is very powerful,
!! letting you use SQL to select specific data, generate reports, and
!! convert the subsequent results to HTML, CSV, flat text and other formats.
!!
!! The NAMELIST output file is essentially a record of the harness calls
!! using the standardized NAMELIST format that is almost trivial to read
!! in with a custom Fortran program. This might be considered a metafile
!! format for a test run. You can replay the data and do whatever you want
!! with it including generating alternate output file types; plotting
!! timing data and so on.
!!
!! The HTML file is handy for importing into word processors or viewing
!! via browsers. Example output files from an fpm(1) package that uses
!! M_framemaker illustrate the different types of output that can be
!! generated.
!!  + [CSV](https://urbanjost.github.io/M_framework/bookkeeper.csv).
!!    Generally. comma-separated files can be read directly into spreadsheet
!!    programs, sqlite3, and several common databases.
!!  + [CSV runtimes](https://urbanjost.github.io/M_framework/bookkeeper_clicks.csv)
!!    Another CSV file that is a record of the runtimes between a
!!    unit test start and end.
!!  + [HTML](https://urbanjost.github.io/M_framework/bookkeeper.html)
!!    An example of a formatted report that can be displayed in a browser.
!!  + [NAMELIST](https://urbanjost.github.io/M_framework/bookkeeper.nml).
!!    Essentially this is a metafile that records the unit test calls.
!!    It is very easy for a custom Fortran program to read back a NAMELIST
!!    file and generate custom outputs instead of modifying bookkeeper(1).
!!
!! The bookkeeper(1) program is an example program that is expected to
!! be customized. It provides for parsing the parameters passed to a
!! M_framemaker external program.
!!
!!     IMPORTING INTO SQLLITE3
!! Here is an example SQLite3 Tool input file that if placed in "test.sql"
!! can be run with
!!
!!      sqlite3 -batch -init test.sql bookkeeper.db .quit
!!
!! if you have sqlite3(1) installed.
!!
!!    .mode csv
!!    .import bookkeeper.csv unit_test
!!    .schema unit_test
!!    ---
!!    --- show all data to show it worked
!!    SELECT * FROM unit_test;
!!    ---
!!    --- example lists unique names
!!    SELECT name FROM unit_test GROUP BY name ORDER BY name ;
!!    ---
!!    --- tally up passed, failed, skipped in a text table
!!    .header on
!!    .mode column unit_test
!!    .width 64 9 9 9
!!    SELECT name,
!!    --- depending on SQL version a simpler IIF/IF, IFNULL, or TOTAL might be better but this works well
!!    --- and is relatively "standard" as much as something like a standard is actually adhered to in
!!    --- SQL-land.
!!    CASE  sum( passed == 'passed'  ) WHEN NULL THEN 0 ELSE sum ( passed == 'passed')  END  AS 'ok',
!!    CASE  sum( passed == 'failed'  ) WHEN NULL THEN 0 ELSE sum ( passed == 'failed')  END  AS 'not ok',
!!    CASE  sum( passed == 'skipped' ) WHEN NULL THEN 0 ELSE sum ( passed == 'skipped') END  AS 'skip'
!!    FROM unit_test
!!    --- WHERE condition
!!    GROUP BY name
!!    ORDER BY name ;
!!    ---
!!    --- If the table already exists, the sqlite3 tool uses all the rows,
!!    --- including the first row, in the CSV file as the actual data to import.
!!    --- Therefore, you should delete the first row of the CSV file with the
!!    --- header labels if adding to an existing table instead of creating
!!    DROP TABLE IF EXISTS unit_test;
!!    --- CREATE TABLE unit_test(
!!    ---   name   TEXT NOT NULL,
!!    ---   date   DATE NOT NULL,
!!    ---   passed TEXT NOT NULL,
!!    ---   msg    TEXT
!!    --- );
!!
!! The last select generates a little tally table showing for all the runs
!! in the input DataBaseFile the number of pass, fail, and skips of the test
!! names. you could tally over a particular date range, only show failures,
!! display the last test status, generating output in HTML or CSV or ...
!! See the SQLite3 Tool documentation for sqlite3(3f) for more information.
!!
!!    name                                          ok         not ok     skip
!!    --------------------------------------------  ---------  ---------  ---------
!!    accdig                                        3          0          0
!!    almost                                        24         0          0
!!    flush_trail                                   0          0          3
!!    in_margin                                     30         0          0
!!    round                                         0          0          3
!!    set_stdout_lun                                0          0          3
!!    significant                                   15         1          2
!!    unit_test                                     0          0          3
!!    unit_test_bad                                 0          0          3
!!    unit_test_end                                 0          0          3
!!    unit_test_good                                0          0          3
!!    unit_test_start                               0          0          6
!!    unit_test_stop                                0          0          3
!!    where_write_message_all                       0          0          3
!!    write_message_only                            4          2          4
!!
!!##UNIQUE FEATURES
!!
!! fpm(1), gdb(1), and M\_framework(3f) work together
!!
!! It is easy to debug an individual test in a debugger. For example to run
!! a test called "crash" with gdb(1) use
!!
!!      fpm test --target crash --runner "gdb -ex run --quiet"
!!
!! A more elaborate example passing arguments to M\_framework(3f) to change
!! default behavior to for example write output to stdout instead of stderr
!! and display compiler version and options and to run all the tests in
!! the gdb(1) debugger (you can enter "q" after each test has run; or enter
!! gdb commands at the prompt):
!!
!!     > fpm test --target '*' --verbose \
!!     > --runner 'gdb -ex "list, 0" -ex run --quiet --args' \
!!     > -- flags=9997,9998,9999 luns=6 level=3
!!
!! This is long enough that an alias or script would facilitate its use,
!! with one changing it to use one's favorite options.
!!
!!    EASILY CALLED BY STANDARDIZED CD/CI SCRIPTS
!!
!! If a CD/CI github automated test script creates a Fortran
!! environment including fpm and simply calls "fpm test" you can standardize
!! your testing and use the same CD/CI scripts for any package. The testing
!! framework is designed with just that scenario in mind.
!!
!! The .github subdirectory in M\_framework package contains examples of just
!! such files that you can copy as-is into your fpm project. Activate test
!! actions in your github repository after adding the .github directory and
!! your package will be automatically tested on several different platforms
!! by the scripts whenever a "push" to the repository occurs.
!!
!! ------------------------------------------------------------------------------------------
!!
!!##SUPPORTS FPM
!!
!! The impatient can try this, assuming git(1) and fpm(1) are installed.
!!
!! Download the github repository and build it with fpm(1)
!! and create a test fpm(1) project:
!!
!!    #!/bin/bash
!!    # first you need to install a few example programs
!!    cd /tmp
!!    git clone https://github.com/urbanjost/M_framework.git
!!    cd M_framework
!!    # install the "unit_test", "bookkeeper", and "test_suite"
!!    # example programs; assuming the default install puts
!!    # them in your path:
!!    fpm install
!!    # "fpm help install" describes how to customize where the
!!    # programs are installed.
!!    #
!!    # go to your fpm package test/ directory.
!!    # here, we will make one up
!!    fpm new /tmp/tryit  # create test project
!!    cd /tmp/tryit/test
!!    # so lets say you plan on adding procedures "a","b",and "c" to your src/tryit.f90
!!    # project code. Set up individual skeleton tests for each procedure.
!!    unit_test a b c                         # a file for each test
!!    test_suite a b c > test_suite_tryit.f90 # or a single file
!!    cd ..  # go to the top of the project
!!    # add M_framework to the developer dependencies
!!    cat >> fpm.toml <<\EOF
!!    [dev-dependencies]
!!    M_framework    = { git = "https://github.com/urbanjost/M_framework.git" }
!!    EOF
!!    #
!!    # test the package
!!    fpm test
!!    # if will say the procedures are untested. Put real calls
!!    # in to unit_test(3f) and see how the default report
!!    # changes
!!    #
!!    # so now to run the default tests is as simple as
!!    fpm test
!!    # run just one test
!!    fpm test unit_test_a
!!    # run tests using globbing; eg. all tests beginning with "unit\_"
!!    fpm test 'unit_*'
!!    # display help on the interactive command options
!!    fpm test -- --help
!!
!!    # you can pass parameters and/or change the unit_test_mode(3f)
!!    # calls to change some of the test behavior
!!
!! So once you want to use this on your own projects, you would normally
!! just add M\_framework(3f) as a developer dependency
!! in your fpm.toml project file and start making tests that call M\_framework(3f).
!!
!! The optional programs demonstrated ("unit\_test", "test\_suite") are just simple
!! stand-alone programs that make skeleton programs to run tests that you can
!! customize (and rename too to avoid confusion).
!!
!!    # some useful things to try. Check out the man-pages for all the unit_test_*(3f)
!!    # procedures.
!!    # Then look at
!!    unit_test --help
!!    fpm test -- help
!!    # run the demo bookkeeper script
!!    fpm test -- command=bookkeeper
!!    # and look at the bookkeeper*.* files in the top of the project
!!
!!##DOCUMENTATION
!!
!!   USER
!!
!!  - An [index](https://urbanjost.github.io/M_framework/man3.html) to HTML versions
!!    of the manpages
!!
!!  - single pages that uses javascript to combine all the HTML descriptions
!!    of the man-pages is at
!!     + [BOOK_M_framework__verify](https://urbanjost.github.io/M_framework/BOOK_M_framework__verify.html)
!!     + [BOOK_M_framework__approx](https://urbanjost.github.io/M_framework/BOOK_M_framework__approx.html)
!!     + [BOOK_M_framework__journal](https://urbanjost.github.io/M_framework/BOOK_M_framework__journal.html)
!!     + [BOOK_M_framework__msg](https://urbanjost.github.io/M_framework/BOOK_M_framework__msg.html)
!!
!!  - man-pages in
!!     + [manpages.zip](https://urbanjost.github.io/M_framework/manpages.zip)
!!     + [manpages.tgz](https://urbanjost.github.io/M_framework/manpages.tgz)
!!
!!    DEVELOPER
!!    + [ford-generated developers' document](https://urbanjost.github.io/M_framework/fpm-ford/index.html)
!!    + [github action status](docs/STATUS.md)
!!
!!##ADDITIONAL DIRECTORY DESCRIPTIONS
!!
!!    - src/ is where the source for the M_framework(3f) module code resides
!!    - docs/ contains HTML documentation and the manpage archives
!!    - test/ contains simple test programs
!!    - example/ has demos
!!
!!##REFERENCES
!!  + See the .github directory in [easy](https://github.com/urbanjost/easy)
!!  + for examples of CD/CI scripts that assume your package can be tested with an "fpm test" command.
!!  + examples that use M_framework in github fpm packages:
!!      * [M_strings](https://github.com/urbanjost/M_strings)
!!      * [M_process](https://github.com/urbanjost/M_process)
!!
!!    These packages used a different reincarnation of the testing harness and are in the process of
!!    being changed to use M_framework(3f) more appropriately, but still contain some useful examples.
!!
!!  + [Fortran Package Manager](https://github.com/fortran-lang/fpm)
!!  + [fpm(1) registry](https://github.com/fortran-lang/fpm-registry)
!!  + [Fortran Wiki: unit testing list](https://fortranwiki.org/fortran/show/Unit+testing+frameworks)
!!  + [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html) for generating documentation for your projects
!!
!!    NOTE
!! M_framework(3f) is subject to interface changes so the generally recommended
!! practice of using a specific commit when using it an an external
!! fpm(1) dependency is highly encouraged.
!!
!! Any feedback (features, performance, ease of use, ...) is appreciated,
!! particularly in the ongoing development phase.
!!
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_framework
