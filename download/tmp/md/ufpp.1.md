[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - ufpp (1)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    ufpp(1) - [DEVELOPER]pre-process FORTRAN source files

CONTENTS

    Synopsis
    Options
    Definition
    Limitations
    Environment
    Examples

SYNOPSIS

             ufpp [[-D] define_list] [-I include_directories] [-i input_file(s)] [-o output_file] [-html] [-system] [-verbose] [
             -prefix character_ADE] [-keeptabs] [-noenv] [-width n] [-d ignore|remove|blank] [-cstyle default|doxygen] [-version] [
             -help [-html]]

OPTIONS

             define_list, -D define_list An optional space-delimited list of expressions used to define variables before file
             processing commences.

             -i input_file(s) The default input file is stdin. Filenames are space-delimited. In a list, @ represents stdin.

             -o output_file The default output file is stdout.

             -I include_directories The directories to search for files specified on $INCLUDE directives.

             -prefix ADE|letter The default directive prefix character is "$". Alternatives may be specified by providing an ASCII
             Decimal Equivalent (Common values are 37=% 42=* 35=# 36=$ 64=@). If the value is not numeric it is assumed to be a
             literal character.

             -html Assumes the input file is HTML that follows the following rules:

             1.    Input lines are not output until a simple < X // MP> directive is found.

             2.    Output stops when a simple < /X // MP> directive is encountered. This allows code to be maintained as part of an
                   HTML document.

             -help [-html] Display documentation and exit. If "-html" is present, write documentation as basic HTML, instead of as
             a text file.

             -verbose All commands on a $SYSTEM directive are echoed to stderr with a + prefix. Text following the string "@(#)" is
             printed to stderr similar to the Unix command what(1) but is otherwise treated as other text input.

             -noenv The $IFDEF and $IFNDEF directives test for an internal ufpp(1) variable and then an environment variable by
             default. This option turns off testing for environment variables.

             -system Allow system commands on $SYSTEM directives to be executed.

             -keeptabs By default tab characters are expanded assuming a stop has been set every eight columns; and trailing
             carriage-return characters are removed. Use this flag to prevent this processing from occurring.

             -cstyle try to style comments generated in $COMMENT blocks for other utilities such as doxygen. Default is to prefix
             lines with  !! . Allowed keywords are currently "default", "doxygen".

             -d ignore|remove|blank Enable special treatment for lines beginning with "d" or "D" The letter will be left as-is (the
             default); removed; or replaced with a blank character. This non-standard syntax has been used to support the optional
             compilation of "debug" code by many Fortran compilers when compiling fixed-format Fortran source.

             -version Display version and exit

             -width n Maximum line length of the output file. Default is 1024. Typically used to trim fixed-format FORTRAN code
             that contains comments or "ident" labels past column 72 when compiling fixed-format Fortran code.

DEFINITION

    By default the stand-alone pre-processor ufpp(1) will interpret lines with "$" in column one, and will output no such lines.
    Other input is conditionally written to the output file based on the directives encountered in the input.

    The syntax for the control lines is as follows:

         $DEFINE   variable_name[=expression]                 [! comment ]
         $ERROR    message_to_stderr                          [! comment ]
         $IF       {constant LOGICAL expression}              [! comment ]
          or
         $IFDEF    {variable_name}                            [! comment ]
          or
         $IFNDEF   {variable_name}                            [! comment ]
                   { sequence of source statements}
         [$ELSEIF  {constant LOGICAL expression}              [! comment ]
                   { sequence of source statements}]
         [$ELSE                                               [! comment ]
                   { sequence of source statements}]
         $ENDIF                                               [! comment ]
         $IDENT    metadata                                   [! comment ]
         $@(#)     metadata                                  [! comment ]
         $INCLUDE  filename                                   [! comment ]
         $OUTPUT   filename  [-append]                        [! comment ]
         $DOCUMENT [comment|write|help|version] [-file NAME]
                   [-append]                                  [! comment ]
         $PRINTENV predefined_name|environment_variable_name  [! comment ]
         $SHOW                                                [! comment ]
         $STOP {stop_value}                                   [! comment ]
         $SYSTEM system_command                               [! comment ]
         $UNDEFINE variable_name                              [! comment ]
         $WARNING  message_to_stderr                          [! comment ]
         $MESSAGE  message_to_stderr                          [! comment ]



    Compiler directives are specified by a "$" in column one, followed by a keyword.

    An exclamation character on a valid directive begins an in-line comment that is terminated by an end-of-line.

    Any LOGICAL expression composed of integer constants, parameters and operators, is valid. Logical operators are

         .NOT.  .AND.  .OR.  .EQV.  .NEQV.  .EQ.  .NE.  .GE.
         .GT.   .LE.   .LT.  +      -       *     /     (
         )      **



    DIRECTIVES

    $DEFINE variable_name [=expression]

    A $DEFINE may appear anywhere in a source file. If the value is ".TRUE." or ".FALSE." then the parameter is of type LOGICAL,
    otherwise the parameter is of type INTEGER and the value must be an INTEGER. If no value is supplied, the parameter is of type
    INTEGER and is given the value 1.

    Constant parameters are defined from the point they are encountered in a $DEFINE directive until program termination unless
    explicitly undefined with a $UNDEFINE directive.

    Example:

        $define A=1
        $define B=1
        $define C=2
        $if ( A + B ) / C .eq. 1
           (a+b)/c is one
        $endif



    $ERROR message

    Write message to stderr and display program condition and exit program.

    $IF/$ELSEIF/$ELSE/$ENDIF directives

    Each of the control lines delineates a block of FORTRAN source. If the expression following the $IF is ".TRUE.", then the lines
    of FORTRAN source following are output. If it is ".FALSE.", and an $ELSEIF follows, the expression is evaluated and treated the
    same as the $IF. If the $IF and all $ELSEIF expressions are ".FALSE.", then the lines of source following the $ELSE are output.
    A matching $ENDIF ends the conditional block.

    $IFDEF/$IFNDEF directives

    $IFDEF and $IFNDEF are special forms of the $IF directive that simply test if a variable name is defined or not. Essentially,
    these are equivalent:

         $IFDEF varname  ==> $IF DEFINED(varname)
         $IFNDEF varname ==> $IF .NOT. DEFINED(varname)



    except that environment variables are tested as well if the -noenv option is not specified.

    $IDENT metadata [-language fortran|c|shell]

    Writes a line using SCCS-metadata format of the following forms:

         language:
         fortran   character(len=*),parameter::ident="@(#)metadata"
         c         #ident "@(#)metadata"
         shell     #@(#) metadata



    This string is generally included for use with the what(1) command.

    The default language is fortran. Depending on your compiler, the optimization level used when compiling, these strings may or
    may not remain in the object files and executables created.

    Do not use the characters double-quote, greater-than, backslash (">\) in the metadata; do not use strings starting with " -"
    either.

    $INCLUDE filename

    Nested read of specified input file. Fifty (50) nesting levels are allowed.

    $OUTPUT filename [-append]

    Specify the output file to write to. Overrides the initial output file specified with command line options. If no output
    filename is given revert back to initial output file. @ is a synonym for stdout.

          -append [.true.|.false]



    Named files open at the beginning by default. Use the -append switch to append to the end of an existing file instead of
    overwriting it.

    $PRINTENV name

    If the name of an uppercase environment variable is given the value of the variable will be placed in the output file. If the
    value is a null string or if the variable is undefined output will be stopped. This allows the system shell to generate code
    lines. This is usually used to pass in information about the compiler environment. For example:

         # If the following command were executed in the bash(1) shell...


          export STAMP="      write(*,*)  COMPILED ON: uname -s ;AT  date   "



    the environment variable STAMP would be set to something like

         write(*,*)  COMPILED ON:Eureka;AT Wed, Jun 12, 2013  8:12:06 PM  



    A version number would be another possibility

         export VERSION="      program_version=2.2"



    Special predefined variable names are:

         Variable Name      Output
         UFPP_DATE  ......  UFPP_DATE="12:58 14Jun2013"
         Where code is assumed to have defined UFPP_DATE as CHARACTER(LEN=15)
         UFPP_FILE  ......  UFPP_FILE="current filename"
         Where code is assumed to have defined UFPP_FILE as CHARACTER(LEN=1024)
         UFPP_LINE  ......  UFPP_LINE=    nnnnnn
         Where code is assumed to have defined UFPP_LINE as INTEGER



    $DOCUMENT [comment|write|help|version] [-file NAME][! comment]

          COMMENT:  write text prefixed by two exclamations and a space
          WRITE:    write text as Fortran WRITE(3f) statements
          HELP:     write text as a subroutine called HELP_USAGE
          VERSION:  write text as a subroutine called HELP_VERSION
                    prefixing lines with @(#) for use with the what(1) command.
          NULL:     Do not write to output file
          END:      End block of documentation



    Causes documentation to be altered in output so it is easily maintained as plain text. This is useful for keeping help text or
    man pages as part of a source file.

    It is assumed the output will not generate lines over 132 columns. FORTRAN is currently the only language supported. A blank
    value also returns to normal output processing. The Fortran generated is free-format Fortran 2003.

    So the text can easily be processed by other utilities such as markdown(1) or txt2man(1) to produce man(1) pages and HTML
    documents the file can be written as-is to $UFPP_DOCUMENT_DIR/doc/NAME with the -file parameter. If the environment variable
    $UFPP_DOCUMENT_DIR is not set the option is ignored.

    $SHOW

    Shows current state of ufpp(1); including variable names and values; and the name of the current input files. All output is
    preceded by an exclamation character.

    Example:

        ufpp A=10 B C D -o paper
        $define z=22
        $show
        $stop


        !======================================================================
        ! *ufpp* CURRENT STATE
        ! *ufpp*    TOTAL LINES READ ............          2
        ! *ufpp*    CONDITIONAL_NESTING_LEVEL....   0
        ! *ufpp*    DATE......................... 11:18 21Jun2013
        ! *ufpp*    ARGUMENTS ................... A=10 B C D -o paper
        ! *ufpp* VARIABLES:
        ! *ufpp*    ! A                               !          10
        ! *ufpp*    ! B                               !           1
        ! *ufpp*    ! C                               !           1
        ! *ufpp*    ! D                               !           1
        ! *ufpp*    ! Z                               !          22
        ! *ufpp* OPEN FILES:
        ! *ufpp*    ! ---- ! UNIT ! LINE NUMBER ! FILENAME
        ! *ufpp*    !    1 !    5 !           2 !
        !======================================================================



    $STOP stop-value

    Stops input file processing. An optional integer value of 1 to 20 will be returned as a status value to the system where
    supported. A value of zero is returned if no value is specified.

    $SYSTEM system_command

    If system command processing is enabled using the -system switch system commands can be executed to create files to be read or
    to execute test programs, for example. $SYSTEM directives are ignored by default; as you clearly need to ensure the input file
    is trusted before allowing commands to be executed.

    Examples:

        $! build variable definitions using GNU/Linux commands
        $SYSTEM echo system= hostname  > compiled.h
        $SYSTEM echo compile_time=" date " >> compiled.h
        $INCLUDE compiled.h


        $! obtain up-to-date copy of source file from HTTP server:
        $SYSTEM wget http://repository.net/src/func.F90 -O -|
        cpp -P -C -traditional >_tmp.f90
        $INCLUDE _tmp.f90
        $SYSTEM  rm _tmp.f90



    $UNDEFINE variable_name

    A symbol defined with $DEFINE can be removed with the $UNDEFINE directive.

    DEFINED(variable_name)

    A special function called DEFINED() may appear only in a $IF or $ELSEIF. If "variable_name" has been defined at that point in
    the source code, then the function value is ".TRUE.", otherwise it is ".FALSE.". A name is defined only if it has appeared in
    the source previously in a $DEFINE directive or been declared on the command line. The names used in compiler directives are
    district from names in the FORTRAN source, which means that "a" in a $DEFINE and "a" in a FORTRAN source statement are totally
    unrelated. The DEFINED() parameter is NOT valid in a $DEFINE directive.

    Example:

        >        Program test
        > $IF .NOT. DEFINED (inc)
        >        INCLUDE   comm.inc  
        > $ELSE
        >        INCLUDE   comm2.inc  
        > $ENDIF
        >        END



    The file, "comm.inc" will be INCLUDEd in the source if the parameter, "inc", has not been previously defined, while INCLUDE
    "comm2.inc" will be included in the source if "inc" has been previously defined. This is useful for setting up a default
    inclusion.

    $WARNING message

    Write message to stderr of form "WARNING message"

    $MESSAGE message

    Write message to stderr of form "message"

LIMITATIONS

    $IF constructs can be nested up to 20 levels deep. Note that using more than two levels typically makes input files less
    readable.

Input files

    o  lines are limited to 1024 columns. Text past column 1024 is ignored.

    o  files currently opened cannot be opened again.

    o  a maximum of 50 files can be nested by $INCLUDE

    o  filenames cannot contain spaces on the command line.

Variable names

    o  cannot be redefined unless first undefined.

    o  are limited to 31 characters.

    o  must start with a letter (A-Z).

    o  are composed of the letters A-Z, digits 0-9 and _ and $.

    o  2048 variable names may be defined at a time.

Major cpp(1) features not present in ufpp(1):

   There are no predefined preprocessor symbols. Use a directive input file
   instead. The predefined variables such as UFPP_DATE can be used as a
   substitute in some cases.


   This program does not provide string (macro) substitution in output
   lines. See cpp(1) and m4(1) and related utilities if macro expansion is
   required.


   While cpp(1) is the de-facto standard for preprocessing Fortran code,
   Part 3 of the Fortran 95 standard (ISO/IEC 1539-3:1998) defines
   Conditional Compilation, but it is (currently) not widely
   supported (See coco(1)).



ENVIRONMENT

    The environment variable $DEFAULT_ufpp can change command defaults. The values for -i, -I, and defined variables from
    $DEFAULT_ufpp will be prepended to the list defined on the command line. Other switches will be replaced by values on the
    command line. For example:

          env DEFAULT_ufpp="CRAY 64BIT -html -d -I CRAY_DIR" ufpp A=10 -i f90.html



    would run the same as the command

          ufpp CRAY 64BIT A=10 -html -d -I CRAY_DIR -i f90.html



    Overriding command defaults can be very useful when builds are done using make(1) files and/or scripts; as platform-specific
    and debug options can be evoked without changing the build-related files.

EXAMPLES

    Define variables on command line:

    Typically, variables are defined on the command line when ufpp(1) is invoked or are grouped together into small files that are
    included with a $INCLUDE or as input files.

         ufpp HP size=64 -i hp_directives.dirs @ test.F90 -o test_out.f90



    defines variables HP and SIZE as if the expressions had been on a $DEFINE and reads file "hp_directives.dirs" and then stdin
    and then test.F90. Output is directed to test_out.f90

Basic conditionals:

   >$! set value of variable "a" if it is not specified on the ufpp(1) command.
   >$IF .NOT.DEFINED(A)
   >$DEFINE a=1  ! define only the first version of SUB1(3f)
   >$ENDIF
   >program conditional_compile
   >   use M_kracken, only : kracken, lget
   >   ! use M_kracken module to crack command line arguments
   >   call kracken("cmd","-help .false. -version .false.")
   >   ! call routine generated by $DOCUMENT HELP
   >   call help_usage(lget("cmd_help"))
   >   ! call routine generated by $DOCUMENT VERSION
   >   call help_version(lget("cmd_version"))
   >   call sub1()
   >end program conditional_compile
   >! select a version of SUB1 depending on the value of ufpp(1) variable "a"
   >$IF a .EQ. 1
   >subroutine sub1
   >   print*, "This is the first SUB1"
   >end subroutine sub1
   >$ELSEIF a .eq. 2
   >subroutine sub1
   >   print*, "This is the second SUB1"
   >end subroutine sub1
   >$ELSE
   >subroutine sub1
   >   print*, "This is the third SUB1"
   >end subroutine sub1
   >$ENDIF
   >$! generate help_usage() procedure (and file to run thru txt2man(1) or other
   >$! filters to make man(1) page if $UFPP_DOCUMENT_DIR is set).
   >$DOCUMENT HELP -file conditional_compile.man
   >NAME
   >    conditional_compile - basic example for ufpp(1) pre-processor.
   >SYNOPSIS
   >    conditional_example [--help] [--version]
   >DESCRIPTION
   >    This is a basic example program showing how documentation can be used
   >    to generate program help text
   >OPTIONS
   >       --help
   >              display this help and exit
   >       --version
   >              output version information and exit
   >$DOCUMENT END
   >$! generate help_version() procedure
   >$DOCUMENT VERSION
   >DESCRIPTION: example program showing conditional compilation with ufpp(1)
   >PROGRAM:     conditional_compile
   >VERSION:     1.0, 20160703
   >AUTHOR:      John S. Urban
   >$DOCUMENT END

-----------------------------------------------------------------------------------------------------------------------------------

                                                             ufpp (1)                                                 July 02, 2017

Generated by manServer 1.08 from 01748848-5e18-4ad4-9879-69f83fecd742 using man macros.
                                                              [ufpp]
