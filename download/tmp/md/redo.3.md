[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - redo (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    redo(3f) - [M_history]Fortran-based Input History Editor

CONTENTS

    Synopsis
    Description
    Usage
         Listing Command History
         Positioning To Previous Commands
         Editing The Current Buffer Line
         Help
         System Commands
         Dumping And Loading The Command History
    Example Program
    Sample Usage
    Author

SYNOPSIS

    subroutine redo(inputline)


         character(len=*) :: inputline



DESCRIPTION

    the redo(3f) routine lets you recall, list, save, and modify previously entered program input. Built-in help is included.

    The redo(3f) input history editor is a simple-to-use input history editor interface modeled on the CDC NOS command REDO. It
    uses a line editor model that means no special escape characters or control characters are required. Typically, only a few
    minutes are required to master usage.

    When using redo(3f) input lines are usually first read into a character variable and then passed to the routine. The returned
    string can then be parsed or read from with an internal READ(3f). So, for example, if you have an existing READ(3f) such as

          READ(*,101) A,I,K



    replace it with something similar to

        USE M_HISTORY,ONLY : REDO
        CHARACTER(LEN=255) :: LINE ! make variable big enough to read a line
              :
              :
        READ(*, (A) ) LINE   ! read line into character variable
        CALL REDO(LINE)      ! pass line to REDO(3f). This is a no-op except
                             ! for storing the line into the input history
                             ! unless the input line is the "r" command
        READ(LINE,101)A,I,K  ! read from variable like you did from file



USAGE

    When prompted for an input line by your program you may at any timeenter "r" on a line by itself, or a line beginning with "r
    r_command" and you will enter the command history edit mode. Now you can recall and edit previous input or compose an input
    line using the editor commands.

    By default, you will be editing the last line you entered, shifted one character to the right by an exclamation character.

    The character you respond with in column one controls what happens next.

    o    If you enter "?" while in command edit mode, help is displayed.

    o    If the last input line is not the desired line to edit, select the line to edit by entering it s line number or by using
         the /,l,u, and d commands (see below for details) to find the desired input line.

    o    Next enter an editing directive (c,m) to edit the selected line. The "change" command will change all occurrences of an
         old string to a new string ...

          c/old/new/



    o    or the "modify" command can be used with the special characters # & and ^ ...

         o    A # under a character will delete a character.

         o    An "&" (ampersand) will cause the character above it to be replaced with a space.

              o To insert a string enter ^string#.

         o    Otherwise, enter a character under one in the currently displayed command and it will replace it.

         o    hit RETURN to start another edit of the line

    o    Once the change is executed you will be prompted for another edit directive

    o    You will stay in edit mode until you enter a return on a blank line to feed your line to your program; or enter "." or "q"
         (which means cancel changes and return blank line).

    A detailed summary of the main edit-mode commands follows. In the descriptions, N stands for a number ...

    LISTING COMMAND HISTORY

            l|p N list from line N. -N shows N last lines

            L|P N same as  l except no line numbers (for pasting)

            /string search for simple string in all history lines Note that the buffer set to the last line displayed

    POSITIONING TO PREVIOUS COMMANDS

      u N up through buffer

      d N down through buffer

      N load line number

    EDITING THE CURRENT BUFFER LINE

             c/oldstring/newstring/ change all occurrences of old string to new string. Note that s (for substitute) is a synonym
             for c (for change).

             For the "c" directive the second character becomes the delimiter. Traditionally one usually uses a delimiter of /
             unless the string you are editing contains /.

             mmod_string If the first character of your entry is m or blank,

             o    REPLACE a string by entering a replacement character under it

             o    LEAVE a character alone by leaving a space under it

             o    DELETE a character by putting a # character under it

             o    BLANK OUT a character by putting an & under it

             o    INSERT A STRING by entering ^STRING#

    The "modify" directive takes a little practice but this single directive accommodates positionally deleting, replacing, and
    inserting text. it is hardest using "modify" to put the strings "&" and "#" into your lines. to put a # or & character into a
    string use the  c command instead or ^&# or ^##.

             ;N N N N ... Append specified lines to current line

    HELP

      h|? display help text

    SYSTEM COMMANDS

       !cmd execute system command

    DUMPING AND LOADING THE COMMAND HISTORY

         w FILENAME write entire command history to specified file

         r FILENAME replace command history with file contents

         a FILENAME append lines from file onto command history

EXAMPLE PROGRAM

    Sample program

       program redoit
       use M_history, only : redo
       implicit none
       character(len=1024) ::  line
       integer :: ios
       write(*, (a) ) &
       &  REDO(3f) COMMAND INPUT EDITOR ,
       &  enter "r" or "r r_command" on the input line to go ,
       &  into history edit mode. Once in history edit mode you ,
       &  may enter "?" to get some help. Enter "quit" to exit ,
       &  the program. 
       do
          write(*, (a) ,advance= no ) >->     ! write prompt
          read(*, (a) ,iostat=ios) line       ! read new input line
          ! if "r", edit and return a line from the history editor
          call redo(line) ! store into history if not "r".
          if(line.eq. quit )stop ! exit program if user enters "quit"
          ! now call user code to process new line of data
          ! As an example, call the system shell using a common f77 extension:
          call execute_command_line(trim(line)) ! f08 equivalent
       enddo
       end program redoit



SAMPLE USAGE

    The example program is basically a loop that reads a command from standard input and then executes it as a subshell unless the
    "r" command is entered.

    Now, we will enter an echo(1) command followed by a few other lines of input. Then we recall the echo(1) command and use a few
    of the features of redo(3) to change and then re-execute the command.

          >echo This isss a Test
          This isss a Test
          >date
          Sun May 31 23:54:09 EDT 2009
          >pwd
          /cygdrive/c/urbanjs/MYCYGWIN/DISKA/public_html/public/CLONE/REDO
          >r                            ! enter edit mode
          00001 echo This isss a Test   ! last commands are displayed
          00002 date
          00003 pwd
          !pwd
          >1                            ! go to first line in history
          !echo This isss a Test
                       ##   t           ! delete and replace characters
          !echo This is a test          ! insert a string
                          ^new #
          !echo This is a new test
          c/test/TEST/                  ! change a substring
          !echo This is a new TEST
                             &          | replace character with spaces
          !echo This is a newTEST
                                        ! a blank line ends editing
          This is a newTEST
          >quit



AUTHOR

    John S. Urban, 1988,2009,2011 (last change: Nov 2015)

-----------------------------------------------------------------------------------------------------------------------------------

                                                             redo (3)                                                 July 02, 2017

Generated by manServer 1.08 from d22fbce9-2ae5-4348-9b35-bdadc9cc7c1b using man macros.
                                                              [redo]
