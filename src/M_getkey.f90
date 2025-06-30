










!>
!!##NAME
!!      system_getkey(3f) - [M_getkey] read single character from keyboard
!!      in hot (raw I/O) mode
!!      (LICENSE:PD)
!!##SYNTAX
!!
!!##DESCRIPTION
!!    SYSTEM_GETKEY(3F) is a C/Fortran combination that (hopefully) lets
!!    Fortran read one character at a time in raw I/O mode on most platforms.
!!
!!    Unfortunately (as of this writing), there is no universal standard
!!    method using the programming languages Fortran or C for reading hot
!!    (raw I/O) keystrokes. In this document "Hot" or "Raw" mode, also
!!    sometimes called "immediate mode" means each keystroke is detected
!!    immediately, without requiring a carriage return.
!!
!!    The best way to know if this will work on your platform is to try it.
!!    There are five alternative versions of the C code in the source to
!!    try if it does not.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_system_getkey
!!    use M_getkey, only : system_getkey
!!    character :: A
!!    integer   :: icount=0
!!    character(len=1),parameter :: null=char(0)
!!       call clear()
!!       write(*,*)'begin striking keys to demonstrate interactive raw I/O mode'
!!       call menu()
!!       do
!!          A=system_getkey()
!!          icount=icount+1
!!          select case(A)
!!          case('a':'e')
!!             write(*,*)'You entered a valid menu item ',A,'=>',ichar(A),icount
!!          case(null)
!!             if(icount.gt.40000000)then
!!                write(*,*)'limit of 40 000 000, calls reached'
!!                stop
!!             endif
!!          case('q')
!!             stop
!!          case default
!!             call clear()
!!             write(*,*)'unknown menu option'
!!             write(*,*)'you entered key=',A,'->',ichar(A),icount
!!             call menu()
!!          end select
!!       enddo
!!       contains
!!
!!       subroutine clear()
!!       ! ANSI VT102 screen clear sequence.
!!       ! May not work in all terminal emulators
!!       write(*,'(a,"[2J")',advance='no')char(27)
!!       flush(6)
!!       write(*,*)
!!       end subroutine clear
!!
!!       subroutine menu()
!!       write(*,"(3x,'a)  first choice   ')")
!!       write(*,"(3x,'b)  second choice  ')")
!!       write(*,"(3x,'c)  third choice   ')")
!!       write(*,"(3x,'d)  fourth choice  ')")
!!       write(*,"(3x,'e)  fifth choice   ')")
!!       write(*,"('enter choice (q to quit):')")
!!       end subroutine menu
!!
!!    end program demo_system_getkey
!!
!!##HOW IT WORKS
!!
!!    The getkey(3c) C routine uses commonly available routines to set to
!!    raw mode, read a keystroke, and reset to normal mode. Once this is
!!    working, it is typically easy to make a Fortran routine that calls
!!    the C routine. How to make Fortran/C interfaces still varies if you
!!    do not have a Fortran 2003 compiler.
!!
!!    getkey.c is the core C routine that you must get working.
!!
!!    The steps to test the SYSTEM_GETKEY(3F) procedure are relatively simple.
!!
!! Test C program
!!
!!    First you need to make sure the C routine will work by itself ...
!!
!!       ########################################
!!       cc -DTESTPRG -DLINUX getkey.c -o testit
!!          or
!!       cc -DTESTPRG -V13    getkey.c -o testit
!!          or
!!       cc -DTESTPRG -V13B   getkey.c -o testit
!!          or
!!       cc -DTESTPRG -DBSD   getkey.c -o testit
!!          then
!!       ./testit
!!       ########################################
!!
!! Test common Fortran/C interface style
!!
!!    Once the C program works; just make an object file and then load it with
!!    one of the Fortran programs, depending on which compilers you have
!!
!! Test using Fortran 2003 ISO_C_BINDING
!!
!!    If you have a Fortran 2003 compiler, there is a standard-based method for
!!    the Fortran-to-C interface that is preferable that uses the ISO_C_BINDINGS
!!    modules. Note that most f90+ compilers now support this 2003 feature as
!!    an extension ...
!!
!!       ########################################
!!       cc -DLinux getkey.c -o
!!       g95 M_getkey.F90 getkey.o -o testit
!!       ./testit
!!       ########################################
!!
!! Running the Test Program
!!
!!    The sample program reads one character at a time until the letter "q"
!!    is entered. If the C program works in stand-alone mode but none of the
!!    Fortran examples work you will have to find out how your programming
!!    environment allows Fortran to call C routines. For this intentionally
!!    simple routine you usually just need to add an underscore to the
!!    C name (ie "_getkey" or "getkey_" or make it uppercase "GETKEY")
!!    to make Fgetkey(3c) a Fortran-callable procedure.
!!
!!##ALTERNATIVES
!!
!!   * Look at the curses(3c) or ncurses(3c) libraries or similar libraries
!!   * Look for extensions in your Fortran compiler documentation (Pass
!!     examples back -- I would be glad to see them).
!!   * In Unix and GNU/1 look at the system commands
!!
!!        stty -cread
!!           or
!!        stty raw -echo min 0  time 5
!!
!!
!!     Sometimes you can use a call to SYSTEM() to set and unset raw I/O
!!     mode and then use standard I/O routines; This is a simple method;
!!     but it is highly OS(Operating System) and compiler dependent and
!!     has very high overhead.
!!   * Routines that allow input-line editing may provide source that has
!!     to read in raw mode.
!!
!!
!!##ORIGINS
!!
!!    This routine has been on a lot of systems over the years; it dates back
!!    to a program that was used to read keypresses from a Tektronix 4010 and
!!    4014 raster storage graphics terminal (an xterm(1) still can emulate a
!!    Tektronix 4010!).
!!
!!##TESTED
!!
!!    Recently tested with the Intel compilers on 1, on a MSWindows
!!    machine in the CygWin application, on HP-UX and Solaris. Some version
!!    of it was used on UniCOS, Tru64, AIX, NextStep, ...
!!
!!    caveat: communication-related characters such as ctrl-S and ctrl-Q
!!    are often still intercepted by the system or by terminal emulators.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! Last updated May 5th, 2009 -- John S. Urban
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! make Fortran/C interface for C routine getkey(3C)
module M_getkey
   use iso_c_binding
   implicit none
   private
   public system_timeout_getkey
   public system_getkey
   public test_suite_M_getkey

      interface
         function system_timeout_getkey(delay) bind(c, name='Ftimeout_getkey')
            use iso_c_binding
            implicit none
            integer(kind=c_int),intent(in),value :: delay
            character(kind=c_char) :: system_timeout_getkey
         end function system_timeout_getkey
      end interface

      interface
         function system_getkey() bind(c, name='Fgetkey')
            use iso_c_binding
            implicit none
            character(kind=c_char) :: system_getkey
         end function system_getkey
      end interface
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_getkey()
use M_verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_verify, only : unit_test_level

!! setup
call test_system_getkey()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_system_getkey()

   call unit_test_start('system_getkey',msg='')
   !!call unit_test('system_getkey', 0.eq.0, 'checking',100)
   call unit_test_done('system_getkey',msg='')
end subroutine test_system_getkey

end subroutine test_suite_M_getkey
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_getkey
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
