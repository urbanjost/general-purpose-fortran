










!>
!!##NAME
!!   M_getopt(3fm) - [ARGUMENTS:M_getopt] parse command line arguments. similar to those in standard C library.
!!   (LICENSE:GPL)
!!
!!##SYNOPSIS
!!
!!   use M_getopt, only : getopt
!!   use M_getopt, only : option_s
!!   use M_getopt, only : optarg,optopt,optind
!!
!!##DESCRIPTION
!!
!!   ch = getopt( optstring, [longopts] )
!!
!!   Returns next option character from command line arguments.
!!    o If an option is not recognized, it returns '?'.
!!    o If no options are left, it returns a null character, char(0).
!!
!!##OPTIONS
!!   optstring  contains characters that are recognized as options.
!!              If a character is followed by a colon, then it takes a required argument.
!!              For example, "x" recognizes "-x", while "x:" recognizes "-x arg" or "-xarg".
!!   opterr     Errors are printed by default. Set opterr=.false. to suppress them.
!!
!!##RETURNS
!!   optopt     is set to the option character, even if it isn't recognized.
!!   optarg     is set to the option's argument.
!!   optind     has the index of the next argument to process. Initially optind=1.
!!
!!   Grouped options are allowed, so "-abc" is the same as "-a -b -c".
!!
!!   If longopts is present, it is an array of type(option_s), where each entry
!!   describes one long option.
!!
!!      type option_s
!!          character(len=4096) :: name
!!          logical             :: has_arg
!!          character           :: val
!!      end type
!!
!!   The name field is the option name, without the leading -- double dash.
!!   Set the has_arg field to true if it requires an argument, false if not.
!!   The val field is returned. Typically this is set to the corresponding short
!!   option, so short and long options can be processed together. (But there
!!   is no requirement that every long option has a short option, or vice-versa.)
!!
!!   Differences from C version:
!!   - when options are finished, C version returns -1 instead of char(0),
!!     and thus stupidly requires an int instead of a char.
!!   - does not support optreset
!!   - does not support "--" as last argument
!!   - if no argument, optarg is blank, not NULL
!!   - argc and argv are implicit
!!
!!   Differences for long options:
!!   - optional argument to getopt(), rather than separate function getopt_long()
!!   - has_arg is logical, and does not support optional_argument
!!   - does not support flag field (and thus always returns val)
!!   - does not support longindex
!!   - does not support "--opt=value" syntax, only "--opt value"
!!   - knows the length of longopts, so does not need an empty last record
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getopts
!!    use M_getopt, only : getopt,option_s,optarg,optopt
!!    implicit none
!!
!!    character(len=*),parameter :: OPTIONS='ab:c'
!!    type(option_s):: opts(2)
!!       opts(1) = option_s( "alpha", .false., 'a' )
!!       opts(2) = option_s( "beta",  .true.,  'b' )
!!       do
!!          PARSE: select case( getopt( OPTIONS, opts ))
!!          case( char(0))
!!             exit PARSE
!!          case( 'a' )
!!             print *, 'option alpha/a', optarg
!!          case( 'b' )
!!             print *, 'option beta/b=', optarg
!!          case( '?' )
!!             print *, 'unknown option ', optopt,' not in ',OPTIONS
!!             stop
!!          case default
!!             print *, 'unhandled option c ', optopt, ' (an intentional bug)'
!!          end select PARSE
!!       end do
!!    end program demo_getopts
!!
!!##COPYRIGHT
!!   Copyright 2008 by Mark Gates
!!
!!   This program is free software; you can redistribute or modify it under
!!   the terms of the GNU general public license (GPL), version 2 or later.
!!
!!   This program is distributed in the hope that it will be useful, but
!!   WITHOUT ANY WARRANTY; without even the implied warranty of
!!   merchantability or fitness for a particular purpose.
!!
!!   If you wish to incorporate this into non-GPL software, please contact
!!   me regarding licensing terms.
!!
!!   Slightly modified from original to integrate it into the GPF (General Purpose
!!   Fortran) format and create a man(1) page - JSU
!-----------------------------------------------------------------------------------------------------------------------------------
module M_getopt
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    implicit none

    character(len=80)     :: optarg        ! Option's value
    character             :: optopt        ! Option's character
    integer               :: optind=1      ! Index of the next argument to process
    logical               :: opterr=.true. ! Errors are printed by default. Set opterr=.false. to suppress them

    type option_s
        character(len=80) :: name          ! Name of the option
        logical           :: has_arg       ! Option has an argument (.true./.false.)
        character         :: short         ! Option's short character equal to optopt
    end type option_s

    integer, private:: grpind=2            ! grpind is index of next option within group; always >= 2

    public test_suite_M_getopt
contains
!-----------------------------------------------------------------------------------------------------------------------------------
character function substr( str, i, j )
! Return str(i:j) if 1 <= i <= j <= len(str), else return empty string.
! This is needed because Fortran standard allows but doesn't *require* short-circuited
! logical AND and OR operators. So this sometimes fails:
!     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
! but this works:
!     if ( substr(str, i+1, i+1) == ':' ) then
character(len=*), intent(in):: str
integer, intent(in):: i, j

   if ( 1 <= i .and. i <= j .and. j <= len(str)) then
       substr = str(i:j)
   else
       substr = ''
   endif
end function substr
!-----------------------------------------------------------------------------------------------------------------------------------
character function getopt( optstring, longopts )
character(len=*), intent(in)           :: optstring
type(option_s),   intent(in), optional :: longopts(:)
character(len=80)                      :: arg

   optarg = ''
   if ( optind > command_argument_count()) then
       getopt = char(0)
   endif

   call get_command_argument( optind, arg )

   if ( present( longopts ) .and. arg(1:2) == '--' ) then
       getopt = process_long( longopts, arg )
   elseif ( arg(1:1) == '-' ) then
       getopt = process_short( optstring, arg )
   else
       getopt = char(0)
   endif

end function getopt
!-----------------------------------------------------------------------------------------------------------------------------------
character function process_long( longopts, arg )
type(option_s), intent(in)   :: longopts(:)
character(len=*), intent(in) :: arg
integer                      :: i = 0
integer                      :: j = 0
integer                      :: len_arg = 0             ! length of arg
logical                      :: has_equalsign = .false. ! arg contains equal sign?

  len_arg = len_trim(arg)

  ! search for equal sign in arg and set flag "has_equalsign" and
  ! length of arg (till equal sign)
  do j=1, len_arg
      if (arg(j:j) == "=") then
          has_equalsign = .true.
          len_arg = j-1
          exit
      endif
  enddo

  ! search for matching long option

  if (.not. has_equalsign) then
      optind = optind + 1
  endif

  do i = 1, size(longopts)
      if ( arg(3:len_arg) == longopts(i)%name ) then
          optopt = longopts(i)%short
          process_long = optopt
          if ( longopts(i)%has_arg ) then
              if (has_equalsign) then ! long option has equal sign between value and option
                  if (arg(len_arg+2:) == '') then ! no value (len_arg+2 value after "="
                      write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                      process_long=char(0) ! Option not valid
                  else
                      call get_command_argument(optind, optarg)
                      optarg = optarg(len_arg+2:)
                      optind = optind + 1
                  endif
              else ! long option has no equal sign between value and option
                  if ( optind <= command_argument_count()) then
                      call get_command_argument( optind, optarg )
                      optind = optind + 1
                  elseif ( opterr ) then
                      write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                      process_long=char(0) ! Option not valid
                  endif
              endif
          endif
          return
      endif
  enddo
  ! else not found
  process_long = char(0)
  optopt='?'
  if ( opterr ) then
      write(stderr, '(a,a,a)') "ERROR: Unrecognized option '", arg(1:len_arg), "'"
  endif
end function process_long
!-----------------------------------------------------------------------------------------------------------------------------------
character function process_short( optstring, arg )
character(len=*), intent(in) :: optstring, arg
integer                      :: i, arglen
   arglen = len( trim( arg ))
   optopt = arg(grpind:grpind)
   process_short = optopt

   i = index( optstring, optopt )
   if ( i == 0 ) then
       ! unrecognized option
       process_short = '?'
       if ( opterr ) then
           write(stderr, '(a,a,a)') "ERROR: Unrecognized option '-", optopt, "'"
       endif
   endif

   if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
       ! required argument
       optind = optind + 1
       if ( arglen > grpind ) then
           ! -xarg, return remainder of arg
           optarg = arg(grpind+1:arglen)
       elseif ( optind <= command_argument_count()) then
           ! -x arg, return next arg
           call get_command_argument( optind, optarg )
           optind = optind + 1
       elseif ( opterr ) then
           write(stderr, '(a,a,a)') "ERROR: Option '-", optopt, "' requires a value"
           process_short = char(0) ! Option not valid
       endif
       grpind = 2
   elseif ( arglen > grpind ) then
       ! no argument (or unrecognized), go to next option in argument (-xyz)
       grpind = grpind + 1
   else
       ! no argument (or unrecognized), go to next argument
       grpind = 2
       optind = optind + 1
   endif

end function process_short
!-----------------------------------------------------------------------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_getopt()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_getopt()
   call test_process_long()
   call test_process_short()
   call test_substr()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()

   call unit_check_start('getopt',msg='')
   !!call unit_check('getopt', 0.eq.0, 'checking',100)
   call unit_check_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_long()

   call unit_check_start('process_long',msg='')
   !!call unit_check('process_long', 0.eq.0, 'checking',100)
   call unit_check_done('process_long',msg='')
end subroutine test_process_long
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_short()

   call unit_check_start('process_short',msg='')
   !!call unit_check('process_short', 0.eq.0, 'checking',100)
   call unit_check_done('process_short',msg='')
end subroutine test_process_short
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_substr()

   call unit_check_start('substr',msg='')
   !!call unit_check('substr', 0.eq.0, 'checking',100)
   call unit_check_done('substr',msg='')
end subroutine test_substr
!===================================================================================================================================
end subroutine test_suite_M_getopt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_getopt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
