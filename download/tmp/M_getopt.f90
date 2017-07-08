!>
!!##NAME
!!   M_getopt(3fm) - [ARGUMENTS:M_getopt]parse command line arguments. similar to those in standard C library.
!!
!!##SYNOPSIS
!!
!!   use getopt_m, only : getopt
!!   use getopt_m, only : option_s
!!   use getopt_m, only : optarg,optopt,optind
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
!!    use getopt_m, only : getopt,option_s,optarg,optopt
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
!!   Slightly modified from original -JSU
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
module getopt_m
    implicit none

    private
    ! procedures
    public getopt
    ! types
    public option_s
    ! values
    public optopt
    public optarg
    public optind
    public opterr

    character(len=4096) :: optarg
    character :: optopt
    integer :: optind = 1
    logical :: opterr = .true.

    type option_s
        character(len=4096) :: name
        logical :: has_arg
        character :: val
    end type

    ! grpind is index of next option within group; always >= 2
    integer, private :: grpind = 2

contains
! ----------------------------------------------------------------------------------------------------------------------------------
character function substr( str, i, j )
! Return str(i:j) if 1 <= i <= j <= len(str), else return empty string.
! This is needed because Fortran standard allows but doesn't *require* short-circuited
! logical AND and OR operators. So this sometimes fails:
!     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
! but this works:
!     if ( substr(str, i+1, i+1) == ':' ) then

        ! arguments
        character(len=*), intent(in) :: str
        integer, intent(in) :: i, j

        if ( 1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        end if
end function substr
!-----------------------------------------------------------------------------------------------------------------------------------
character function getopt( optstring, longopts )
! arguments
character(len=*), intent(in) :: optstring
type(option_s), intent(in), optional :: longopts(:)

        ! local variables
        character(len=4096) :: arg

        optarg = ''
        getopt = ''
        if ( optind > iargc()) then
            getopt = char(0)
        end if

        call getarg( optind, arg )
        if ( present( longopts ) .and. arg(1:2) == '--' ) then
            getopt = process_long( longopts, arg )
        else if ( arg(1:1) == '-' ) then
            getopt = process_short( optstring, arg )
        else
            getopt = char(0)
        end if
end function getopt
!-----------------------------------------------------------------------------------------------------------------------------------
character function process_long( longopts, arg )
! arguments
type(option_s), intent(in) :: longopts(:)
character(len=*), intent(in) :: arg

        ! local variables
        integer :: i

        ! search for matching long option
        optind = optind + 1
        do i = 1, size(longopts)
            if ( arg(3:) == longopts(i)%name ) then
                optopt = longopts(i)%val
                process_long = optopt
                if ( longopts(i)%has_arg ) then
                    if ( optind <= iargc()) then
                        call getarg( optind, optarg )
                        optind = optind + 1
                    else if ( opterr ) then
                        print '(a,a,a)', "Error: option '", trim(arg), "' requires an argument"
                    end if
                end if
                return
            end if
        end do
        ! else not found
        process_long = '?'
        if ( opterr ) then
            print '(a,a,a)', "Error: unrecognized option '", trim(arg), "'"
        end if
    end function process_long
!-----------------------------------------------------------------------------------------------------------------------------------
character function process_short( optstring, arg )
! arguments
character(len=*), intent(in) :: optstring, arg

        ! local variables
        integer :: i, arglen

        arglen = len( trim( arg ))
        optopt = arg(grpind:grpind)
        process_short = optopt

        i = index( optstring, optopt )
        if ( i == 0 ) then
            ! unrecognized option
            process_short = '?'
            if ( opterr ) then
                print '(a,a,a)', "Error: unrecognized option '-", optopt, "'"
            end if
        end if
        if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
            ! required argument
            optind = optind + 1
            if ( arglen > grpind ) then
                ! -xarg, return remainder of arg
                optarg = arg(grpind+1:arglen)
            else if ( optind <= iargc()) then
                ! -x arg, return next arg
                call getarg( optind, optarg )
                optind = optind + 1
            else if ( opterr ) then
                print '(a,a,a)', "Error: option '-", optopt, "' requires an argument"
            end if
            grpind = 2
        else if ( arglen > grpind ) then
            ! no argument (or unrecognized), go to next option in argument (-xyz)
            grpind = grpind + 1
        else
            ! no argument (or unrecognized), go to next argument
            grpind = 2
            optind = optind + 1
        end if
end function process_short
!-----------------------------------------------------------------------------------------------------------------------------------
end module getopt_m
!-----------------------------------------------------------------------------------------------------------------------------------
