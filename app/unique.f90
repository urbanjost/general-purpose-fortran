program unique ! this is a utility program. It is typically built using ccall(1).
use M_CLI2,                       only : set_args, lget, sgets, iget, unnamed
use M_uuid,                       only : generate_uuid
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
integer :: version
integer :: repeat
integer :: i,j
character(len=10),allocatable :: methods(:)
character(len=:),allocatable  :: prefix
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)

   ! define arguments, default values and crack command line
   call setup()
   call set_args('--method:m "4" --urn:U F --repeat:r 1',help_text,version_text)
   methods=[character(len=10) :: sgets('method'),unnamed]        ! get value of command line argument -method or unnamed strings
   if(size(methods).eq.0)methods=["4"]
   repeat=iget('repeat')                                         ! get value of command line argument -repeat
   prefix=merge('urn:uuid:','         ',lget('urn'))             ! get value of command line argument -urn
   prefix=trim(prefix)

   do i=1,size(methods)
      select case(methods(i))
      case(   '0','nil');    version=0
      case(   '1','time');   version=1
      case('','4','random'); version=4
      case default;          version=4
         write(stderr,'(*(g0,1x))')'*unique* unknown method',methods
         stop 1
      endselect
      do j=1,repeat
         write(*,'(2a)')prefix,generate_uuid(version)
      enddo
   enddo

contains
subroutine setup()

! @(#)help_usage(3f): sets help information

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>       ',&
'@(#)PROGRAM:        unique(1)>                                                  ',&
'@(#)DESCRIPTION:    output a UUID (Universally Unique ID)>                      ',&
'@(#)VERSION:        1.0, 20180427>                                              ',&
'@(#)AUTHOR:         John S. Urban>                                              ',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>             ',&
'@(#)COMPILED:       Tue, Feb 9th, 2021 9:19:16 PM>                              ',&
'']
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'    unique(1f) - [FUNIX] generate a UUID (Universally Unique ID) string         ',&
'                  per RFC 4122                                                  ',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    unique [[ --method] NAME][ -urn][ -repeat N]]|[ --help|--version]           ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   unique(3f) generates UUID strings according to the RFC 4122                  ',&
'   standard.                                                                    ',&
'                                                                                ',&
'   A universally unique identifier (UUID) is a 128-bit number used to           ',&
'   identify information in computer systems. When generated according           ',&
'   to standard methods UUIDs are for practical purposes unique.                 ',&
'                                                                                ',&
'   Standard methods 0,1, and 4 are supported as described in RFC 4122.          ',&
'                                                                                ',&
'   UUID strings are particularly useful as keys for relational database         ',&
'   entries, and for building unique temporary file names (especially in         ',&
'   cross-mounted filesystems that more than one OS is utilizing).               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    --method NAME  Select the UUID version type. Supported methods are          ',&
'                   nil|0, random|4, time|1.                                     ',&
'                                                                                ',&
'                   0. Nil UUID (ie. ''''00000000-0000-0000-0000-000000000000'''')',&
'                   1. time-based UUID                                           ',&
'                   2. Not implemented                                           ',&
'                   3. Not implemented                                           ',&
'                   4. pseudo-RNG(Random Number Generator) based                 ',&
'                   5. Not implemented                                           ',&
'                                                                                ',&
'    --urn       RFC 4122 defines a Uniform Resource Name (URN)                  ',&
'                namespace for UUIDs. IE., the output is                         ',&
'                prefixed with "urn:uuid:".                                      ',&
'                                                                                ',&
'    --repeat N  Number of UUID strings to generate                              ',&
'                                                                                ',&
'    --help      display this help and exit                                      ',&
'                                                                                ',&
'    --version   output version information and exit                             ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample commands                                                               ',&
'                                                                                ',&
'   unique                                                                       ',&
'   4bb8051e-4af3-11e8-6603-4254ffee9a14                                         ',&
'                                                                                ',&
'   unique -urn                                                                  ',&
'   urn:uuid:e9fd7cab-69f2-4cd6-4b5e-d54b9fbf617a                                ',&
'                                                                                ',&
'   unique -method time -repeat 4                                                ',&
'   f2a2faf0-833a-11e9-7373-5eb4cfd7e237                                         ',&
'   f2a2faf0-833a-11e9-7373-afbb9f7b9100                                         ',&
'   f2a2faf0-833a-11e9-7373-cde3ffff3681                                         ',&
'   f2a2faf0-833a-11e9-7373-271cfbfd42bc                                         ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
!>
!!##NAME
!!     unique(1f) - [FUNIX] generate a UUID (Universally Unique ID) string
!!                   per RFC 4122
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     unique [[ --method] NAME][ -urn][ -repeat N]]|[ --help|--version]
!!
!!##DESCRIPTION
!!    unique(3f) generates UUID strings according to the RFC 4122
!!    standard.
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems. When generated according
!!    to standard methods UUIDs are for practical purposes unique.
!!
!!    Standard methods 0,1, and 4 are supported as described in RFC 4122.
!!
!!    UUID strings are particularly useful as keys for relational database
!!    entries, and for building unique temporary file names (especially in
!!    cross-mounted filesystems that more than one OS is utilizing).
!!
!!##OPTIONS
!!     --method NAME  Select the UUID version type. Supported methods are
!!                    nil|0, random|4, time|1.
!!
!!                    0. Nil UUID (ie. ''00000000-0000-0000-0000-000000000000'')
!!                    1. time-based UUID
!!                    2. Not implemented
!!                    3. Not implemented
!!                    4. pseudo-RNG(Random Number Generator) based
!!                    5. Not implemented
!!
!!     --urn       RFC 4122 defines a Uniform Resource Name (URN)
!!                 namespace for UUIDs. IE., the output is
!!                 prefixed with "urn:uuid:".
!!
!!     --repeat N  Number of UUID strings to generate
!!
!!     --help      display this help and exit
!!
!!     --version   output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    unique
!!    4bb8051e-4af3-11e8-6603-4254ffee9a14
!!
!!    unique -urn
!!    urn:uuid:e9fd7cab-69f2-4cd6-4b5e-d54b9fbf617a
!!
!!    unique -method time -repeat 4
!!    f2a2faf0-833a-11e9-7373-5eb4cfd7e237
!!    f2a2faf0-833a-11e9-7373-afbb9f7b9100
!!    f2a2faf0-833a-11e9-7373-cde3ffff3681
!!    f2a2faf0-833a-11e9-7373-271cfbfd42bc
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end subroutine setup
end program unique
