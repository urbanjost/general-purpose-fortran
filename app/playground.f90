program playground
   use, intrinsic :: iso_fortran_env, only: &
    & stderr => ERROR_UNIT,&
    & stdin => INPUT_UNIT,&
    & stdout => OUTPUT_UNIT
   use M_io, only: filebyte, basename
   use M_cli2, only: set_args, get_args, names => unnamed
   use M_cli2, only: sget
   use M_strings, only: percent_encode
   implicit none
   character(len=1), allocatable :: text(:) ! array to hold file in memory
   character(len=:), allocatable :: help_text(:)
   character(len=:), allocatable :: version_text(:)
   character(len=:), allocatable :: shortname
   character(len=:), allocatable :: play(:)
   character(len=:), allocatable :: type
   integer                      :: i, j, k
   call setup()
   call set_args(' --type "html" ', help_text, version_text)
!-----------------------------------------------------------------------------------------------------------------------------------
   ! start file
   type=sget('type')
   select case (type)
   case ('html')
      call write ([CHARACTER(LEN=128) :: &
       '<!DOCTYPE html>                                 ', &
       '<html xmlns="http://www.w3.org/1999/xhtml">     ', &
       '<head>                                          ', &
       '  <meta name="generator" content="playground" />', &
       '  <title>playground</title>                     ', &
       '</head>                                         ', &
       '<body>                                          ', &
       ' '])
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   ! process input files
   do i = 1, size(names)
      call filebyte(names(i), text) ! allocate character array and copy file into it
      if (.not. allocated(text)) then
         write (stderr, *) '*playground* failed to load file '//trim(names(i))
         cycle
      endif
      shortname = basename(names(i))//'      '
      if (index(shortname, 'demo_') .eq. 1) shortname = shortname(6:)
      shortname = trim(shortname)

      select case (type)
      case ('html')
         ! write percent-encrypted copy of code as a URI
         write (stdout, '(*(g0))', advance='no') '  <a href="https://play.fortran-lang.org/?code=', percent_encode(text), '" '
         play = [CHARACTER(LEN=128) :: &
          '  target="_blank" title="Open in Fortran Playground">', &
          '  <img src="https://raw.githubusercontent.com/fortran-lang/playground/main/frontend/src/fortran-logo.png"', &
          '  alt="Fortran logo" class="align-text-bottom" height="15.5" /> '//shortname//'', &
          '  </a>', &
          '  <xmp>']
         call write(play)
         ! now write file to stdout as-is
         write (stdout, '(*(a:))', advance='no') text
         deallocate (text)  ! release memory
         write (*, '(*(a:))') '  </xmp>'
      case ('md')
         call write ([CHARACTER(LEN=128) :: ' ', ' '])
         ! write percent-encrypted copy of code as a URI
         write (stdout, '(*(g0))', advance='no') '    open [', shortname, ']'
         write (stdout, '(*(g0))', advance='no') '(https://play.fortran-lang.org/?code=', percent_encode(text), ')'
         play = [CHARACTER(LEN=128) :: &
                 '  in Fortran Playground    ', &
                 '  ']
         write (stdout, '(a)') (trim(play(j)), j=1, size(play))
         ! now write file to stdout as-is
         write (stdout, '(*(a:))') '```fortran'
         write (stdout, '(*(a:))', advance='no') text
         write (*, '(*(a:))') '```'
         deallocate (text)  ! release memory
      case default
         write (stderr,*) '<ERROR> unknown type', type, 'must be from {html|md}'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   ! close out file
   select case (type)
   case ('html')
      call write ([CHARACTER(LEN=128) :: '</body>', '</html>', ''])
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
contains

   subroutine write (strings)
      character(len=*), intent(in) :: strings(:)
      integer :: i
      write (stdout, '(a)') (trim(strings(i)), i=1, size(strings))
   end subroutine write

   subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'playground(1f) - convert Fortran file to an HTML document that uploads',&
'                 the code to "Fortran Playground"                     ',&
'                                                                      ',&
'SYNOPSIS                                                              ',&
'    playground [ --help| --version] [ --type OUTPUT_TYPE] *.[fF]90    ',&
'                                                                      ',&
'DESCRIPTION                                                           ',&
'                                                                      ',&
'create an HTML document from Fortran source files that includes a     ',&
'click-able download to the "Fortran Playground".                      ',&
'                                                                      ',&
'URL encoding, officially known as percent-encoding, is a method to    ',&
'encode arbitrary data in a Uniform Resource Identifier (URI) using only',&
'a limited subset of ASCII characters, replacing them with one or more  ',&
'character triplets that consist of the percent character and a two-digit',&
'hexadecimal value.                                                      ',&
'                                                                        ',&
'OPTIONS                                                                 ',&
'    --type         "html", or "md"                                      ',&
'    --help         display this help and exit                           ',&
'    --version      output version information and exit                  ',&
'    filename(s)    Fortran source files                                 ',&
'                                                                        ',&
'EXAMPLES                                                                ',&
'  Sample commands                                                       ',&
'                                                                        ',&
'   > $  playground hello.f90 > playground.html                          ',&
'   > <!DOCTYPE html>                                                    ',&
'   > <html xmlns="http://www.w3.org/1999/xhtml">                        ',&
'   > <head>                                                             ',&
'   >   <meta name="generator" content="playground" />                   ',&
'   >   <title>playground</title>                                        ',&
'   > </head>                                                            ',&
'   > <body>                                                             ',&
'   >                                                                    ',&
'   >   <a href="https://play.fortran-lang.org/?code=                    ',&
'   >   program%20hello%5Fworld                                          ',&
'   >   %0A%20%20%20write%28%2A%2C%2A%29%27Hello%20World%21%27           ',&
'   >   %0Aend%20program%20hello%5Fworld                                 ',&
'   >   %0A"                                                             ',&
'   >   target="_blank" title="Open in Fortran Playground">              ',&
'   >   <img src="https://raw.githubusercontent.com/fortran-lang/        ',&
'   >   playground/main/frontend/src/fortran-logo.png"                   ',&
'   >   alt="Fortran logo" class="align-text-bottom" height="15.5" /> hello',&
'   >   </a>                                                               ',&
'   >   <xmp>                                                              ',&
'   > program hello_world                                                  ',&
'   >    write(*,*)''''Hello World!''''                                    ',&
'   > end program hello_world                                              ',&
'   >   </xmp>                                                             ',&
'   > </body>                                                              ',&
'   > </html>                                                              ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        playground(1)                                       ',&
'DESCRIPTION:    create an HTML document from Fortran sources files  ',&
'VERSION:        1.0, 2023-05-20                                     ',&
'AUTHOR:         John S. Urban                                       ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html      ',&
'']
   end subroutine setup
end program playground
