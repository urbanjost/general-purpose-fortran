subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'     _mkdir(1f) - [FUNIX:FILESYSTEM] call mkdir(3c) to make directories         ',&
'     (LICENSE:PD)                                                               ',&
'SYNOPSIS                                                                        ',&
'     _mkdir DIRECTORY ... [OPTION]...                                           ',&
'DESCRIPTION                                                                     ',&
'       Create the DIRECTORY(ies), if they do not already exist.                 ',&
'       The file permission mode by default is "a=rxw-umask".                    ',&
'OPTIONS                                                                         ',&
'   DIRECTORY  directory pathnames. Limited to 4096 characters per pathname.     ',&
'   --parents  no error if existing, make parent directories as needed           ',&
'   --verbose  print a message for each created directory                        ',&
'   --help     display this help and exit                                        ',&
'   --version  output version information and exit                               ',&
'EXAMPLE                                                                         ',&
'  Samples:                                                                      ',&
'                                                                                ',&
'   # silently make specified directory and any needed parent directories        ',&
'   _mkdir A/B/C -parents                                                        ',&
'                                                                                ',&
'   # show creation of three directories                                         ',&
'   _mkdir A B C                                                                 ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!      _mkdir(1f) - [FUNIX:FILESYSTEM] call mkdir(3c) to make directories
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      _mkdir DIRECTORY ... [OPTION]...
!!##DESCRIPTION
!!        Create the DIRECTORY(ies), if they do not already exist.
!!        The file permission mode by default is "a=rxw-umask".
!!##OPTIONS
!!    DIRECTORY  directory pathnames. Limited to 4096 characters per pathname.
!!    --parents  no error if existing, make parent directories as needed
!!    --verbose  print a message for each created directory
!!    --help     display this help and exit
!!    --version  output version information and exit
!!##EXAMPLE
!!
!!   Samples:
!!
!!    # silently make specified directory and any needed parent directories
!!    _mkdir A/B/C -parents
!!
!!    # show creation of three directories
!!    _mkdir A B C
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        _mkdir(1f)>',&
'@(#)DESCRIPTION:    Create the specified directories if they do not already exist.>',&
'@(#)VERSION:        1.0, 2016-12-04>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2021-07-01 09:04:57 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program demo_system_mkdir
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system,  only : system_mkdir, system_perror
use M_system,  only : R_GRP,R_OTH,R_USR,RWX_G,RWX_O
use M_system,  only : RWX_U,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
use M_system,  only : DEFFILEMODE, ACCESSPERMS
use M_io,      only : dirname
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: fname
character(len=4096),allocatable    :: above(:)
character(len=4096)                :: scratch
integer                            :: ierr, i, j
logical                            :: verbose
logical                            :: parents
   call kracken('mkdir','-help .F. -version .F. -verbose .F. -parents .F. ')  ! define and crack the command line
   call help_usage(lget('mkdir_help'))                                        ! if -help display help and exit
   call help_version(lget('mkdir_version'))                                   ! if -version display help and exit
   filenames=sgets('mkdir_oo')                                                ! get directory names
   verbose=lget('mkdir_verbose')                                              ! get -verbose switch
   parents=lget('mkdir_parents')                                              ! get -parent switch

   do i=1,size(filenames)
      fname=trim(filenames(i))
      if(parents)then                                                         ! build an array of all the directory names
         above=[character(len=4096) :: fname ]
         scratch=fname
         do
            scratch=dirname(scratch)                         ! remove leaf from pathname and repeat till pathname leafs are consumed
            if(scratch.eq.'')exit
            if(scratch.eq.'.')exit
            above=[above,scratch]
         enddo
         do j=size(above),1,-1                                                ! start with shortest name and do a mkdir on them all
            ierr = system_mkdir(above(j), IANY([W_USR, R_USR, X_USR]))        ! make directory
            if(verbose)then
               if(ierr.ne.0)then
                  call system_perror('*_mkdir*')
               else
                  write(*,'(a,a)')'_*mkdir* : created directory ', trim(above(j))
               endif
            endif
         enddo
      else                                                                    ! no -parent option
         ierr = system_mkdir(fname, IANY([W_USR, R_USR, X_USR]))              ! make directory
         if(ierr.ne.0)then
            call system_perror('*_mkdir*:'//fname)
         elseif(verbose)then
            write(*,'(a,a)')'_*mkdir* : created directory ',fname
         endif
      endif
   enddo
end program demo_system_mkdir
