!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_namelist(3fm) - [ARGUMENTS::M_namelist::INTRO] - define a NAMELIST
!!    in a module template to provide command line argument parsing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Usage:
!!
!!      use M_namelist, only : print_dictionary, unnamed
!!      use M_namelist, only : debug
!!      use M_namelist, only : oneline
!!
!!##DESCRIPTION
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    !program demo_M_namelist
!!    module M_namelist__arguments
!!    use M_namelist,    only : print_dictionary, oneline, unnamed
!!
!!    ! >>> CHANGE THIS
!!    ! declare and initialize a namelist. Letter_ denotes an uppercase short command keyword
!!    real              :: x=111.1, y=222.2, z=333.3
!!    real              :: point(3)=[10.0,20.0,30.0]
!!    character(len=80) :: title=" "
!!    logical           :: l=.false., l_=.false.
!!    logical           :: help=.false., version=.false., v=.false., h=.false.
!!    equivalence       (help,h),(version,v)
!!    namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
!!    ! << END OF CHANGES
!!
!!    contains
!!       subroutine get_args()
!!       integer :: iostat
!!       character(len=255) :: message ! use for I/O error messages
!!       character(len=1000) :: hold_namelist(60)
!!          hold_namelist=''
!!          write(hold_namelist,nml=args,iostat=iostat,iomsg=message)
!!          if(iostat.eq.0)then
!!             read(hold_namelist,nml=args,iostat=iostat,iomsg=message)
!!          endif
!!          if(iostat.ne.0)then
!!             write(*,'("ERROR:",i0,1x,a)')iostat, trim(message)
!!             call print_dictionary()
!!             stop 1
!!          endif
!!       end subroutine get_args
!!    end module M_namelist__arguments
!!
!!    program short
!!    use M_namelist__arguments, only : get_args, unnamed
!!    use M_namelist__arguments  ! make user variables available
!!    implicit none
!!    integer :: i
!!       call get_args()  ! crack command line options
!!       ! >> USER YOUR VARIABLES HERE. FOR EXAMPLE:
!!       write(*,*)'VALUES ARE NOW ', new_line('A'),&
!!       &'x        ',x,              new_line('A'),&
!!       &'y        ',y,              new_line('A'),&
!!       &'z        ',z,              new_line('A'),&
!!       &'point    ',point,          new_line('A'),&
!!       &'title    ',title,          new_line('A'),&
!!       &'help     ',help,'h ',h,    new_line('A'),&
!!       &'version  ',version,'v ',v, new_line('A'),&
!!       &'l        ',l,              new_line('A'),&
!!       &'l_       ',l_
!!       if(allocated(unnamed))then
!!          if(size(unnamed).gt.0)then
!!             write(*,'(a)')'UNNAMED:'
!!             write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!          endif
!!       endif
!!       !<< END OF EXAMPLE USAGE OF VARIABLES
!!    end program short
!!    !end program demo_M_namelist
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    CC0-1.0
module M_namelist
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT    ! access computing environment
use M_list,    only : insert, locate, replace, remove
use M_strings, only : isupper, lower, quote, upper
private
!===================================================================================================================================
public  :: print_dictionary
public  :: oneline
public debug
public unnamed

private :: namelist_to_dictionary
private :: update
private :: get
private :: wipe_dictionary

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
end type option
!===================================================================================================================================
character(len=:),allocatable   :: keywords(:)
character(len=:),allocatable   :: values(:)
integer,allocatable            :: counts(:)
logical,allocatable            :: present_in(:)

logical                        :: keyword_single=.true.
character(len=:),allocatable   :: passed_in
character(len=:),allocatable   :: namelist_name

character(len=:),allocatable   :: unnamed(:)
logical                        :: debug=.false.
logical                        :: return_all

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [ARGUMENTS:M_namelist] update internal dictionary given keyword and value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine update(key,val)
!!
!!    character(len=*),intent(in)           :: key
!!    character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!    Update internal dictionary in M_namelist(3fm) module.
!!##OPTIONS
!!    key  name of keyword to add, replace, or delete from dictionary
!!    val  if present add or replace value associated with keyword. If not
!!         present remove keyword entry from dictionary.
!!##RETURNS
!!##EXAMPLES
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    CC0-1.0
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val_local
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,' VAL=',val
      else
         write(stderr,*)'*update* DEBUG: KEY=',key
      endif
   endif
   if(present(val))then
      val_local=val
      ilen=len_trim(val_local)
      call locate(keywords,key,place)                   ! find where string is or should be
      if(place.lt.1)then                                ! if string was not found insert it
         call insert(keywords,key,iabs(place))
         call insert(values,val_local,iabs(place))
         call insert(counts,ilen,iabs(place))
         call insert(present_in,.true.,iabs(place))
      else
         call replace(values,val_local,place)
         call replace(counts,ilen,place)
         call replace(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate(keywords,key,place)
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(values,place)
         call remove(counts,place)
         call remove(present_in,place)
      endif
   endif
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,' VAL=',val, &
                &size(keywords),size(values),size(counts),size(present_in)
      else
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,size(keywords),size(values),size(counts),size(present_in)
      endif
      write(stderr,*)present_in
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wipe_dictionary(3fp) - [ARGUMENTS:M_namelist] reset private M_namelist(3fm) dictionary to empty
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine wipe_dictionary()
!!##DESCRIPTION
!!    reset private M_namelist(3fm) dictionary to empty
!!##EXAMPLES
!!
!!    program demo_wipe_dictionary
!!    use M_namelist, only : dictionary
!!       call wipe_dictionary()
!!    end program demo_wipe_dictionary
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    CC0-1.0
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##SYNOPSIS
!!
!!    get(3f) - [ARGUMENTS:M_namelist] get dictionary value associated with key name in private M_namelist(3fm) dictionary
!!##DESCRIPTION
!!    Get dictionary value associated with key name in private M_namelist(3fm) dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dictionary_to_namelist(nml)
character(len=:),allocatable,intent(out) :: nml
integer :: i
character(len=:),allocatable :: newkeyword
   ! build namelist string
   nml=namelist_name//' '
   if(return_all)then  ! if returning all first do keywords not present on command line so equivalences work
      do i=1,size(keywords)
         if(isupper(keywords(i)(1:1)))then
            newkeyword=trim(lower(keywords(i)))//'_'
         else
            newkeyword=trim(keywords(i))
         endif
         if(.not.present_in(i))then
            nml=nml//newkeyword//'='//trim(values(i))//' '
         endif
      enddo
   endif

   do i=1,size(keywords)  ! now only do keywords present on command line
      if(isupper(keywords(i)(1:1)))then
         newkeyword=trim(lower(keywords(i)))//'_'
      else
         newkeyword=trim(keywords(i))
      endif
      if(present_in(i))then
         nml=nml//newkeyword//'='//trim(values(i))//' '
      endif
   enddo

   nml=nml//' /'
   if(debug)then
      write(stderr,'(a)')'NAMELIST:'
      write(stderr,'(a)')nml
      if(allocated(unnamed))then
         if(size(unnamed).gt.0)then
            write(stderr,'(a)')'UNNAMED'
            write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
         endif
      endif
   endif
end subroutine dictionary_to_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   print_dictionary(3f) - [ARGUMENTS:M_namelist] print internal dictionary
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine print_dictionary(header)
!!
!!    character(len=*),intent(in),optional :: header
!!##DESCRIPTION
!!   Print the internal dictionary. This routine is intended to print the
!!   state of the argument list if an error occurs.
!!##OPTIONS
!!   HEADER  label to print before printing the state of the command
!!           argument list.
!!##EXAMPLES
!!
!!    Typical usage:
!!
!!     program demo_print_dictionary
!!     use M_namelist,  only : unnamed, print_dictionary
!!     implicit none
!!     integer                      :: i
!!     character(len=255)           :: message ! use for I/O error messages
!!     character(len=:),allocatable :: readme  ! stores updated namelist
!!     integer                      :: ios
!!     real               :: x, y, z
!!     logical            :: help, h
!!     equivalence       (help,h)
!!     namelist /args/ x,y,z,help,h
!!     character(len=*),parameter :: cmd='&ARGS X=1 Y=2 Z=3 HELP=F H=F /'
!!     ! initialize namelist from string and then update from command line
!!     readme=cmd
!!     read(readme,nml=args,iostat=ios,iomsg=message)
!!     if(ios.ne.0)then
!!        write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!        call print_dictionary('OPTIONS:')
!!        stop 1
!!     endif
!!     ! all done cracking the command line
!!     ! use the values in your program.
!!     write(*,nml=args)
!!     ! the optional unnamed values on the command line are
!!     ! accumulated in the character array "UNNAMED"
!!     if(allocated(unnamed))then
!!        if(size(unnamed).gt.0)then
!!           write(*,'(a)')'files:'
!!           write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!        endif
!!     endif
!!     end program demo_print_dictionary
!!
!!    Sample output
!!
!!    Calling the sample program with an unknown
!!    parameter produces the following:
!!
!!       $ ./print_dictionary -A
!!       UNKNOWN SHORT KEYWORD: -A
!!       KEYWORD             PRESENT  VALUE
!!       z                   F        [3]
!!       y                   F        [2]
!!       x                   F        [1]
!!       help                F        [F]
!!       h                   F        [F]
!!
!!       STOP 2
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    CC0-1.0
subroutine print_dictionary(header)
character(len=*),intent(in),optional :: header
integer          :: i
   if(present(header))then
      if(header.ne.'')then
         write(stderr,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(stderr,'(*(a,t21,a,t30,a))')'KEYWORD','PRESENT','VALUE'
         write(stderr,'(*(a,t21,l1,t30,"[",a,"]",/))')(trim(keywords(i)),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(stderr,'(a)')'UNNAMED'
         write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    namelist_to_dictionary(3f) - [ARGUMENTS:M_namelist] parse namelist string and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine namelist_to_dictionary(string)
!!
!!    character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!   must start with a keyword, any keyword that appears must have a value. A character array can have more than one delimited string
!!   unallocated and null values are not allowed
!!   set parameter name to blank
!!   find undelimited =
!!   find previous , or beginning of string. in-between is a keyword= to , that starts a keyword is a value
!!   one keyword and value are known store them
!!
!!##OPTIONS
!!    STRING   string is character input string to define command
!!
!!##EXAMPLES
!!
!!   Typical string:
!!
!!    >&ARGS
!!    > L = F,
!!    > A_="xxxxxxxxxxxxxxxxxxxxxxxxxx                                                      ",
!!    > B_="Value B                                                                         ",
!!    > P= 2*0.00000000      ,
!!    > C_=         10,         20,         30, XYZ_=(-123.000000,-456.000000),
!!    > X=  0.0000000000000000     ,
!!    > Y=  0.0000000000000000     ,
!!    > Z=  0.0000000000000000     ,
!!    > /
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    CC0-1.0
subroutine namelist_to_dictionary(string)
implicit none

! ident_1="@(#) M_namelist namelist_to_dictionary(3f) parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy       ! working copy of string
character(len=:),allocatable      :: dummy_bug   ! bug in gfortran 7.4.0 where if dummy is on LHS and used in RHS get wrong result
character(len=:),allocatable      :: keyword_value
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
logical                           :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! current character being processed
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: istart
integer                           :: iend
integer                           :: ileft
integer                           :: icut
integer                           :: i
integer                           :: iback1,iback2
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* INPUT=',trim(string)
   endif
   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   islen=islen-1                                        ! by definition last character in NAMELIST output is /
   dummy=trim(adjustl(string(:islen)))
   ! strip off namelist group name
   ileft=index(dummy,'&')
   dummy_bug=adjustl(dummy(ileft+1:))
   ileft=index(dummy_bug,' ')
   if(ileft.eq.0)then
      ileft=len(dummy_bug)
   endif
   namelist_name=upper('&'//dummy_bug(:ileft-1))
   dummy=adjustl(dummy_bug(ileft:))

   islen=len(dummy)
   dummy=dummy//'    '
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* NAMELIST_NAME=['//namelist_name//']'
      write(stderr,*)'*namelist_to_dictionary* DUMMY=['//dummy//']'
   endif


   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   delmt=.false.       ! whether in a character string or not
   prev=" "
   istart=1
   do ipoint=1,islen
      currnt=dummy(ipoint:ipoint)             ! store current character into currnt
      if(currnt=="=".and..not.delmt)then ! end of a parameter name
         keyword_value=''
         iend=0
         do i=ipoint-1,1,-1
            if(dummy(i:i).eq.' ')cycle
            ! found non-space
            iback1=index(dummy(:i),' ',back=.true.)
            iback2=index(dummy(:i),',',back=.true.)
            iend=max(iback1,iback2)
            exit
         enddo
         if(iend.ne.0)then
            call splitit()
         endif
         istart=iend+1
      elseif(currnt  ==  """")then
         if(prev  ==  """")then               ! second of a double quote, put quote in
            delmt=.not.delmt
         elseif(delmt)then
            delmt=.false.
         else
            delmt=.true.
         endif
      endif
      prev=currnt
      if(ipoint.ge.islen)then
         iend=ipoint
         call splitit()
      endif
   enddo
   if(debug)then
      call print_dictionary('NAMELIST TO DICTIONARY')
   endif
contains

subroutine splitit()
integer :: ilast
keyword_value=dummy(istart:iend)
! split keyword_value on first = and convert values to lowercase except for LETTER_ convert to uppercase LETTER and
! remove trailing , as NAMELIST output being read should not contain null values as everything in a namelist needs
! to be allocated (at least in this version of Fortran?).
   icut=index(keyword_value,'=')
   if(icut.eq.0)then
      write(stderr,*)'*splitit* INTERNAL ERROR: KEYWORD_VALUE=['//keyword_value//']'
   else
      if(debug)then
         write(stderr,*)'*splitit* KEYWORD_VALUE=['//keyword_value//']',icut
      endif
      keyword=adjustl(trim(lower(keyword_value(:icut-1))))
      if(len(keyword).eq.2)then
         if(keyword(2:2).eq.'_')then
            keyword=upper(keyword(1:1))
         endif
      endif
      if(icut.eq.len(keyword_value))then
         value=''
      else
         value=trim(adjustl(keyword_value(icut+1:)))
         ilast=len(value)
         if(ilast.eq.0)then
            value=''
         else
            if(value(ilast:ilast).eq.',')then
               value=trim(value(:ilast-1))
            endif
         endif
      endif
      call update(keyword,value)
   endif
end subroutine splitit

end subroutine namelist_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function oneline(str) result (string)

! ident_2="@(#) M_strings oneline(3f) append an array of character variables with space separator into a single CHARACTER variable"

character(len=*),intent(in)          :: str(:)
character(len=:),allocatable         :: string
integer                              :: i
character(len=1),parameter           :: sep=' '

   string=''
   do i = 1,size(str)
      string=string//trim(str(i))//sep
   enddo
end function oneline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
