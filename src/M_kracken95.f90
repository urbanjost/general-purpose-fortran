!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! The M_KRACKEN95(3f) interface file is free and unencumbered software released into the public domain.
! For further details see the file UNLICENSE.txt or refer to <http;//unlicense.org/>
!
! Even so, I ask that you send me interesting alterations that are available for public use; and
! that you include a note in the source acknowledging the original author (1989,1996,2013 John S. Urban).
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_kracken95
implicit none
! "@(#)M_kracken95(3f,module):parse command line options of Fortran programs using Unix-like syntax"
!===================================================================================================================================
   private
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: kracken                ! define command and default parameter values from command arguments
   public :: setprompts             ! define prompts for commands in interactive mode
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rget                   ! fetch real    value of name VERB_NAME from the language dictionary
   public :: dget                   ! fetch double  value of name VERB_NAME from the language dictionary
   public :: iget                   ! fetch integer value of name VERB_NAME from the language dictionary
   public :: lget                   ! fetch logical value of name VERB_NAME from the language dictionary
   public :: sget                   ! fetch string  value of name VERB_NAME from the language dictionary.
   public :: sgetl                  ! fetch string  value of name VERB_NAME from the language dictionary.
   public :: retrev                 ! retrieve token value as string from Language Dictionary when given NAME
!-!   public :: retrev_string_variable_length
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: delim                  ! parse a string and store tokens into an array
   public :: string_to_real         ! returns real value from numeric character string NOT USING CALCULATOR
   public :: string_to_dble         ! returns double precision value from numeric character string NOT USING CALCULATOR
!-----------------------------------------------------------------------------------------------------------------------------------
   public  :: dissect               ! for user-defined commands: define defaults, then process user input
   private :: parse                 ! parse user command and store tokens into Language Dictionary
   private :: store                 ! replace dictionary name's value (if allow=add add name if necessary)
   private :: bounce                ! find location (index) in Language Dictionary where VARNAM can be found
   private :: add_string            ! Add new string name to Language Library dictionary
   private :: send_message
   private :: get_command_arguments ! get_command_arguments: return all command arguments as a string
   private :: igets                 ! return the subscript value of a string when given it's name
   private :: uppers                ! return copy of string converted to uppercase
   private :: menu                  ! generate an interactive menu when -? option is used
!-----------------------------------------------------------------------------------------------------------------------------------
   public  :: kracken_comment
!-----------------------------------------------------------------------------------------------------------------------------------
! length of verbs and entries in Language dictionary
! NOTE:   many parameters were reduced in size so as to just accommodate being used as a command line parser.
!         In particular, some might want to change:
   integer, parameter,public :: IPic=100                           ! number of entries in language dictionary
   integer, parameter,public :: IPvalue=4096                       ! length of keyword value
   integer, parameter,public :: IPcmd=32768                        ! length of command
   integer, parameter,public :: IPverb=20                          ! length of verb
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter        :: dp = kind(0.d0)
   integer, parameter        :: k_int = SELECTED_INT_KIND(9)       ! integer*4
   integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
!-----------------------------------------------------------------------------------------------------------------------------------
   ! dictionary for Language routines
   character(len=IPvalue),dimension(IPic)     :: values=" "        ! contains the values of string variables
   character(len=IPverb),dimension(IPic)      :: dict_verbs=" "    ! string variable names
   integer(kind=k_int),dimension(IPic)        :: ivalue=0          ! significant lengths of string variable values
   character(len=1),save                      :: kracken_comment='#'
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine retrev(name,val,llen,ier)
! "@(#)retrev(3f): retrieve token value from Language Dictionary when given NAME"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)  :: name        ! name of variable to retrieve value for in form VERB_NAME
      character(len=*),intent(out) :: val         ! value for requested variable
      integer,intent(out)          :: llen        ! position of last non-blank character in requested variable
      integer,intent(out)          :: ier         ! error flag 0=found requested variable; -1=entry not found
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                      :: isub        ! subscript in dictionary where requested entry and corresponding value are found
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                            ! get index entry is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                            ! entry was in dictionary
         val=values(isub)                         ! retrieve corresponding value for requested entry
         llen=ivalue(isub)                        ! get significant length of value
         ier=0                                    ! indicate requested entry name was successfully found
      else                                        ! entry was not in dictionary
         val=" "                                  ! set value to blank
         llen=0                                   ! set length to zero
         ier=-1                                   ! set error flag to indicate requested entry was not found
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine retrev
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine string_to_dble(chars,value8,ierr)
! "@(#)string_to_dble(3f): returns double precision value from numeric character string"
                                                  !      works with any g-format input, including integer, real, and exponential.
      character(len=*),intent(in)  :: chars       ! string assumed to represent a numeric value
      real(kind=k_dbl),intent(out) :: value8      ! double precision value to return; set to zero on error.
      integer,intent(out)          :: ierr        ! if an error occurs in the read, a non-zero value is returned.
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=13)            :: frmt        ! FORMAT to use to read the value from the string
      integer                      :: ios         ! error flag returned from internal READ
!-----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                                     ! initialize the error flag
!-----------------------------------------------------------------------------------------------------------------------------------
      write(unit=frmt,fmt="( ""(bn,g"",i5,"".0)"" )")len(chars)  ! build FORMAT to read the value based on length of input string
      read(unit=chars,fmt=frmt,iostat=ios)value8                 ! read the value from the string using an internal read
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ios /= 0 )then                                          ! if an error occurred in reading from the string report it
         value8=0.0_k_dbl                                        ! set the returned value to zero on error
         call send_message("*string_to_dble* - cannot produce number from this string["//trim(chars)//"]")
         ierr=ios
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine string_to_dble
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine string_to_real(chars,valu,ierr)
! "@(#)string_to_real(3f): returns real value from numeric character string"
      character(len=*),intent(in)  :: chars
      real,intent(out)             :: valu
      integer,intent(out)          :: ierr
      real(kind=k_dbl)             :: valu8
!-----------------------------------------------------------------------------------------------------------------------------------
      call string_to_dble(chars,valu8,ierr)         ! get value as double precision and stuff into a real variable
      valu=real(valu8)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine string_to_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function dget(keyword)
! "@(#)dget(3f): given keyword fetch value from Language Dictionary as a dble (zero on error)"
   real(kind=dp)               :: dget              ! function type
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in) :: keyword           ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)      :: value             ! value returned
   integer                     :: llen              ! length of value found
   integer                     :: ier               ! error flag on call to retrieve value
   real(kind=dp)               :: a8                ! number to return
!-----------------------------------------------------------------------------------------------------------------------------------
   value=" "                                        ! initialize value found for keyword in case an error occurs
   call retrev(keyword, value, llen, ier)           ! find value associated with keyword
   call string_to_dble(value(:llen), a8, ier)       ! convert the string to a numeric value
   dget = a8
!-----------------------------------------------------------------------------------------------------------------------------------
end function dget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function rget(keyword)
! "@(#)rget(3f): given keyword, fetch single real value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   real                        :: rget             ! function type
   character(len=*),intent(in) :: keyword          ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   rget=real(dget(keyword))                        ! just call DGET(3f) but change returned value to type REAL
!-----------------------------------------------------------------------------------------------------------------------------------
end function rget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(keyword)
! "@(#)iget(3f): given keyword, fetch integer value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: iget            ! function type
   character(len=*),intent(in)  :: keyword         ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   iget = int(dget(keyword))                       ! just call DGET(3f) but change returned value to type INTEGER
!-----------------------------------------------------------------------------------------------------------------------------------
end function iget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lget(keyword)
! "@(#)lget(3f): given keyword, fetch logical value from language dictionary (zero on error)"
!-----------------------------------------------------------------------------------------------------------------------------------
   logical                      :: lget            ! procedure type
   character(len=*),intent(in)  :: keyword         ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)       :: value           ! value corresponding to the requested keyword
   integer                      :: llen            ! length of VALUE returned by RETREV(3f)
   integer                      :: ier             ! flag returned by RETREV(3f) indicating if an error occurred in retrieving value
!-----------------------------------------------------------------------------------------------------------------------------------
   lget=.false.                                    ! initialize return value to .false.
   call retrev(keyword, value, llen, ier)          ! get value for corresponding keyword from language dictionary
                                                   ! report on error ????
   value=adjustl(uppers(value,llen))               ! convert value to uppercase, left spaces trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   if(value(:llen).ne."#N#")then
      select case(value(1:1))                      ! check first letter
      case('T','Y',' ')                            ! anything starting with "T" or "Y" or a blank is TRUE (true,t,yes,y,...)
         lget=.true.
      case('F','N')                                ! assume this is false or no
         lget=.false.
      case('.')                                    ! looking for fortran logical syntax .STRING.
         select case(value(2:2))
         case('T')                                 ! assume this is .t. or .true.
            lget=.true.
         case('F')                                 ! assume this is .f. or .false.
            lget=.false.
         case default
            call send_message("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value(:llen))
         end select
      case default
            call send_message("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value(:llen))
      end select
   else                                            ! special value "#N#" is assumed FALSE
      lget=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
character(len=IPvalue) function sget(name,iilen) result(string)
! "@(#)sget(3f): Fetch string value and length of specified NAME the language dictionary"
!     This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
      character(len=*),intent(in)   :: name     !  name to look up in dictionary
      integer,intent(out),optional  :: iilen    !  length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                       :: isub     ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                          ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                          ! if index is valid return string
         string=values(isub)
      else                                      ! if index is not valid return blank string
         string(:)=" "
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(present(iilen))then                    ! if iilen is present on call, return the value
         iilen=ivalue(isub)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function sgetl(name,iilen) result(string)
! "@(#)sgetl(3f): Fetch string value for NAME from language dictionary up to length iilen"
!     This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
      character(len=*),intent(in)  :: name        ! name to look up in dictionary
      integer,intent(in)           :: iilen       ! length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=iilen)          :: string
      integer                      :: isub
!-----------------------------------------------------------------------------------------------------------------------------------
      isub=igets(name)                            ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
      if(isub > 0)then                            ! if index is valid return string
         string=values(isub)
      else                                        ! if index is not valid return blank string
         string(:)=" "
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sgetl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine kracken(verb,string,error_return)
! "@(#)kracken(3f): define and parse command line options"
!     get the entire command line argument list and pass it and the
!     prototype to dissect()
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)    :: string
      character(len=*),intent(in)    :: verb
      integer,intent(out),optional   :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPcmd)           :: command
      integer                        :: iilen
      integer                        :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
      if(present(error_return))error_return=0
!-----------------------------------------------------------------------------------------------------------------------------------
      call get_command_arguments(command,iilen,ier)
      if(ier.ne.0)then
         call send_message("*kracken* could not get command line arguments")
         if(present(error_return))error_return=ier
      else
         call dissect(verb,string,command(:iilen),iilen,ier)
         ! if calling procedure is not testing error flag stop program on error
         if(.not.present(error_return).and.ier.ne.0)then
            call send_message("*kracken* (V 20151212) STOPPING: error parsing arguments")
            stop
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine setprompts(verb,init)
! "@(#)setprompts(3f): set explicit prompts for keywords in interactive mode"
      character(len=*),intent(in)  :: verb   ! verb name to define prompts for
      character(len=*),intent(in)  :: init   ! string to define prompts instead of values
      call parse('?'//trim(verb),init,"add") ! initialize command, prefixing verb with question mark character to designate prompts
end subroutine setprompts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect(verb,init,pars,ipars,error_return)
! "@(#)dissect(3f): convenient call to parse() -- define defaults, then process"
!
      character(len=*),intent(in)  :: verb            ! the name of the command to be reset/defined  and then set
      character(len=*),intent(in)  :: init            ! used to define or reset command options; usually hard-set in the program.
      character(len=*),intent(in)  :: pars            ! defines the command options to be set, usually from a user input file
      integer,intent(in)           :: ipars           ! length of the user-input string pars.
      integer,intent(out),optional :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                      :: ier
      character(len=IPvalue)       :: varvalue        ! value of environment variable
      integer                      :: ipars2
!-----------------------------------------------------------------------------------------------------------------------------------
      call store(trim(verb)//'_?','.false.',"add",ier)  ! all commands have the option -? to invoke prompt mode
      call parse(trim(verb),init,"add") ! initialize command
!-----------------------------------------------------------------------------------------------------------------------------------
      ! if environment variable DEFAULT_verbname is set apply it as defaults to define _verb values
      ! for programs that want to determine the values set by the command definition and the variable
      ! before user selections are applied
      call parse('_'//trim(verb),init,"add") ! initialize _command
      call get_environment_variable('DEFAULT_'//trim(verb),varvalue)
      call parse('_'//trim(verb),trim(varvalue),"no_add") ! process and store as _CMD_VERB for appending
!-----------------------------------------------------------------------------------------------------------------------------------
      if(varvalue.ne.' ')then
         call parse(verb,trim(varvalue),"no_add")            ! process environment variable
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ipars <= 0)then
         ipars2=len(pars(:ipars))
      else
         ipars2=ipars
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call parse(verb,pars(:ipars2),"no_add",ier) ! process user command options
      if(lget(trim(verb)//'_?'))then    ! if -? option was present prompt for values
         call menu(verb)
      endif
      if(present(error_return))error_return=ier
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine dissect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parse(verb,string,allow,error_return)
! "@(#)parse(3f,private): parse user command and store tokens into Language Dictionary"
!!!   set up odd for future expansion
!!!   need to handle a minus followed by a blank character
!-----------------------------------------------------------------------------------------------------------------------------------
!     given a string of form
!         verb  -keyword1 value1 -keyword2 value2 ...
!     define three arrays of the form
!     verb_keyword(i) : value(i)  : len_trim(value(i))
!     -keyword(i) will become verb__keyword(i)
!
!     values may be in double quotes.
!     if tokens contain alphameric characters an unquoted # signifies the rest of the line is a comment.
!     adjacent double quotes put one double quote into value
!     processing ends when an end of string is encountered
!     the variable name for the first value is verb_oo
!     call it once to give defaults
!     leading and trailing blanks are removed from values
!
!-----------------------------------------------------------------------------------------------------------------------------------
! @(#)parse+ for left-over command string for Language routines
!     optionally needed if you are going to allow multiple commands on a line
      ! number of characters left over,
      ! number of non-blank characters in actual parameter list
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)          :: verb
      character(len=*),intent(in)          :: string
      character(len=*),intent(in)          :: allow
      character(len=IPvalue+2)             :: dummy
      character(len=IPvalue),dimension(2)  :: var
      character(len=3)                     :: delmt
      character(len=2)                     :: init
      character(len=1)                     :: currnt
      character(len=1)                     :: prev
      character(len=1)                     :: forwrd
      character(len=IPvalue)               :: val
      character(len=IPverb)                :: name
      integer,dimension(2)                 :: ipnt
      integer                              :: ilist
      integer                              :: ier
      integer,optional,intent(out)         :: error_return
      integer                              :: islen
      integer                              :: ipln
      integer                              :: ipoint
      integer                              :: itype
      integer                              :: ifwd
      integer                              :: ibegin
      integer                              :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
      ilist=1
      init="oo"
      ier=0
      if(present(error_return)) error_return=0
      islen=len_trim(string)   ! find number of characters in input string
      ! if input string is blank, even default variable will not be changed
      if(islen  ==  0)then
         return
      endif
      dummy=string             ! working mutable copy of STRING
      ipln=len_trim(verb)      ! find number of characters in verb prefix string
!-----------------------------------------------------------------------------------------------------------------------------------
      var(2)=init         ! initial variable name
      var(1)=" "          ! initial value of a string
      ipoint=0            ! ipoint is the current character pointer for (dummy)
      ipnt(2)=2           ! pointer to position in parameter name
      ipnt(1)=1           ! pointer to position in parameter value
      itype=1             ! itype=1 for value, itype=2 for variable
!-----------------------------------------------------------------------------------------------------------------------------------
      delmt="off"
      prev=" "
!-----------------------------------------------------------------------------------------------------------------------------------
      do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)
!-----------------------------------------------------------------------------------------------------------------------------------
      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
      ! beginning of a parameter name
         if(forwrd.eq.'-')then         ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1            ! ignore second - instead
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(var(1)(:ipnt(1)-1))
            do
               if(iend  ==  0)then   !len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               else if(var(1)(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=var(1)(ibegin:iend)
            if(var(2)(:ipnt(2)).eq.'oo'.and.allow.ne.'add'.and.val.eq.'')then
               ! do not allow a blank value to override initial value so can have default
            else
               call store(name,val,allow,ier)       ! store name and it's value
            endif
            if(present(error_return).and.ier.ne.0)error_return=ier
         else
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=" "                                 ! store name and null value
            call store(name,val,allow,ier)
            if(present(error_return).and.ier.ne.0)error_return=ier
         endif
         ilist=ilist+ipln+1+ipnt(2)
         ilist=ilist+1
         itype=2                          ! change to filling a variable name
         var(1)=" "                       ! clear value for this variable
         var(2)=" "                       ! clear variable name
         ipnt(1)=1                        ! restart variable value
         ipnt(2)=1                        ! restart variable name
!-----------------------------------------------------------------------------------------------------------------------------------
      elseif(currnt == kracken_comment.and.delmt == "off")then   ! rest of line is comment
         islen=ipoint
         dummy=" "
         prev=" "
         cycle
!-----------------------------------------------------------------------------------------------------------------------------------
      ! rest of line is another command(s)
         islen=ipoint
         dummy=" "
         prev=" "
         cycle
!-----------------------------------------------------------------------------------------------------------------------------------
      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
         ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
                var(itype)(ipnt(itype):ipnt(itype))=currnt
                ipnt(itype)=ipnt(itype)+1
                delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
                delmt="off"
            else
                delmt="on"
            endif
         else     ! add character to current parameter name or parameter value
            var(itype)(ipnt(itype):ipnt(itype))=currnt
            ipnt(itype)=ipnt(itype)+1
            if(currnt /= " ")then
            endif
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      prev=currnt
      if(ipoint <= islen)then
         cycle
      endif
      exit
      enddo
end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine store(name1,value1,allow1,ier)
! "@(#)store(3f,private): replace dictionary name's value (if allow='add' add name if necessary)"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)        :: name1       ! name in dictionary of from VERB_KEYWORD
      character(len=*),intent(in)        :: value1      ! value to be associated to NAME1
      character(len=*),intent(in)        :: allow1      ! flag to allow new VERB_KEYWORD name being added
      integer,intent(out)                :: ier         ! flag if error occurs in adding or setting value
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPverb)              :: name
      integer                            :: indx
      character(len=10)                  :: allow
      character(len=IPvalue)             :: value
      character(len=IPvalue)             :: mssge       !  the  message/error/string  value
      integer                            :: nnlen
      integer                            :: new
      integer                            :: ii
      integer                            :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
      value=" "
      name=" "
      allow=" "
      name=name1                                        ! store into a standard size variable for this type
      value=value1                                      ! store into a standard size variable for this type
      allow=allow1                                      ! store into a standard size variable for this type
      nnlen=len(name1)
!-----------------------------------------------------------------------------------------------------------------------------------
      call bounce(name,indx,dict_verbs,ier,mssge)       ! determine storage placement of the variable and whether it is new
      if(ier  ==  -1)then                               ! an error occurred in determining the storage location
         call send_message("error occurred in *store*")
         call send_message(mssge)
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(indx > 0)then                                  ! found the variable name
         new=1
      else if(indx <= 0.and.allow  ==  "add")then       ! check if the name needs added
         call add_string(name,nnlen,indx,ier)           ! adding the new variable name in the variable name array
         if(ier  ==  -1)then
            call send_message("*store* could not add "//name(:nnlen))
            call send_message(mssge)
            return
         endif
         new=0
!-----------------------------------------------------------------------------------------------------------------------------------
      else                                              ! did not find variable name but not allowed to add it
         ii=index(name,"_")
         call send_message("########################################################")
         call send_message("error: UNKNOWN OPTION -"//name(ii+1:))
         if(ii > 0)then
            call send_message(name(:ii-1)//" parameters are")
            do i10=1,IPic
               if(name(:ii)  ==  dict_verbs(i10)(:ii))then
                  if(dict_verbs(i10)(ii:ii+1).eq.'__')then
                     call send_message(" --"//dict_verbs(i10)(ii+2:len_trim(dict_verbs(i10)))//" "//values(i10)(:ivalue(i10)))
                  else
                     call send_message(" -"//dict_verbs(i10)(ii+1:len_trim(dict_verbs(i10)))//" "//values(i10)(:ivalue(i10)))
                  endif
               endif
            enddo
         endif
         call send_message("########################################################")
         ier=-10
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      values(iabs(indx))=value               ! store a defined variable's value
      ivalue(iabs(indx))=len_trim(value)     ! store length of string
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine bounce(varnam,indx,dictionary,ier,mssge)
! "@(#)bounce(3f,private): find index in Language Dictionary where VARNAM can be found"
!
!     If VARNAM is not found report where it should be placed as a NEGATIVE index number.
!     Assuming DICTIONARY is an alphabetized array
!     Assuming all variable names are lexically greater than a blank string.
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)                :: varnam      ! variable name to look up in dictionary
      integer,intent(out)                        :: indx        ! location where variable is or should be
      character(len=*),dimension(:),intent(in)   :: dictionary  ! sorted dictionary array to find varnam in
      integer,intent(out)                        :: ier
      character(len=*),intent(out)               :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                                    :: maxtry      ! maximum number of tries that should be required
      integer                                    :: imin
      integer                                    :: imax
      integer                                    :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
      maxtry=int(log(float(IPic))/log(2.0)+1.0)                 ! calculate max number of tries required to find a conforming name
      indx=(IPic+1)/2
      imin=1
      imax=IPic
!-----------------------------------------------------------------------------------------------------------------------------------
      do i10=1,maxtry
         if(varnam  ==  dictionary(indx))then
            return
         else if(varnam > dictionary(indx))then
            imax=indx-1
         else
            imin=indx+1
         endif
         if(imin > imax)then
            indx=-imin
            if(iabs(indx) > IPic)then
               mssge="error 03 in bounce"
               ier=-1
               return
            endif
            return
         endif
         indx=(imax+imin)/2
         if(indx > IPic.or.indx <= 0)then
            mssge="error 01 in bounce"
            ier=-1
            return
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      mssge="error 02 in bounce"
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine bounce
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine add_string(newnam,nchars,indx,ier)
! "@(#)add_string(3f,private): Add new string name to Language Library dictionary"
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      character(len=*),intent(in)       :: newnam     ! new variable name to add to dictionary
      integer,intent(in)                :: nchars     ! number of characters in NEWNAM
      integer,intent(in)                :: indx
      integer,intent(out)               :: ier
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      integer                           :: istart
      integer                           :: i10
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      istart=iabs(indx)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!     if last position in the name array has already been used, then report no room is left and set error flag and error message.
      if(dict_verbs(IPic) /= " ")then                 ! check if dictionary full
         call send_message("*add_string* no room left to add more string variable names")
         ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(istart.gt.IPic)then
         call send_message("*add_string* dictionary size exceeded")
         ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      else
         do i10=IPic-1,istart,-1
!           pull down the array to make room for new value
            values(i10+1)=values(i10)
            ivalue(i10+1)=ivalue(i10)
            dict_verbs(i10+1)=dict_verbs(i10)
         enddo
         values(istart)=" "
         ivalue(istart)= 0
         dict_verbs(istart)=newnam(1:nchars)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine add_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function igets(chars0)
! "@(#)igets(3f,private): return the subscript value of a string when given it's name"
!     WARNING: only request value of names known to exist
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)        :: chars0
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=IPverb)              :: chars
      character(len=IPvalue)             :: mssge
      integer                            :: ierr
      integer                            :: indx
      integer                            :: igets
!-----------------------------------------------------------------------------------------------------------------------------------
      chars=chars0
      ierr=0
      indx=0
      call bounce(chars,indx,dict_verbs,ierr,mssge)             ! look up position
!-----------------------------------------------------------------------------------------------------------------------------------
      if((ierr  ==  -1).or.(indx <= 0))then
         call send_message("*igets* variable "//trim(chars)//" undefined")
         igets=-1                                                ! very unfriendly subscript value
      else
         igets=indx
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function igets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine delim(line0,array,n,iicount,ibegin,iterm,iilen,dlim)
! "@(#)delim(3f): parse a string and store tokens into an array"
!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) = '#N#' do not store into string array  (KLUDGE))
!
!     also icount number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
      character(len=*),intent(in)                 :: line0
      integer,intent(in)                          :: n
      !character(len=*),dimension(n),intent(out)  :: array
      character(len=*),dimension(:),intent(out)   :: array
      integer,intent(out)                         :: iicount
      !integer,dimension(n),intent(out)           :: ibegin
      integer,dimension(:),intent(out)            :: ibegin
      !integer,dimension(n),intent(out)           :: iterm
      integer,dimension(:),intent(out)            :: iterm
      integer,intent(out)                         :: iilen
      character(len=*),intent(in)                 :: dlim
      character(len=IPcmd)                        :: line
      logical                                     :: lstore
      integer                                     :: idlim
      integer                                     :: icol
      integer                                     :: iarray
      integer                                     :: istart
      integer                                     :: iend
      integer                                     :: i10
      integer                                     :: ifound
      iicount=0
      iilen=len_trim(line0)
      if(iilen > IPcmd)then
         call send_message("*delim* input line too long")
      endif
      line=line0
      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)                                ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim  ==  0)then
            idlim=1  ! blank string
         endif
      endif
!     command was totally blank
      if(iilen  ==  0)then
         return
      endif
!     there is at least one non-blank character in the command
!     iilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1
      if(array(1)  ==  "#N#")then                            ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif
      do iarray=1,n,1                                        ! store into each array element until done or too many words
         if(index(dlim(1:idlim),line(icol:icol))  ==  0)then ! if current character is not a delimiter
           istart=icol                                       ! start new token on the non-delimiter character
           ibegin(iarray)=icol
           iend=iilen-istart+1+1                             ! assume no delimiters so put past end of line
           do i10=1,idlim
              ifound=index(line(istart:iilen),dlim(i10:i10))
              if(ifound > 0)then
                iend=min(iend,ifound)
              endif
           enddo
            if(iend <= 0)then                                ! no remaining delimiters
              iterm(iarray)=iilen
              if(lstore)then
                 array(iarray)=line(istart:iilen)
              endif
              iicount=iarray
              return
            else
              iend=iend+istart-2
              iterm(iarray)=iend
              if(lstore)then
                 array(iarray)=line(istart:iend)
              endif
            endif
           icol=iend+2
         else
           icol=icol+1
           cycle
         endif
   !     last character in line was a delimiter, so no text left
   !     (should not happen where blank=delimiter)
         if(icol > iilen)then
           iicount=iarray
           return
         endif
      enddo
!     more than n elements
      iicount=n
end subroutine delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine send_message(msg)
! "@(#)send_message(3f,private): general message routine"
      character(len=*),intent(in) :: msg                      ! message to display
      print "(""# "",a)", msg(:len_trim(msg))               ! write message
end subroutine send_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine get_command_arguments(string,istring_len,istatus)
! "@(#)get_command_arguments(3f,private): return all command arguments as a string"
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(out) :: string                     ! string of all arguments to create
   integer,intent(out)          :: istring_len                ! last character position set in output string
   integer,intent(out)          :: istatus                    ! status (non-zero means error)
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: istring_len_new            ! length of output if append next argument
   integer                      :: string_len                 ! allowed length of output string
   integer                      :: ilength                    ! length of individual arguments
   integer                      :: i                          ! loop count
   character(len=IPvalue)       :: value                      ! store individual arguments one at a time
   integer                      :: ifoundspace
!-----------------------------------------------------------------------------------------------------------------------------------
   string=""                                                  ! initialize returned output string
   string_len=len(string)                                     ! find out how big the output string can be
   istring_len=0                                              ! initialize returned output string length
   istatus=0                                                  ! initialize returned error code
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,command_argument_count()                            ! append any arguments together
      call get_command_argument(i,value,ilength,istatus)      ! get next argument
      istring_len_new=istring_len+ilength+1                   ! calculate total string length plus one for a separator
      !---------------------
      ! BEGIN GUESS AT RE-QUOTING STRING
      !---------------------
      ! if argument contains a space and does not contain a double-quote and is short enough to have double quotes added
      ! assume this argument was quoted but that the shell stripped the quotes and add double quotes. This is an optional
      ! behavior and assumes an operating system that strips the quotes from quoted strings on the command line. If the
      ! operating system is smarter than that remove this section
      if(ilength.gt.0)then
         ifoundspace=index(value(:ilength),' ')
         if(index(value(:ilength),' ').ne.0.and.index(value(:ilength),'"').eq.0)then
            ilength=ilength+2
            if(ilength.le.len(value))then
               value='"'//value(:ilength)//'"'
            endif
         endif
      endif
      !---------------------
      ! END GUESS AT RE-QUOTING STRING
      !---------------------
      if(ilength.gt.len(value))then
         call send_message('*get_command_arguments* argument too big')
         stop
      elseif(istatus /= 0) then                               ! stop appending on error
         call send_message('*get_command_arguments* error obtaining argument')
         stop
      elseif(istring_len_new.gt.string_len)then               ! not enough room to store argument
         call send_message('*get_command_arguments* output too long, command trimmed')
         stop
      endif
      string=string(:istring_len)//value(:ilength)            ! append strings together
      istring_len=istring_len_new
   enddo
   istring_len=len_trim(string)                               ! keep track of length and so do not need to use len_trim
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine get_command_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppers(input_string,output_size) result (output_string)
! "@(#)uppers(3f,private): return copy of input string converted to uppercase"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in) :: input_string         ! input string to convert to uppercase
      integer,intent(in)          :: output_size          ! size of output string
      character(len=output_size)  :: output_string        ! output string converted to uppercase
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=1)            :: letter               ! current letter
      integer                     :: ilet                 ! ADE (ASCII Decimal Equivalent) of current letter
      integer                     :: icount               ! counter used to increment thru the input string
!-----------------------------------------------------------------------------------------------------------------------------------
      if(len_trim(input_string).gt.output_size)then       ! warn that length of input longer than length of output
         call send_message("*uppers* - input string longer than output string")
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      output_string=" "                                   ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
      do icount=1,min(output_size,len_trim(input_string)) ! loop thru input one character at a time up to length of output string
         letter=input_string(icount:icount)               ! extract next letter
         ilet=ichar(letter)                               ! get integer ADE (ASCII Decimal Equivalent) of letter
                                                          ! NOTE: lowercase a-z in ASCII is an ADE of 97 to 122
                                                          !       uppercase A-Z in ASCII is an ADE of 65 to 90
         if((ilet >= 97) .and.(ilet <= 122))then          ! find if current letter is a lowercase letter
            output_string(icount:icount)=char(ilet-32)    ! convert lowercase a-z to uppercase A-Z and store into output string
         else                                             ! character is not a lowercase letter, just put it in output
            output_string(icount:icount)=letter           ! store character as-is
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function uppers
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine menu(verb)
! "@(#)menu(3f,private): prompt for values using a menu interface"
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: verb
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)         :: reply
   character(len=IPvalue)         :: prompt
   integer                        :: ii
   integer                        :: icount
   integer                        :: ios
   integer                        :: i10
   integer                        :: i20
   integer                        :: istart
   integer                        :: iend
   integer                        :: ifound
   integer                        :: ireply
   real                           :: valu
   integer                        :: ierr
   integer                        :: indx
   character(len=IPvalue)         :: mssge   !  the message/error/string  value returned by BOUNCE(3f)
!-----------------------------------------------------------------------------------------------------------------------------------
   ii=len_trim(verb)
   write(*,*)verb(:ii)//" parameters are"
   istart=1
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do
      icount=0                                                ! how many entries in the dictionary belong to this command
      iend=IPic                                               ! last dictionary entry to search for current command
      MAKEMENU: do i10=istart,iend                            ! search dictionary for keywords for current command
         if(verb(:ii)//'_'  ==  dict_verbs(i10)(:ii+1))then   ! found part of the desired command
            if(istart.eq.0)istart=i10                         ! store index to the beginning of this command
            icount=icount+1                                   ! count keywords that start with VERB_
            if(dict_verbs(i10).eq.verb(:ii)//'_?')then        ! do not show the keyword VERB_?
               cycle MAKEMENU
            endif
            call bounce('?'//dict_verbs(i10),indx,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
            if(indx.gt.0)then
               prompt=values(indx)
            else
               prompt=' '
            endif
            if(prompt.eq.'')then
               write(*,'(i4,")",a,a)') i10,dict_verbs(i10)(ii+2:),trim(values(i10)(:ivalue(i10)))
            elseif(prompt.eq.'#N#')then                       ! special prompt value which means to skip prompting
            else
               write(*,'(i4,")",a,":[",a,"]")') i10,trim(prompt),trim(values(i10))
            endif
         endif
      enddo MAKEMENU
      iend=icount+istart-1                                 ! no need to go thru entire dictionary on subsequent passes
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(a)',advance='no')'Enter number of parameter to change(0 to finish):'
      read(*,'(a)',iostat=ios)reply
      reply=adjustl(reply)
      valu=-1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(reply(1:1).eq.'-')then            ! assume this is the beginning of a respecification of options using -keyword value ...
         call parse(verb,trim(reply)//' -? .false.',"no_add")
         cycle INFINITE
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(REPLY)
!-----------------------------------------------------------------------------------------------------------------------------------
      case('@DUMP')                                   ! debug option to dump dictionary
         do i20=1,IPic
            if(dict_verbs(i20).ne.' ')then
                 write(*,'(a,a,a)')i20,dict_verbs(i20),':',trim(values(i20)(:ivalue(i20)))
            endif
         enddo
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('.','q')
         stop
         !exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('?')
         write(*,*)'--------------------------------------------------------------------------------'
         write(*,*)' Enter '
         write(*,*)'   o  NNN                   the number of the option to change the value for'
         write(*,*)'   o  "-keyword value ..."  to respecify values'
         write(*,*)'   o  ?                     display this help'
         write(*,*)'   o  .                     stop the program'
         write(*,*)''
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
         call string_to_real(reply,valu,ierr)              ! try to convert to a number
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireply=int(valu)
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ireply.eq.0)then
         exit INFINITE
      elseif((valu.lt.istart).or.(valu.gt.iend))then
         write(*,*)'illegal menu choice ',istart,'<=',valu,'<=',iend
!-----------------------------------------------------------------------------------------------------------------------------------
      else
         ifound=ireply                                     ! index into dictionary for requested keyword and value
         if(dict_verbs(ifound).eq.verb(:ii)//'_?')then     ! replaced this with FINISHED so exit
            exit INFINITE
         endif
         call bounce('?'//dict_verbs(ifound),indx,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
         if(indx.gt.0)then
            prompt=values(indx)
         else
            prompt=' '
         endif
         if(prompt.eq.'')then
            write(*,'("Enter value for ",a,":")',advance='no') trim(dict_verbs(ifound)(ii+2:))
         elseif(prompt.eq.'#N#')then                       ! special prompt value
         else
            write(*,'(a,":")',advance='no') trim(prompt)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         read(*,'(a)',iostat=ios)reply
         call store(dict_verbs(ifound),reply,"no_add",ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine menu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! YOUR COMPILER MUST SUPPORT ALLOCATABLE SCALAR CHARACTER VARIABLES BEFORE
! ACTIVATING THIS CODE.
!-----------------------------------------------------------------------------------------------------------------------------------
! From: "Felix Becker" <felix.becker@zih.tu-dresden.de>
! Subject: Variable-length string interface for "retrev".
! Date: Tuesday, May 28, 2013 11:51 AM
! 
! Hello John Urban,
! 
! using your Fortran "M_kracken95" module, I wrote a small wrapper that
! allows for "val" to be of unknown length, and that allows just getting
! the length of val without getting val and vice versa.
! 
! (using allocatable strings, and optional arguments).
! 
! Tested with gfortran version '4.8.0 20130502 (prerelease)'.
! 
! I am aware that M_kracken95 itself uses fixed length strings, and I did
! not fiddle with that; just providing a more flexible user interface on
! top of that.
! 
! Find my quick hack attached, use it if you want.
! 
!-----------------------------------------------------------------------------------------------------------------------------------
!-!subroutine retrev_string_variable_length(name,val,len,ier)
!-!  !!! @(#)retrev_string_variable_length: A wrapper for "retrev" from the module "M_kracken95":
!-!  !!!   - allows for the length of 'val' not being known before
!-!  !!!   - allows for 'val', 'len' and 'ier' being optional.
!-!  !!!
!-!  !!! If present: 'val' must be allocatable (i.e. not an "ordinary"
!-!  !!!   'character(len=[...])'-variable).
!-!  !!! If present and allocated: 'val' gets deallocated and then re-allocated.
!-!  !!!
!-!  !!! Tested with gfortran version '4.8.0 20130502 (prerelease)'.
!-!  character(len=*),intent(in)                       ::  name
!-!  character(len=:),intent(out),allocatable,OPTIONAL ::  val
!-!  integer,intent(out),OPTIONAL                      ::  llen
!-!  integer,intent(out),OPTIONAL                      ::  ier
!-!  
!-!  integer                                           ::  len_internal
!-!  integer                                           ::  ier_internal
!-!  character(llen=0)                                 ::  dummystring
!-!  
!-!  call retrev(name,dummystring,len_internal,ier_internal)
!-!  
!-!  if (present(val)) then
!-!    if (allocated(val)) then
!-!      deallocate(val)
!-!    end if
!-!    call allocate_string(int(len_internal,kind=8),val)
!-!    call retrev(name,val,len_internal,ier_internal)
!-!  end if
!-!  
!-!  if (present(llen)) then
!-!    llen = len_internal
!-!  end if
!-!  
!-!  if (present(ier)) then
!-!    ier = ier_internal
!-!  end if
!-!end subroutine retrev_string_variable_length
!-----------------------------------------------------------------------------------------------------------------------------------
!-!subroutine allocate_string(stringlength,stringvariable)
!-!  !!! For various reasons, we need a subroutine to allocate a character
!-!  !!! of llen=stringlength where stringlength is determined at runtime.
!-!  !!! @(#)allocate_string: allocate string
!-!  integer(kind=8),intent(in)                  :: stringlength
!-!  character(len=:),allocatable,intent(out)    :: stringvariable
!-!  
!-!  allocate(character(len=stringlength) :: stringvariable)
!-!
!-!end subroutine allocate_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_kracken95
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! HISTORY:
! updated 20151212
! allow cmd_oo to have default value
! requote token from the command line if it starts with - and has a space in it to make is possible to pass "-[^0-9-] values
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131224
! minor cleanup
! updated 20131214
! added preliminary setprompts and menu as routines to explore prompting modes.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131206
! added optional error flag to KRACKEN(3f). If the error flag is not present, an error will cause the program to stop instead of
! always returning to the calling procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131201
! create name CMD__NAME if --NAME is specified; so --version and --help
! are more easily used
! add dget(3f) function for returning doubleprecision values
! rename parse_two(3f) to dissect(3f) and make it public so input from sources other than
! command line arguments can be parsed easily.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131029
! read environment variable DEFAULT_CMD
!-----------------------------------------------------------------------------------------------------------------------------------
