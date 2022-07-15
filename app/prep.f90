!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  @(#)prep: FORTRAN preprocessor
!  Fortran preprocessor originally based on public-domain FPP preprocessor from Lahey Fortran Code Repository
!     http://www.lahey.com/code.htm
!  Extensively rewritten since under a MIT License.
!     2013-10-03,2020-12-19,2021-06-12 : John S. Urban
!
! CONSIDER
! make $OUTPUT file nestable
! allow multiple files on $INCLUDE?
! undocument $BLOCK HELP|VERSION?
! %,>>,<< operators
! replace math parsing with M_calculator (but add logical operators to M_calculator)
! cpp-like procedure macros
! cpp or fpp compatibility mode
! line control  # linenumber "file"
! modularize and modernize calculator expression, if/else/endif
!
! REMOVED $REDEFINE and no longer produce warning message if redefine a variable, more like fpp(1) and cpp(1)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_expression
use M_strings,   only : str_replace=>replace
private
public normalize_logical_operators
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine normalize_logical_operators(string)
character(len=*) :: string

   string=str_replace(string,'==','.EQ.')
   string=str_replace(string,'/=','.NE.')
   string=str_replace(string,'!=','.NE.')
   string=str_replace(string,'>=','.GE.')
   string=str_replace(string,'<=','.LE.')
   string=str_replace(string,'>','.GT.')
   string=str_replace(string,'<','.LT.')
   string=str_replace(string,'&&','.AND.')
   string=str_replace(string,'||','.OR.')
   string=str_replace(string,'!','.NOT.')
   string=str_replace(string,'.XOR.','.NEQV.')

end subroutine normalize_logical_operators
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_expression
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_fpp                                                              !@(#)M_fpp(3f): module used by prep program
USE ISO_FORTRAN_ENV, ONLY : STDERR=>ERROR_UNIT, STDOUT=>OUTPUT_UNIT,STDIN=>INPUT_UNIT
use M_io,        only : get_tmp, dirname, uniq, fileopen, filedelete, get_env  ! Fortran file I/O routines
use M_kracken95, only : sget, dissect, lget                                    ! load command argument parsing module
use M_strings,   only : nospace, v2s, substitute, upper, lower, isalpha, split, delim, str_replace=>replace, sep, atleast, unquote
use M_strings,   only : glob
use M_expression
use M_list,     only : dictionary
implicit none

logical,save                         :: G_debug=.false.

integer,parameter                    :: num=2048                       ! number of named values allowed
integer,public,parameter             :: G_line_length=4096             ! allowed length of input lines
integer,public,parameter             :: G_var_len=63                   ! allowed length of variable names

logical,public                       :: G_ident=.false.                ! whether to write IDENT as a comment or CHARACTER

character(len=G_line_length),public  :: G_source                       ! original source file line
character(len=G_line_length),public  :: G_outline                      ! message to build for writing to output

type(dictionary)                     :: table

type file_stack
   integer                           ::  unit_number
   integer                           ::  line_number=0
   character(len=G_line_length)      ::  filename
end type
type(file_stack),public              ::  G_file_dictionary(250)

type parcel_stack
   integer                           ::  unit_number
   integer                           ::  line_number=0
   character(len=G_line_length)      ::  name
end type
type(parcel_stack),public            :: G_parcel_dictionary(500)

integer,save                         :: G_line_number=0
logical,save,public                  :: G_inparcel=.false.
integer,public                       :: G_iocount=0
integer,public                       :: G_parcelcount=0
integer,public                       :: G_io_total_lines=0
integer,public                       :: G_iwidth                       ! maximum width of line to write on output unit
logical,public                       :: G_noenv=.false.                ! ignore environment variables in $IFDEF and $IFNDEF

integer,public                       :: G_iout                         ! output unit
integer,save,public                  :: G_iout_init                    ! initial output unit
!integer,public                       :: G_ihelp=stderr                 ! output unit for help text
integer,public                       :: G_ihelp=stdout                 ! output unit for help text
character(len=10),public             :: G_outtype='asis'

integer,public                       :: G_inc_count=0
character(len=G_line_length),public  :: G_inc_files(50)
character(len=:),allocatable,save    :: G_MAN
logical,save                         :: G_MAN_COLLECT=.false.
logical,save                         :: G_MAN_PRINT=.false.
character(len=:),allocatable         :: G_MAN_FILE
character(len=10)                    :: G_MAN_FILE_POSITION='ASIS      '

integer,public                       :: G_nestl=0                      ! count of if/elseif/else/endif nesting level
integer,public,parameter             :: G_nestl_max=20                 ! maximum nesting level of conditionals

logical,save,public                  :: G_verbose=.false.           ! verbose, including write strings after @(#) like what(1).
logical,save,public                  :: G_system_on=.false.            ! allow system commands or not on $SYSTEM

logical,public,save                  :: G_condop(0:G_nestl_max)        ! storage to keep track of previous write flags
data G_condop(0:G_nestl_max) /.true.,G_nestl_max*.false./
logical,public                       :: G_dc                           ! flag to determine write flag

logical,public                       :: G_write=.true.                 ! whether non-if/else/endif directives should be processed
logical,public                       :: G_llwrite=.true.               ! whether to write current line or skip it

integer,public                       :: G_comment_count=0
character(len=10),public             :: G_comment_style=' '
character(len=:),allocatable,public  :: G_comment
character(len=:),allocatable,save    :: G_scratch_file
integer,save                         :: G_scratch_lun=-1

character(len=:),allocatable,save    :: G_extract_start
character(len=:),allocatable,save    :: G_extract_stop
logical,save                         :: G_extract=.false.
logical,save                         :: G_extract_auto=.true.
logical,save                         :: G_extract_flag=.false.

character(len=:),allocatable         :: keywords(:)
character(len=:),allocatable         :: values(:)
integer,allocatable                  :: counts(:)

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cond()       !@(#)cond(3f): process conditional directive assumed to be in SOURCE '$verb...'
character(len=G_line_length) :: line                       ! directive line with leading prefix character (default is $) removed
character(len=G_line_length) :: verb                       ! first word of command converted to uppercase
character(len=G_line_length) :: options                    ! everything after first word of command till end of line or !
character(len=G_line_length) :: all_options                ! everything after first word of command till end of line or !
character(len=G_line_length) :: upopts                     ! directive line with leading prefix removed; uppercase; no spaces
logical,save                 :: eb=.false.
integer,save                 :: noelse=0
integer                      :: verblen
logical                      :: ifound

   line=adjustl(G_source(2:))                              ! remove leading prefix and spaces from directive line

   if (index(line//' ',G_comment).ne.0) then               ! assume if directive contains G_comment comment is present
                                                           ! LIMITATION: EVEN MESSAGES CANNOT CONTAIN COMMENTS
      line=line(:index(line//' ',G_comment)-1)             ! trim trailing comment from directive
   endif
   if (line(1:1).eq.G_comment)line=''
   if(line(1:4).eq.'@(#)')then
      verblen=5
   else
      verblen=scan(line,' (')
   endif
   if(verblen.eq.0)then
      verblen=len(line)
      verb=line
      options=' '
   else
      verb=line(:verblen-1)
      options=adjustl(line(verblen:))
   endif
   all_options=adjustl(G_source(verblen+1:))
   verb=upper(verb)
   upopts=nospace(upper(options))                          ! remove spaces from directive

   if(G_debug.and.G_verbose)then                           ! if processing lines in a logically selected region
      write(stderr,*)'G_SOURCE='//trim(g_source)
      write(stderr,*)'LINE='//trim(line)
      write(stderr,*)'VERB='//trim(verb)
      write(stderr,*)'OPTIONS='//trim(options)
      write(stderr,*)'ALL_OPTIONS='//trim(all_options)
      write(stderr,*)'UPOPTS='//trim(upopts)
      call flushit()
   endif

   ifound=.true.
   if(G_write)then                                                    ! if processing lines in a logically selected region

      if(G_inparcel.and.(VERB.ne.'PARCEL'.and.VERB.ne.'ENDPARCEL') )then
         call write_out(trim(G_source))                               ! write data line
         return
      endif
                                                                      ! process the directive
      select case(VERB)
      case('  ')                                                      ! entire line is a comment
      case('DEFINE','DEF','LET');     call define(upopts)             ! only process DEFINE if not skipping data lines
      case('REDEFINE','REDEF'); call define(upopts)                   ! only process DEFINE if not skipping data lines
      case('UNDEF','UNDEFINE','DELETE'); call undef(upper(options))   ! only process UNDEF if not skipping data lines
      case('OUTPUT');           call output_case(options)             ! Filenames can be case sensitive
      case('PARCEL');           call parcel_case(upopts)
      case('ENDPARCEL');        call parcel_case(' ')
      case('BLOCK');            call document(options)
      case('ENDBLOCK');         call document(' ')
      case('SET','REPLACE');    call set(options)
      case('UNSET');            call unset(upper(options))   ! only process UNSET if not skipping data lines
      case('IDENT','@(#)');     call ident(options)
      case('SHOW') ;            call show_state(upper(options),msg='')
      case('MESSAGE');          call write_err(unquote(all_options))      ! trustingly trim MESSAGE from directive
      case('QUIT');             call stop('0 '//all_options)
      case('ERROR');            call stop('1 '//all_options)
      CASE('GET_ARGUMENTS');    call write_get_arguments()
      CASE('HELP');             call short_help(stderr)
      case('STOP');                                        call stop(all_options)
      case('INCLUDE','READ');                              call include(options,50+G_iocount)    ! Filenames can be case sensitive
      case('POST','CALL','DO');                            call prepost(upper(options))
      case('IMPORT','GET_ENVIRONMENT_VARIABLE');           call import(options)
      case('SYSTEM','EXECUTE_COMMAND_LINE');               call exe()
      case default
         ifound=.false.
      end select
   endif

   select case(VERB)                                                  ! process logical flow control even if G_write is false
   case('ELSE','ELSEIF','ELIF');  call else(verb,upopts,noelse,eb)
   case('ENDIF');          call endif(noelse,eb)
   case('IF');       call if(upopts,noelse,eb)
   case('IFDEF','IFNDEF'); call def(verb,upopts,noelse,eb)
   case default
      if(.not.ifound)then
         !==========================================================================================================================
         !PROPOSAL>!==================== allow assignments  name=expression by default
         !upopts=nospace(upper(line))     ! remove spaces from directive
         !if(index(upopts,'=').gt.1.and.G_write)then  ! maybe try it as a simple expression instead; but should do it earlier
         !                                ! to behave like fortran and not have reserved words, and only if LHS is
         !                                ! an acceptable variable name
         !   call define(upopts)          ! only process DEFINE if not skipping data lines
         !else
         !   call stop_prep('*prep* ERROR(001) - UNKNOWN COMPILER DIRECTIVE ['//trim(verb)//']: '//trim(G_SOURCE))
         !endif
         !PROPOSAL<!====================
            call stop_prep('*prep* ERROR(001) - UNKNOWN COMPILER DIRECTIVE ['//trim(verb)//']: '//trim(G_SOURCE))
         !PROPOSAL!====================
         !==========================================================================================================================
      endif
   end select

end subroutine cond
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine exe()                              !@(#)exe(3f): Execute the command line specified by the string COMMAND.
character(len=G_line_length)  :: command      ! directive line with leading prefix and directive name removed
character(len=G_line_length)  :: defineme     ! scratch string for writing a DEFINE directive in to return command status
integer                       :: icmd=0
integer                       :: cstat
character(len=256)            :: sstat

   if(G_system_on)then
      command=adjustl(G_source(2:))                                                 ! remove $ from directive
      command=command(7:)                                                           ! trim SYSTEM from directive
      if(G_verbose)then
         call write_err('+ '//command)
      endif

      ! not returning command status on all platforms
      call execute_command_line (command, exitstat=icmd,cmdstat=cstat,cmdmsg=sstat) ! execute system command

      if(icmd.ne.0)then                                                             ! if system command failed exit program
         call stop_prep('*prep* ERROR(002) - SYSTEM COMMAND FAILED:'//v2s(icmd))
      endif
   else
      call stop_prep('*prep* ERROR(003) - SYSTEM DIRECTIVE ENCOUNTERED BUT NOT ENABLED:'//trim(G_SOURCE))
   endif

   write(defineme,'("CMD_STATUS=",i8)')icmd
   defineme=nospace(defineme)
   call define(defineme)

end subroutine exe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_get_arguments()                !@(#)write_get_arguments(3f): write block for processing M_CLI command line parsing
integer :: i
character(len=132),parameter :: text(*)=[character(len=132) :: &
"function get_arguments()"                                                                              ,&
"character(len=255)           :: message ! use for I/O error messages"                                  ,&
"character(len=:),allocatable :: string  ! stores command line argument"                                ,&
"integer                      :: get_arguments"                                                         ,&
"integer :: command_line_length"                                                                        ,&
"   call get_command(length=command_line_length)   ! get length needed to hold command"                 ,&
"   allocate(character(len=command_line_length) :: string)"                                             ,&
"   call get_command(string)"                                                                           ,&
"   ! trim off command name and get command line arguments"                                             ,&
"   string=adjustl(string)//' '                    ! assuming command verb does not have spaces in it"  ,&
"   string=string(index(string,' '):)"                                                                  ,&
"   string='&cmd '//string//' /'                   ! add namelist prefix and terminator"                ,&
"   read(string,nml=cmd,iostat=get_arguments,iomsg=message) ! internal read of namelist"                ,&
"   if(get_arguments.ne.0)then"                                                                         ,&
"      write(*,'(''ERROR:'',i0,1x,a)')get_arguments, trim(message)"                                       ,&
"      write(*,*)'COMMAND OPTIONS ARE'"                                                                 ,&
"      write(*,nml=cmd)"                                                                                ,&
"      stop 1"                                                                                          ,&
"   endif"                                                                                              ,&
"end function get_arguments"                                                                            ,&
"" ]
do i=1,size(text)
   write(G_iout,'(a)')trim(text(i))
enddo
end subroutine write_get_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine output_case(opts)                                  !@(#)output_case(3f): process $OUTPUT directive
character(len=*)              :: opts
character(len=G_line_length)  :: filename                     ! filename on $OUTPUT command
character(len=20)             :: position
integer                       :: ios
      call dissect2('output','-oo --append .false.',opts)     ! parse options and inline comment on input line
      filename=sget('output_oo')
      select case(filename)
      case('@')
         G_iout=stdout
      case(' ')                                               ! reset back to initial output file
         if(G_iout.ne.stdout.and.G_iout.ne.G_iout_init)then   ! do not close current output if it is stdout or default output file
            close(G_iout,iostat=ios)
         endif
         G_iout=G_iout_init
      case default
         G_iout=61
         close(G_iout,iostat=ios)
         if(lget('output_append'))then; position='append'; else; position='asis'; endif
         open(unit=G_iout,file=filename,iostat=ios,action='write',position=position)
         if(ios.ne.0)then
            call stop_prep('*prep* ERROR(004) - FAILED TO OPEN OUTPUT FILE:'//trim(filename))
         endif
      end select
   if(G_verbose)then
      call write_err( '+ OUTPUT FILE CHANGED TO: '//trim(filename) )
   endif
end subroutine output_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parcel_case(opts)                          !@(#)parcel_case(3f): process $PARCEL directive
character(len=*)              :: opts
character(len=G_line_length)  :: name                 ! name on $PARCEL command
integer                       :: ios
integer                       :: lun
character(len=256)            :: message
   call dissect2('parcel','-oo ',opts)                ! parse options and inline comment on input line
   name=sget('parcel_oo')
   if(name.eq.'')then
      G_inparcel=.false.
      G_iout=G_iout_init
   else
      open(newunit=lun,iostat=ios,action='readwrite',status='scratch',iomsg=message)
      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(005) - FAILED TO OPEN PARCEL SCRATCH FILE:'//trim(name)//' '//trim(message))
      else
         G_parcelcount=G_parcelcount+1
         G_parcel_dictionary(G_parcelcount)%name=name
         G_parcel_dictionary(G_parcelcount)%unit_number=lun
         G_inparcel=.true.
         G_iout=lun
      endif
   endif
end subroutine parcel_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine prepost(opts)                          !@(#)prepost(3f): process $POST directive
character(len=*)                          :: opts
character(len=G_line_length)              :: list
character(len=:),allocatable              :: names(:)        ! names on $POST command
character(len=:),allocatable              :: fors(:)         ! names on $POST --for 
integer                                   :: i
integer                                   :: j,jsz
   call dissect2('PARCEL','-oo --FOR ',opts)                 ! parse options and inline comment on input line
   list=sget('PARCEL_oo')
   call split(list,names,delimiters=' ,')                    ! parse string to an array parsing on delimiters
   list=sget('PARCEL_FOR')
   call split(list,fors,delimiters=' ,')                     ! parse string to an array parsing on delimiters
   jsz=size(fors)
   do i=size(names),1,-1
      if(jsz.eq.0)then
         call post(names(i))
      else
         do j=jsz,1,-1
            call post(names(i))
            call post(fors(j))
         enddo
      endif
   enddo
end subroutine prepost
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine ident(opts)                                 !@(#)ident(3f): process $IDENT directive
character(len=*)              :: opts
character(len=G_line_length)  :: lang               ! language on $IDENT command
character(len=:),allocatable  :: text
integer,save                  :: ident_count=1

   call dissect2('ident','-oo --language fortran',opts)  ! parse options and inline comment on input line
   text=trim(sget('ident_oo'))
   lang=sget('ident_language')

   select case(lang)
   case('fortran')    !x! should make look for characters not allowed in metadata, continue over multiple lines, ...
      select case(len(text))
      case(:89)
         if(G_ident)then
            write(G_iout,'("character(len=*),parameter::ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         else
            write(G_iout,'("! ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         endif
         ident_count=ident_count+1
      case(90:126)
         if(G_ident)then
            write(G_iout,'("character(len=*),parameter::ident_",i0,"=""&")')ident_count
            write(G_iout,'(''&@(#)'',a,''"'')')text
         else
            write(G_iout,'("! ident_",i0,"=""@(#)",a,''"'')')ident_count,text
         endif
         ident_count=ident_count+1
      case default
         call stop_prep('*prep* ERROR(006) - IDENT TOO LONG:'//trim(G_SOURCE))
      end select
   case('c')
         write(G_iout,'(a)')'#ident "@(#)'//text//'"'
   case default
         call stop_prep('*prep* ERROR(007) - IDENT LANGUAGE UNKNOWN:'//trim(G_SOURCE))
   end select

end subroutine ident
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine define(allopts)                                  !@(#)define(3f): process 'DEFINE variablename[=expression]' directive
character(len=*),intent(in)    :: allopts                   ! packed uppercase working copy of input line with leading $verb removed

character(len=G_line_length):: temp                         ! scratch
integer                     :: iequ                         ! location of "=" in the directive, if any
integer                     :: i                            ! loop through each expression
integer                     :: j                            ! index thru variable dictionary to see if variable is already defined
integer                     :: iname                        ! length of variable name
integer                     :: istore                       ! location of variable name in dictionary
character(len=:),allocatable :: array(:)
character(len=:),allocatable :: opts
character(len=G_var_len)     :: value                       ! returned variable value

   call split(allopts,array,delimiters=';')                 ! parse string to an array parsing on delimiters

   do i=1,size(array)
     opts=trim(array(i))
     if(opts.eq.'')cycle
      ! CHECK COMMAND SYNTAX
      iequ=index(opts,'=')                                     ! find "=" in "variable_name=expression" if any
      iname=merge(len_trim(opts),iequ-1,iequ.eq.0)             ! find end of variable name
      call checkname(opts(:iname))                             ! check that variable name is composed of allowed characters

      if (iequ.eq.0) then                                      ! if no = then variable assumes value of 1
         temp='1'                                              ! set string to default value
      else                                                     ! =value string trails name on directive
         temp=opts(iequ+1:)                                    ! get expression
      endif
      if(G_debug) write(stderr,*)'*define* :LINE:'//trim(temp)
      call normalize_logical_operators(temp)
      if(G_debug) write(stderr,*)'*define* :LINE:AFTER NORMALIZE:'//trim(temp)
      call parens(temp)
      if(G_debug) write(stderr,*)'*define* :LINE:AFTER PARENS:'//trim(temp)
      call math(temp,1,len_trim(temp))
      if(G_debug) write(stderr,*)'*define* :LINE:AFTER MATH:'//trim(temp)
      call doop(temp,1,len_trim(temp))
      if(G_debug) write(stderr,*)'*define* :LINE:AFTER DOOP:'//trim(temp)
      call logic(temp,1,len_trim(temp))
      if(G_debug) write(stderr,*)'*define* :LINE:AFTER LOGIC:'//trim(temp)

      temp=nospace(temp)
      select case(temp)
      case('.FALSE.','.TRUE.')
         call table%set(opts(:iname),temp)
      case default ! assumed a number
         if ( verify(temp(1:1),'0123456789+-').eq.0 .and.  verify(temp(2:len_trim(temp)),'0123456789').eq.0 ) then
            call table%set(opts(:iname),temp)
         elseif (temp(1:1).ge.'A'.and.temp(1:1).le.'Z'.or.temp(1:1).eq.'_')then ! appears to be variable name not number or logical
            value=table%get(temp)                                                  ! find defined parameter in dictionary
           if (value.eq.'')then                                                   ! unknown variable name
              call stop_prep('*prep* ERROR(120) - UNDEFINED VARIABLE NAME:'//trim(G_source))
           else
              call table%set(opts(:iname),value)
           endif
         else
            call stop_prep('*prep* ERROR(008) - NOT LOGICAL OR INTEGER EXPRESSION:'//trim(allopts))
         endif
      end select
   enddo

end subroutine define
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function getdate(name) result(s)         !@(#) getdate(3f): Function to write date and time into returned string in different styles
character(len=*),intent(in),optional :: name

character(len=*),parameter           :: month='JanFebMarAprMayJunJulAugSepOctNovDec'
character(len=*),parameter           :: fmt = '(I2.2,A1,I2.2,I3,1X,A3,1x,I4)'
character(len=*),parameter           :: cdate = '(A3,1X,I2.2,1X,I4.4)'
character(len=:),allocatable         :: s
character(len=80)                    :: line
integer                              :: v(8)
character(len=10)                    :: name_

   call date_and_time(values=v)
   name_='prep'
   if(present(name))name_=name
   select case(lower(name_))
   case('prep')  ; write(line,fmt) v(5), ':', v(6), v(3), month(3*v(2)-2:3*v(2)), v(1) ! PREP_DATE="00:39  5 Nov 2013"
   case('date')  ; write(line,'(i4.4,"-",i2.2,"-",i2.2)') v(1),v(2),v(3)
   case('cdate') ; write(line,cdate) month(3*v(2)-2:3*v(2)), v(3), v(1)
   case('long')  ; write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   case('time')  ; write(line,'(i2.2,":",i2.2,":",i2.2)') v(5),v(6),v(7)
   case default  ; write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   end select
   s=trim(line)
end function getdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine checkname(line)                                                 !@(#)name(3f): test for legal variable name
character(len=*)          :: line
integer                   :: i

   if (len(line).eq.0)then
    call stop_prep("*prep* ERROR(028) - NULL VARIABLE NAME:"//trim(G_source))
   else if (line(1:1).lt.'A'.or.line(1:1).gt.'Z'.and.line(1:1).ne.'_')then                         ! variable names start with a-z
    call stop_prep("*prep* ERROR(016) - NAME DOES NOT START WITH ALPHAMERIC OR '_' (OR GENERAL SYNTAX ERROR):"//trim(G_source))
   elseif(len_trim(line).gt.G_var_len)then
     call stop_prep('*prep* ERROR(017) - VARIABLE NAME EXCEEDS '//v2s(G_var_len)//' CHARACTERS:'//trim(G_source))
   endif

   do i=2,len_trim(line)                                                 ! name uses $  _ and letters (A-Z) digits (0-9)
      if(line(i:i).ne.'$'.and.line(i:i).ne.'_'.and.     &
      & (line(i:i).lt.'A'.or.line(i:i).gt.'Z').and.     &
      & (line(i:i).lt.'0'.or.line(i:i).gt.'9')) then
      call stop_prep('*prep* ERROR(018) - VARIABLE NAME CONTAINS UNALLOWED CHARACTER(OR GENERAL SYNTAX ERROR):'//trim(G_source))
      endif
   enddo

end subroutine checkname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine getval(line,ipos1,ipos2,value)     !@(#)getval(3f): get value from dictionary for given variable name or return input
character(len=G_line_length),intent(in)   :: line                           ! current(maybe partial) directive line
integer,intent(in)                        :: ipos1                          ! beginning position of variable name in LINE
integer,intent(in)                        :: ipos2                          ! ending position of variable name in LINE
character(len=G_var_len),intent(out)      :: value                          ! returned variable value

character(len=G_line_length)           :: temp                              ! copy of substring being examined
integer                                :: i
integer                                :: ivalue

   temp=line(ipos1:ipos2)                                                   ! place variable name/value substring into TEMP

   if (temp(1:1).eq.' ')then                                                ! did not find expected variable name or value
      call stop_prep('*prep* ERROR(019) - INCOMPLETE STATEMENT.'//trim(G_SOURCE))
   endif

   if (temp(1:1).ge.'A'.and.temp(1:1).le.'Z'.or.temp(1:1).eq.'_') then      ! appears to be a variable name (not number or logical)

     value=table%get(temp)                                                  ! find defined parameter in dictionary
     if (value.eq.'')then                                                   ! unknown variable name
        call stop_prep('*prep* ERROR(020) - UNDEFINED VARIABLE NAME:'//trim(G_source))
     endif
     return
   else                                                                     ! not a variable name, try as a value
     read(temp(1:11),'(i11)',err=3) ivalue                                  ! try string as a numeric integer value
     write(value,'(i11)') ivalue                                            ! write numeric value into VALUE
     return                                                                 ! successfully return numeric VALUE

3    continue                                                               ! failed to read numeric value
     value=temp(:G_var_len)                                                 ! test TEMP as a logical
     if (value.ne.'.FALSE.'.and.value.ne.'.TRUE.')then                      ! if here, value should be a logical
        call stop_prep('*prep* ERROR(021) - SYNTAX ERROR.'//trim(G_source))
     endif
                                                                            ! value is ".TRUE." or ".FALSE."
   endif

   if(temp(1:1).ge.'A')then
      call stop_prep('*prep* ERROR(022) - $DEFINE VALUE MUST BE AN INTEGER OR LOGICAL CONSTANT.'//trim(G_source))
   endif

end subroutine getval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine unset(opts)                                     !@(#)unset(3f): process UNSET directive
character(len=*)             :: opts                       ! directive with no spaces, leading prefix removed, and all uppercase
character(len=:),allocatable :: names(:)
integer                      :: i,k

   ! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   ! allow basic globbing where * is any string and ? is any character
   if (len_trim(opts).eq.0) then                           ! if no variable name
      call stop_prep('*prep* ERROR(023) - $UNSET MISSING TARGETS:'//trim(G_source))
   endif
   call split(opts,names,delimiters=' ;,')

   do k=1,size(names)
      if(G_verbose)then
         call write_err('+ $UNSET '//names(k))
      endif
      call prep_update(names(k))
   enddo
end subroutine unset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine undef(opts)                                     !@(#)undef(3f): process UNDEFINE directive
character(len=*)             :: opts                       ! directive with no spaces, leading prefix removed, and all uppercase
character(len=:),allocatable :: names(:)
integer                      :: i,k

   ! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   ! allow basic globbing where * is any string and ? is any character
   if (len_trim(opts).eq.0) then                           ! if no variable name
      call stop_prep('*prep* ERROR(023) - $UNDEFINE MISSING TARGETS:'//trim(G_source))
   endif
   call split(opts,names,delimiters=' ;,')

   do k=1,size(names)
      if(G_verbose)then
         call write_err('+ $UNDEFINE '//names(k))
      endif
      do i=size(table%key),1,-1                           ! find defined variable to be undefined by searching dictionary
         if (glob(trim(table%key(i)),trim(names(k))))then   ! found the requested variable name
            call  table%del(table%key(i))
         endif
      enddo
   enddo

end subroutine undef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine if(opts,noelse,eb)                                 !@(#)if(3f): process IF and ELSEIF directives
character(len=*),intent(in)  :: opts
integer,intent(out)          :: noelse
logical                      :: eb
character(len=G_var_len)     :: name
character(len=G_var_len)     :: value
integer                      :: ios
integer                      :: i
integer                      :: ithen
character(len=G_line_length) :: expression

   noelse=0
   G_write=.false.

   G_nestl=G_nestl+1                                          ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif

   expression=opts
   ithen=len_trim(opts)                                       ! trim off ")THEN"
   if(ithen.gt.5)then
      if(expression(ithen-4:ithen).eq.')THEN'.and.expression(1:1).eq.'(')then
         expression=expression(2:ithen-5)
      endif
   endif

   FIND_DEFINED: do                                               ! find and reduce all DEFINED() functions to ".TRUE." or ".FALSE."
      if (index(expression,'DEFINED(').ne.0) then                 ! find a DEFINED() function
         call ifdefined(expression,index(expression,'DEFINED('))  ! reduce DEFINED() function that was found
         expression=nospace(expression)                           ! remove any spaces from rewritten expression
         cycle                                                    ! look for another DEFINED() function
      endif
      exit                                                        ! no remaining DEFINED() functions so exit loop
   enddo FIND_DEFINED

   call normalize_logical_operators(expression)
   call parens(expression)
   if (index(expression,'.').eq.0) then                            ! if line should be a variable only
      if (expression(1:1).ge.'A'.and.expression(1:1).le.'Z'.or.expression(1:1).eq.'_') then ! check name starts with valid character
         call checkname(expression)                        ! check that expression contains only a legitimate variable name
         name=expression(:G_var_len)                      ! get variable name
         value=table%get(name)
         if (value.eq.'') then                           ! if failed to find variable name
            call stop_prep('*prep* ERROR(024) - UNDEFINED PARAMETER IN IF:'//trim(G_source))
         endif
         read(value,'(l4)',iostat=ios) G_dc          ! convert variable value to a logical
         if(ios.ne.0)then
            call stop_prep('*prep* ERROR(025) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
         endif
      else                                                 ! this should have been a variable name
         call stop_prep('*prep* ERROR(026) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
      endif
   else                                                    ! a period is present in the expression so it needs evaluated
      call eval(expression)                                ! evaluate line
   endif
   if (.not.G_dc.or..not.G_condop(G_nestl-1).or.eb)then
      return                                               ! check to make sure previous IF was true
   endif
   G_condop(G_nestl)=.true.
   G_write=G_condop(G_nestl)
end subroutine if
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine def(verb,opts,noelse,eb)                  !@(#)def(3f): process IFDEF and IFNDEF directives
character(len=*),intent(in)  :: verb
character(len=*),intent(in)  :: opts
integer,intent(out)          :: noelse
logical                      :: eb
character(len=G_var_len)     :: name
character(len=G_var_len)     :: value
integer                      :: i
character(len=:),allocatable :: varvalue

   noelse=0
   G_write=.false.
   G_nestl=G_nestl+1                                 ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif
   call checkname(opts)                              ! check that opts contains only a legitimate variable name
   value=opts                                        ! set VALUE to variable name
   G_dc=.true.                                       ! initialize

   name=table%get(value)
   if (name.eq.'') then                           ! if failed to find variable name
      G_dc=.false.
   endif
   if((.not.G_noenv).and.(.not.G_dc))then            ! if not found in variable dictionary check environment variables if allowed
      varvalue=get_env(value)
      if(len_trim(varvalue).ne.0)then
         G_dc=.true.
      endif
   endif
   if(verb.eq.'IFNDEF')then
      G_dc=.not.G_dc
   endif
   if (.not.G_dc.or..not.G_condop(G_nestl-1).or.eb)then
      return                                               ! check to make sure previous IFDEF or IFNDEF was true
   endif
   G_condop(G_nestl)=.true.
   G_write=G_condop(G_nestl)
end subroutine def
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine ifdefined(line,ipos1)                         !@(#)ifdefined(3f): process and reduce DEFINED() function that was found
character(len=G_line_length)         :: line
integer,intent(in)                   :: ipos1

character(len=G_line_length)         :: newl
character(len=G_var_len),allocatable :: ifvars(:)
character(len=G_var_len),allocatable :: value
integer                              :: j

   newl=line(ipos1+7:) ! defined(

   if (len_trim(newl).eq.1.or.index(newl,')').eq.0.or. index(newl,')').eq.2)then
      call stop_prep("*prep* ERROR(027) - INCOMPLETE STATEMENT."//trim(G_SOURCE))
   endif

   G_dc=.true.
   line(ipos1:ipos1+6+index(newl,')'))='.TRUE.'
   ifvars= sep(newl(2:index(newl,')')-1),',')

   LIST: do j=1,size(ifvars)

      call checkname(ifvars(j))                          ! test for legal variable name
      value=table%get(ifvars(j))
      if(value.ne.'')cycle LIST
      G_dc=.false.
      line(ipos1:ipos1+6+index(newl,')'))='.FALSE.'
      exit LIST

   enddo LIST

end subroutine ifdefined
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine else(verb,opts,noelse,eb)                       !@(#)else(3f): process else and elseif
character(len=*)              :: verb
character(len=*)              :: opts
integer                       :: noelse
integer                       :: ithen
logical                       :: eb
character(len=G_line_length)  :: expression

   expression=opts
   ithen=len_trim(opts)  ! trim off ")THEN"
   if(ithen.gt.5)then
      if(expression(ithen-4:ithen).eq.')THEN'.and.expression(1:1).eq.'(')then
         expression=expression(2:ithen-5)
      endif
   endif

   if(noelse.eq.1.or.G_nestl.eq.0) then                    ! test for else instead of elseif
      call stop_prep("*prep* ERROR(031) - MISPLACED $ELSE OR $ELSEIF DIRECTIVE:"//trim(G_SOURCE))
   endif
   if(verb.eq.'ELSE')then
      noelse=1
   endif
   if(.not.G_condop(G_nestl-1))return                      ! if was true so ignore else
   eb=.false.
   if(G_condop(G_nestl)) then
       eb=.true.
       G_write=.false.
   elseif(len_trim(expression).ne.0)then                   ! elseif detected
     G_nestl=G_nestl-1                                     ! decrease if level because it will be incremented in subroutine if
     call if(expression,noelse,eb)
   else                                                    ! else detected
     G_condop(G_nestl)=.true.
     G_write=.true.
   endif

end subroutine else
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine endif(noelse,eb)                             !@(#)endif(3f): process ENDIF directive
integer,intent(out)           :: noelse
logical,intent(out)           :: eb

   ! if no ELSE or ELSEIF present insert ELSE to simplify logic
   if(noelse.eq.0)then
      call else('ELSE',' ',noelse,eb)
   endif

   G_nestl=G_nestl-1                                           ! decrease if level

   if(G_nestl.lt.0)then
      call stop_prep("*prep* ERROR(032) - MISPLACED $ENDIF DIRECTIVE:"//trim(G_source))
   endif

   noelse=0                                                    ! reset else level
   eb=.not.G_condop(G_nestl+1)
   G_write=.not.eb
   G_condop(G_nestl+1)=.false.

   if(G_nestl.eq.0)then
      G_write=.true.
      eb=.false.
   endif

end subroutine endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parens(line)                       !@(#)parens(3f): find subexpressions in parenthesis and process them
character(len=G_line_length)    :: line       ! line        -
integer                         :: i
integer                         :: j

   if(G_debug.and.G_verbose)then
       write(stderr,*)'*parens* TOP:LINE:'//trim(line)
   endif
   TILLDONE: do
      if (index(line,')').ne.0) then          ! closing parens found
         do i=index(line,')'),1,-1            ! find first right paren, then backwards to left paren (find innermost set of parens)
            if (line(i:i).eq.'(') exit
         enddo
         if (i.eq.0) then
            call stop_prep("*prep* ERROR(033) - CONSTANT LOGICAL EXPRESSION REQUIRED:"//trim(G_source))
         endif
         call math(line,i+1,index(line,')')-1)
         call doop(line,i+1,index(line,')')-1)
         call logic(line,i+1,index(line,')')-1)
         if (i.eq.1.and.index(line,')').eq.len_trim(line)) then          ! rewrite line after no more parens
            line=line(i+1:index(line,')')-1)
         elseif (i.eq.1) then                                            ! rewrite line after first set of parens
            line=line(2:index(line,')')-1)//line(index(line,')')+1:)
         elseif (index(line,')').eq.len_trim(line)) then                 ! rewrite line after last set of parens on line

            if (line(i+1:i+1).eq.'-'.and.index('*/+-',line(i-1:i-1)).ne.0) then
               do j=i-2,1,-1
                  if (index('*/+-',line(j:j)).ne.0) exit
               enddo
               !if (j.eq.i-2) then
               !   call stop_prep("*prep* 1**(-1) NOT IMPLEMENTED YET")
               !endif

               select case (index('*/+-',line(i-1:i-1)))
               case(1,2)
                  if (j.eq.0) then
                     line='-'//line(:i-1)//line(i+2:index(line,')')-1)
                  else
                     line=line(:j)//'(-'//line(j+1:i-1)//line(i+2:index(line,')'))
                  endif
               case(3)
                  line=line(:i-2)//'-'//line(i+2:index(line,')')-1)
               case(4)
                  line=line(:i-2)//'+'//line(i+2:index(line,')')-1)
               case default
               end select
            else
               line=line(:i-1)//line(i+1:index(line,')')-1)
            endif
         elseif (line(i+1:i+1).eq.'-'.and.index('*/+-',line(i-1:i-1)).ne.0) then
            do j=i-2,1,-1
               if (index('*/+-',line(j:j)).ne.0) exit
            enddo
            !if (j.eq.i-2) then
            !   !call stop_prep("*prep* 1**(-1) Not Implemented Yet")
            !endif

            select case (index('*/+-',line(i-1:i-1)))
            case(1,2)
               if (j.eq.0) then
                  line='-'//line(:i-1)//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
               else
                  line=line(:j)//'(-'//line(j+1:i-1)//line(i+2:index(line,')'))//line(index(line,')')+1:)
               endif
            case(3)
               line=line(:i-2)//'-'//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
            case(4)
               line=line(:i-2)//'+'//line(i+2:index(line,')')-1)//line(index(line,')')+1:)
            case default
            end select
         else
            line=line(:i-1)//line(i+1:index(line,')')-1)//line(index(line,')')+1:)
         endif
      line=nospace(line)
      cycle TILLDONE
   elseif (index(line,'(').ne.0) then
      call stop_prep('*prep* ERROR(034) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
   endif
   exit
   enddo TILLDONE
   if(G_debug.and.G_verbose)then
       write(stderr,*)'*parens* BOTTOM:LINE:'//trim(line)
   endif
end subroutine parens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine math(line,ipos1,ipos2)                             !@(#)math(3f):
integer                               :: ipos1
integer                               :: ipos2
integer                               :: i,j
character(len=G_line_length)          :: line
character(len=G_line_length)          :: newl

   newl=line(ipos1:ipos2)
   i=1

   do
      j=index(newl(i:),'.')
      if (j.ne.0.and.j.ne.1) then
         call domath(newl(i:j+i-2),j-1)
         i=i+j
      elseif (j.eq.1) then
         i=i+1
      else
         call domath(newl(i:),ipos2-i+1)
         exit
      endif
   enddo

   line(ipos1:ipos2)=newl
   line=nospace(line)

end subroutine math
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine domath(line,ipos2)            !@(#)domath(3f): reduce integer expression containing  +-/* and ** operators
character(len=*)                :: line
integer                         :: ipos2

character(len=11)               :: temp
character(len=G_line_length)    :: newl
character(len=2),save           :: ops(3)= ['**','*/','+-']
integer                         :: i
integer                         :: j
integer                         :: loc
integer                         :: minus1
integer                         :: i1
integer                         :: i2
integer                         :: l
integer                         :: len
integer                         :: numop

  if (ipos2.eq.0) return
  loc=0
  j=0
  minus1=1
  newl=line(:ipos2)
  OVERALL: do numop=1,3                         ! check **, then */, then +-
     TILLDONE: do                               ! keep doing reduction of current operators
       i=index(newl,ops(numop))                 ! find location in input string where operator string was found
       if (numop.ne.1) then                     ! if not the two-character operator ** check for either operator of current group
         i=index(newl,ops(numop)(1:1))          ! find  first operator of group, if present
         j=index(newl,ops(numop)(2:2))          ! find second operator of group, if present
         i=max(i,j)                             ! find right-most operator, if any
         if (i*j.ne.0) i=min(i,j)               ! if at least one operator is present find left-most
       endif
       IF (I.EQ.0) cycle OVERALL                ! did not find these operators

       LEN=1                                    ! operator length
       IF (NUMOP.EQ.1) LEN=2
       IF (I.EQ.len_trim(NEWL)) then            ! if operator is at end of string
          call stop_prep("*prep* ERROR(035) - INCOMPLETE STATEMENT. OPERATOR (**,/,*,+,-) AT STRING END:"//trim(G_SOURCE))
          exit OVERALL
       endif
       IF (I.EQ.1.AND.NUMOP.NE.3) then          ! if operator at beginning of string and not +-
        call stop_prep("*prep* ERROR(036) - SYNTAX ERROR. OPERATOR (**,*,/) NOT ALLOWED TO PREFIX EXPRESSION:"//trim(G_SOURCE))
          exit OVERALL
       endif
       if (.not.(i.eq.1.and.numop.eq.3)) then   ! if processing +- operators and sign at beginning of string skip this
          if (index('*/+-',newl(i-1:i-1)).ne.0.or.index('*/+-',newl(i+len:i+len)).ne.0) then
            call stop_prep('*prep* ERROR(037) - SYNTAX ERROR IN DOMATH:'//trim(G_source))
            exit OVERALL
          endif
       endif

       i1=0
       if (.not.(i.eq.1.and.numop.eq.3)) then
          do j=i-1,1,-1
            if (index('*/+-.',newl(j:j)).eq.0) cycle
            exit
          enddo
          if (.not.(j.eq.i-1.and.j.ne.1))then
             i1=get_integer_from_string(newl(j+1:i-1))
          endif
       endif
       do l=i+len_trim(ops(numop)),len_trim(newl)
         if (index('*/+-.',newl(l:l)).eq.0) cycle
         exit
       enddo

       i2=get_integer_from_string(newl(i+len:l-1))

       if (numop.eq.1) then
         i1=i1**i2*minus1
       else
          select case (index('*/+-',newl(i:i)))
          case(1)
             i1=i1*i2*minus1
          case(2)
             if(i2.eq.0)then
                call stop_prep('*prep* ERROR(038) - DIVIDE BY ZERO:'//trim(G_source))
                exit OVERALL
             endif
             i1=i1/i2*minus1
          case(3)
          if (i1.ne.0) then
            i1=i1*minus1+i2
          else
            i1=i1+i2*minus1
          endif
          case(4)
             if (i1.ne.0) then
               i1=i1*minus1-i2
             else
               i1=i1-i2*minus1
             endif
          case default
             call stop_prep('*prep* ERROR(039) - INTERNAL PROGRAM ERROR:'//trim(G_source))
             exit OVERALL
          end select
       endif

       if (i1.le.0) then
         if (j.eq.i-1.and.j.ne.1) then
           minus1=-1
           i1=abs(i1)
           loc=j+1
           newl(j+1:j+1)=' '
           l=l-1
           newl=nospace(newl)
         elseif (i.eq.1.and.numop.eq.3) then
           minus1=-1
           i1=abs(i1)
           loc=i
           newl(j:j)=' '
           l=l-1
           j=j-1
           newl=nospace(newl)
         else
           minus1=1
         endif
       else
         minus1=1
       endif
       write(temp,'(i11)') i1
       temp=nospace(temp)
       if (j.eq.0.and.l.gt.len_trim(newl)) then
         newl=temp(:len_trim(temp))
         cycle overall
       elseif (j.eq.0) then
         newl=temp(:len_trim(temp))//newl(l:)
       elseif (l.gt.len_trim(newl)) then
         newl=newl(:j)//temp(:len_trim(temp))
       else
         newl=newl(:j)//temp(:len_trim(temp))//newl(l:)
       endif
       if(i1.lt.0)then  ! if i1 is negative, could produce +-
          call substitute(newl,'+-','-')
       endif
     enddo TILLDONE
  enddo OVERALL

  if (minus1.eq.-1.and.(loc.eq.0.or.loc.eq.1)) then
     newl(:G_line_length)='-'//trim(newl)  !x! note potentially trimming a character off the end
  elseif (minus1.eq.-1.and.loc.ne.1) then
     newl=newl(:loc-1)//'-'//newl(loc:)
  endif

  line(:ipos2)=newl(:len_trim(newl))

end subroutine domath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine doop(line,ipos1,ipos2)                       !@(#)doop(3f): find VAL.OP.VAL strings and reduce to .TRUE. or .FALSE.
character(len=G_line_length)    :: line
integer                         :: ipos1
integer                         :: ipos2

character(len=4),parameter      :: ops(6) = ['.EQ.','.NE.','.GE.','.GT.','.LE.','.LT.']
character(len=G_var_len)        :: val1
character(len=G_var_len)        :: val2
integer                         :: ival1, ival2
character(len=7)                :: temp

character(len=G_line_length)    :: newl
integer                         :: i,j,k

   newl=line(ipos1:ipos2)
   CHECK_EACH_OP_TYPE: do i=1,6
      FIND_MORE_OF: do
         G_dc=.false.
         if (index(newl,ops(i)).ne.0) then                       ! found current operator looking for
            do j=index(newl,ops(i))-1,1,-1
               if (newl(j:j).eq.'.') then
                  exit
               endif
            enddo
            call getval(newl,j+1,index(newl,ops(i))-1,val1)
            do k=index(newl,ops(i))+4,len_trim(newl)
               if (newl(k:k).eq.'.')then
                  exit
               endif
            enddo
            call getval(newl,index(newl,ops(i))+4,k-1,val2)
            ival1=get_integer_from_string(val1)
            ival2=get_integer_from_string(val2)
            select case(i)                                       ! determine truth
            case(1)                                              ! .eq.
               if (ival1.eq.ival2) G_dc=.true.
            case(2)                                              ! .ne.
               if (ival1.ne.ival2) G_dc=.true.
            case(3)                                              ! .ge.
               if (ival1.ge.ival2) G_dc=.true.
            case(4)                                              ! .gt.
               if (ival1.gt.ival2) G_dc=.true.
            case(5)                                              ! .le.
               if (ival1.le.ival2) G_dc=.true.
            case(6)                                              ! .lt.
               if (ival1.lt.ival2) G_dc=.true.
            case default
            end select
            temp='.FALSE.'
            if (G_dc) temp='.TRUE.'
            call rewrit(newl,temp(:len_trim(temp)),j,j,k,k)
            newl=nospace(newl)
            cycle
         endif
         exit
      enddo FIND_MORE_OF
   enddo CHECK_EACH_OP_TYPE
   if (ipos1.ne.1) then
      line=line(:ipos1-1)//newl(:len_trim(newl))//line(ipos2+1:)
   else
      line=newl(:len_trim(newl))//line(ipos2+1:)
   endif
   line=nospace(line)
end subroutine doop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
logical function true_or_false(line,ipos1,ipos2)       !@(#)true_or_false(3f): convert variable name or .TRUE./.FALSE. to a logical value
character(len=G_line_length),intent(in) :: line              ! line containing string to interpret as a logical value
integer,intent(in)                      :: ipos1             ! starting column of substring in LINE
integer,intent(in)                      :: ipos2             ! ending column of substring in LINE

character(len=G_var_len)                :: value
character(len=G_var_len)                :: substring
integer                                 :: ios               ! error code returned by an internal READ

   true_or_false=.false.                                     ! initialize return value
   substring=line(ipos1:ipos2)                               ! extract substring from LINE to interpret

   select case (substring)                                   ! if string is not a logical string assume it is a variable name
   case ('.FALSE.','.F.')
      true_or_false=.false.                                  ! set appropriate return value
   case ('.TRUE.','.T.')
      true_or_false=.true.                                   ! set appropriate return value
   case default                                              ! assume this is a variable name, find name in dictionary
      value=table%get(substring)

      if (value.eq.'') then                                  ! if not a defined variable name stop program
         call stop_prep('*prep* ERROR(040) - UNDEFINED VARIABLE.'//trim(G_source))
      else
         read(value,'(l4)',iostat=ios) true_or_false         ! try to read a logical from the value for the variable name
         if(ios.ne.0)then                                    ! not successful in reading string as a logical value
            call stop_prep('*prep* ERROR(041) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
         endif
      endif

   end select

end function true_or_false
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine logic(line,ipos1,ipos2)           !@(#)logic(3f): process .OP. operator strings
character(len=*)             :: line
integer,intent(in)           :: ipos1, ipos2

logical                      :: one, two
character(len=7)             :: temp
character(len=G_line_length) :: newl
character(len=6),save        :: ops(5)= (/'.NOT. ','.AND. ','.OR.  ','.EQV. ','.NEQV.'/)
integer                      :: i,j,k,l
integer                      :: ieqv
integer                      :: ineqv
integer                      :: i1
integer                      :: iop
integer                      :: len
integer                      :: len1
integer                      :: len2

   newl=line(ipos1:ipos2)
   len1=0
   len2=0
   one=.false.
   LOOP: do i=1,3
      20 continue
           LEN=len_trim(ops(i))
           IF (INDEX(NEWL,OPS(I)(:len_trim(OPS(I)))).EQ.0) cycle
           I1=INDEX(NEWL,OPS(I)(:len_trim(OPS(I))))-1
           J=I1+1
           LEN1=0
           IF (I.NE.1) then
              OUTER: DO J=I1,1,-1
                INNER: DO K=1,5
                   LEN1=5
                   IF (K.EQ.3) LEN1=4
                   IF (INDEX(NEWL(J:I1),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUTER
                enddo INNER
              enddo OUTER
              IF (J.EQ.0) LEN1=1
              ONE=true_or_false(NEWL,J+LEN1,I1)
           endif

           OUT: DO L=I1+LEN,len_trim(NEWL)
             IN: DO K=1,5
                LEN2=5
                IF (K.EQ.3) LEN2=4
                IF (INDEX(NEWL(I1+LEN:L),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUT
             enddo IN
           enddo OUT

           IF (L.GT.len_trim(NEWL)) LEN2=0
           TWO=true_or_false(NEWL,I1+LEN+1,L-LEN2)

           select case(i)
           case(1); G_dc=.not.two
           case(2); G_dc=one.and.two
           case(3); G_dc=one.or.two
           case default
              call stop_prep('*prep* internal error')
           end select

           temp='.FALSE.'
           if (G_dc) temp='.TRUE.'
           call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
        goto 20
   enddo LOOP
   TILLDONE: do
      ieqv=index(newl,'.EQV.')
      ineqv=index(newl,'.NEQV.')
      if (ieqv*ineqv.eq.0.and.ieqv.ne.ineqv) then
        iop=max(ieqv,ineqv)
      elseif (ieqv.ne.0) then
        iop=min(ieqv,ineqv)
      elseif (ipos1.eq.1) then
        line=newl(:len_trim(newl))//line(ipos2+1:)
        return
      else
        line=line(:ipos1-1)//newl(:len_trim(newl))//line(ipos2+1:)
        return
      endif
      len=5
      if (index(newl,'.EQV.').ne.iop) len=6
      do j=iop-1,1,-1
         if (newl(j:j+1).eq.'V.') exit
      enddo
      if (j.eq.0) len1=1
      one=true_or_false(newl,j+len1,iop-1)
      do l=iop+len,len_trim(newl)
         if (newl(l:l+1).eq.'.E'.or.newl(l:l+1).eq.'.N') exit
      enddo
      if (l.gt.len_trim(newl)) len2=0
      two=true_or_false(newl,iop+len,l+len2)
      G_dc=one.eqv.two
      if (len.ne.5) G_dc=one.neqv.two
      temp='.FALSE.'
      if (G_dc) temp='.TRUE.'
      call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
   enddo TILLDONE
end subroutine logic
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine eval(line)                                   !@(#)eval(3f): evaluate math expression to .TRUE. or .FALSE.
character(len=G_line_length)   :: line
character(len=7)               :: value

   call parens(line)
   call math(line,1,len_trim(line))
   call doop(line,1,len_trim(line))
   call logic(line,1,len_trim(line))
   value=line(1:7)

   if (value.ne.'.TRUE.'.and.value.ne.'.FALSE.') then
      call stop_prep('*prep* ERROR(043) - value neither true or false:'//trim(value)//' when evaluating: '//trim(G_source))
   endif

   read(value,'(l4)') G_dc

end subroutine eval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_integer_from_string(line) !@(#)get_integer_from_string(3f): read integer value from line
                                                                    ! assume string is a variable name or an integer value
character(len=*),intent(in)  :: line                                ! string to read an integer value from
integer                      :: i                                   ! index of variable dictionary where variable name is stored
integer                      :: ios                                 ! I/O error value to check to see if internal reads succeeded
integer                      :: get_integer_from_string             ! integer value to return if string is converted successfully
character(len=:),allocatable :: value
   get_integer_from_string=0
   if(len_trim(line).eq.0)then
      get_integer_from_string=0
   elseif (verify(line,'0123456789 +-').eq.0) then                  ! assumed a number
      read(line,'(i11)',iostat=ios) get_integer_from_string         ! try to read integer value from input string
      if(ios.ne.0)then                                              ! failed to convert the string to an integer, so stop
        call stop_prep('*prep* ERROR(044) - MUST BE INTEGER:"'//trim(line)//'" IN '//trim(G_source))
      endif
   else                                                             ! input is not a number, assume it represents a variable name
      value=table%get(line)
      if (value.eq.'')then                                          ! if variable name not found in dictionary, stop
        call stop_prep('*prep* ERROR(045) - UNDEFINED VARIABLE NAME:"'//trim(line)//'" IN '//trim(G_source))
      else
         read(value,'(i11)',iostat=ios) get_integer_from_string     ! read integer value from the value associated with name
         if(ios.ne.0)then                                           ! failed reading integer from value, stop
           call stop_prep('*prep* ERROR(046) - MUST BE INTEGER:"'//trim(line)//"="//trim(value)//'" IN '//trim(G_source))
         endif
      endif
   endif                                                            ! return integer value
end function get_integer_from_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rewrit(line,temp,j,j1,l,l1)           !@(#)rewrit(3f):
character(len=G_line_length)  line
character(len=*)           :: temp
integer                    :: j
integer                    :: j1
integer                    :: l
integer                    :: l1

   if (j.eq.0.and.l.gt.len_trim(line)) then      ! done
      line=temp
   elseif (j.eq.0) then                          ! first item
      line=temp//line(l1:)
   elseif (l.gt.len_trim(line)) then             ! last item
      if (j1.ne.0) then
         line=line(:j1)//temp
      else
         line=temp
      endif
   else                                          ! middle item
        line=line(:j1)//temp//line(l1:)
   endif
end subroutine rewrit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine document(opts)                    !@(#)document(3f): process BLOCK command to start or stop special processing
character(len=*),intent(in)  :: opts
integer                      :: ierr
integer                      :: ios
character(len=G_line_length) :: options                 ! everything after first word of command till end of line or !

! CHECK COMMAND SYNTAX
   if(G_outtype.eq.'help')then  ! if in 'help' mode wrap up the routine
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))"
      write(G_iout,'(a)')"   stop ! if --help was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_usage"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   elseif(G_outtype.eq.'variable')then     ! if in 'variable' mode wrap up the variable
      write(G_iout,'(a)')"'']"
   elseif(G_outtype.eq.'system')then
         close(unit=G_scratch_lun,iostat=ios)
         call execute_command_line( trim(sget('block_cmd'))//' < '//G_scratch_file//' > '//G_scratch_file//'.out')
         ierr=filedelete(G_scratch_file)
         options=G_scratch_file//'.out'
         call include(options,50+G_iocount)    ! Filenames can be case sensitive
   elseif(G_outtype.eq.'version')then  ! if in 'version' mode wrap up the routine
      write(G_iout,'("''@(#)COMPILED:       ",a,"'',&")') getdate('long')//'>'
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))"
      !x!write(G_iout,'(a)')'   write(*,*)"COMPILER VERSION=",COMPILER_VERSION()'
      !x!write(G_iout,'(a)')'   write(*,*)"COMPILER OPTIONS=",COMPILER_OPTIONS()'
      write(G_iout,'(a)')"   stop ! if --version was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_version"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   endif

   ! parse options on input line
   call dissect2('block','--oo --file --cmd sh --varname textblock --style "#N#" --append .false.',opts)
   ! if a previous command has opened a --file FILENAME flush it, because a new one is being opened or this is an END command
   ! and if a --file FILENAME has been selected open it
   call print_comment_block()

   ! now can start new section
   G_MAN=''
   if(sget('block_file').ne.'')then
      G_MAN_FILE=sget('block_file')
      G_MAN_COLLECT=.true.
   else
      G_MAN_FILE=''
      G_MAN_COLLECT=.false.
   endif

   G_MAN_PRINT=.false.
   if(lget('block_append'))then
      G_MAN_FILE_POSITION='APPEND'
   else
      G_MAN_FILE_POSITION='ASIS'
   endif

   select case(upper(sget('block_oo')))

   case('COMMENT')
      G_outtype='comment'
      G_MAN_PRINT=.true.
      G_MAN_COLLECT=.true.
      if(sget('block_style').ne.'#N#')then
         G_comment_style=lower(sget('block_style'))             ! allow formatting comments for particular post-processors
      endif
   case('NULL')
      G_outtype='null'

   case('SET','REPLACE')
      G_outtype='set'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('DEFINE')
      G_outtype='define'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('REDEFINE')
      G_outtype='redefine'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
   case('MESSAGE')
      G_outtype='message'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.

   case('SHELL','SYSTEM')
      G_outtype='system'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
      if(G_system_on)then                             ! if allowing commands to be executed
         flush(unit=G_iout,iostat=ios)
         !!G_scratch_file=scratch('prep_scratch.'))
         G_scratch_file=trim(uniq(get_tmp()//'prep_scratch.'))  !! THIS HAS TO BE A UNIQUE NAME -- IMPROVE THIS
         G_scratch_lun=fileopen(G_scratch_file,'rw',ierr)
         if(ierr.lt.0)then
            call stop_prep('*prep* ERROR(048) - FILTER COMMAND FAILED TO OPEN PROCESS:'//trim(G_SOURCE))
         endif
      else
         call stop_prep('*prep* ERROR(049) - FILTER COMMAND BLOCK ENCOUNTERED BUT SYSTEM COMMANDS NOT ENABLED:'//trim(G_SOURCE))
      endif

   case('VARIABLE')
      G_outtype='variable'
      write(G_iout,'(a)')trim(sget('block_varname'))//'=[ CHARACTER(LEN=128) :: &'
      G_MAN_PRINT=.true.

   case('HELP')
      G_outtype='help'
      write(G_iout,'(a)')'subroutine help_usage(l_help)'
      write(G_iout,'(a)')'implicit none'
      write(G_iout,'(a)')'character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"'
      write(G_iout,'(a)')'logical,intent(in)             :: l_help'
      !write(G_iout,'(a)')'character(len=128),allocatable :: help_text(:)'
      write(G_iout,'(a)')'character(len=:),allocatable :: help_text(:)'
      write(G_iout,'(a)')'integer                        :: i'
      write(G_iout,'(a)')'logical                        :: stopit=.false.'
      write(G_iout,'(a)')'stopit=.false.'
      write(G_iout,'(a)')'if(l_help)then'
! NOTE: Without the type specification this constructor would have to specify all of the constants with the same character length.
      write(G_iout,'(a)')'help_text=[ CHARACTER(LEN=128) :: &'

         select case(G_comment_style)  ! duplicate help text as a comment for some code documentation utilities
         case('doxygen')               ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            G_MAN_PRINT=.true.
         case('fort')                  ! convert plain text to ford  comment blocks with some automatic markdown highlights
            G_MAN_PRINT=.true.
         case('none')                  ! do not print comment lines from block
            G_MAN_PRINT=.false.
         case default
         end select

   case('VERSION')
      G_outtype='version'
      write(G_iout,'(a)')'subroutine help_version(l_version)'
      write(G_iout,'(a)')'implicit none'
      write(G_iout,'(a)')'character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"'
      write(G_iout,'(a)')'logical,intent(in)             :: l_version'
      !write(G_iout,'(a)')'character(len=128),allocatable :: help_text(:)'
      write(G_iout,'(a)')'character(len=:),allocatable   :: help_text(:)'
      write(G_iout,'(a)')'integer                        :: i'
      write(G_iout,'(a)')'logical                        :: stopit=.false.'
      write(G_iout,'(a)')'stopit=.false.'
      write(G_iout,'(a)')'if(l_version)then'
! NOTE: Without the type specification this constructor would have to specify all of the constants with the same character length.
      write(G_iout,'(a)')'help_text=[ CHARACTER(LEN=128) :: &'

   case('WRITE')
      G_outtype='write'
   case('','END')
      G_outtype='asis'
      G_MAN_COLLECT=.false.
   case('ASIS')
      G_outtype='asis'
   case default
      call stop_prep('*prep* ERROR(047) - UNEXPECTED "BLOCK" OPTION. FOUND:'//trim(sget('block_oo'))//' IN '//trim(G_source) )
   end select

   G_comment_count=0
end subroutine document
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_comment_block() !@(#)print_comment_block(3f): format comment block to file in document directory and output
character(len=:),allocatable :: filename
character(len=:),allocatable :: varvalue
integer                      :: ios,iend,lun

   if(.not.allocated(G_MAN))then
      return
   endif

   varvalue=get_env('PREP_DOCUMENT_DIR')

   if(varvalue.ne.''.and.G_MAN.ne.''.and.G_MAN_FILE.ne.' ')then ! if $BLOCK --file FILE is present generate file in directory/doc

      iend=len_trim(varvalue)
      if(varvalue(iend:iend).ne.'/')then
         filename=trim(varvalue)//'/doc/'//trim(G_MAN_FILE)
      else
         filename=trim(varvalue)//'doc/'//trim(G_MAN_FILE)
      endif

      open(newunit=lun,file=filename,iostat=ios,action='write',position=G_MAN_FILE_POSITION)

      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(054) - FAILED TO OPEN DOCUMENT OUTPUT FILE:'//trim(filename))
      else
         if(len(G_MAN).gt.1)then                   ! the way the string is built it starts with a newline
            write(lun,'(a)',iostat=ios) G_MAN(2:)
         else
            write(lun,'(a)',iostat=ios) G_MAN
         endif
         if(ios.ne.0)then
            call write_err('G_MAN='//G_MAN)
            call stop_prep('*prep* ERROR(055) - FAILED TO WRITE OUTPUT FILE:'//trim(filename))
         endif
      endif

      close(unit=lun,iostat=ios)

   endif

   ! now if $BLOCK COMMENT print comment block
   if(G_MAN_PRINT)then
      call format_G_MAN()
   endif

end subroutine print_comment_block
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine format_g_man()
character(len=:),allocatable   :: array1(:)   ! output array of tokens
character(len=:),allocatable   :: array(:)    ! output array of tokens
integer                        :: ios
integer                        :: i

   ALL: block
      WRITEIT: block

         select case(G_comment_style)

         case('doxygen')                 ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN).gt.1)then      ! the way the string is built it starts with a newline

               CALL split(G_MAN,array1,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               if(allocated(array))deallocate(array)
               allocate(character(len=len(array1)+6) :: array(size(array1)))  ! make room for !! and ##
               array(:)=array1
               deallocate(array1)

               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)).eq.array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)).ne.array(i))then
                     array(i)='##'//trim(array(i))
                     select case(array(i))
                     case('##SYNOPSIS','##EXAMPLES','##EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!'//'!'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array).gt.0)then
                  write(G_iout,'("!",">",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!","!",a)',iostat=ios)trim(array(i))
                  if(ios.ne.0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!",131("="))')

         case('ford')                    ! convert plain text to doxygen comment blocks with some automatic markdown highlights
            if(len(G_MAN).gt.1)then      ! the way the string is built it starts with a newline
               CALL split(G_MAN,array1,delimiters=new_line('N'),nulls='return') ! parse string to an array parsing on delimiters
               !======================================================================================== nvfortran bug
               ! array=[character(len=(len(array1)+6)) :: array1] !! pad with trailing spaces

               if(allocated(array))deallocate(array)
               allocate(character(len=len(array1)+6) :: array(size(array1)))  ! make room for !! and ##
               array(:)=array1
               !========================================================================================
               deallocate(array1)

               do i=1,size(array)        ! lines starting with a letter and all uppercase letters is prefixed with "##"
                  if( upper(array(i)).eq.array(i) .and. isalpha(array(i)(1:1)).and.lower(array(i)).ne.array(i))then
                     array(i)='## '//trim(array(i))
                     select case(array(i))
                     case('## SYNOPSIS','## EXAMPLES','## EXAMPLE')
                        array(i)=trim(array(i))//new_line('N')//'!>'
                     endselect
                  else
                     array(i)=' '//trim(array(i))
                  endif
               enddo

               if(size(array).gt.0)then
                  write(G_iout,'("!>",a)')trim(array(1))
               endif

               do i=2,size(array)
                  write(G_iout,'("!>",a)',iostat=ios)trim(array(i))
                  if(ios.ne.0)exit WRITEIT
               enddo

            endif
            !x!write(G_iout,'("!>",131("="))')

         case('none')                    ! ignore comment block

         case default
            if(len(G_MAN).gt.1)then                       ! the way the string is built it starts with a newline
               G_MAN=G_MAN(2:)//repeat(' ',2*len(G_MAN))  ! make sure the white-space exists
               call substitute(G_MAN,NEW_LINE('A'),NEW_LINE('A')//'! ')
               G_MAN='! '//trim(G_MAN)
            endif
            write(G_iout,'(a)',iostat=ios) G_MAN
            if(ios.ne.0)exit WRITEIT
            !x!write(G_iout,'("!",131("="))')
         end select

         exit ALL
      endblock WRITEIT
      call write_err('G_MAN='//G_MAN)
      call stop_prep('*prep* ERROR(056) - FAILED TO WRITE COMMENT BLOCK')
   endblock ALL
end subroutine format_g_man
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine show_state(list,msg)                        !@(#)debug(3f): process $SHOW command or state output when errors occur
character(len=*),intent(in),optional :: list
character(len=*),intent(in) :: msg
integer                     :: i, j
character(len=:),allocatable   :: array(:)    ! output array of tokens
character(len=*),parameter  :: fmt='(*(g0,1x))'
   if(present(list))then
      if(list.ne.'')then
         ! print variables:
         CALL split(list,array,delimiters=' ;,') ! parse string to an array parsing on delimiters
         do j=1,size(array)
            do i=1,size(table%key)
               if(glob(trim(table%key(i)),trim(array(j))))then ! write variable and corresponding value
                  write(G_iout,fmt)"! ",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)))
               endif
            enddo
         enddo
         return
      endif
   endif
   write(G_iout,'(a)')'!==============================================================================='
   write(G_iout,'(a)')'! '//trim(msg)

   write(G_iout,'(a)')'! Current state of prep(1):('//getdate()//')'
   write(G_iout,'("! Total lines read ............... ",i0)')G_io_total_lines     ! write number of lines read
   write(G_iout,'("! Conditional nesting level....... ",i0)')G_nestl              ! write nesting level
   write(G_iout,'("! G_WRITE (general processing).... ",l1)')G_write              ! non-if/else/endif directives processed
   write(G_iout,'("! G_LLWRITE (write input lines)... ",l1)')G_llwrite            ! non-if/else/endif directives processed

   call write_arguments()

   write(G_iout,'(a)')'! Open files:'
   write(G_iout,'(a)')'!    unit ! line number ! filename'
   do i=1,G_iocount                                                               ! print file dictionary
      ! write table of files
      write(G_iout,'("!    ",i4," ! ",i11," ! ",a)') &
      &  G_file_dictionary(i)%unit_number,    &
      &  G_file_dictionary(i)%line_number,    &
      &  trim(G_file_dictionary(i)%filename )
   enddo

   write(G_iout,'(a)')'! INCLUDE directories:'
   do i=1,G_inc_count
      write(G_iout,'("!    ",a)') trim(G_inc_files(i))
   enddo

   write(G_iout,'(a)')'! Variables:'
   do i=1,size(table%key)                                                        ! print variable dictionary
      write(G_iout,fmt)"!    $DEFINE",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)) )
   enddo

   write(G_iout,'(a)')'! Parcels:'
   do i=1,G_parcelcount
      write(G_iout,fmt) '!   ',trim(G_parcel_dictionary(i)%name)
   enddo

   if(size(keywords).gt.0)then
      write(G_iout,fmt)'! SET strings:(There are',size(keywords),'keywords defined)'
      write(G_iout,'(3(g0,1x))')('!    $SET',keywords(i),values(i)(:counts(i)),i=1,size(keywords))
   endif

   write(G_iout,'(a)')'!-------------------------------------------------------------------------------'
end subroutine show_state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_arguments() !@(#)write_arguments(3f): return all command arguments as a string

integer                      :: istatus          !  status (non-zero means error)
integer                      :: ilength          !  length of individual arguments
integer                      :: i                !  loop count
integer                      :: icount           !  count of number of arguments available
character(len=255)           :: value            !  store individual arguments one at a time

   write(G_iout,'(a)',advance='no')'! Arguments ...................... '
   icount=command_argument_count()               ! intrinsic gets number of arguments
   do i=1,icount
      call get_command_argument(i,value,ilength,istatus)
      write(G_iout,'(a,1x)',advance='no')value(:ilength)
   enddo
   write(G_iout,'(a)')
end subroutine write_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine include(line,iunit)  !@(#)include(3f): add file to input file list
implicit none
character(len=G_line_length),intent(in)  :: line
integer,intent(in)                       :: iunit
integer                                  :: ios
character(len=4096)                      :: message
character(len=G_line_length)             :: line_unquoted
integer                                  :: iend

   line_unquoted=adjustl(unquote(line))                   ! remove " from filename using Fortran list-directed I/O rules
   iend=len_trim(line_unquoted)
   if(len(line_unquoted).ge.2)then
      if(line_unquoted(1:1).eq.'<'.and.line_unquoted(iend:iend).eq.'>')then       ! remove < and > from filename
         line_unquoted=line_unquoted(2:iend-1)
      endif
   endif

   if(iunit.eq.5.or.line_unquoted.eq.'@')then                   ! assume this is stdin
      G_iocount=G_iocount+1
      G_file_dictionary(G_iocount)%unit_number=5
      G_file_dictionary(G_iocount)%filename=line_unquoted
      return
   endif

   call findit(line_unquoted)

   open(unit=iunit,file=trim(line_unquoted),iostat=ios,status='old',action='read',iomsg=message)
   if(ios.ne.0)then
      call show_state(msg='OPEN IN INCLUDE')
      call write_err(message)
      call stop_prep("*prep* ERROR(057) - FAILED OPEN OF INPUT FILE("//v2s(iunit)//"):"//trim(line_unquoted))
   else
      rewind(unit=iunit)
      G_iocount=G_iocount+1
      if(G_iocount.gt.size(G_file_dictionary))then
         call stop_prep('*prep* ERROR(058) - INPUT FILE NESTING TOO DEEP:'//trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=iunit
      G_file_dictionary(G_iocount)%filename=line_unquoted
      G_file_dictionary(G_iocount)%line_number=0
   endif

end subroutine include
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine post(parcel_name)  !@(#)post(3f): switch to scratch file defined by PARCEL
implicit none
character(len=*),intent(in)  :: parcel_name
integer                      :: ifound
integer                      :: ios
character(len=4096)          :: message
integer                      :: i
   ifound=-1
   do i=1,G_parcelcount
      if(G_parcel_dictionary(i)%name.eq.parcel_name)then
         ifound=G_parcel_dictionary(i)%unit_number
         exit
      endif
   enddo
   if(ifound.eq.-1)then
      call stop_prep('*prep* ERROR(059) - PARCEL NAME NOT DEFINED:'//trim(G_source))
   else
      inquire(unit=ifound,iostat=ios)
      rewind(unit=ifound,iostat=ios,iomsg=message)
      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(060) - ERROR REWINDING PARCEL:'//trim(G_source)//':'//trim(message))
      endif

      if(G_debug)then
         do
            read(ifound,'(a)',iostat=ios)message
            if(ios.ne.0)exit
            write(stdout,*)'>>>'//trim(message)
         enddo
         rewind(unit=ifound,iostat=ios,iomsg=message)
      endif

      G_iocount=G_iocount+1
      if(G_iocount.gt.size(G_file_dictionary))then
         call stop_prep('*prep* ERROR(061) - INPUT FILE NESTING TOO DEEP:'//trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=ifound
      G_file_dictionary(G_iocount)%filename=parcel_name
      G_file_dictionary(G_iocount)%line_number=0
   endif
end subroutine post
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine findit(line) !@(#)findit(3f): look for filename in search directories if name does not exist and return modified name
character(len=G_line_length)    :: line
character(len=G_line_length)    :: filename
logical                         :: file_exist
integer                         :: i
integer                         :: iend_dir

   inquire(file=trim(line), exist=file_exist)                    ! test if input filename exists
   if(file_exist)then                                            ! if file exits then return filename
      return
   endif

   if(G_inc_count.gt.0)then                                      ! if search directories have been specified search for file
      do i=1,G_inc_count
         iend_dir=len_trim(G_inc_files(i))
         if(G_inc_files(i)(iend_dir:iend_dir).ne.'/')then
            filename=G_inc_files(i)(:iend_dir)//'/'//trim(line)
         else
            filename=G_inc_files(i)(:iend_dir)//trim(line)
         endif
         inquire(file=trim(filename), exist=file_exist)
         if(file_exist)then                                      ! if find filename exit
            line=filename
            return
         endif
      enddo
   else                                                          ! file did not exist and no search directories have been specified
      filename=trim(line)
   endif

   call stop_prep("*prep* ERROR(062) - MISSING INPUT FILE:"//trim(filename))

end subroutine findit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine opens()                   !@(#)opens(3f): use expression on command line to  open input files

integer,parameter                     :: n=50                       ! maximum number of tokens to look for
character(len=G_line_length)          :: array(n)                   ! the array to fill with tokens
character(len=1)                      :: dlim=' '                   ! string of single characters to use as delimiters

integer                               :: icount                     ! how many tokens are found
integer                               :: ibegin(n)                  ! starting column numbers for the tokens in INLINE
integer                               :: iterm(n)                   ! ending column numbers for the tokens in INLINE
integer                               :: ilen                       ! is the position of last non‐blank character in INLINE
character(len=G_line_length)          :: in_filename1=''            ! input filename, default is stdin
character(len=G_line_length)          :: in_filename2=''            ! input filename, default is stdin
integer                               :: i, ii
integer                               :: ivalue
character(len=G_line_length)          :: dir                        ! directory used by an input file

   in_filename1(:G_line_length)  = sget('_prep_i')                  ! get values from command line
   in_filename2(:G_line_length)  = sget('prep_i')                   ! get values from command line
   if(in_filename1.ne.''.and.in_filename2.eq.in_filename1)then
      in_filename2=''
   endif
   if(in_filename2.eq.'')then                                       ! read stdin if no -i on command line
      in_filename2  = '@'
   endif

   ! break command argument prep_i into single words
   call delim(adjustl(trim(sget('_prep_i'))//' '//trim(in_filename2)),array,n,icount,ibegin,iterm,ilen,dlim)
   ivalue=50                                ! starting file unit to use
   do i=icount,1,-1
      G_source='$include '//trim(array(i))  ! for messages
      call include(array(i),ivalue)
      ivalue=ivalue+1

      ALREADY: block                       ! store directory path of input files as an implicit directory for reading $INCLUDE files
         dir=dirname(array(i))
         do ii=1,G_inc_count
            if(G_inc_files(ii).eq.dir)exit ALREADY
         enddo
         G_inc_count=G_inc_count+1
         G_inc_count=min(G_inc_count,size(G_inc_files)) ! guard against too many files; !x! should warn on overflow
         G_inc_files(G_inc_count)=dir
      endblock ALREADY

   enddo

! >>>
!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount) .eq. ilen)" to see if you got to the end to warn if not all files include

end subroutine opens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine includes()         !@(#)includes(3f): use expression on command line to  get include directories

integer,parameter                     :: n=50                    ! maximum number of tokens to look for
character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters
integer                               :: ibegin(n)               ! starting column numbers for the tokens in G_inc_files
integer                               :: iterm(n)                ! ending column numbers for the tokens in G_inc_files
integer                               :: ilen                    ! is the position of last non‐blank character in G_inc_files

   ! G_inc_files is the array to fill with tokens
   ! G_inc_count is the number of tokens found

   ! break command argument prep_I into single words
   call delim(adjustl(trim(sget('_prep_I'))//' '//trim(sget('prep_I'))),G_inc_files,n,G_inc_count,ibegin,iterm,ilen,dlim)
end subroutine includes
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine defines()       !@(#)defines(3f): use expressions on command line to initialize dictionary and define variables
integer,parameter                     :: n=300                   ! maximum number of tokens to look for
character(len=G_line_length)          :: array(n)                ! the array to fill with tokens
character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters

integer                               :: icount                  ! how many tokens are found
integer                               :: ibegin(n)               ! starting column numbers for the tokens in INLINE
integer                               :: iterm(n)                ! ending column numbers for the tokens in INLINE
integer                               :: ilen                    ! is the position of last non‐blank character in INLINE
character(len=G_line_length)          :: in_define1=''           ! variable definition from environment variable
character(len=G_line_length)          :: in_define2=''           ! variable definition from environment variable and command
integer                               :: i

   if(allocated(keywords))deallocate(keywords)
   if(allocated(values))deallocate(values)
   if(allocated(counts))deallocate(counts)
   allocate(character(len=0) :: keywords(0))
   allocate(character(len=0) :: values(0))
   allocate(counts(0))

   in_define1=sget('_prep_oo')
   in_define2=sget('prep_oo')
   if(in_define1.ne.''.and.in_define2.eq.in_define1)then            ! if duplicates remove one
      in_define2=''
   endif
   ! break command argument prep_oo into single words
   call delim(adjustl(trim(in_define1)//' '//trim(in_define2))//' '//trim(sget('prep_D')),array,n,icount,ibegin,iterm,ilen,dlim)
   do i=1,icount
      G_source='$redefine '//trim(array(i))
      call cond() ! convert variable name into a "$define variablename" directive and process it
   enddo

!   If ARRAY(N) fills before reaching the end of the line the routine stops.
!   Check "if(iend(icount) .eq. ilen)" to see if you got to the end.

end subroutine defines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop(opts)                    !@(#)stop(3f): process stop directive
character(len=*),intent(in)  :: opts
integer                      :: ivalue
character(len=:),allocatable :: message
integer                      :: iend

! CHECK COMMAND SYNTAX
   if(opts.eq.'')then
      call stop_prep('',stop_value=1)
   else
      iend=index(opts,' ')
      if(iend.eq.0)then
         iend=len_trim(opts)
         message=' '
      else
         message=unquote(trim(opts(iend:)))
         write(stderr,'(a)')message
         call flushit()
      endif

      ivalue=get_integer_from_string(opts(:iend))

      if(ivalue.eq.0)then
         if(.not.G_debug)stop
      elseif(message.eq.'')then
         call stop_prep('',stop_value=ivalue)
         !call stop_prep('*prep* ERROR(050) - UNEXPECTED "STOP" VALUE='//trim(opts)//'. FROM:'//trim(G_source))
      else
         if(.not.G_debug)stop ivalue
      endif

   endif
end subroutine stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_prep(message,stop_value)                   !@(#)stop_prep(3f): write MESSAGE to stderr and exit program
character(len=*),intent(in)  :: message
integer,optional :: stop_value
integer :: stop_value_local
   stop_value_local=1
   if( present(stop_value) )stop_value_local=stop_value
   !call write_err(message)
   call write_err(trim(G_SOURCE))
   call show_state(msg=message)
   if(.not.G_debug)stop stop_value_local
end subroutine stop_prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine warn_prep(message)                   !@(#)warn_prep(3f): write MESSAGE to stderr and and continue program
character(len=*),intent(in)  :: message
   call write_err(message)
   call write_err(trim(G_SOURCE))
end subroutine warn_prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

! This documentation is a combination of
!    o the original Lahey documentation of fpp(1) from "LAHEY FORTRAN REFERENCE MANUAL"; Revision C, 1992;
!    o documentation for the features subsequently added to the program.
!    o examination of the code.

subroutine help_usage(l_help) !@(#)help_usage(3f): prints help information
implicit none
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
!-------------------------------------------------------------------------------
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   prep(1) - [DEVELOPER] preprocess FORTRAN source files                        ',&
'   (LICENSE:MIT)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   prep [[-D] define_list]                                                      ',&
'        [-I include_directories]                                                ',&
'        [-i input_file(s)]                                                      ',&
'        [-o output_file]                                                        ',&
'        [--system]                                                              ',&
'        [--type FILE_TYPE | --start START_STRING --stop STOP_STRING]            ',&
'        [--prefix character|ADE]                                                ',&
'        [--keeptabs]                                                            ',&
'        [--noenv]                                                               ',&
'        [--width n]                                                             ',&
'        [-d ignore|remove|blank]                                                ',&
'        [--comment default|doxygen|ford|none]                                   ',&
'        [--ident]                                                               ',&
'        [--verbose]                                                             ',&
'        [--version]                                                             ',&
'        [--help]                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   A preprocessor performs operations on input files before they are passed to  ',&
'   a compiler, including conditional selection of lines based on directives     ',&
'   contained in the file. This makes it possible to use a single source file    ',&
'   even when different code is required for different execution environments.   ',&
'                                                                                ',&
'   The prep(1) preprocessor has additional features that allow documentation    ',&
'   in the same file as the source and the generation of generic code using a    ',&
'   simple templating technique. The basic directives ....                       ',&
'                                                                                ',&
'   * Conditionally output parts of the source file (controlled by expressions   ',&
'     on the directives $IF, $IFDEF, $IFNDEF, and $ENDIF. The expressions may    ',&
'     include variables defined on the command line or via the directives        ',&
'     $DEFINE, and $UNDEFINE).                                                   ',&
'                                                                                ',&
'   * Include other files (provided by directive $INCLUDE).                      ',&
'                                                                                ',&
'   * Define parcels of text that may be replayed multiple times with            ',&
'     expansion, allowing for basic templating (controlled by directives         ',&
'     $PARCEL/$ENDPARCEL and $POST). The mechanism supported is to replace       ',&
'     text of the form ${NAME} with user-supplied strings similar to the         ',&
'     POSIX shell (controlled by directives $SET, $USET and $IMPORT).            ',&
'                                                                                ',&
'   * Filter blocks of text and convert them to comments, a CHARACTER array,     ',&
'     Fortran WRITE statements, ... (provided by the $BLOCK directive.)          ',&
'                                                                                ',&
'     The blocks of text may also be written to a file and executed, with        ',&
'     stdout captured and included in the prep(1) output file.                   ',&
'                                                                                ',&
'     Blocked text may optionally be simultaneously written to a separate file,  ',&
'     typically for use as documentation.                                        ',&
'                                                                                ',&
'   * Call system commands (using the $SYSTEM directive).                        ',&
'                                                                                ',&
'   * Generate multiple output files from a single input file (using $OUTPUT).   ',&
'                                                                                ',&
'   * Record the parameters used and the date and time executed as Fortran       ',&
'     comments in the output (using $SHOW).                                      ',&
'                                                                                ',&
'   * Cause an error (controlled by directive $STOP or $ERROR) and produce       ',&
'     messages on stderr (using $MESSAGE).                                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   define_list, -D define_list  An optional space-delimited list of expressions ',&
'                                used to define variables before file processing ',&
'                                commences.                                      ',&
'                                                                                ',&
'   -i input_files               The default input file is stdin. Filenames are  ',&
'                                space-delimited. In a list, @ represents stdin. ',&
'                                                                                ',&
'   The suggested suffix for Fortran input files is ".ff" for code files unless  ',&
'   they contain $SYSTEM directives in which case ".FF" is preferred. $INCLUDE   ',&
'   files should use ".ffinc" and ".FFINC" if they include prep(1) directives.   ',&
'   This naming convention is not required.                                      ',&
'                                                                                ',&
'   Files may also end in supported suffixes such as ".md", as explained under   ',&
'   the --type option description.                                               ',&
'                                                                                ',&
'   -o output_file               The default output file is stdout.              ',&
'                                                                                ',&
'   -I include_directories  The directories to search for files specified on     ',&
'                           $INCLUDE directives.                                 ',&
'                                                                                ',&
'   --system         Allow system commands on $SYSTEM directives to be executed. ',&
'                                                                                ',&
'   --type FILETYPE  This flag indicates to skip input lines until after a       ',&
'                    specific start string is encountered and to stop once a     ',&
'                    specific end string is found, left-justified on lines by    ',&
'                    themselves.                                                 ',&
'                                                                                ',&
'                        FileType  Start_String            Stop_String           ',&
'                        --------  ------------            -----------           ',&
'                        md        ```fortran              ```                   ',&
'                        html      <xmp>                   </xmp>                ',&
'                        tex       \begin{minted}{Fortran} \end{minted}          ',&
'                        auto                                                    ',&
'                        none                                                    ',&
'                                                                                ',&
'                    The default type is "auto", in which case files will be     ',&
'                    processed according to their file suffix.                   ',&
'                                                                                ',&
'                    This allows for easily extracting code from common document ',&
'                    formats. This is particularly useful with extended markdown ',&
'                    formats, allowing for code source to be easily documented   ',&
'                    and for tests in documents to be able to be extracted and   ',&
'                    tested. "auto" switches processing mode depending on input  ',&
'                    file suffix, treating supported file suffixes               ',&
'                    ("md","html","tex") appropriately.                          ',&
'                                                                                ',&
'   --start STRING   Same as --type except along with --stop allows for custom   ',&
'                    strings to be specified.                                    ',&
'                                                                                ',&
'   --stop STRING    Same as --type except along with --start allows for custom  ',&
'                    strings to be specified.                                    ',&
'                                                                                ',&
'   --comment        Try to style comments generated in $BLOCK COMMENT blocks    ',&
'                    for other utilities such as doxygen. Default is to          ',&
'                    prefix lines with ''! ''. Allowed keywords are              ',&
'                    currently "default", "doxygen","none","ford".               ',&
'                    THIS IS AN ALPHA FEATURE AND NOT FULLY IMPLEMENTED.         ',&
'                                                                                ',&
'   --prefix ADE|letter  The directive prefix character. The default is "$".     ',&
'                        If the value is numeric it is assumed to be an ASCII    ',&
'                        Decimal Equivalent (Common values are 37=% 42=* 35=#    ',&
'                        36=$ 64=@).                                             ',&
'                                                                                ',&
'   --noenv          The $IFDEF and $IFNDEF directives test for an internal      ',&
'                    prep(1) variable and then an environment variable by        ',&
'                    default. This option turns off testing for environment      ',&
'                    variables.                                                  ',&
'                                                                                ',&
'   --keeptabs       By default tab characters are expanded assuming a stop has  ',&
'                    been set every eight columns; and trailing carriage-return  ',&
'                    are removed. Use this flag to prevent this processing from  ',&
'                    from occurring.                                             ',&
'                                                                                ',&
'   --ident          The output of the $IDENT directive is in the form of a      ',&
'                    comment by default. If this flag is set the output is       ',&
'                    of the form described in the $IDENT documentation           ',&
'                    so executables and object code can contain the metadata     ',&
'                    for use with the what(1) command. Note this generates an    ',&
'                    unused variable which some compilers might optimize         ',&
'                    away depending on what compilation options are used.        ',&
'                                                                                ',&
'   -d ignore|remove|blank  Enable special treatment for lines beginning         ',&
'                           with "d" or "D". The letter will be left as-is       ',&
'                           (the default); removed; or replaced with a blank     ',&
'                           character. This non-standard syntax has been         ',&
'                           used to support the optional compilation of          ',&
'                           "debug" code by many Fortran compilers when          ',&
'                           compiling fixed-format Fortran source.               ',&
'                                                                                ',&
'   --width n   Maximum line length of the output file. The default is 1024.     ',&
'               The parameter is typically used to trim fixed-format FORTRAN     ',&
'               code that contains comments or "ident" labels past column 72     ',&
'               when compiling fixed-format Fortran code.                        ',&
'                                                                                ',&
'   --verbose   All commands on a $SYSTEM directive are echoed to stderr with a  ',&
'               "+" prefix. Text following the string "@(#)" is printed to stderr',&
'               similar to the Unix command what(1) but is otherwise treated as  ',&
'               other text input. Additional descriptive messages are produced.  ',&
'                                                                                ',&
'   --version   Display version and exit                                         ',&
'                                                                                ',&
'   --help      Display documentation and exit.                                  ',&
'                                                                                ',&
'INPUT FILE SYNTAX                                                               ',&
'                                                                                ',&
'   The prep(1) preprocessor directives begin with "$" (by default) in column    ',&
'   one, and prep(1) will output no such lines. Other input is conditionally     ',&
'   written to the output file(s) based on the case-insensitive command names.   ',&
'                                                                                ',&
'   An exclamation character FOLLOWED BY A SPACE on most directives              ',&
'   begins an in-line comment that is terminated by an end-of-line. The space    ',&
'   is required so comments are not confused with C-style logical operators such ',&
'   as "!", which may NOT be followed by a space.                                ',&
'                                                                                ',&
' VARIABLES AND EXPRESSIONS                                                      ',&
'                                                                                ',&
'   INTEGER or LOGICAL expressions are used to conditionally select              ',&
'   output lines. An expression is composed of INTEGER and LOGICAL               ',&
'   constants, variable names, and operators. Operators are processed            ',&
'   as in Fortran and/or C expressions. The supported operators are ...          ',&
'                                                                                ',&
'       #-----#-----#-----#-----#-----#                #-----#-----#             ',&
'       |  +  |  -  |  *  |  /  |  ** | Math Operators #  (  |  )  | Grouping    ',&
'       #-----#-----#-----#-----#-----#                #-----#-----#             ',&
'       Logical Operators                                                        ',&
'       #-----#-----#-----#-----#-----#-----#-----#-----#-----#-----#------#     ',&
'       | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.|.EQV.|.NEQV.|     ',&
'       |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || | ==  |  !=  |     ',&
'       #-----#  != #-----#-----#-----#-----#-----#-----#-----#-----#------#     ',&
'             #-----#                                                            ',&
'       C-style operators NOT supported:   %,  <<,  >>, &,  ~                    ',&
'                                                                                ',&
'DIRECTIVES                                                                      ',&
'                                                                                ',&
' The directives fall into the following categories:                             ',&
'                                                                                ',&
' VARIABLE DEFINITION FOR CONDITIONALS                                           ',&
'   Directives for defining variables ...                                        ',&
'                                                                                ',&
'      $DEFINE   variable_name[=expression] [;...]          [! comment ]         ',&
'      $UNDEFINE|$UNDEF variable_name [;...]                [! comment ]         ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'      $DEFINE variable_name [=expression]; ... [! comment ]                     ',&
'                                                                                ',&
'   Defines a numeric or logical variable name and its value. The variable       ',&
'   names may subsequently be used in the expressions on the conditional output  ',&
'   selector directives $IF, $ELSEIF, $IFDEF, and $IFNDEF.                       ',&
'                                                                                ',&
'   If the result of the expression is ".TRUE." or ".FALSE." then the variable   ',&
'   will be of type LOGICAL, otherwise the variable is of type INTEGER (and the  ',&
'   expression must be an INTEGER expression or null). If no value is supplied   ',&
'   the variable is given the INTEGER value "1".                                 ',&
'                                                                                ',&
'   Variables are defined from the point they are declared in a $DEFINE          ',&
'   directive or the command line until program termination unless explicitly    ',&
'   undefined with a $UNDEFINE directive.                                        ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > $define A                        ! will have default value of "1"         ',&
'    > $define B = 10 - 2 * 2**3 / 3    ! integer expressions                    ',&
'    > $define C=1+1; D=(-40)/(-10)                                              ',&
'    > $define bigd= d .ge. a; bigb = ( (b >= c) && (b > 0) )  ! logical         ',&
'    > $if ( A + B ) / C .eq. 1                                                  ',&
'    >    (a+b)/c is one                                                         ',&
'    > $endif                                                                    ',&
'   Note expressions are not case-sensitive.                                     ',&
'                                                                                ',&
'       $UNDEFINE variable_name[; ...]                                           ',&
'                                                                                ',&
'   A symbol defined with $DEFINE can be removed with the $UNDEFINE directive.   ',&
'   Multiple names may be specified, preferably separated by semi-colons.        ',&
'                                                                                ',&
'   Basic globbing is supported, where "*" represents any string, and "?"        ',&
'   represents any single character.                                             ',&
'                                                                                ',&
'       DEFINED(variable_name[,...])                                             ',&
'                                                                                ',&
'   A special function called DEFINED() may appear only in a $IF or $ELSEIF.     ',&
'   If "variable_name" has been defined at that point in the source code,        ',&
'   then the function value is ".TRUE.", otherwise it is ".FALSE.". A name is    ',&
'   defined only if it has appeared in the source previously in a $DEFINE        ',&
'   directive or been declared on the command line.                              ',&
'   The names used in compiler directives are district from names in the         ',&
'   FORTRAN source, which means that "a" in a $DEFINE and "a" in a FORTRAN       ',&
'   source statement are totally unrelated.                                      ',&
'   The DEFINED() variable is NOT valid in a $DEFINE directive.                  ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    >        Program test                                                       ',&
'    > $IF .NOT. DEFINED (inc)                                                   ',&
'    >        INCLUDE "comm.inc"                                                 ',&
'    > $ELSE                                                                     ',&
'    >        INCLUDE "comm2.inc"                                                ',&
'    > $ENDIF                                                                    ',&
'    >        END                                                                ',&
'                                                                                ',&
'   The file, "comm.inc" will be included in the source if the variable          ',&
'   "inc", has not been previously defined, while INCLUDE "comm2.inc" will       ',&
'   be included in the source if "inc" has been defined.                         ',&
'                                                                                ',&
'   Predefined variables are                                                     ',&
'                                                                                ',&
'    SYSTEMON = .TRUE. if --system was present on the command line, else .FALSE. ',&
'                                                                                ',&
'    UNKNOWN = 0 LINUX   = 1 MACOS   = 2 WINDOWS = 3                             ',&
'    CYGWIN  = 4 SOLARIS = 5 FREEBSD = 6 OPENBSD = 7                             ',&
'    In addition OS is set to what the program guesses the system type is.       ',&
'                                                                                ',&
'     > $if OS == LINUX                                                          ',&
'     >    write(*,*)"System type is Linux"                                      ',&
'     > $elseif OS == WINDOWS                                                    ',&
'     >    write(*,*)"System type is MSWindows"                                  ',&
'     > $else                                                                    ',&
'     >    write(*,*)"System type is unknown"                                    ',&
'     > $endif                                                                   ',&
'                                                                                ',&
' CONDITIONAL CODE SELECTION                                                     ',&
'   directives for conditionally selecting input lines ...                       ',&
'                                                                                ',&
'       $IF  logical_integer-based expression |                                  ',&
'       $IFDEF [variable_name|environment_variable] |                            ',&
'       $IFNDEF [variable_name|environment_variable]         [! comment ]        ',&
'               { sequence of source statements}                                 ',&
'       [$ELSEIF|$ELIF logical_integer-based expression      [! comment ]        ',&
'               { sequence of source statements}]                                ',&
'       [$ELSE                                               [! comment ]        ',&
'               { sequence of source statements}]                                ',&
'       $ENDIF                                               [! comment ]        ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $IF/$ELSEIF/$ELSE/$ENDIF directives ...                                  ',&
'                                                                                ',&
'   Each of these control lines delineates a block of source lines. If the       ',&
'   expression following the $IF is ".TRUE.", then the following lines of        ',&
'   source following are output. If it is ".FALSE.", and an $ELSEIF              ',&
'   follows, the expression is evaluated and treated the same as the $IF. If     ',&
'   the $IF and all $ELSEIF expressions are ".FALSE.", then the lines of         ',&
'   source following the optional $ELSE are output. A matching $ENDIF ends the   ',&
'   conditional block.                                                           ',&
'                                                                                ',&
'       $IFDEF/$IFNDEF directives ...                                            ',&
'                                                                                ',&
'   $IFDEF and $IFNDEF are special forms of the $IF directive that simply test   ',&
'   if a variable name is defined or not.                                        ',&
'                                                                                ',&
'   The expressions may optionally be enclosed in parenthesis and followed by    ',&
'   the keyword "THEN", ie. they may use Fortran syntax. For example, the        ',&
'   previous example may also be written as:                                     ',&
'                                                                                ',&
'     > $IF(OS .EQ. LINUX)THEN                                                   ',&
'     >    write(*,*)"System type is Linux"                                      ',&
'     > $ELSEIF(OS .EQ. WINDOWS)THEN                                             ',&
'     >    write(*,*)"System type is MSWindows"                                  ',&
'     > $ELSE                                                                    ',&
'     >    write(*,*)"System type is unknown"                                    ',&
'     > $ENDIF                                                                   ',&
'                                                                                ',&
'   Essentially, these are equivalent:                                           ',&
'                                                                                ',&
'       $IFDEF varname  ==> $IF DEFINED(varname)                                 ',&
'       $IFNDEF varname ==> $IF .NOT. DEFINED(varname)                           ',&
'                                                                                ',&
'   except that environment variables are tested as well by $IFDEF and $IFNDEF   ',&
'   if the --noenv option is not specified, but never by the function DEFINED(), ',&
'   allowing for environment variables to be selectively used or ignored.        ',&
'   The --noenv switch is therefore only needed for compatibility with fpp(1).   ',&
'   For the purposes of prep(1) an environment variable is defined if it is      ',&
'   returned by the system and has a non-blank value.                            ',&
'                                                                                ',&
' MACRO STRING EXPANSION AND TEXT REPLAY                                         ',&
'   Directives for defining replayable text blocks ...                           ',&
'                                                                                ',&
'       $PARCEL [blockname] / $ENDPARCEL                     [! comment ]        ',&
'       $POST     blockname(s)                               [! comment ]        ',&
'       $SET varname  string                                                     ',&
'       $UNSET varname(s)                                    [! comment ]        ',&
'       $IMPORT   envname[;...]                              [! comment ]        ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $PARCEL [blockname] / $ENDPARCEL                     [! comment ]        ',&
'                                                                                ',&
'   The block of lines between a "$PARCEL name" and "$ENDPARCEL" directive are   ',&
'   written to a scratch file WITHOUT expanding directives. the scratch file can ',&
'   then be read in with the $POST directive much like a named file can be with  ',&
'   $INCLUDE except the file is automatically deleted at program termination.    ',&
'                                                                                ',&
'       $POST     blockname(s)                               [! comment ]        ',&
'                                                                                ',&
'   Read in a scratch file created by the $PARCEL directive. Combined with       ',&
'   $SET and $IMPORT directives this allows you to replay a section of input     ',&
'   and replace strings as a simple templating technique, or to repeat lines     ',&
'   like copyright information or definitions of (obsolescent) Fortran COMMON    ',&
'   blocks, but contained in source files without the need for separate          ',&
'   INCLUDE files or error-prone repetition of the declarations.                 ',&
'                                                                                ',&
'       $SET varname  string                                                     ',&
'                                                                                ',&
'   If a $SET or $IMPORT directive defines a name prep(1) enters expansion mode. ',&
'   In this mode anywhere the string "${NAME}" is encountered in subsequent      ',&
'   output it is replaced by "string".                                           ',&
'                                                                                ',&
'   o values are case-sensitive but variable names are not.                      ',&
'   o expansion of a line may cause it to be longer than allowed by some         ',&
'     compilers. Automatic breaking into continuation lines does not occur.      ',&
'   o comments are not supported on a $SET directive because everything past the ',&
'     variable name becomes part of the value.                                   ',&
'   o The pre-defined values $FILE, $LINE, $DATE, and $TIME ( for input file,    ',&
'     line in input file, date and time ) are NOT ACTIVE until at least one      ',&
'     one $SET or $IMPORT directive is processed. That is, unless a variable     ',&
'     is defined no ${NAME} expansion occurs.                                    ',&
'   o The time and date refers to the time of processing, not the time of        ',&
'     compilation or loading.                                                    ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > $set author  William Shakespeare                                          ',&
'    > write(*,*)''By ${AUTHOR}''                                                ',&
'    > write(*,*)''File ${FILE}''                                                ',&
'    > write(*,*)''Line ${LINE}''                                                ',&
'    > write(*,*)''Date ${DATE}''                                                ',&
'    > write(*,*)''Time ${TIME}''                                                ',&
'   ...                                                                          ',&
'                                                                                ',&
'       $SET varname(s)                                                          ',&
'                                                                                ',&
'   Unset variables set with the $SET directive.                                 ',&
'                                                                                ',&
'       $IMPORT   envname[;...]                              [! comment ]        ',&
'                                                                                ',&
'   The values of environment variables may be imported just like their names    ',&
'   and values were used on a $SET directive. The names of the variables are     ',&
'   case-sensitive in regards to obtaining the values, but the names become      ',&
'   case-insensitive in prep(). That is, "import home" gets the lowercase        ',&
'   environment variable "home" and then sets the associated value for the       ',&
'   variable "HOME" to the value.                                                ',&
'                                                                                ',&
'    > $import HOME USER                                                         ',&
'    > write(*,*)''HOME ${HOME}''                                                ',&
'    > write(*,*)''USER ${USER}''                                                ',&
'                                                                                ',&
' EXTERNAL FILES                                                                 ',&
'   Directives for reading and writing external files ...                        ',&
'                                                                                ',&
'       $OUTPUT   filename  [--append]                          [! comment ]     ',&
'       $INCLUDE filename                                                        ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $OUTPUT   filename  [--append]                          [! comment ]     ',&
'                                                                                ',&
'   Specifies the output file to write to. This overrides the initial output file',&
'   specified with command line options. If no output filename is given          ',&
'   prep(1) reverts back to the initial output file. "@" is a synonym for stdout.',&
'                                                                                ',&
'   Files are open at the first line by default. Use the --append switch to      ',&
'   append to the end of an existing file instead of overwriting it.             ',&
'                                                                                ',&
'       $INCLUDE filename                                                        ',&
'                                                                                ',&
'   Read in the specified input file. Fifty (50) nesting levels are allowed.     ',&
'   Following the tradition of cpp(1) if "<filename>" is specified the file is   ',&
'   only searched for relative to the search directories, otherwise it is        ',&
'   searched for as specified first. Double-quotes in the filename are treated   ',&
'   as in Fortran list-directed input.                                           ',&
'                                                                                ',&
' TEXT BLOCK FILTERS                                                             ',&
'   (--file is ignored unless $PREP_DOCUMENT_DIR is set)                         ',&
'                                                                                ',&
'      $BLOCK   [null|comment|write|variable [--varname NAME]|                   ',&
'               set|system|message|define                                        ',&
'               help|version] [--file NAME [--append]]      [! comment ]         ',&
'      $ENDBLOCK                                            [! comment ]         ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'   $BLOCK has several forms but in all cases operates on a block of lines:      ',&
'                                                                                ',&
'     basic filtering:                                                           ',&
'      $BLOCK [comment|null|write                 [--file NAME [--append]]       ',&
'     creating a CHARACTER array:                                                ',&
'      $BLOCK VARIABLE --varname NAME             [--file NAME [--append]]       ',&
'     block versions of prep(1) commands:                                        ',&
'      $BLOCK set|system|message|define           [--file NAME [--append]]       ',&
'     specialized procedure construction:                                        ',&
'      $BLOCK help|version                        [--file NAME [--append]]       ',&
'                                                                                ',&
'      NULL:      Do not write into current output file                          ',&
'      COMMENT:   write text prefixed by an exclamation and a space or according ',&
'                 to the style selected by the --comment style selected on the   ',&
'                 command line.                                                  ',&
'      WRITE:     write text as Fortran WRITE(3f) statements                     ',&
'                 The Fortran generated is free-format. It is assumed the        ',&
'                 output will not generate lines over 132 columns.               ',&
'      VARIABLE:  write as a text variable. The name may be defined using        ',&
'                 the --varname switch. Default name is "textblock".             ',&
'      MESSAGE:   All the lines in the block are treated as options to $MESSAGE  ',&
'      SET:       All the lines in the block are treated as options to $SET      ',&
'      DEFINE:    All the lines in the block are treated as options to $DEFINE   ',&
'      SYSTEM:    The lines are gathered into a file and executed by the shell   ',&
'                 with the stdout being written to a scratch file and then read  ',&
'      END:       End block of specially processed text                          ',&
'                                                                                ',&
'   special-purpose modes primarily for use with the M_kracken module:           ',&
'                                                                                ',&
'      HELP:      write text as a subroutine called HELP_USAGE                   ',&
'      VERSION:   write text as a subroutine called HELP_VERSION prefixing       ',&
'                 lines with @(#) for use with the what(1) command.              ',&
'                                                                                ',&
'   If the "--file NAME" option is present the text is written to the            ',&
'   specified file unfiltered except for string expansion. This allows           ',&
'   documentation to easily be maintained in the source file. It can be          ',&
'   tex, html, markdown or any plain text. The filename will be prefixed         ',&
'   with $PREP_DOCUMENT_DIR/doc/ . If the environment variable                   ',&
'   $PREP_DOCUMENT_DIR is not set the option is ignored.                         ',&
'                                                                                ',&
'   The --file output can subsequently easily be processed by other utilities    ',&
'   such as markdown(1) or txt2man(1) to produce man(1) pages and HTML documents.',&
'   $SYSTEM commands may follow the $BLOCK block text to optionally post-process ',&
'   the doc files.                                                               ',&
'                                                                                ',&
'   $ENDBLOCK ends the block.                                                    ',&
!!!!$! which is preferred; but a blank value or "END" on a $BLOCK directive does as well.
'                                                                                ',&
' IDENTIFIERS                                                                    ',&
'   Directives for producing metadata ...                                        ',&
'                                                                                ',&
'       $IDENT|$@(#) metadata [--language fortran|c|shell]      [! comment ]     ',&
'                                                                                ',&
'   $IDENT is a special-purpose directive useful to users of SCCS-metadata.      ',&
'   The string generated can be used by the what(1) command,                     ',&
'                                                                                ',&
'   When the command line option "--ident [LANGUAGE]" is specified this directive',&
'   writes a line using SCCS-metadata format of one of the following forms:      ',&
'                                                                                ',&
'     language:                                                                  ',&
'     fortran   character(len=*),parameter::ident="@(#)metadata"                 ',&
'     c         #ident "@(#)metadata"                                            ',&
'     shell     #@(#) metadata                                                   ',&
'                                                                                ',&
'   The default language is "fortran".                                           ',&
'                                                                                ',&
'   Depending on your compiler and the optimization level used when compiling,   ',&
'   the output string may not remain in the object files and executables created.',&
'                                                                                ',&
'   If the -ident switch is not specified, a Fortran comment line is generated   ',&
'   of the form                                                                  ',&
'                                                                                ',&
'       ! ident_NNN="@(#)this is metadata"                                       ',&
'                                                                                ',&
'   "$@(#)" is an alias for "$IDENT" so the source file itself will contain      ',&
'   SCCS-metadata so the metadata can be displayed with what(1) even for the     ',&
'   unprocessed files.                                                           ',&
'                                                                                ',&
'   Do not use the characters double-quote, greater-than, backslash (ie. ">\)    ',&
'   in the metadata to remain compatible with SCCS metadata syntax.              ',&
'   Do not use strings starting with " -" either.                                ',&
'                                                                                ',&
' INFORMATION                                                                    ',&
'   Informative directives for writing messages to stderr or inserting           ',&
'   state information into the output file ...                                   ',&
'                                                                                ',&
'       $SHOW [variable_name[;...]]                          [! comment ]        ',&
'       $MESSAGE  message_to_stderr                                              ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'       $MESSAGE  message_to_stderr                                              ',&
'                                                                                ',&
'   Write message to stderr.                                                     ',&
'   Note that messages for $MESSAGE do not treat "! " as starting a comment      ',&
'                                                                                ',&
'       $SHOW [variable_name[;...]]                          [! comment ]        ',&
'                                                                                ',&
'   Shows current state of prep(1); including variable names and values and      ',&
'   the name of the current input files. All output is preceded by an            ',&
'   exclamation character.                                                       ',&
'                                                                                ',&
'   If a list of defined variable names is present only those variables and      ',&
'   their values are shown.                                                      ',&
'                                                                                ',&
'   Basic globbing is supported, where "*" represents any string, and "?"        ',&
'   represents any single character.                                             ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    > prep A=10 B C D -o paper                                                  ',&
'    > $define z=22                                                              ',&
'    > $show B Z                                                                 ',&
'    > $show                                                                     ',&
'    > $show H*;*H;*H*! show beginning with "H", ending with "H", containing "H" ',&
'    > $stop 0                                                                   ',&
'    >                                                                           ',&
'    > !  B  =  1                                                                ',&
'    > !  Z  =  22                                                               ',&
'    > !================================================================         ',&
'    > !                                                                         ',&
'    > ! Current state of prep(1):(18:39 20 Jun 2021)                            ',&
'    > ! Total lines read ............... 2                                      ',&
'    > ! Conditional nesting level....... 0                                      ',&
'    > ! G_WRITE (general processing).... T                                      ',&
'    > ! G_LLWRITE (write input lines)... T                                      ',&
'    > ! Arguments ...................... A=10 B C D -o paper                    ',&
'    > ! Open files:                                                             ',&
'    > !    unit ! line number ! filename                                        ',&
'    > !       5 !           2 ! @                                               ',&
'    > ! INCLUDE directories:                                                    ',&
'    > !    .                                                                    ',&
'    > ! Variables:                                                              ',&
'    > !    $DEFINE UNKNOWN  =  0                                                ',&
'    > !    $DEFINE LINUX  =  1                                                  ',&
'    > !    $DEFINE MACOS  =  2                                                  ',&
'    > !    $DEFINE WINDOWS  =  3                                                ',&
'    > !    $DEFINE CYGWIN  =  4                                                 ',&
'    > !    $DEFINE SOLARIS  =  5                                                ',&
'    > !    $DEFINE FREEBSD  =  6                                                ',&
'    > !    $DEFINE OPENBSD  =  7                                                ',&
'    > !    $DEFINE OS  =  1                                                     ',&
'    > !    $DEFINE A  =  10                                                     ',&
'    > !    $DEFINE B  =  1                                                      ',&
'    > !    $DEFINE C  =  1                                                      ',&
'    > !    $DEFINE D  =  1                                                      ',&
'    > !    $DEFINE Z  =  22                                                     ',&
'    > ! Parcels:                                                                ',&
'    > !================================================================         ',&
'                                                                                ',&
' SYSTEM COMMANDS                                                                ',&
'   Directives that execute system commands ...                                  ',&
'                                                                                ',&
'       $SYSTEM system_command                                                   ',&
'                                                                                ',&
'   If system command processing is enabled using the --system switch system     ',&
'   commands can be executed for such tasks as creating files to be read or to   ',&
'   further process documents created by $BLOCK. $SYSTEM directives are errors   ',&
'   by default; as you clearly need to ensure the input file is trusted before   ',&
'   before allowing commands to be executed. Commands that are system-specific   ',&
'   may need to be executed conditionally as well.                               ',&
'                                                                                ',&
'   Examples:                                                                    ',&
'                                                                                ',&
'    > $! build variable definitions using GNU/Linux commands                    ',&
'    > $SYSTEM echo system=`hostname` > compiled.h                               ',&
'    > $SYSTEM echo compile_time="`date`" >> compiled.h                          ',&
'    > $INCLUDE compiled.h                                                       ',&
'                                                                                ',&
'    > $if systemon      ! if --system switch is present on command line         ',&
'    > $!  obtain up-to-date copy of source file from HTTP server:               ',&
'    > $   SYSTEM wget http://repository.net/src/func.F90 -O - >_tmp.f90         ',&
'    > $   INCLUDE _tmp.f90                                                      ',&
'    > $   SYSTEM  rm _tmp.f90                                                   ',&
'    > $endif                                                                    ',&
'                                                                                ',&
'   System commands may also appear in a $BLOCK section. Combining several       ',&
'   features this uses the Linux getconf(1) command to write some lines          ',&
'   into a scratch file that are then read back in to define variables describing',&
'   the current platform.                                                        ',&
'                                                                                ',&
'    > $IF OS == LINUX                                                           ',&
'    > $                                                                         ',&
'    > $block system ! use getconf(1) command to get system values               ',&
'    > (                                                                         ',&
'    > echo LEVEL_2_CACHE_SIZE $(getconf LEVEL2_CACHE_SIZE)                      ',&
'    > echo LEVEL_3_CACHE_SIZE $(getconf LEVEL3_CACHE_SIZE)                      ',&
'    > ) >_getconf.inc                                                           ',&
'    > $endblock                                                                 ',&
'    > $block set                 ! read in output of getconf(1)                 ',&
'    > $include _getconf.inc                                                     ',&
'    > $endblock                                                                 ',&
'    > $system rm -f _getconf.inc ! cleanup                                      ',&
'    > $                                                                         ',&
'    > $ELSE                                                                     ',&
'    > $                                                                         ',&
'    > $error " ERROR: Not Linux. did not obtain system values"                  ',&
'    > $                                                                         ',&
'    > $ENDIF                                                                    ',&
'    > $! create code using values for this platform                             ',&
'    >    integer, parameter :: L2_CACHE_SZ=${LEVEL2_CACHE_SIZE}                 ',&
'    >    integer, parameter :: L3_CACHE_SZ=${LEVEL3_CACHE_SIZE}                 ',&
'                                                                                ',&
' PROGRAM TERMINATION                                                            ',&
'   Directives for stopping file processing (note there is no comment field):    ',&
'                                                                                ',&
'      $STOP     [stop_value ["message"]]                                        ',&
'      $QUIT     ["message"]                                                     ',&
'      $ERROR    ["message"]                                                     ',&
'                                                                                ',&
'   Details ...                                                                  ',&
'                                                                                ',&
'      $STOP     [stop_value ["message"]]                                        ',&
'                                                                                ',&
'   Stops the prep(1) program. The integer value will be returned as an exit     ',&
'   status value by the system where supported.                                  ',&
'                                                                                ',&
'   o A value of "0" causes normal program termination.                          ',&
'   o The default value is "1".                                                  ',&
'   o comments are not supported on these directives; the entire line following  ',&
'     the directive command becomes part of the message.                         ',&
'   o If a message is supplied it is displayed to stderr.                        ',&
'     If the value is not zero ("0") and no message is supplied the "$SHOW"      ',&
'     directive is called before stopping.                                       ',&
'   o "$QUIT" is an alias for "$STOP 0".                                         ',&
'   o "$ERROR" is a synonym for "$STOP 1"                                        ',&
'                                                                                ',&
'     >$IFNDEF TYPE                                                              ',&
'     >$STOP 10 "ERROR: ""TYPE"" not defined"                                    ',&
'     >$ENDIF                                                                    ',&
'                                                                                ',&
'LIMITATIONS                                                                     ',&
'                                                                                ',&
'   $IF constructs can be nested up to 20 levels deep. Note that using           ',&
'   more than two levels typically makes input files less readable.              ',&
'                                                                                ',&
'   $ENDBLOCK is required after a $BLOCK or --file FILENAME is not written.      ',&
'                                                                                ',&
'   Nesting of $BLOCK sections not allowed.                                      ',&
'   $INCLUDE may be nested fifty (50) levels.                                    ',&
'                                                                                ',&
'   Input files                                                                  ',&
'                                                                                ',&
'   o lines are limited to a maximum of 1024 columns. Text past the limit is     ',&
'     ignored.                                                                   ',&
'   o files cannot be concurrently opened multiple times                         ',&
'   o a maximum of 50 files can be nested by $INCLUDE                            ',&
'   o filenames cannot contain spaces on the command line.                       ',&
'                                                                                ',&
'   Variable names                                                               ',&
'                                                                                ',&
'   o are limited to 63 characters.                                              ',&
'   o must start with a letter (A-Z) or underscore(_).                           ',&
'   o are composed of the letters A-Z, digits 0-9 and _ and $.                   ',&
'   o 2048 variable names may be defined at a time.                              ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'                                                                                ',&
'  Define variables on command line:                                             ',&
'                                                                                ',&
'  Typically, variables are defined on the command line when prep(1) is          ',&
'  invoked but can be grouped together into small files that are included        ',&
'  with a $INCLUDE or as input files.                                            ',&
'                                                                                ',&
'    > prep HP size=64 -i hp_directives.dirs test.F90 -o test_out.f90            ',&
'                                                                                ',&
'  defines variables HP and SIZE as if the expressions had been on a             ',&
'  $DEFINE and reads file "hp_directives.dirs" and then test.F90.                ',&
'  Output is directed to test_out.f90                                            ',&
'                                                                                ',&
'  Basic conditionals:                                                           ',&
'                                                                                ',&
'   > $! set variable "a" if not specified on the prep(1) command.               ',&
'   > $IF .NOT.DEFINED(A)                                                        ',&
'   > $   DEFINE a=1  ! so only define the first version of SUB(3f) below        ',&
'   > $ENDIF                                                                     ',&
'   >    program conditional_compile                                             ',&
'   >       call sub()                                                           ',&
'   >    end program conditional_compile                                         ',&
'   > $! select a version of SUB depending on the value of variable "a"          ',&
'   > $IF a .EQ. 1                                                               ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the first SUB"                                      ',&
'   >    end subroutine sub                                                      ',&
'   > $ELSEIF a .eq. 2                                                           ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the second SUB"                                     ',&
'   >    end subroutine sub                                                      ',&
'   > $ELSE                                                                      ',&
'   >    subroutine sub                                                          ',&
'   >       print*, "This is the third SUB"                                      ',&
'   >    end subroutine sub                                                      ',&
'   > $ENDIF                                                                     ',&
'                                                                                ',&
'  Common use of $BLOCK                                                          ',&
'                                                                                ',&
'   > $!                                                                         ',&
'   > $BLOCK NULL --file manual.tex                                              ',&
'   > This is a block of text that will be ignored except it is optionally       ',&
'   > written to a $PREP_DOCUMENT_DIR/doc/ file when $PREP_DOCUMENT_DIR is set.  ',&
'   > $ENDBLOCK                                                                  ',&
'   >                                                                            ',&
'                                                                                ',&
'  This is a block of text that will be converted to comments and optionally     ',&
'  appended to a $PREP_DOCUMENT_DIR/doc/ file when $PREP_DOCUMENT_DIR is set.    ',&
'                                                                                ',&
'   > $BLOCK COMMENT--file conditional_compile.man                               ',&
'   > NAME                                                                       ',&
'   >    conditional_compile - basic example for prep(1) preprocessor.           ',&
'   > SYNOPSIS                                                                   ',&
'   >    conditional_example [--help] [--version]                                ',&
'   > DESCRIPTION                                                                ',&
'   >    This is a basic example program showing how documentation can be        ',&
'   >    used to generate program help text                                      ',&
'   > OPTIONS                                                                    ',&
'   >    --help     display this help and exit                                   ',&
'   >    --version  output version information and exit                          ',&
'   > $ENDBLOCK                                                                  ',&
'                                                                                ',&
'GENERAL TEMPLATING                                                              ',&
'  A parcel can be posted multiple times, changing the value of variables        ',&
'  before each post.                                                             ',&
'                                                                                ',&
'   > $PARCEL mysub                                                              ',&
'   > subroutine mysub_${TYPE}(a,b)                                              ',&
'   > use, intrinsic :: iso_fortran_env, only : &                                ',&
'   > & real_kinds, real32,real64,real128                                        ',&
'   > implicit none                                                              ',&
'   > integer,parameter  :: wp=${TYPE}                                           ',&
'   > real(kind=wp) :: a,b                                                       ',&
'   >    write(*,*)10.0_wp                                                       ',&
'   >    write(*,*) "this is for type ${TYPE}"                                   ',&
'   > end subroutine mysub_${TYPE}                                               ',&
'   >                                                                            ',&
'   > $ENDPARCEL                                                                 ',&
'   > $set type real32                                                           ',&
'   > $post mysub                                                                ',&
'   > $set type real64                                                           ',&
'   > $post mysub                                                                ',&
'   > $set type real128                                                          ',&
'   > $post mysub                                                                ',&
'                                                                                ',&
'NOTE                                                                            ',&
'  Not documented elsewhere, note that there is a developer flag (--debug) that  ',&
'  can be useful when learning prep(1) usage (but it should not be used in       ',&
'  production). Among other things it deactivates the termination of the program ',&
'  upon detection of an error. This mode thus allows for simple interactive use. ',&
'  In addition, when in this mode entering "$HELP" produces a cribsheet, which   ',&
'  may also be displayed by "prep --crib".                                       ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   MIT                                                                          ',&
'']

   WRITE(stdout,'(a)')(trim(help_text(i)),i=1,size(help_text))
   call stop('0')
endif
end subroutine help_usage

subroutine help_version(l_version) !@(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.

stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        prep(1f)>',&
'@(#)DESCRIPTION:    Fortran Preprocessor>',&
!'@(#)VERSION:        4.0.0: 20170502>',&
!'@(#)VERSION:        5.0.0: 20201219>',&
'@(#)VERSION:        8.1.1: 20220405>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE       https://github.com/urbanjost/prep.git/>',&
'']
   WRITE(stdout,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   call stop('0')
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine short_help(lun) !@(#)short_help(3f): prints help information
implicit none
integer,intent(in) :: lun
character(len=:),allocatable :: help_text(:)
integer                        :: i
help_text=[ CHARACTER(LEN=128) :: &
"EXPRESSIONS                                                                     ",&
"  numeric operators are +,-,*,/,**, (). Logical operators are                   ",&
"   >  | .EQ.| .NE.| .GE.| .GT.| .LE.| .LT.|.NOT.|.AND.| .OR.| .EQV.|.NEQV.|     ",&
"   >  |  == |  /= |  >= |  >  |  <= |  <  |  !  |  && |  || |  ==  |  !=  |     ",&
"  $DEFINE variable_name[=expression][;...]                                      ",&
'   > Predefined values are "OS", which is set to a guess of the system type, and',&
"   > UNKNOWN=0 LINUX=1 MACOS=2 WINDOWS=3 CYGWIN=4 SOLARIS=5 FREEBSD=6 OPENBSD=7.",&
"   > SYSTEMON is .TRUE. if --system is present on the command line, else .FALSE.",&
"  $UNDEFINE|$UNDEF variable_name[;...]                                          ",&
"CONDITIONAL CODE SELECTION:                                                     ",&
"  $IF logical_integer-based_expression | $IFDEF|$IFNDEF variable_or_envname     ",&
"  $IF DEFINED(varname[,...]) | $IF .NOT. DEFINED(varname[,...]) |               ",&
"  $ELSEIF|$ELIF logical_integer-based_expression                                ",&
"  $ELSE                                                                         ",&
"  $ENDIF                                                                        ",&
"MACRO STRING EXPANSION AND TEXT REPLAY:                                         ",&
"   > Unless at least one variable name is defined no ${NAME} expansion occurs.  ",&
"  $SET varname string                                                           ",&
"  $IMPORT envname[;...]                                                         ",&
"   > $set author  William Shakespeare                                           ",&
"   > $import HOME                                                               ",&
"   > write(*,*)'${AUTHOR} ${DATE} ${TIME} File ${FILE} Line ${LINE} HOME ${HOME}",&
"  $PARCEL [blockname] ... $ENDPARCEL ! a reuseable parcel of expandable text    ",&
"  $POST   blockname(s)  ! insert a defined parcel of text                       ",&
"EXTERNAL FILES (see $BLOCK ... --file also)                                     ",&
"  $OUTPUT filename [--append]                                                   ",&
"  $INCLUDE filename                                                             ",&
"TEXT BLOCK FILTERS (--file writes to $PREP_DOCUMENT_DIR/doc/NAME)               ",&
"  $BLOCK [comment|null|write|variable [--varname NAME]|set|system|message|      ",&
"         define|help|version][--file NAME [--append]] ... $ENDBLOCK             ",&
"INFORMATION                                                                     ",&
"  $MESSAGE message_to_stderr                                                    ",&
"  $SHOW [defined_variable_name][;...]                                           ",&
"SYSTEM COMMANDS (see also: $BLOCK SYSTEM)                                       ",&
"  $SYSTEM command                                                               ",&
"  $STOP [stop_value[ ""message""]] | $QUIT [""message""]| $ERROR [""message""]        "]
   WRITE(lun,'(a)')(trim(help_text(i)),i=1,size(help_text))
end subroutine short_help
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_out(line)  !@(#)writeout(3f):  write (most) source code lines to output file
character(len=*),intent(in)    :: line
integer                        :: istart

   if(G_verbose)then              ! echo "what" lines to stderr
      istart=index(line,'@(#)')
      if(istart.ne.0)then
         call write_err( '+ -->>'//trim(line(istart+4:)) )
      endif
   endif

   call www(line)
end subroutine write_out
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine www(line) !@(#)www(3f):  change line into a WRITE, HELP/VERSION, COMMENT output line
integer,parameter              :: linewidth=128
character(len=*),intent(in)    :: line
character(len=:),allocatable   :: buff
character(len=115)             :: chunk
integer                        :: ilen
integer                        :: ios
character(len=256)             :: message

   select case(trim(G_outtype))

   case('comment')                             ! write as a Fortran comment preceded by two explanations and a space
                                               ! will be written later at end of BLOCK section

   case('null')                                ! do not write

   case('set','replace')                       ! do not write
      call set(line)

   case('define')                              ! do not write
      call define(nospace(upper(line)))

   case('redefine')                            ! do not write
      call define(nospace(upper(line)))

   case('message')                             ! do not write
      call write_err(line)                     ! trustingly trim MESSAGE from directive

   case('system')
      write(G_scratch_lun,'(a)',iostat=ios,iomsg=message)trim(line)
      if(ios.lt.0)then
         call stop_prep('*prep* ERROR(068) - FAILED TO WRITE TO PROCESS:'//trim(line)//':'//trim(message))
      endif

   case('variable')
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")           ! change single quotes in input to two adjacent single quotes
      ilen=min(len_trim(buff),linewidth)       ! make all lines have at least linewidth characters for a more legible output
      write(G_iout,'("''",a,"'',&")') buff(:ilen)

   case('help')
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")           ! change single quotes in input to two adjacent single quotes
      ilen=max(len_trim(buff),linewidth)              ! make all lines have at least 80 characters for a more legible output
      write(G_iout,'("''",a,"'',&")') buff(:ilen)

   case('version')                             ! write version information with SCCS ID prefix for use with what(1) command
      write(G_iout,'("''@(#)",a,"'',&")')trim(line(:min(len_trim(line),128-1)))//'>'

                                               !x! should handle longer lines and split them
   case('write')                               ! convert string to a Fortran write statement to unit "IO"
      buff=trim(line)                          ! do not make a line over 132 characters. Trim input line if needed
      buff=buff//repeat(' ',max(linewidth,len(buff))) ! ensure space in buffer for substitute
      call substitute(buff,"'","''")
      write(G_iout,'(a)',advance='no')'write(io,''(a)'')'''
      chunk=buff
      write(G_iout,'(a)',advance='no')trim(chunk)
      write(G_iout,'(a)')''''

   case('','asis')
      write(G_iout,'(a)')trim(line(:min(len(line),G_iwidth)))

   case default
      call stop_prep('*prep* ERROR(063) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_source))
      call stop_prep('*prep* ERROR(064) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_outtype))

   end select

   if(G_MAN_COLLECT)then
      G_MAN=G_MAN//new_line('N')//trim(line)
   endif

   G_comment_count=G_comment_count+1

end subroutine www
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_err(msg) !@(#)M_verify::write_err(3f): writes a message to standard error using a standard f2003 method
character(len=*),intent(in) :: msg
integer                     :: ios

   write(stderr,'(a)',iostat=ios) trim(msg)
   call flushit()
end subroutine write_err
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect2(verb,init,pars,error_return) !@(#)dissect2(3f): convenient call to parse() -- define defaults, then process
!
character(len=*),intent(in)  :: verb             ! the name of the command to be reset/defined  and then set
character(len=*),intent(in)  :: init             ! used to define or reset command options; usually hard-set in the program.
character(len=*),intent(in)  :: pars             ! defines the command options to be set, usually from a user input file
integer                      :: ipars            ! length of the user-input string pars.
integer,intent(out),optional :: error_return
   ipars=len(pars)
   call dissect(verb,init,pars,ipars,error_return)
end subroutine dissect2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine import(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: names(:)
integer                      :: i
   names=sep(line,' ,;')
   do i=1,size(names)
      call set(names(i)//' '//get_env(names(i)))
   enddo
end subroutine import
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: temp
character(len=:),allocatable :: name
character(len=:),allocatable :: val
integer                      :: iend
integer                      :: i
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

  temp=adjustl(line)
  iend=index(temp,' ')
  iend=merge(len(temp),iend,iend.eq.0)
  name=adjustl(upper(temp(:iend)))
  if(name.ne.'')then
    if(len(temp).gt.iend)then
       val=temp(min(iend+1,len(temp)):)
    else
       val=' '
    endif
    ! insert and replace entries
    call prep_update(name,val)
  endif

contains

function get(key) result(valout)
use M_list,      only : insert, locate, replace, remove                   ! Basic list lookup and maintenance
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

end subroutine set
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine prep_update(key,valin)
use M_list,      only : insert, locate, replace, remove                   ! Basic list lookup and maintenance
! call prep_update('a','the value')     ! update (add or replace) entry
!call prep_update('a')                  ! remove entry
!write(stderr,*)'get b=>',get('b') ! get value
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: valin
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val
if(present(valin))then
   val=valin
   ilen=len_trim(val)
   ! find where string is or should be
   call locate(keywords,key,place)
   ! if string was not found insert it
   if(place.lt.1)then
      call insert(keywords,key,iabs(place))
      call insert(values,val,iabs(place))
      call insert(counts,ilen,iabs(place))
   else
      call replace(values,val,place)
      call replace(counts,ilen,place)
   endif
else
   call locate(keywords,key,place)
   if(place.gt.0)then
      call remove(keywords,place)
      call remove(values,place)
      call remove(counts,place)
   endif
endif
end subroutine prep_update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_variables(line)
!@(#) brute force variable substitution. maybe add something like wordexp(3c) with command expansion only if --system?
! this is just to try the concept. Either use wordexp or an equivalent to replicate "here document" processing.
! note no automatic continuation of the line if it extends past allowed length, which for Fortran is currently 132 for free-format
! the way this is written it would do recursive substitution and does not know when there is just not a match
character(len=*)              :: line
character(len=:),allocatable  :: temp,search
integer,parameter             :: toomany=1000
integer                       :: i, j
character(len=4096)           :: scratch

if(index(line,'${').ne.0)then
   write(scratch,'(i0)')G_file_dictionary(G_iocount)%line_number

   call set('LINE ' // scratch)
   call set('FILE ' // G_file_dictionary(G_iocount)%filename )
   call set('TIME ' // getdate('time'))
   call set('DATE ' // getdate('cdate'))
   temp=trim(line)
   INFINITE: do i=1,toomany
      do j=1,size(keywords)
         if(index(temp,'${').ne.0)then
            search='${'//trim(keywords(j))//'}'
            temp=str_replace(temp,search,values(j)(:counts(j)),ignorecase=.true.)
         else
            exit INFINITE
         endif
      enddo
   enddo INFINITE
   line=temp
endif

end subroutine expand_variables
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!> Determine the OS type by guessing
subroutine get_os_type()
!!
!! At first, the environment variable `OS` is checked, which is usually
!! found on Windows. Then, `OSTYPE` is read in and compared with common
!! names. If this fails too, check the existence of files that can be
!! found on specific system types only.
!!
!! Returns OS_UNKNOWN if the operating system cannot be determined.
!!
!! calling POSIX or C routines would be far better, M_system::like system_uname(3f)
!! but trying to use portable Fortran. If assume compiled by certain compilers could
!! use their extensions as well. Most have a uname(3f) function.
!!
integer, parameter :: OS_UNKNOWN = 0
integer, parameter :: OS_LINUX   = 1
integer, parameter :: OS_MACOS   = 2
integer, parameter :: OS_WINDOWS = 3
integer, parameter :: OS_CYGWIN  = 4
integer, parameter :: OS_SOLARIS = 5
integer, parameter :: OS_FREEBSD = 6
integer, parameter :: OS_OPENBSD = 7
character(len=G_var_len) :: val
integer           :: r
logical           :: file_exists
character(len=80) :: scratch

   call define('UNKNOWN=0' )
   call define('LINUX=1' )
   call define('MACOS=2' )
   call define('WINDOWS=3' )
   call define('CYGWIN=4' )
   call define('SOLARIS=5' )
   call define('FREEBSD=6' )
   call define('OPENBSD=7' )

   r = OS_UNKNOWN
   ! Check environment variable `OS`.
   val=get_env('OS')
   if ( index(val, 'Windows_NT') > 0) then
       r = OS_WINDOWS
   else
      ! Check environment variable `OSTYPE`.
      val=get_env('OSTYPE')
      if (val.ne.'') then
          if (index(val, 'linux') > 0) then      ! Linux
              r = OS_LINUX
          elseif (index(val, 'darwin') > 0) then ! macOS
              r = OS_MACOS
          elseif (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then ! Windows, MSYS, MinGW, Git Bash
              r = OS_WINDOWS
          elseif (index(val, 'cygwin') > 0) then ! Cygwin
              r = OS_CYGWIN
          elseif (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then ! Solaris, OpenIndiana, ...
              r = OS_SOLARIS
          elseif (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then ! FreeBSD
              r = OS_FREEBSD
          elseif (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then ! OpenBSD
              r = OS_OPENBSD
          endif
      endif
   endif
   if(r.eq.OS_UNKNOWN)then
      inquire (file='/etc/os-release', exist=file_exists) ! Linux
      if (file_exists) r = OS_LINUX
      inquire (file='/usr/bin/sw_vers', exist=file_exists) ! macOS
      if (file_exists) r = OS_MACOS
      inquire (file='/bin/freebsd-version', exist=file_exists) ! FreeBSD
      if (file_exists) r = OS_FREEBSD
   endif
   scratch=' '
   write(scratch,'("OS=",i0)')r
   call define(scratch)
end subroutine get_os_type
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function ends_in(string) result(ending)
character(*), intent(in)  :: string
character(:), allocatable :: ending
integer                   :: n1
   n1=index(string,'.',back=.true.)
   if (n1 < 1 .or. n1.eq.len(string) ) then
       ending=''
   else
       ending=string(n1+1:)
   endif
end function ends_in
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flushit()
integer :: ios
      flush(unit=stdout,iostat=ios)
      flush(unit=stderr,iostat=ios)
end subroutine flushit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_fpp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program prep                                                 !@(#)prep(1f): preprocessor for Fortran/FORTRAN source code
use M_kracken95, only : kracken, lget, rget, iget, sget, kracken_comment
use M_strings,   only : notabs, isdigit, switch
use M_fpp

implicit none
character(len=G_line_length) :: out_filename=''           ! output filename, default is stdout
character(len=1)             :: prefix                    ! directive prefix character
character(len=1)             :: letterd                   !

character(len=G_line_length) :: line                      ! working copy of input line
logical                      :: keeptabs=.false.          ! flag whether to retain tabs and carriage returns or not
integer                      :: ilast
integer                      :: ios
character(len=1024)          :: cmd=' &
   & -i                          &
   & -D                          &
   & -I                          &
   & -o                          &
   & --prefix           36       &
   & --keeptabs         .false.  &
   & -d                 ignore   &
   & --help             .false.  &
   & --verbose          .false.  &
   & --system           .false.  &
   & --version          .false.  &
   & --crib             .false.  &
   & --debug            .false.  &
   & --noenv            .false.  &
   & --comment          COMMENT  &
   & --ident            .false.  &
   & --width            1024     &
   & --start            " "      &
   & --stop             " "      &
   & --type             auto     &
   & '
logical                       :: isscratch

                                                                 ! allow formatting comments for particular post-processors
   G_comment_style=get_env('PREP_COMMENT_STYLE')                 ! get environment variable for -comment switch
   if(G_comment_style.eq.'')G_comment_style='default'            ! if environment variable not set set default
   call substitute(cmd,'COMMENT',trim(G_comment_style))          ! change command line to have correct default
                                                                 ! this would actually allow any parameter after number
   G_comment='! '
   kracken_comment=G_comment
   call kracken('prep',cmd)                                      ! define command arguments, default values and crack command line

   G_inc_files=' '

   out_filename(:G_line_length) = sget('prep_o')
   if ( all(isdigit(switch(trim(sget('prep_prefix'))))) ) then   ! if all characters are numeric digits
      prefix = char(iget('prep_prefix'))                         ! assume this is an ADE
   else
      prefix = sget('prep_prefix')                               ! not a digit so not an ADE so assume a literal character
   endif
   G_ident=lget('prep_ident')                                    ! write IDENT as comment or CHARACTER variable
   G_iwidth                     = iget('prep_width')
   G_iwidth=max(0,G_iwidth)
   letterd(1:1)               = sget('prep_d')
   G_noenv=lget('prep_noenv')

   if(out_filename.eq.'')then                                    ! open output file
      G_iout=stdout
   elseif(out_filename.eq.'@')then
      G_iout=stdout
      G_IHELP=stdout
   else
      G_iout=60
      G_IHELP=60
      open(unit=60,file=out_filename,iostat=ios,action='write')
      if(ios.ne.0)then
         call stop_prep('*prep* ERROR(065) - FAILED TO OPEN OUTPUT FILE:'//trim(out_filename))
      endif
   endif
   G_iout_init=G_iout

   call help_version(lget('prep_version'))                 ! if version switch is present display version and exit
   call help_usage(lget('prep_help'))                      ! if help switch is present display help and exit
   if(lget('prep_crib'))then
      call short_help(stdout)
      stop
   endif
   G_debug=lget('prep_debug')                              ! turn on debug mode for developer

   keeptabs=lget('prep_keeptabs')
   G_verbose=lget('prep_verbose')                          ! set flag for special mode where lines with @(#) are written to stderr
   if(G_verbose)then
      call write_err('+ verbose mode on ')
   endif
   G_comment_style=lower(sget('prep_comment'))             ! allow formatting comments for particular post-processors
   G_system_on = lget('prep_system')                       ! allow system commands on $SYSTEM directives
   if(G_system_on)then
      call define('SYSTEMON=.TRUE.')
   else
      call define('SYSTEMON=.FALSE.')
   endif
   !TODO! have an auto mode where start and end are selected based on file suffix
   select case(sget('prep_type'))
   case('md','.md')
      G_extract_start='```fortran'
      G_extract_stop='```'
   case('html','.html','htm','.htm')
      ! flaw is HTML is not case sensitive
      G_extract_start='<xmp>'
      G_extract_stop='</xmp>'
   case('tex')
      G_extract_start='\begin{minted}{Fortran}'
      G_extract_stop='\end{minted}'
   case('auto')
      G_extract_start=''
      G_extract_stop=''
      G_extract_auto=.true.
      G_extract=.true.
   case('none')
      G_extract_start=''
      G_extract_stop=''
      G_extract_auto=.false.
      G_extract=.false.
   case default
      G_extract_start=trim(sget('prep_start'))
      G_extract_stop=trim(sget('prep_stop'))
   end select
   if(G_extract_start.ne.''.or.G_extract_stop.ne.'')G_extract=.true.

   call get_os_type()
   call defines()                                          ! define named variables declared on the command line
   call includes()                                         ! define include directories supplies on command line
   call opens()                                            ! convert input filenames into $include directives
   call auto()

   READLINE: do                                            ! read loop to read input file
      read(G_file_dictionary(G_iocount)%unit_number,'(a)',end=7) line
      if(G_extract)then                                    ! in extract mode
         if(line.eq.G_extract_start)then                   ! start extracting
            G_extract_flag=.true.
            cycle READLINE
         elseif(line.eq.G_extract_stop.and.G_extract_flag)then        ! stop extracting
            G_extract_flag=.false.
            cycle READLINE
         elseif(.not.G_extract_flag)then                   ! skip if not extracting
            cycle READLINE
         endif
      endif
      !TODO! should line count include skipped lines?
      G_io_total_lines=G_io_total_lines+1
      G_file_dictionary(G_iocount)%line_number=G_file_dictionary(G_iocount)%line_number+1

      if(keeptabs)then
         G_source=line
      else
         call notabs(line,G_source,ilast)                  ! expand tab characters and trim trailing ctrl-M from DOS files
      endif

      if(G_inparcel)then                                   ! do not expand lines stored in a parcel
      elseif(size(keywords).ne.0)then                      ! expand variables if any variable is defined, else skip for efficieny
         call expand_variables(G_source)                   ! expand ${NAME} strings
      endif

      select case (line(1:1))                              ! special processing for lines starting with 'd' or 'D'
      case ('d','D')
         select case(letterd(1:1))
         case('i')                                         ! ignore
         case('r')                                         ! remove
            cycle
         case('b',' ')                                     ! blank
            line(1:1)=' '
         case('c')                                         ! comment
            line(1:1)='C'
         case('e')                                         ! exclamation
            line(1:1)='!'
         end select
      end select

      if (line(1:1).eq.prefix.and.line(2:2).ne.'{') then   ! prefix must be in column 1 for conditional compile directive
         call cond()                                       ! process directive
      elseif (G_write) then                                ! if last conditional was true then write line
         call write_out(trim(G_source))                    ! write data line
      endif
      cycle

7     continue                                                      ! end of file encountered on input
      if(G_file_dictionary(G_iocount)%unit_number.ne.5)then
         inquire(unit=G_file_dictionary(G_iocount)%unit_number,iostat=ios,named=isscratch)
         if(.not.isscratch.and.(G_file_dictionary(G_iocount)%unit_number.gt.0))then
            close(G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         elseif(isscratch.or.(G_file_dictionary(G_iocount)%unit_number.lt.-1))then 
            rewind(unit=G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         endif
      endif

      G_iocount=G_iocount-1
      if(G_scratch_lun.ne.-1)then
         ios=filedelete(G_scratch_file//'.out')
         G_scratch_lun=-1
      endif

      if(G_iocount.lt.1)exit
      call auto() ! if in auto mode determine strings for new file

   enddo READLINE

   if (G_nestl.ne.0) then                                           ! check to make sure all if blocks are closed
      call stop_prep('*prep* ERROR(067) - $IF BLOCK NOT CLOSED.')
   endif
   call print_comment_block()

   contains

subroutine auto()
   if(G_extract_auto)then
      select case(ends_in(G_file_dictionary(G_iocount)%filename) )
      case('md','.md')
         G_extract_start='```fortran'
         G_extract_stop='```'
      case('tex')
         G_extract_start='\begin{minted}{Fortran}'
         G_extract_stop='\end{minted}'
      case('html','.html','htm','.htm')
         G_extract_start='<xmp>'
         G_extract_stop='</xmp>'
      case default
         G_extract_start=trim(sget('prep_start'))
         G_extract_stop=trim(sget('prep_stop'))
      end select
      if(G_extract_start.eq.''.and.G_extract_stop.eq.'')then
         G_extract=.false.
      else
         G_extract=.true.
      endif
   endif
end subroutine auto

end program prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!        ! create a character string dictionary
!        call table%set('B','bee')
!        call table%set('C','see')
!        call table%set('D','dee')
!        write(*,*)'A=',table%get('A')
!        write(*,*)'C=',table%get('C')
!        write(*,*)'notthere=',table%get('notthere')
!        call print_dict()
!        ! delete dictionary entries
!        call  table%del('A')
!        call  table%del('C')
!        call  table%del('z') ! a noop as there is no key of 'z'
!        call print_dict()
!        ! clear dictionary
!        call  table%clr()
!        call print_dict()
!      contains
!      subroutine print_dict()
!      integer :: i
!         ! the dictionary is just three arrays
!         write(*,'("DICTIONARY:")')
!         write(*,'(*(a,"==>","[",a,"]",/))') &
!         & (trim(table%key(i)),               &
!         & table%value(i)(:table%count(i)),    &
!         & i=1,size(table%key))
!         !
!      end subroutine print_dict
