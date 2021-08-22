!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!  @(#)prep: FORTRAN pre-processor
!  Fortran preprocessor originally based on public-domain FPP pre-processor from Lahey Fortran Code Repository
!     http://www.lahey.com/code.htm
!  Extensively rewritten since under a MIT License.
!     2013-10-03,2020-12-19,2021-06-12 : John S. Urban

module M_fpp                                                                 !@(#)M_fpp(3f): module used by prep program
USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT, OUTPUT_UNIT                          ! access computing environment ; Standard: Fortran 2003
use M_io,        only : get_tmp, dirname, uniq, fileopen, filedelete         ! Fortran file I/O routines
use M_kracken95, only : sget, dissect, lget                                  ! load command argument parsing module
use M_strings,   only : nospace, v2s, substitute, upper, lower, isalpha, split, delim, str_replace=>replace, sep, atleast
use M_list,      only : insert, locate, replace, remove                      ! Basic list lookup and maintenance
   implicit none

   logical,save                         :: debug=.false.

   integer,parameter                    :: num=2048                       ! number of named values allowed
   integer,public,parameter             :: G_line_length=4096             ! allowed length of input lines
   integer,public,parameter             :: G_var_len=31                   ! allowed length of variable names

   integer,public                       :: G_numdef=0                     ! number of defined variables in dictionary
   logical,public                       :: G_ident=.false.                ! whether to write IDENT as a comment or CHARACTER

   character(len=G_line_length),public  :: G_source                       ! original source file line
   character(len=G_line_length),public  :: G_outline                      ! message to build for writing to output

   character(len=G_var_len),public      :: G_defval(num)                  ! variable values in variable dictionary
   character(len=G_var_len),public      :: G_defvar(num)                  ! variables in variable dictionary

   type file_stack
      integer                              ::  unit_number
      integer                              ::  line_number=0
      character(len=G_line_length)         ::  filename
   end type
   type(file_stack),public              ::  G_file_dictionary(50)

   type parcel_stack
      integer                              ::  unit_number
      integer                              ::  line_number=0
      character(len=G_line_length)         ::  name
   end type
   type(parcel_stack),public            ::  G_parcel_dictionary(500)

   integer,save                         :: G_line_number=0
   logical,save,public                  :: G_asis=.false.
   logical,save,public                  :: G_expand=.false.
   integer,public                       :: G_iocount=0
   integer,public                       :: G_parcelcount=0
   integer,public                       :: G_io_total_lines=0
   integer,public                       :: G_iwidth                       ! maximum width of line to write on output unit
   logical,public                       :: G_noenv=.false.                ! ignore environment variables in $IFDEF and $IFNDEF

   integer,public                       :: G_iout                         ! output unit
   integer,save,public                  :: G_iout_init                    ! initial output unit
  !integer,public                       :: G_ihelp=ERROR_UNIT             ! output unit for help text
   integer,public                       :: G_ihelp=OUTPUT_UNIT            ! output unit for help text
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

   logical,save,public                  :: G_write_what=.false.           ! write strings after @(#) similar to what(1).
   logical,save,public                  :: G_system_on=.false.            ! allow system commands or not on $SYSTEM

   logical,public,save                  :: G_condop(0:G_nestl_max)        ! storage to keep track of previous write flags
   data G_condop(0:G_nestl_max) /.true.,G_nestl_max*.false./
   logical,public                       :: G_dc                           ! flag to determine write flag

   logical,public                       :: G_write=.true.                 ! whether non-if/else/endif directives should be processed
   logical,public                       :: G_llwrite=.true.               ! whether to write current line or skip it

   integer,public                       :: G_comment_count=0
   character(len=10),public             :: G_comment_style=' '
   character(len=:),allocatable,save    :: G_scratch_file
   integer,save                         :: G_scratch_lun=-1

   character(len=:),allocatable   :: keywords(:)
   character(len=:),allocatable   :: values(:)
   integer,allocatable            :: counts(:)


   contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cond()       !@(#)cond(3f): process conditional directive assumed to be in SOURCE '$verb...'
   character(len=G_line_length) :: line                    ! directive line with leading prefix character (default is $) removed
   character(len=G_line_length) :: verb                    ! first word of command converted to uppercase
   character(len=G_line_length) :: options                 ! everything after first word of command till end of line or !
   character(len=G_line_length) :: upopts                  ! directive line with leading prefix removed; uppercase; no spaces
   logical,save                 :: eb=.false.
   integer,save                 :: noelse=0
   integer                      :: verblen

   line=adjustl(G_source(2:))                              ! remove leading prefix and spaces from directive line

   if (index(line,'!').ne.0) then                          ! if directive contains an exclamation a comment is present
                                                           ! LIMITATION: EVEN MESSAGES CANNOT CONTAIN COMMENTS
      line=line(:index(line,'!')-1)                        ! trim trailing comment from directive
   endif

   verblen=index(line,' ')
   if(verblen.eq.0)then
      verblen=len(line)
      verb=line
      options=' '
   else
      verb=line(:verblen-1)
      options=adjustl(line(verblen+1:))
   endif
   verb=upper(verb)
   upopts=nospace(upper(options))                          ! remove spaces from directive

   !write(*,*)'G_SOURCE='//trim(g_source)
   !write(*,*)'LINE='//trim(line)
   !write(*,*)'VERB='//trim(verb)
   !write(*,*)'OPTIONS='//trim(options)
   !write(*,*)'UPOPTS='//trim(upopts)

   if(G_asis.and.VERB.eq.'PARCEL')then
      call parcel_case(options)                                       ! end parcel ignoring options
      return
   elseif(G_asis)then
      call write_out(trim(G_source))                                  ! write data line
      return
   elseif(G_write)then                                                ! if processing lines in a logically selected region
                                                                      ! process the directive
      select case(VERB)
      case('  ')                                                      ! entire line is a comment
      case('DEFINE');           call define(upopts,1)                 ! only process DEFINE if not skipping data lines
      case('REDEFINE');         call define(upopts,0)                 ! only process DEFINE if not skipping data lines
      case('UNDEF','UNDEFINE'); call undef(upopts)                    ! only process UNDEF if not skipping data lines
      case('INCLUDE');          call include(options,50+G_iocount)    ! Filenames can be case sensitive
      case('OUTPUT');           call output_case(options)             ! Filenames can be case sensitive
      case('PARCEL');           call parcel_case(upopts)
      case('POST');             call post(upopts)
      case('BLOCK');            call document(options)
      case('SET');              call set(options)
      case('IMPORT');           call import(options)
      case('IDENT','@(#)');     call ident(options)
      case('SHOW') ;            call debug_state(options)
      case('SYSTEM');           call exe()
      case('MESSAGE');          call stderr(G_source(2:))             ! trustingly trim MESSAGE from directive
      case('STOP');             call stop(upopts)
      case('QUIT');             call stop('0')
      end select
   endif
   select case(VERB)                                                  ! process logical flow control even if G_write is false

   case('DEFINE','INCLUDE','SHOW','STOP','QUIT')
   case('SYSTEM','UNDEF','UNDEFINE','MESSAGE','REDEFINE')
   case('OUTPUT','IDENT','@(#)','BLOCK','IMPORT')
   case('PARCEL','POST','SET')
   case(' ')

   case('ELSE','ELSEIF');  call else(verb,upopts,noelse,eb)
   case('ENDIF');          call endif(noelse,eb)
   case('IF');             call if(upopts,noelse,eb)
   case('IFDEF','IFNDEF'); call def(verb,upopts,noelse,eb)

   case default
      call stop_prep('*prep:cond* ERROR(001) - UNKNOWN COMPILER DIRECTIVE ['//trim(verb)//']: '//trim(G_SOURCE))
   end select
end subroutine cond
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine exe()                                 ! @(#)exe(3f): Execute the command line specified by the string COMMAND.
   character(len=G_line_length)  :: command      ! directive line with leading prefix and directive name removed
   character(len=G_line_length)  :: defineme     ! scratch string for writing a DEFINE directive in to return command status
   integer                       :: icmd=0
   integer                       :: cstat
   character(len=256)            :: sstat

   if(G_system_on)then
      command=adjustl(G_source(2:))                                                 ! remove $ from directive
      command=command(7:)                                                           ! trim SYSTEM from directive
      if(G_write_what)then
         call stderr('+'//command)
      endif

      ! not returning command status on all platforms
      call execute_command_line (command, exitstat=icmd,cmdstat=cstat,cmdmsg=sstat) ! execute system command

      if(icmd.ne.0)then                                                             ! if system command failed exit program
         call stop_prep('*prep:exe* ERROR(002) - SYSTEM COMMAND FAILED:'//v2s(icmd))
      endif
   else
      call stop_prep('*prep:exe* ERROR(003) - SYSTEM DIRECTIVE ENCOUNTERED BUT NOT ENABLED:'//trim(G_SOURCE))
   endif

   write(defineme,'("CMD_STATUS=",i8)')icmd
   defineme=nospace(defineme)
   call define(defineme,0)

end subroutine exe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine output_case(opts)                             !@(#)output_case(3f): process $OUTPUT directive
   character(len=*)              :: opts
   character(len=G_line_length)  :: filename             ! filename on $OUTPUT command
   character(len=20)             :: position
   integer                       :: ios
      call dissect2('output','-oo --append .false.',opts)  ! parse options and inline comment on input line
      filename=sget('output_oo')
      select case(filename)
      case('@')
         G_iout=6
      case(' ')                                          ! reset back to initial output file
         if(G_iout.ne.6.and.G_iout.ne.G_iout_init)then   ! do not close current output if it is stdout or default output file
            close(G_iout,iostat=ios)
         endif
         G_iout=G_iout_init
      case default
         G_iout=61
         close(G_iout,iostat=ios)
         if(lget('output_append'))then; position='append'; else; position='asis'; endif
         open(unit=G_iout,file=filename,iostat=ios,action='write',position=position)
         if(ios.ne.0)then
            call stop_prep('*perf:output_case* ERROR(004) - FAILED TO OPEN OUTPUT FILE:'//trim(filename))
         endif
      end select
   if(G_write_what)then
      write(ERROR_UNIT,'(a)')'*perf:output_case*: OUTPUT FILE CHANGED TO:'//trim(filename)
   endif
end subroutine output_case
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parcel_case(opts)                             !@(#)parcel_case(3f): process $PARCEL directive
character(len=*)              :: opts
character(len=G_line_length)  :: name                 ! name on $PARCEL command
integer                       :: ios
integer                       :: lun
character(len=256)            :: message
   call dissect2('parcel','-oo ',opts)  ! parse options and inline comment on input line
   name=sget('parcel_oo')
   if(name.eq.'')then
      G_asis=.false.
      G_iout=G_iout_init
   else
      open(newunit=lun,iostat=ios,action='readwrite',status='scratch',iomsg=message)
      if(ios.ne.0)then
         call stop_prep('*prep:parcel_case* ERROR(005) - FAILED TO OPEN PARCEL SCRATCH FILE:'//trim(name)//' '//trim(message))
      else
         G_parcelcount=G_parcelcount+1
         G_parcel_dictionary(G_parcelcount)%name=name
         G_parcel_dictionary(G_parcelcount)%unit_number=lun
         G_asis=.true.
         G_iout=lun
      endif
   endif
end subroutine parcel_case
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
   case('fortran')    !*! should make look for characters not allowed in metadata, continue over multiple lines, ...
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
         call stop_prep('*prep:exe* ERROR(006) - IDENT TOO LONG:'//trim(G_SOURCE))
      end select
   case('c')
         write(G_iout,'(a)')'#ident "@(#)'//text//'"'
   case default
         call stop_prep('*prep:exe* ERROR(007) - IDENT LANGUAGE UNKNOWN:'//trim(G_SOURCE))
   end select

end subroutine ident
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine define(opts,ireset)                              !@(#)define(3f): process 'DEFINE variablename[=expression]' directive
character(len=*),intent(in)    :: opts                      ! packed uppercase working copy of input line with leading $verb removed
integer,intent(in)             :: ireset                    ! 0= can redefine variable, anything else fail on redefine

   character(len=G_line_length):: temp                      ! scratch
   integer                     :: iequ                      ! location of "=" in the directive, if any
   integer                     :: j                         ! index thru variable dictionary to see if variable is already defined
   integer                     :: iname                     ! length of variable name
   integer                     :: istore                    ! location of variable name in dictionary

! CHECK COMMAND SYNTAX
   iequ=index(opts,'=')                                     ! find "=" in "variable_name=expression" if any
   if (opts(1:1).eq.' '.or.iequ.eq.len_trim(opts)) then     ! no variable name in packed string or string after = is null
      call stop_prep('*prep:define* ERROR(008) - INCOMPLETE STATEMENT:'//trim(opts))
   endif
   if (iequ.gt.G_var_len+1) then                            ! variable name too long
      call stop_prep('*prep:define* ERROR(009) - MISSPELLING OR NAME LENGTH EXCEEDS '//v2s(G_var_len)//' CHARACTERS:'//trim(opts))
   endif

   if(iequ.eq.0)then                                        ! find end of variable name
      iname=len_trim(opts)
   else
      iname=iequ-1
   endif

   call name(opts(:iname))                                  ! check that variable name is composed of allowed characters

   istore=0
   if (G_numdef.ne.0) then                                  ! test for redefinition of defined name
      do j=1,G_numdef
         if (opts(:iname).eq.G_defvar(j)) then
            istore=j
            if(ireset.ne.0)then                             ! fail if redefinitions are not allowed on this call
               call stop_prep('*prep:define* ERROR(010) - REDEFINITION OF DEFINED NAME INVALID:'//trim(opts))
            endif
         endif
      enddo
   endif

   if(istore.eq.0)then                                      ! new variable name
      G_numdef=G_numdef+1                                   ! increment number of defined variables
      istore=G_numdef
   endif
   if (iequ.eq.0) then                                      ! if no = then variable assumes value of 1
      G_defvar(istore)=opts                                 ! store variable name from line with no =value string
      temp='1'                                              ! set string to default value
   else                                                     ! =value string trails name on directive
      G_defvar(istore)=opts(:iequ-1)                        ! store variable name from line with =value string
      temp=opts(iequ+1:)                                       ! get expression
   endif

   call parens(temp)                                        !
   if (iequ.ne.0) then
      temp=opts(:iequ)//temp
   endif

   call math(temp,iequ+1,len_trim(opts))
   call doop(temp,iequ+1,len_trim(opts))
   call logic(temp,iequ+1,len_trim(opts))
   call getval(temp,iequ+1,len_trim(opts),G_defval(istore))

end subroutine define
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function getdate(name) result(s)         !@(#) getdate(3f): Function to write date and time into returned string
character(len=*),optional :: name

! PURPOSE - Return a string with the current date and time
character(len=*),parameter         :: month='JanFebMarAprMayJunJulAugSepOctNovDec'
character(len=*),parameter         :: fmt = '(I2.2,A1,I2.2,I3,1X,A3,1x,I4)'
character(len=*),parameter         :: cdate = '(A3,1X,I2.2,1X,I4.4)'
character(len=:),allocatable       :: s
character(len=80)                  :: line
integer,dimension(8)               :: v
character(len=10) :: name_

   call date_and_time(values=v)
   name_='prep'
   if(present(name))name_=name
   select case(lower(name_))
   case('prep') ! PREP_DATE="00:39  5 Nov 2013"
   write(line,fmt) v(5), ':', v(6), v(3), month(3*v(2)-2:3*v(2)), v(1)
   case('date')
   write(line,'(i4.4,"-",i2.2,"-",i2.2)') v(1),v(2),v(3)
   case('cdate')
   write(line,cdate) month(3*v(2)-2:3*v(2)), v(3), v(1)
   case('long')
   write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   case('time')
   write(line,'(i2.2,":",i2.2,":",i2.2)') v(5),v(6),v(7)
   case default
   write(line,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",sp,i0)') v(1),v(2),v(3),v(5),v(6),v(7),v(4)
   end select
   s=trim(line)
end function getdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine name(line)                                                 !@(#)name(3f): test for legal variable name
character(len=*)          :: line
   integer                :: i

   if (line(1:1).lt.'A'.or.line(1:1).gt.'Z'.and.line(1:1).ne.'_')then                         ! variable names start with a-z
     call stop_prep("*prepname* ERROR(016) -VARIABLE NAME DOES NOT START WITH ALPHAMERIC(OR GENERAL SYNTAX ERROR):"//trim(G_source))
   endif

   if(len_trim(line).gt.G_var_len)then
      call stop_prep('*prepname* ERROR(017) - VARIABLE NAME EXCEEDS '//v2s(G_var_len)//' CHARACTERS:'//trim(G_source))
   endif

   do i=2,len_trim(line)                                                 ! name uses $  _ and letters (A-Z) digits (0-9)
      if(line(i:i).ne.'$'.and.line(i:i).ne.'_'.and.     &
      & (line(i:i).lt.'A'.or.line(i:i).gt.'Z').and.     &
      & (line(i:i).lt.'0'.or.line(i:i).gt.'9')) then
       call stop_prep('*prepname* ERROR(018) -VARIABLE NAME CONTAINS UNALLOWED CHARACTER(OR GENERAL SYNTAX ERROR):'//trim(G_source))
      endif
   enddo

end subroutine name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine getval(line,ipos1,ipos2,value)     !@(#)getval(3f): get value from dictionary for given variable name or return input
character(len=G_line_length),intent(in)   :: line                           ! current(maybe partial) directive line
integer,intent(in)                        :: ipos1                          ! beginning position of variable name in LINE
integer,intent(in)                        :: ipos2                          ! ending position of variable name in LINE
character(len=G_var_len),intent(out)      :: value                          ! returned variable value

   character(len=G_line_length)           :: temp                           ! copy of substring being examined
   integer                                :: i
   integer                                :: ivalue

   temp=line(ipos1:ipos2)                                                   ! place variable name/value substring into TEMP

   if (temp(1:1).eq.' ')then                                                ! did not find expected variable name or value
      call stop_prep('*prepgetvalue* ERROR(019) - INCOMPLETE STATEMENT.'//trim(G_SOURCE))
   endif

   if (temp(1:1).ge.'A'.and.temp(1:1).le.'Z') then                          ! appears to be a variable name (not number or logical)

     value=temp(:G_var_len)
     do i=1,G_numdef                                                        ! find defined parameter in dictionary
        if (G_defvar(i).eq.value)exit
     enddo
     if (i.gt.G_numdef)then                                                 ! unknown variable name
        call stop_prep('*prep* ERROR(020) - UNDEFINED PARAMETER IN GETVAL:'//trim(G_source))
     endif
     value=G_defval(i)                                                      ! (trusted) value for variable name found in dictionary
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
subroutine undef(opts)                                     !@(#)undef(3f): process UNDEFINE directive
character(len=*)     :: opts                               ! directive with no spaces, leading prefix removed, and all uppercase
   integer                     :: ifound                   ! subscript for location of variable to delete
   integer                     :: i,j

! REMOVE VARIABLE IF FOUND IN VARIABLE NAME DICTIONARY
   if (len_trim(opts).eq.0) then                           ! if no variable name
      call stop_prep('*prepundef* ERROR(023) - INCOMPLETE STATEMENT:'//trim(G_source))
   endif

   ifound=-1                                               ! initialize subscript for variable name to be searched for to bad value
   do i=1,G_numdef                                         ! find defined variable to be undefined by searching dictionary
      if (G_defvar(i).eq.opts)then                         ! found the requested variable name
         ifound=i                                          ! record the subscript that the name was located at
         exit                                              ! found the variable so no longer any need to search remaining names
      endif
   enddo

   if (ifound.lt.1) then                                   ! variable name not found
      return                                               ! quietly ignore unknown name (or syntax error!)
   endif

   do j=ifound,G_numdef-1                                  ! remove variable name and value from list of variable names and values
     G_defvar(j)=G_defvar(j+1)                             ! replace the value to be removed with the one above it and then repeat
     G_defval(j)=G_defval(j+1)
   enddo

   G_numdef=G_numdef-1                                     ! decrement number of defined variables

end subroutine undef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine if(opts,noelse,eb)                              !@(#)if(3f): process IF and ELSEIF directives
character(len=*)                :: opts
integer,intent(out)             :: noelse
logical                         :: eb
   character(len=G_var_len)     :: value
   integer                      :: ios
   integer                      :: i

   noelse=0
   G_write=.false.

   G_nestl=G_nestl+1                                       ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif

   FIND_DEFINED: do                                        ! find and reduce all DEFINED() functions to ".TRUE." or ".FALSE."
      if (index(opts,'DEFINED(').ne.0) then                ! find a DEFINED() function
         call ifdef(opts,index(opts,'DEFINED('))           ! reduce DEFINED() function that was found
         opts=nospace(opts)                                ! remove any spaces from rewritten expression
         cycle                                             ! look for another DEFINED() function
      endif
      exit                                                 ! no remaining DEFINED() functions so exit loop
   enddo FIND_DEFINED

   call parens(opts)
   if (index(opts,'.').eq.0) then                          ! if line should be a variable only
      if (opts(1:1).ge.'A'.and.opts(1:1).le.'Z') then      ! check that variable name starts with a valid character
         call name(opts)                                   ! check that opts contains only a legitimate variable name
         value=opts(:G_var_len)                            ! set VALUE to variable name
         do i=1,G_numdef                                   ! find variable in variable dictionary
            if (G_defvar(i).eq.value) exit
         enddo
         if (i.gt.G_numdef) then                           ! if failed to find variable name
            call stop_prep('*prep* ERROR(024) - UNDEFINED PARAMETER IN IF:'//trim(G_source))
         endif
         read(G_defval(i),'(l4)',iostat=ios) G_dc          ! convert variable value to a logical
         if(ios.ne.0)then
            call stop_prep('*prep* ERROR(025) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
         endif
      else                                                 ! this should have been a variable name
         call stop_prep('*prep* ERROR(026) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
      endif
   else                                                    ! a period is present in the expression so it needs evaluated
      call eval(opts)                                      ! evaluate line
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
subroutine def(verb,opts,noelse,eb)                           !@(#)def(3f): process IFDEF and IFNDEF directives
character(len=*),intent(in)     :: verb
character(len=*),intent(in)     :: opts
integer,intent(out)             :: noelse
logical                         :: eb
   character(len=G_var_len)     :: value
   integer                      :: i
   integer                      :: istatus

   noelse=0
   G_write=.false.
   G_nestl=G_nestl+1                                 ! increment IF nest level
   if (G_nestl.gt.G_nestl_max) then
      call stop_prep('*prep* ABORT(bh) - "IF" BLOCK NESTING TOO DEEP, LIMITED TO '//v2s(G_nestl_max)//' LEVELS:'//trim(G_source))
   endif
   call name(opts)                                   ! check that opts contains only a legitimate variable name
   value=opts                                        ! set VALUE to variable name
   G_dc=.true.                                       ! initialize
   do i=1,G_numdef                                   ! find variable in variable dictionary
      if (G_defvar(i).eq.value) exit
   enddo
   if (i.gt.G_numdef) then                           ! if failed to find variable name
      G_dc=.false.
   endif
   if((.not.G_noenv).and.(.not.G_dc))then            ! if not found in variable dictionary check environment variables if allowed
      call get_environment_variable(trim(value),status=istatus)
      if(istatus.eq.0)then
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
subroutine ifdef(line,ipos1)                            !@(#)ifdef(3f): process and reduce DEFINED() function that was found
   character(len=G_line_length)   :: line
   character(len=G_line_length)   :: newl
   integer                        :: ipos1
   character(len=G_var_len)       :: ifvar
   integer                        :: i

   newl=line(ipos1+7:)

   if (len_trim(newl).eq.1.or.index(newl,')').eq.0.or. index(newl,')').eq.2)then
      call stop_prep("*prepifdef* ERROR(027) - INCOMPLETE STATEMENT."//trim(G_SOURCE))
   endif
   if (index(newl,')').gt.33)then
      call stop_prep("*prepifdef* ERROR(028) - MISSPELLING OR NAME LENGTH EXCEEDS "//v2s(G_var_len)//" CHARACTERS."//trim(G_source))
   endif
   ifvar= newl(2:index(newl,')')-1)
   if (newl(2:2).lt.'A'.or.newl(2:2).gt.'Z')then
      call stop_prep("*prepifdef* ERROR(029) - CONSTANT LOGICAL EXPRESSION REQUIRED."//trim(G_source))
   endif
   do i=3,index(newl,')')-1
      IF (NEWL(I:I).NE.'$'.AND.NEWL(I:I).NE.'_'.AND.(NEWL(I:I).LT.'A' &
       &  .OR.NEWL(I:I).GT.'Z').AND.(NEWL(I:I).LT.'0'                 &
       &  .or.newl(i:i).gt.'9')) then
         call stop_prep("*prepifdef* ERROR(030) - CONSTANT LOGICAL EXPRESSION REQUIRED."//trim(G_source))
      endif
   enddo

   G_dc=.false.
   line(ipos1:ipos1+6+index(newl,')'))='.FALSE.'

   do i=1,G_numdef                                        ! sequentially search for variable in variable dictionary
     if (G_defvar(i).eq.ifvar) then
       G_dc=.true.
       line(ipos1:ipos1+6+index(newl,')'))='.TRUE.'
       exit
     endif
   enddo

end subroutine ifdef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine else(verb,opts,noelse,eb)                       !@(#)else(3f): process else and elseif
character(len=*)              :: verb
character(len=*)              :: opts                      !
integer                       :: noelse
logical                       :: eb

if(debug)then
   write(*,*)'*ELSE* TOP'
   write(*,*)'        G_NESTL =',g_nestl
   write(*,*)'        EB      =',eb
   write(*,*)'        NOELSE  =',noelse
   write(*,*)'        G_WRITE =',g_write
   write(*,*)'        G_CONDOP=',g_condop
endif

   if(noelse.eq.1.or.G_nestl.eq.0) then                    ! test for else instead of elseif
      call stop_prep("*prepelse* ERROR(031) - MISPLACED $ELSE OR $ELSEIF DIRECTIVE:"//trim(G_SOURCE))
   endif
   if(verb.eq.'ELSE')then
      noelse=1
   endif
   if(.not.G_condop(G_nestl-1))return                      ! if was true so ignore else
   eb=.false.
   if(G_condop(G_nestl)) then
       eb=.true.
       G_write=.false.
   elseif(len_trim(opts).ne.0)then                         ! elseif detected
     G_nestl=G_nestl-1                                     ! decrease if level because it will be incremented in subroutine if
     call if(opts,noelse,eb)
   else                                                    ! else detected
     G_condop(G_nestl)=.true.
     G_write=.true.
   endif

if(debug)then
   write(*,*)'*ELSE* BOTTOM'
   write(*,*)'        G_NESTL =',g_nestl
   write(*,*)'        EB      =',eb
   write(*,*)'        NOELSE  =',noelse
   write(*,*)'        G_WRITE =',g_write
   write(*,*)'        G_CONDOP=',g_condop
endif
end subroutine else
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine endif(noelse,eb)                             !@(#)endif(3f): process ENDIF directive
integer,intent(out)           :: noelse
logical,intent(out)           :: eb

   if(debug)then
      write(*,*)'*ENDIF* TOP'
      write(*,*)'        G_NESTL =',g_nestl
      write(*,*)'        EB      =',eb
      write(*,*)'        NOELSE  =',noelse
      write(*,*)'        G_WRITE =',g_write
      write(*,*)'        G_CONDOP=',g_condop
   endif

   ! if no ELSE or ELSEIF present insert ELSE to simplify logic
   if(noelse.eq.0)then
      call else('ELSE',' ',noelse,eb)
   endif

   G_nestl=G_nestl-1                                           ! decrease if level

   if(G_nestl.lt.0)then
      call stop_prep("*prependif* ERROR(032) - MISPLACED $ENDIF DIRECTIVE:"//trim(G_source))
   endif

   noelse=0                                                    ! reset else level
   eb=.not.G_condop(G_nestl+1)
   G_write=.not.eb
   G_condop(G_nestl+1)=.false.

   if(G_nestl.eq.0)then
      G_write=.true.
      eb=.false.
   endif

   if(debug)then
      write(*,*)'*ENDIF* BOTTOM'
      write(*,*)'        G_NESTL =',g_nestl
      write(*,*)'        EB      =',eb
      write(*,*)'        NOELSE  =',noelse
      write(*,*)'        G_WRITE =',g_write
      write(*,*)'        G_CONDOP=',g_condop
   endif

end subroutine endif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parens(line)                       !@(#)parens(3f): find subexpressions in parenthesis and process them
character(len=G_line_length)    :: line       ! line        -
   integer                      :: i
   integer                      :: j

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
               if (j.eq.i-2) then
                  call stop_prep("*prep* 1**(-1) NOT IMPLEMENTED YET")
               endif

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
            if (j.eq.i-2) then
               call stop_prep("*prep* 1**(-1) NOT IMPLEMENTED YET")
            endif

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
end subroutine parens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine math(line,ipos1,ipos2)                             !@(#)math(3f):
   integer                               :: ipos1
   integer                               :: ipos2
   integer                               :: i,j
   character(len=G_line_length)            :: line
   character(len=G_line_length)            :: newl

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
character(len=*)                             :: line
integer                                      :: ipos2

   character(len=11)                         :: temp
   character(len=G_line_length)                :: newl
   character(len=2),save                     :: ops(3)= (/'**','*/','+-'/)
   integer                                   :: i
   integer                                   :: j
   integer                                   :: loc
   integer                                   :: minus1
   integer                                   :: i1
   integer                                   :: i2
   integer                                   :: l
   integer                                   :: len
   integer                                   :: numop

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
           call stop_prep("*prepdomath* ERROR(035) - INCOMPLETE STATEMENT. OPERATOR (**,/,*,+,-) AT STRING END:"//trim(G_SOURCE))
        endif
        IF (I.EQ.1.AND.NUMOP.NE.3) then          ! if operator at beginning of string and not +-
         call stop_prep("*prepdomath* ERROR(036)-SYNTAX ERROR. OPERATOR (**,*,/) NOT ALLOWED TO PREFIX EXPRESSION:"//trim(G_SOURCE))
        endif
        if (.not.(i.eq.1.and.numop.eq.3)) then   ! if processing +- operators and sign at beginning of string skip this
           if (index('*/+-',newl(i-1:i-1)).ne.0.or.index('*/+-',newl(i+len:i+len)).ne.0) then
              call stop_prep('*prepdomath* ERROR(037) - SYNTAX ERROR IN DOMATH:'//trim(G_source))
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
                 call stop_prep('*prepdomath* ERROR(038) - DIVIDE BY ZERO:'//trim(G_source))
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
              call stop_prep('*prepdomath* ERROR(039) - INTERNAL PROGRAM ERROR:'//trim(G_source))
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
      newl(:G_line_length)='-'//newl  !*! note potentially trimming a character off the end
   elseif (minus1.eq.-1.and.loc.ne.1) then
      newl=newl(:loc-1)//'-'//newl(loc:)
   endif

   line(:ipos2)=newl(:len_trim(newl))

end subroutine domath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine doop(line,ipos1,ipos2)                       !@(#)doop(3f): find VAL.OP.VAL strings and reduce to .TRUE. or .FALSE.
character(len=G_line_length)       :: line
integer                            :: ipos1
integer                            :: ipos2

   character(len=4),parameter      :: ops(6) = (/'.EQ.','.NE.','.GE.','.GT.','.LE.','.LT.'/)
   character(len=G_var_len)        :: val1
   character(len=G_var_len)        :: val2
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
            select case(i)                                       ! determine truth
            case(1)                                              ! .eq.
               if (val1.eq.val2) G_dc=.true.
            case(2)                                              ! .ne.
               if (val1.ne.val2) G_dc=.true.
            case(3)                                              ! .ge.
               if (val1.ge.val2) G_dc=.true.
            case(4)                                              ! .gt.
               if (val1.gt.val2) G_dc=.true.
            case(5)                                              ! .le.
               if (val1.le.val2) G_dc=.true.
            case(6)                                              ! .lt.
               if (val1.lt.val2) G_dc=.true.
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
   logical function trufal(line,ipos1,ipos2)       ! @(#)trufal(3f): convert variable name or .TRUE./.FALSE. to a logical value
   character(len=G_line_length),intent(in) :: line           ! line containing string to interpret as a logical value
   integer,intent(in)                    :: ipos1            ! starting column of substring in LINE
   integer,intent(in)                    :: ipos2            ! ending column of substring in LINE

   character(len=G_var_len)                :: value          ! substring to extract from LINE
   integer                               :: i                ! loop counter
   integer                               :: ios              ! error code returned by an internal READ
   integer                               :: ifound           ! index in dictionary at which a variable name was found, or -1

   trufal=.false.                                            ! initialize return value
   value=line(ipos1:ipos2)                                   ! extract substring from LINE to interpret
   ifound=-1                                                 ! flag if successfully converted string, or index variable name found

   select case (value)                                       ! if string is not a logical string assume it is a variable name
   case ('.FALSE.','.F.')
      ifound=0                                               ! set flag to indicate a good value has been found
      trufal=.false.                                         ! set appropriate return value
   case ('.TRUE.','.T.')
      ifound=0                                               ! set flag to indicate a good value has been found
      trufal=.true.                                          ! set appropriate return value
   case default                                              ! assume this is a variable name, find name in dictionary
      do i=1,G_numdef
         if (G_defvar(i).eq.value) then                      ! found variable name in dictionary
            ifound=i                                         ! record index in diction where variable was found
            exit
         endif
      enddo

      if (ifound.eq.-1) then                                 ! if not a defined variable name stop program
         call stop_prep('*preptrufal* ERROR(040) - UNDEFINED PARAMETER.'//trim(G_source))
      endif

      read(G_defval(ifound),'(l4)',iostat=ios) trufal        ! try to read a logical from the value for the variable name

      if(ios.ne.0)then                                       ! not successful in reading string as a logical value
            call stop_prep('*preptrufal* ERROR(041) - CONSTANT LOGICAL EXPRESSION REQUIRED.'//trim(G_source))
      endif

   end select

   if (ifound.lt.0) then                                     ! not a variable name or string '.TRUE.' or '.FALSE.'
      call stop_prep('*preptrufal* ERROR(042) - CONSTANT LOGICAL EXPRESSION REQUIRED:'//trim(G_source))
   endif

   end function trufal
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
           LEN=5
           IF (I.EQ.3) LEN=4
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
              ONE=TRUFAL(NEWL,J+LEN1,I1)
           endif

           OUT: DO L=I1+LEN,len_trim(NEWL)
             IN: DO K=1,5
                LEN2=5
                IF (K.EQ.3) LEN2=4
                IF (INDEX(NEWL(I1+LEN:L),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUT
             enddo IN
           enddo OUT

           IF (L.GT.len_trim(NEWL)) LEN2=0
           TWO=TRUFAL(NEWL,I1+LEN+1,L-LEN2)

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
      ineqv=index(newl,'.NEQV')
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
      one=trufal(newl,j+len1,iop-1)
      do l=iop+len,len_trim(newl)
         if (newl(l:l+1).eq.'.E'.or.newl(l:l+1).eq.'.N') exit
      enddo
      if (l.gt.len_trim(newl)) len2=0
      two=trufal(newl,iop+len,l+len2)
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
character(len=G_line_length)        :: line
   character(len=7)               :: value

   call parens(line)
   call math(line,1,len_trim(line))
   call doop(line,1,len_trim(line))
   call logic(line,1,len_trim(line))
   value=line(1:7)

   if (value.ne.'.TRUE.'.and.value.ne.'.FALSE.') then
      call stop_prep('*prepeval* ERROR(043) - value neither true or false:'//trim(value)//' when evaluating: '//trim(G_source))
   endif

   read(value,'(l4)') G_dc

end subroutine eval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_integer_from_string(line) !@(#)get_integer_from_string(3f): read integer value from line(ipos1:ipos2)
                                                         ! assume string is a variable name or an integer value
   character(len=*),intent(in)  :: line                             ! string containing substring to read an integer value from

   character(len=G_var_len)     :: value                            ! the substring
   integer                      :: i                                ! index of variable dictionary where variable name is stored
   integer                      :: ios                              ! I/O error value to check to see if internal reads succeeded
   integer                      :: get_integer_from_string          ! integer value to return if string is converted successfully
   integer                      :: ipos1, ipos2

   ipos1=min(1,len(line))
   ipos2=len(line)
   ipos2=min(max(1,ipos2),ipos2)
   if(len(line).eq.0)then
      get_integer_from_string=0
   elseif (line(ipos1:ipos1).ge.'A'.and.line(ipos1:ipos1).le.'Z') then  ! not a number, now assumed to  be a variable name
      value= line(ipos1:ipos2)                                      ! extract substring that is assumed to be a variable name
      i=-1                                                          ! this will be index where variable name is found in dictionary
      do i=1,G_numdef                                               ! scan variable dictionary for the variable name
        if (G_defvar(i).eq.value) exit
      enddo
      if (i.gt.G_numdef.or.i.lt.0)then                              ! if variable name not found in dictionary, stop
        call stop_prep('*prepgi* ERROR(044) - UNDEFINED PARAMETER:'//trim(G_source))
      endif
      read(G_defval(i),'(i11)',iostat=ios) get_integer_from_string  ! read integer value from the value associated with name
      if(ios.ne.0)then                                              ! failed reading integer from value, stop
        call stop_prep('*prepgi* ERROR(045) - MUST BE INTEGER:'//trim(G_source))
      endif
   else                                                             ! input is not a variable name, assume it represents an integer
      read(line(ipos1:ipos2),'(i11)',iostat=ios) get_integer_from_string               ! try to read integer value from input string
      if(ios.ne.0)then                                              ! failed to convert the string to an integer, so stop
        call stop_prep('*prepgi* ERROR(046) - MUST BE INTEGER:'//trim(G_source))
      endif
   endif                                                            ! return integer value
end function get_integer_from_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rewrit(line,temp,j,j1,l,l1)                           !@(#)rewrit(3f):
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
subroutine document(opts)                    ! @(#)document(3f): process BLOCK command to start or stop special processing
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
   elseif(G_outtype.eq.'shell')then
         close(unit=G_scratch_lun,iostat=ios)
         call execute_command_line( trim(sget('block_cmd'))//' < '//G_scratch_file//' > '//G_scratch_file//'.out')
         ierr=filedelete(G_scratch_file)
         options=G_scratch_file//'.out'
         call include(options,50+G_iocount)    ! Filenames can be case sensitive
   elseif(G_outtype.eq.'version')then  ! if in 'version' mode wrap up the routine
      write(G_iout,'("''@(#)COMPILED:       ",a,"'',&")') getdate('long')//'>'
      write(G_iout,'(a)')"'']"
      write(G_iout,'(a)')"   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))"
      !*!write(G_iout,'(a)')'   write(*,*)"COMPILER VERSION=",COMPILER_VERSION()'
      !*!write(G_iout,'(a)')'   write(*,*)"COMPILER OPTIONS=",COMPILER_OPTIONS()'
      write(G_iout,'(a)')"   stop ! if --version was specified, stop"
      write(G_iout,'(a)')"endif"
      write(G_iout,'(a)')"end subroutine help_version"
      !x!write(G_iout,'("!",a)')repeat('-',131)
   endif

   call dissect2('block','--oo --file --cmd sh --varname textblock --append .false.',opts) ! parse options on input line
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

   case('NULL')
      G_outtype='null'

   case('SHELL')
      G_outtype='shell'
      G_MAN_PRINT=.false.
      G_MAN_COLLECT=.false.
      if(G_system_on)then                             ! if allowing commands to be executed
         flush(unit=G_iout,iostat=ios)
         !!G_scratch_file=scratch('prep_scratch.'))
         G_scratch_file=trim(uniq(get_tmp()//'prep_scratch.'))  !! THIS HAS TO BE A UNIQUE NAME -- IMPROVE THIS
         G_scratch_lun=fileopen(G_scratch_file,'rw',ierr)
         if(ierr.lt.0)then
            call stop_prep('*prep:filter* ERROR: FILTER COMMAND FAILED TO OPEN PROCESS:'//trim(G_SOURCE))
         endif
      else
         call stop_prep('*prep:filter* ERROR: FILTER COMMAND BLOCK ENCOUNTERED BUT SYSTEM COMMANDS NOT ENABLED:'//trim(G_SOURCE))
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
      write(*,*)'*prepstop* ERROR(047) - UNEXPECTED "BLOCK" OPTION. FOUND:'//trim(G_source)
      write(*,*)'*prepstop* ERROR(048) - UNEXPECTED "BLOCK" OPTION. FOUND:'//trim(sget('block_oo'))
      call stop_prep('*prepstop* ERROR(049) - UNEXPECTED "BLOCK" OPTION. FOUND:'//sget('block_man'))
   end select

   G_comment_count=0
end subroutine document
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop(opts)                    ! @(#)stop(3f): process stop directive
character(len=*),intent(in) :: opts
integer                     :: ivalue

! CHECK COMMAND SYNTAX
   if(opts.ne.'')then
      ivalue=get_integer_from_string(opts)
      if(ivalue.eq.0)then
         stop
      elseif(ivalue.ge.1.and.ivalue.le.20)then
         !*!stop ivalue
         stop 3
      else
         call stop_prep('*prepstop* ERROR(050) - UNEXPECTED "STOP" VALUE='',i10,''. FOUND:'',a)'//trim(G_source))
      endif
   else
      stop 1
   endif
end subroutine stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_comment_block() !@(#)print_comment_block(3f): format comment block to file in document directory and output
   character(len=:),allocatable :: filename
   character(len=1024)          :: varvalue
   character(len=*),parameter   :: varname='PREP_DOCUMENT_DIR'
   integer                      :: ios,iend,istatus,ilength

   if(.not.allocated(G_MAN))then
      return
   endif

   call get_environment_variable(varname,varvalue,ilength,istatus)
   select case(istatus)
   case(0)
   case(-1);     call stop_prep('ERROR(051) print_comment_block - VARIABLE VALUE TOO LONG:'//trim(varname))
   case(1);     !call stop_prep('ERROR(print_comment_block) - VARIABLE DOES NOT EXIST:'//trim(varname))
   case(2);      call stop_prep('ERROR(052) print_comment_block - COMPILER DOES NOT SUPPORT ENVIRONMENT VARIABLES:'//trim(varname))
   case default; call stop_prep('ERROR(053) print_comment_block - UNEXPECTED STATUS VALUE '//v2s(istatus)//':'//trim(varname))
   end select

   if(ilength.ne.0.and.G_MAN.ne.''.and.G_MAN_FILE.ne.' ')then ! if $BLOCK ... --file FILE is present generate file in directory/doc
      filename=trim(varvalue)//'/doc/'

      iend=len_trim(varvalue)

      if(varvalue(iend:iend).ne.'/')then
         filename=trim(varvalue)//'/doc/'//trim(G_MAN_FILE)
      else
         filename=trim(varvalue)//'doc/'//trim(G_MAN_FILE)
      endif

      if(filename.eq.' ') filename='OOPS.txt'

      open(unit=70,file=filename,iostat=ios,action='write',position=G_MAN_FILE_POSITION)


      if(ios.ne.0)then

         call stop_prep('ERROR(054) print_comment_block - FAILED TO OPEN DOCUMENT OUTPUT FILE:'//trim(filename))
      else
         if(len(G_MAN).gt.1)then                   ! the way the string is built it starts with a newline
            write(70,'(a)',iostat=ios) G_MAN(2:)
         else
            write(70,'(a)',iostat=ios) G_MAN
         endif
         if(ios.ne.0)then
            call stderr('G_MAN='//G_MAN)
            call stop_prep('ERROR(055) print_comment_block - FAILED TO WRITE OUTPUT FILE:'//trim(filename))
         endif
      endif

      close(70,iostat=ios)

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
   character(len=:),allocatable   :: array(:) ! output array of tokens
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
               array=[character(len=(len(array1)+6)) :: array1] !! pad with trailing spaces
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
      call stderr('G_MAN='//G_MAN)
      call stop_prep('ERROR(056) print_comment_block - FAILED TO WRITE COMMENT BLOCK')
   endblock ALL
end subroutine format_g_man
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stops(iexit) !@(#)stops(3f): irritating that the value on STOP is scalar-char-initialization-expr. 1-20 OK this way
integer,intent(in)   :: iexit
   select case(iexit)
   case  (1)      ;  stop  1
   case  (2)      ;  stop  2
   case  (3)      ;  stop  3
   case  (4)      ;  stop  4
   case  (5)      ;  stop  5
   case  (6)      ;  stop  6
   case  (7)      ;  stop  7
   case  (8)      ;  stop  8
   case  (9)      ;  stop  9
   case  (10)     ;  stop  10
   case  (11)     ;  stop  11
   case  (12)     ;  stop  12
   case  (13)     ;  stop  13
   case  (14)     ;  stop  14
   case  (15)     ;  stop  15
   case  (16)     ;  stop  16
   case  (17)     ;  stop  17
   case  (18)     ;  stop  18
   case  (19)     ;  stop  19
   case  (20)     ;  stop  20
   case  default  ;  stop
   end   select
end subroutine stops
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine debug_state(msg)                        !@(#)debug(3f): process $SHOW command or state output when errors occur
character(len=*),intent(in) :: msg
integer                     :: i
character(len=*),parameter  :: fmt='(*(g0,1x))'

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
   do i=1,G_numdef                                                                 ! print variable dictionary
      write(G_iout,fmt)"!    $DEFINE",trim(G_defvar(i)),' = ',adjustl(G_defval(i)) ! write variable and corresponding value
   enddo

   if(size(keywords).gt.0)then
      write(G_iout,fmt)'! SET strings:(There are',size(keywords),'keywords defined)'
      write(G_iout,'(3(g0,1x))')('!    $SET',keywords(i),values(i)(:counts(i)),i=1,size(keywords))
   endif

   write(G_iout,'(a)')'!-------------------------------------------------------------------------------'
end subroutine debug_state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_arguments() ! @(#)write_arguments(3f): return all command arguments as a string

   integer                      :: istatus          !  status (non-zero means error)
   integer                      :: ilength          !  length of individual arguments
   integer                      :: i                !  loop count
   integer                      :: icount           !  count of number of arguments available
   character(len=255)           :: value            !  store individual arguments one at a time

   write(G_iout,'(a)',advance='no')'! Arguments ...................... '
   icount=command_argument_count()                  ! intrinsic gets number of arguments
   do i=1,icount
      call get_command_argument(i,value,ilength,istatus)
      write(G_iout,'(a,1x)',advance='no')value(:ilength)
   enddo
   write(G_iout,'(a)')
   call write_out('')
end subroutine write_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine include(line,iunit)  ! @(#)include(3f): add file to input file list
implicit none
character(len=G_line_length),intent(in)  :: line
integer,intent(in)                       :: iunit
   integer                               :: ios
   character(len=4096)                   :: message

   if(iunit.eq.5.or.line.eq.'@')then                   ! assume this is stdin
      G_iocount=G_iocount+1
      G_file_dictionary(G_iocount)%unit_number=5
      G_file_dictionary(G_iocount)%filename=line
      return
   endif

   call findit(line)

   open(unit=iunit,file=trim(line),iostat=ios,status='old',action='read',iomsg=message)
   if(ios.ne.0)then
      call debug_state('OPEN IN INCLUDE')
      call stderr(message)
      call stop_prep("*prep* ERROR(057) - FAILED OPEN OF INPUT FILE("//v2s(iunit)//"):"//trim(line))
   else
      rewind(unit=iunit)
      G_iocount=G_iocount+1
      if(G_iocount.gt.size(G_file_dictionary))then
         call stop_prep('*prep* ERROR(058) - INPUT FILE NESTING TOO DEEP:'//trim(G_source))
      endif
      G_file_dictionary(G_iocount)%unit_number=iunit
      G_file_dictionary(G_iocount)%filename=line
      G_file_dictionary(G_iocount)%line_number=0
   endif

end subroutine include
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine post(parcel_name)  ! @(#)post(3f): switch to scratch file defined by PARCEL
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

      !d!do
      !d!   read(ifound,'(a)',iostat=ios)message
      !d!   if(ios.ne.0)exit
      !d!   write(*,*)'>>>'//trim(message)
      !d!enddo
      !d!rewind(unit=ifound,iostat=ios,iomsg=message)

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
character(len=G_line_length)             :: line
   character(len=G_line_length)          :: filename
   logical                               :: file_exist
   integer                               :: i
   integer                               :: iend_dir

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

   integer,parameter                     :: n=50                    ! maximum number of tokens to look for
   character(len=G_line_length)          :: array(n)                ! the array to fill with tokens
   character(len=1)                      :: dlim=' '                ! string of single characters to use as delimiters

   integer                               :: icount                  ! how many tokens are found
   integer                               :: ibegin(n)               ! starting column numbers for the tokens in INLINE
   integer                               :: iterm(n)                ! ending column numbers for the tokens in INLINE
   integer                               :: ilen                    ! is the position of last non‐blank character in INLINE
   character(len=G_line_length)          :: in_filename1=''         ! input filename, default is stdin
   character(len=G_line_length)          :: in_filename2=''         ! input filename, default is stdin
   integer                               :: i, ii
   integer                               :: ivalue
   character(len=G_line_length)          :: dir                     ! directory used by an input file

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
         G_inc_count=min(G_inc_count,size(G_inc_files)) ! guard against too many files; !*! should warn on overflow
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
subroutine defines()       !@(#)defines(3f): use expressions on command line to define variables
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
subroutine stop_prep(message)                   !@(#)stop_prep(3f): write MESSAGE to stderr and exit program
character(len=*),intent(in)  :: message
   call stderr(message)
   call stderr(trim(G_SOURCE))
   call debug_state('message')
   stop 1
end subroutine stop_prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

! This documentation is a combination of
!    o the original Lahey documentation of fpp(1) from "LAHEY FORTRAN REFERENCE MANUAL"; Revision C, 1992;
!    o examination of the code
!    o and documentation for the features subsequently added to the program.

subroutine help_usage(l_help)
implicit none
!@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   prep(1) - [DEVELOPER] pre-process FORTRAN source files                       ',&
'   (LICENSE:MIT)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   prep  [[-D] define_list]                                                     ',&
'         [-I include_directories]                                               ',&
'         [-i input_file(s)]                                                     ',&
'         [-o output_file]                                                       ',&
'         [--system]                                                             ',&
'         [--verbose]                                                            ',&
'         [--prefix character|ADE]                                               ',&
'         [--keeptabs]                                                           ',&
'         [--noenv]                                                              ',&
'         [--width n]                                                            ',&
'         [-d ignore|remove|blank]                                               ',&
'         [--comment default|doxygen|ford|none]                                  ',&
'         [--ident]                                                              ',&
'         [--version]                                                            ',&
'         [--help]                                                               ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   By default the pre-processor prep(1) will interpret lines with "$" in column ',&
'   one, and will output no such lines. Other input is conditionally written to  ',&
'   the output file based on the directives encountered in the input. It does    ',&
'   not support parameterized macros but does support string substitution and    ',&
'   the inclusion of free-format text blocks that may be converted to Fortran    ',&
'   comments or CHARACTER variable definitions while simultaneously being used   ',&
'   to generate documentation files. INTEGER or LOGICAL expressions may be used  ',&
'   to select output lines.                                                      ',&
'                                                                                ',&
'   The suggested suffix for Fortran input files is ".ff" for code files unless  ',&
'   they contain $SYSTEM directives in which case ".FF" is preferred. $INCLUDE   ',&
'   files should use ".ffinc" and ".FFINC" if they include prep(1) directives.   ',&
'   This naming convention is not required.                                      ',&
'                                                                                ',&
'   An exclamation character on a valid directive begins an in-line comment      ',&
'   that is terminated by an end-of-line.                                        ',&
'                                                                                ',&
'   An expression is composed of INTEGER and LOGICAL constants, parameters       ',&
'   and operators. Operators are                                                 ',&
'                                                                                ',&
'     .NOT.  .AND.  .OR.  .EQV.  .NEQV.  .EQ.  .NE.  .GE.                        ',&
'     .GT.   .LE.   .LT.  +      -       *     /     (                           ',&
'     )      **                                                                  ',&
'                                                                                ',&
'   The syntax for the directive lines is as follows:                            ',&
'                                                                                ',&
'    :CONDITIONAL CODE SELECTION                                                 ',&
'     $DEFINE   variable_name[=expression]                 [! comment ]          ',&
'     $UNDEFINE variable_name                              [! comment ]          ',&
'     $IF       expression| [$IFDEF|$IFNDEF variable_name] [! comment ]          ',&
'               { sequence of source statements}                                 ',&
'     [$ELSEIF  {LOGICAL or INTEGER expression}            [! comment ]          ',&
'               { sequence of source statements}]                                ',&
'     [$ELSE                                               [! comment ]          ',&
'               { sequence of source statements}]                                ',&
'     $ENDIF                                               [! comment ]          ',&
'     $STOP     [stop_value]                               [! comment ]          ',&
'                                                                                ',&
'    :MACRO STRING EXPANSION AND TEXT REPLAY                                     ',&
'     $SET      varname  string                                                  ',&
'     $IMPORT   envname(s)                                                       ',&
'     $PARCEL   blockname                                  [! comment ]          ',&
'     $POST     blockname                                  [! comment ]          ',&
'                                                                                ',&
'    :FILE AND TEXT BLOCK USAGE                                                  ',&
'     $OUTPUT   filename  [-append]                        [! comment ]          ',&
'     $INCLUDE  filename                                   [! comment ]          ',&
![=========
!     consider to change so just $BLOCK name/$ENDBLOCK and
!     $OUTPUT $BLOCK $POST all replaced by $POST if no options gulp file and process if any options so what $BLOCK does now
!     $BLOCK
!     $ENDBLOCK
!=========]
'     $BLOCK    [comment|null|write|help|version|variable [-varname NAME]]       ',&
!'               [shell [-cmd NAME]] |                                            ',&
'               [-file NAME [-append]]                     [! comment ]          ',&
'                                                                                ',&
'    :IDENTIFIERS                                                                ',&
'     $IDENT | $@(#)    metadata                           [! comment ]          ',&
'                                                                                ',&
'    :INFORMATION                                                                ',&
'     $MESSAGE  message_to_stderr                                                ',&
'     $SHOW                                                [! comment ]          ',&
'                                                                                ',&
'    :SYSTEM COMMANDS                                                            ',&
'     $SYSTEM   system_command                             [! comment ]          ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   define_list, -D define_list  An optional space-delimited list of expressions ',&
'                                used to define variables before file processing ',&
'                                commences.                                      ',&
'   -i input_files               The default input file is stdin. Filenames are  ',&
'                                space-delimited. In a list, @ represents stdin. ',&
'   -o output_file               The default output file is stdout.              ',&
'   -I include_directories       The directories to search for files specified on',&
'                                $INCLUDE directives.                            ',&
'   --prefix ADE|letter  The default directive prefix character is "$".          ',&
'                        Alternatives may be specified by providing an           ',&
'                        ASCII Decimal Equivalent (Common values are 37=%        ',&
'                        42=* 35=# 36=$ 64=@). If the value is not numeric       ',&
'                        it is assumed to be a literal character.                ',&
'                                                                                ',&
'   --help           Display documentation and exit.                             ',&
'   --verbose        All commands on a $SYSTEM directive are echoed              ',&
'                    to stderr with a + prefix. Text following the               ',&
'                    string "@(#)" is printed to stderr similar to               ',&
'                    the Unix command what(1) but is otherwise                   ',&
'                    treated as other text input.                                ',&
'                                                                                ',&
'   --noenv          The $IFDEF and $IFNDEF directives test for an               ',&
'                    internal prep(1) variable and then an                       ',&
'                    environment variable by default. This option                ',&
'                    turns off testing for environment variables.                ',&
'   --system         Allow system commands on $SYSTEM directives to              ',&
'                    be executed.                                                ',&
'   --keeptabs       By default tab characters are expanded assuming             ',&
'                    a stop has been set every eight columns; and                ',&
'                    trailing carriage-return characters are removed.            ',&
'                    Use this flag to prevent this processing from               ',&
'                    occurring.                                                  ',&
'   --comment        try to style comments generated in $BLOCK blocks            ',&
'                    for other utilities such as doxygen. Default is to          ',&
'                    prefix lines with ''! ''. Allowed keywords are              ',&
'                    currently "default", "doxygen","none","ford".               ',&
'                    THIS IS AN ALPHA FEATURE AND NOT FULLY IMPLEMENTED.         ',&
'   --ident          The output of the $IDENT directive is in the form of a      ',&
'                    comment by default. If this flag is set the output is       ',&
'                    of the form described in the $IDENT documentation           ',&
'                    so executables and object code can contain the metadata     ',&
'                    for use with the what(1) command. Note this generates an    ',&
'                    unused variable which some compilers might optimize         ',&
'                    away depending on what compilation options are used.        ',&
'   -d ignore|remove|blank  Enable special treatment for lines beginning         ',&
'                           with "d" or "D" The letter will be left as-is        ',&
'                           (the default); removed; or replaced with a blank     ',&
'                           character. This non-standard syntax has been         ',&
'                           used to support the optional compilation of          ',&
'                           "debug" code by many Fortran compilers when          ',&
'                           compiling fixed-format Fortran source.               ',&
'   --version        Display version and exit                                    ',&
'   --width n        Maximum line length of the output file. The default is 1024.',&
'                    Typically used to trim fixed-format FORTRAN code that       ',&
'                    contains comments or "ident" labels past column 72          ',&
'                    when compiling fixed-format Fortran code.                   ',&
'                                                                                ',&
'   DIRECTIVES                                                                   ',&
'                                                                                ',&
'   $DEFINE variable_name [=expression]                                          ',&
'                                                                                ',&
'   A $DEFINE may appear anywhere in a source file. If the value is ".TRUE."     ',&
'   or ".FALSE." then the parameter is of type LOGICAL, otherwise the            ',&
'   parameter is of type INTEGER and the value must be an INTEGER. If no         ',&
'   value is supplied, the parameter is of type INTEGER and is given the         ',&
'   value 1.                                                                     ',&
'                                                                                ',&
'   Constant parameters are defined from the point they are encountered in a     ',&
'   $DEFINE directive until program termination unless explicitly                ',&
'   undefined with a $UNDEFINE directive.                                        ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    $define A=1                                                                 ',&
'    $define B=1                                                                 ',&
'    $define C=2                                                                 ',&
'    $if ( A + B ) / C .eq. 1                                                    ',&
'       (a+b)/c is one                                                           ',&
'    $endif                                                                      ',&
'                                                                                ',&
'   $IF/$ELSEIF/$ELSE/$ENDIF directives                                          ',&
'                                                                                ',&
'   Each of the control lines delineates a block of FORTRAN source. If the       ',&
'   expression following the $IF is ".TRUE.", then the lines of FORTRAN          ',&
'   source following are output. If it is ".FALSE.", and an $ELSEIF              ',&
'   follows, the expression is evaluated and treated the same as the $IF. If     ',&
'   the $IF and all $ELSEIF expressions are ".FALSE.", then the lines of         ',&
'   source following the $ELSE are output. A matching $ENDIF ends the            ',&
'   conditional block.                                                           ',&
'                                                                                ',&
'   $IFDEF/$IFNDEF directives                                                    ',&
'                                                                                ',&
'   $IFDEF and $IFNDEF are special forms of the $IF directive that simply test   ',&
'   if a variable name is defined or not. Essentially, these are equivalent:     ',&
'                                                                                ',&
'     $IFDEF varname  ==> $IF DEFINED(varname)                                   ',&
'     $IFNDEF varname ==> $IF .NOT. DEFINED(varname)                             ',&
'                                                                                ',&
'   except that environment variables are tested as well if the --noenv option   ',&
'   is not specified.                                                            ',&
'                                                                                ',&
'   $IDENT metadata [-language fortran|c|shell]                                  ',&
'                                                                                ',&
'   When the command line option "-ident" is specified this directive            ',&
'   writes a line using SCCS-metadata format of one of the following forms:      ',&
'                                                                                ',&
'     language:                                                                  ',&
'     fortran   character(len=*),parameter::ident="@(#)metadata"                 ',&
'     c         #ident "@(#)metadata"                                            ',&
'     shell     #@(#) metadata                                                   ',&
'                                                                                ',&
'   This string is generally included for use with the what(1) command.          ',&
'                                                                                ',&
'   "$@(#)" is an alias for "$IDENT" so the source file itself will contain      ',&
'   SCCS-metadata so the metadata can be displayed with what(1).                 ',&
'                                                                                ',&
'   The default language is "fortran". Depending on your compiler and the        ',&
'   optimization level used when compiling, these strings may or may not         ',&
'   remain in the object files and executables created.                          ',&
'                                                                                ',&
'   Do not use the characters double-quote, greater-than, backslash (">\)        ',&
'   in the metadata to remain compatible with SCCS metadata syntax.              ',&
'   Do not use strings starting with " -" either.                                ',&
'                                                                                ',&
'   $OUTPUT filename [-append]                                                   ',&
'                                                                                ',&
'   Specify the output file to write to. Overrides the initial output file       ',&
'   specified with command line options. If no output filename is given          ',&
'   revert back to initial output file. @ is a synonym for stdout.               ',&
'                                                                                ',&
'      -append [.true.|.false]                                                   ',&
'                                                                                ',&
'   Named files open at the beginning by default. Use the -append switch to      ',&
'   append to the end of an existing file instead of overwriting it.             ',&
'                                                                                ',&
'   $INCLUDE filename                                                            ',&
'                                                                                ',&
'   Nested read of specified input file. Fifty (50) nesting levels are allowed.  ',&
'                                                                                ',&
'   $PARCEL [name]                                                               ',&
'                                                                                ',&
'   The lines between a "$PARCEL name" and "$PARCEL" block are written WITHOUT   ',&
'   expanding directives to a scratch file that can then be read in with the     ',&
'   $POST directive much like a named file can be with $INCLUDE.                 ',&
'                                                                                ',&
'   $POST name                                                                   ',&
'                                                                                ',&
'   Read in the scratch file created by the $PARCEL directive. Combined with     ',&
'   $SET directives this allows you to replay a section of input and replace     ',&
'   strings as a simple templating technique.                                    ',&
'                                                                                ',&
'   $SET name string                                                             ',&
'                                                                                ',&
'   If a $SET directive defines a name prep(1) enters expansion mode. In this    ',&
'   mode anywhere the string "${NAME}" is encountered in subsequent output it    ',&
'   is replaced by "string". Comments should not be used on a $SET directive.    ',&
'   Note expansion of a line may cause it to be longer than allowed by some      ',&
'   compilers. Automatic breaking into continuation lines does not occur.        ',&
'                                                                                ',&
'   IF A $SET DIRECTIVE HAS BEEN DEFINED the "standard" preprocessor values      ',&
'   ${FILE}, ${LINE}, ${DATE}, and ${TIME} are also available. The time          ',&
'   data refers to the time of processing, not the current time nor the time     ',&
'   of compilation or loading.                                                   ',&
'                                                                                ',&
'   $IMPORT names(s)                                                             ',&
'                                                                                ',&
'   The values of environment variables may be imported such that their names    ',&
'   and values will be set as if a $SET command had been done on them.           ',&
'                                                                                ',&
'   $BLOCK [comment|null|write|help|version  [-file NAME [-append]]              ',&
'     or                                                                         ',&
'   $BLOCK VARIABLE --varname NAME  [--file NAME]                                ',&
'                                                                                ',&
'      COMMENT:   write text prefixed by an exclamation and a space              ',&
'      WRITE:     write text as Fortran WRITE(3f) statements                     ',&
'                 The Fortran generated is free-format. It is assumed the        ',&
'                 output will not generate lines over 132 columns.               ',&
'      HELP:      write text as a subroutine called HELP_USAGE                   ',&
'      VERSION:   write text as a subroutine called HELP_VERSION prefixing       ',&
'                 lines with @(#) for use with the what(1) command.              ',&
'      NULL:      Do not write into current output file                          ',&
'      VARIABLE:  write as a text variable. The name may be defined using        ',&
'                 the --varname switch. Default name is "textblock".             ',&
!'      SHELL:     run text in block as a shell and replace with the stdout       ',&
!'                 generated by the shell. The shell may be specified by the -cmd ',&
!'                 option. The default shell is bash(1).                          ',&
'      END:       End block of specially processed text                          ',&
'                                                                                ',&
'   If the "-file NAME" option is present the *unaltered* text is written to     ',&
'   the specified file. This allows documentation to easily be maintained in     ',&
'   the source file. It can be tex, html, markdown or any plain text.            ',&
'   The filename will be prefixed with $PREP_DOCUMENT_DIR/doc/ . If the          ',&
'   environment variable $PREP_DOCUMENT_DIR is not set the option is ignored.    ',&
'                                                                                ',&
'   The text can easily be processed by other utilities such as markdown(1)      ',&
'   or txt2man(1) to produce man(1) pages and HTML documents. $SYSTEM commands   ',&
'   may follow the $BLOCK block text to optionally post-process the doc files.   ',&
'                                                                                ',&
'   A blank value or "END" returns to normal output processing.                  ',&
'                                                                                ',&
'   $SHOW                                                                        ',&
'                                                                                ',&
'   Shows current state of prep(1); including variable names and values; and     ',&
'   the name of the current input files. All output is preceded by an            ',&
'   exclamation character.                                                       ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    prep A=10 B C D -o paper                                                    ',&
'    $define z=22                                                                ',&
'    $show                                                                       ',&
'    $stop 0                                                                     ',&
'                                                                                ',&
'     > !================================================================        ',&
'     > !                                                                        ',&
'     > ! Current state of prep(1):(18:39 20 Jun 2021)                           ',&
'     > ! Total lines read ............... 2                                     ',&
'     > ! Conditional nesting level....... 0                                     ',&
'     > ! G_WRITE (general processing).... T                                     ',&
'     > ! G_LLWRITE (write input lines)... T                                     ',&
'     > ! Arguments ...................... A=10 B C D -o paper                   ',&
'     > ! Open files:                                                            ',&
'     > !    unit ! line number ! filename                                       ',&
'     > !       5 !           2 ! @                                              ',&
'     > ! INCLUDE directories:                                                   ',&
'     > !    .                                                                   ',&
'     > ! Variables:                                                             ',&
'     > !    $DEFINE UNKNOWN  =  0                                               ',&
'     > !    $DEFINE LINUX  =  1                                                 ',&
'     > !    $DEFINE MACOS  =  2                                                 ',&
'     > !    $DEFINE WINDOWS  =  3                                               ',&
'     > !    $DEFINE CYGWIN  =  4                                                ',&
'     > !    $DEFINE SOLARIS  =  5                                               ',&
'     > !    $DEFINE FREEBSD  =  6                                               ',&
'     > !    $DEFINE OPENBSD  =  7                                               ',&
'     > !    $DEFINE OS  =  1                                                    ',&
'     > !    $DEFINE A  =  10                                                    ',&
'     > !    $DEFINE B  =  1                                                     ',&
'     > !    $DEFINE C  =  1                                                     ',&
'     > !    $DEFINE D  =  1                                                     ',&
'     > !    $DEFINE Z  =  22                                                    ',&
'     > !================================================================        ',&
'                                                                                ',&
'   $STOP stop_value                                                             ',&
'                                                                                ',&
'   Stops input file processing. An optional integer value of 0 to 20            ',&
'   will be returned as a status value to the system where supported. A          ',&
'   value of two ("2") is returned if no value is specified. Any value           ',&
'   from one ("1") to twenty ("20") also causes an implicit execution of         ',&
'   the "$SHOW" directive before the program is stopped. A value of "0"          ',&
'   causes normal program termination. "$QUIT" is an alias for "$STOP 0".        ',&
'                                                                                ',&
'   $SYSTEM system_command                                                       ',&
'                                                                                ',&
'   If system command processing is enabled using the --system switch system     ',&
'   commands can be executed for such tasks as creating files to be read or to   ',&
'   further process documents created by $BLOCK. $SYSTEM directives are ignored  ',&
'   by default; as you clearly need to ensure the input file is trusted before   ',&
'   before allowing commands to be executed. Commands that are system-specific   ',&
'   may need to be executed conditionally as well.                               ',&
'                                                                                ',&
'   Examples:                                                                    ',&
'                                                                                ',&
'    $! build variable definitions using GNU/Linux commands                      ',&
'    $SYSTEM echo system=`hostname` > compiled.h                                 ',&
'    $SYSTEM echo compile_time="`date`" >> compiled.h                            ',&
'    $INCLUDE compiled.h                                                         ',&
'                                                                                ',&
'    $! obtain up-to-date copy of source file from HTTP server:                  ',&
'    $SYSTEM wget http://repository.net/src/func.F90 -O - >_tmp.f90              ',&
'    $INCLUDE _tmp.f90                                                           ',&
'    $SYSTEM  rm _tmp.f90                                                        ',&
'                                                                                ',&
'   $UNDEFINE variable_name                                                      ',&
'                                                                                ',&
'   A symbol defined with $DEFINE can be removed with the $UNDEFINE              ',&
'   directive.                                                                   ',&
'                                                                                ',&
'   DEFINED(variable_name)                                                       ',&
'                                                                                ',&
'   A special function called DEFINED() may appear only in a $IF or $ELSEIF.     ',&
'   If "variable_name" has been defined at that point in the source code,        ',&
'   then the function value is ".TRUE.", otherwise it is ".FALSE.". A name is    ',&
'   defined only if it has appeared in the source previously in a $DEFINE        ',&
'   directive or been declared on the command line.                              ',&
'   The names used in compiler directives are district from names in the         ',&
'   FORTRAN source, which means that "a" in a $DEFINE and "a" in a FORTRAN       ',&
'   source statement are totally unrelated.                                      ',&
'   The DEFINED() parameter is NOT valid in a $DEFINE directive.                 ',&
'                                                                                ',&
'   Example:                                                                     ',&
'                                                                                ',&
'    >        Program test                                                       ',&
'    > $IF .NOT. DEFINED (inc)                                                   ',&
'    >        INCLUDE ''''comm.inc''''                                           ',&
'    > $ELSE                                                                     ',&
'    >        INCLUDE ''''comm2.inc''''                                          ',&
'    > $ENDIF                                                                    ',&
'    >        END                                                                ',&
'                                                                                ',&
'   The file, "comm.inc" will be INCLUDEd in the source if the parameter,        ',&
'   "inc", has not been previously defined, while INCLUDE "comm2.inc" will       ',&
'   be included in the source if "inc" has been previously defined. This is      ',&
'   useful for setting up a default inclusion.                                   ',&
'                                                                                ',&
'   Predefined values are                                                        ',&
'                                                                                ',&
'    UNKNOWN = 0 LINUX   = 1 MACOS   = 2 WINDOWS = 3                             ',&
'    CYGWIN  = 4 SOLARIS = 5 FREEBSD = 6 OPENBSD = 7                             ',&
'    In addition OS is set to what the program guesses the system type is.       ',&
'                                                                                ',&
'   $MESSAGE WARNING message                                                     ',&
'                                                                                ',&
'   Write message to stderr                                                      ',&
'                                                                                ',&
'LIMITATIONS                                                                     ',&
'                                                                                ',&
'   $IF constructs can be nested up to 20 levels deep. Note that using           ',&
'   more than two levels typically makes input files less readable.              ',&
'                                                                                ',&
'   $BLOCK is required after a $BLOCK or --file FILENAME is not written.         ',&
'                                                                                ',&
'   Nesting of $BLOCK sections not allowed.                                      ',&
'                                                                                ',&
'   Messages for $MESSAGE do not treat an exclamation as starting a comment      ',&
'                                                                                ',&
'  Input files                                                                   ',&
'                                                                                ',&
'   o lines are limited to 1024 columns. Text past column 1024 is ignored.       ',&
'   o files currently opened cannot be opened again.                             ',&
'   o a maximum of 50 files can be nested by $INCLUDE                            ',&
'   o filenames cannot contain spaces on the command line.                       ',&
'                                                                                ',&
' Variable names                                                                 ',&
'                                                                                ',&
'   o cannot be redefined unless first undefined.                                ',&
'   o are limited to 31 characters.                                              ',&
'   o must start with a letter (A-Z).                                            ',&
'   o are composed of the letters A-Z, digits 0-9 and _ and $.                   ',&
'   o 2048 variable names may be defined at a time.                              ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'                                                                                ',&
'  Define variables on command line:                                             ',&
'                                                                                ',&
'   Typically, variables are defined on the command line when prep(1) is         ',&
'   invoked but can be grouped together into small files that are included       ',&
'   with a $INCLUDE or as input files.                                           ',&
'                                                                                ',&
'     prep HP size=64 -i hp_directives.dirs test.F90 -o test_out.f90             ',&
'                                                                                ',&
'   defines variables HP and SIZE as if the expressions had been on a            ',&
'   $DEFINE and reads file "hp_directives.dirs" and then test.F90.               ',&
'   Output is directed to test_out.f90                                           ',&
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
'   > This is a block of text that will be ignored on output but                 ',&
'   > optionally written to a doc/ file when $PREP_DOCUMENT_DIR is set.          ',&
'   > $BLOCK                                                                     ',&
'   > $!                                                                         ',&
'   > $BLOCK COMMENT --file manual.tex --append                                  ',&
'   > This is a block of text that will be converted to comments and             ',&
'   > optionally appended to a doc/ file when $PREP_DOCUMENT_DIR is set.         ',&
'   > $BLOCK                                                                     ',&
'   > $!                                                                         ',&
'                                                                                ',&
'  Creating a help_usage(3f) subroutine and writing the same documentation       ',&
'  to a doc file (if the environment variable $PREP_DOCUMENT_DIR is set).        ',&
'                                                                                ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@         ',&
'   > $! generate help_usage() procedure and file to run thru txt2man(1)         ',&
'   > $! to make man(1) page if $PREP_DOCUMENT_DIR is set.                       ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@         ',&
'   > $BLOCK HELP --file conditional_compile.man                                 ',&
'   > NAME                                                                       ',&
'   >    conditional_compile - basic example for prep(1) pre-processor.          ',&
'   > SYNOPSIS                                                                   ',&
'   >    conditional_example [--help] [--version]                                ',&
'   > DESCRIPTION                                                                ',&
'   >    This is a basic example program showing how documentation can be        ',&
'   >    used to generate program help text                                      ',&
'   > OPTIONS                                                                    ',&
'   >    --help     display this help and exit                                   ',&
'   >    --version  output version information and exit                          ',&
'   > $BLOCK                                                                     ',&
'                                                                                ',&
'  Creating a help_version(3f) subroutine                                        ',&
'                                                                                ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@         ',&
'   > $! generate help_version() procedure                                       ',&
'   > $BLOCK VERSION                                                             ',&
'   > DESCRIPTION: example program showing conditional compilation               ',&
'   > PROGRAM:     conditional_compile                                           ',&
'   > VERSION:     1.0.0, 20160703                                               ',&
'   > AUTHOR:      John S. Urban                                                 ',&
'   > $BLOCK                                                                     ',&
'   > $!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@        ',&
'                                                                                ',&
'  Sample program using help_usage(f), help_version(3f) and M_kracken95(3f):     ',&
'                                                                                ',&
'   > program conditional_compile                                                ',&
'   >    use M_kracken95, only : kracken, lget                                   ',&
'   >    ! use M_kracken95 module to crack command line arguments                ',&
'   >    call kracken("cmd","--help .false. --version .false.")                  ',&
'   >    ! call routine generated by $BLOCK HELP                                 ',&
'   >    call help_usage(lget("cmd_help"))                                       ',&
'   >    ! call routine generated by $BLOCK VERSION                              ',&
'   >    call help_version(lget("cmd_version"))                                  ',&
'   > end program conditional_compile                                            ',&
'                                                                                ',&
' SET USAGE                                                                      ',&
'  Note values are case-sensitive but variable names are not, and there are      ',&
'  pre-defined values for input file, line in input file, date and time that     ',&
'  are NOT ACTIVE until at least one $SET or $IMPORT directive is processed.     ',&
'  That is, unless a variable name is defined no ${NAME} expansion occurs.       ',&
'                                                                                ',&
'   > $set author  William Shakespeare                                           ',&
'   > $import HOME                                                               ',&
'   > write(*,*)''By ${AUTHOR}''                                                 ',&
'   > write(*,*)''File ${FILE}''                                                 ',&
'   > write(*,*)''Line ${LINE}''                                                 ',&
'   > write(*,*)''Date ${DATE}''                                                 ',&
'   > write(*,*)''Time ${TIME}''                                                 ',&
'   > write(*,*)''HOME ${HOME}''                                                 ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   MIT                                                                          ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage

subroutine help_version(l_version)
implicit none
!@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        prep(1f)>',&
'@(#)DESCRIPTION:    Fortran Pre-processor>',&
!'@(#)VERSION:        4.0.0: 20170502>',&
!'@(#)VERSION:        5.0.0: 20201219>',&
'@(#)VERSION:        6.0.0: 20210613>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE       https://github.com/urbanjost/prep.git/>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_out(line)  ! @(#)writeout(3f):  write (most) source code lines to output file
character(len=*),intent(in)    :: line
   integer                     :: istart

   if(G_write_what)then              ! echo "what" lines to stderr
      istart=index(line,'@(#)')
      if(istart.ne.0)then
         write(ERROR_UNIT,'("-->>",a)')trim(line(istart+4:))
      endif
   endif

   call www(line)
end subroutine write_out
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine www(line) ! @(#)www(3f):  change line into a WRITE, HELP/VERSION, COMMENT output line
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

   case('shell')
      write(G_scratch_lun,'(a)',iostat=ios,iomsg=message)trim(line)
      if(ios.lt.0)then
         call stop_prep('*prep:stop* ERROR(68) - FAILED TO WRITE TO PROCESS:'//trim(line)//':'//trim(message))
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

                                               !*! should handle longer lines and split them
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
      call stop_prep('*prepstop* ERROR(063) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_source))
      call stop_prep('*prepstop* ERROR(064) - UNEXPECTED "BLOCK" VALUE. FOUND:'//trim(G_outtype))

   end select

   if(G_MAN_COLLECT)then
      G_MAN=G_MAN//new_line('N')//trim(line)
   endif

   G_comment_count=G_comment_count+1

end subroutine www
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! duplicates from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stderr(msg)
implicit none
character(len=*),intent(in) :: msg
! ident_2="@(#)M_verify::stderr(3f): writes a message to standard error using a standard f2003 method"
integer             :: ios

   write(error_unit,'(a)',iostat=ios) trim(msg)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dissect2(verb,init,pars,error_return)
! "@(#)dissect2(3f): convenient call to parse() -- define defaults, then process"
!
      character(len=*),intent(in)  :: verb            ! the name of the command to be reset/defined  and then set
      character(len=*),intent(in)  :: init            ! used to define or reset command options; usually hard-set in the program.
      character(len=*),intent(in)  :: pars            ! defines the command options to be set, usually from a user input file
      integer                      :: ipars           ! length of the user-input string pars.
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
   names=sep(line)
   do i=1,size(names)
      call set(names(i)//' '//system_getenv(names(i)))
   enddo
end subroutine import
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set(line)
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
character(len=:),allocatable :: val
integer                      :: iend
integer                      :: i
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

  G_expand=.true.

  call dissect2('set','-oo' ,line) ! parse options on input line
  iend=index(line//' ',' ')
  name=upper(line(:iend))
  if(name.eq.'')then
    ! show array
    if(size(keywords).gt.0)then
       write(G_iout,*)'>>>',size(keywords)
       write(G_iout,'(*("!",a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
    endif
  else
     val=line(min(iend+1,len(line)):)
    ! insert and replace entries
    call update(name,val)
    ! remove some entries
    !call update('a')
    ! get some values
    !write(*,*)'get b=>',get('b')
  endif

contains
subroutine update(key,valin)
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
end subroutine update

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

end subroutine set
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_variables(line)
! @(#) brute force variable substitution. maybe add something like wordexp(3c) with command expansion only if --system?
! this is just to try the concept. Either use wordexp or an equivalent to replicate "here document" processing.
! note no automatic continuation of the line if it extends past allowed length, which for Fortran is currently 132 for free-format
! the way this is written it would do recursive substitution and does not know when there is just not a match
character(len=*)              :: line
character(len=:),allocatable  :: temp,search
integer,parameter             :: toomany=1000
integer                       :: i, j
character(len=4096)           :: scratch

write(scratch,'(i0)')G_file_dictionary(G_iocount)%line_number

call set('LINE ' // scratch)
call set('FILE ' // G_file_dictionary(G_iocount)%filename )
call set('TIME ' // getdate('time'))
call set('DATE ' // getdate('cdate'))
temp=trim(line)
do i=1,toomany
   do j=1,size(keywords)
      if(index(temp,'${').ne.0)then
         search='${'//trim(keywords(j))//'}'
         temp=str_replace(temp,search,values(j)(:counts(j)),ignorecase=.true.)
      else
         exit
      endif
   enddo
enddo
line=temp
end subroutine expand_variables
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_getenv(3f) - [M_system:ENVIRONMENT] get environment variable
!!    from Fortran by calling get_environment_variable(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function system_getenv(name,default)
!!
!!     character(len=:),allocatable         :: system_getenv
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: default
!!
!!##DESCRIPTION
!!    The system_getenv() function gets the value of an environment variable.
!!
!!##OPTIONS
!!    name     Return the value of the specified environment variable or
!!             blank if the variable is not defined.
!!    default  If the value returned would be blank this value will be used
!!             instead.
!!
!!##EXAMPLE
!!
!!   Sample setting an environment variable from Fortran:
!!
!!    program demo_system_getenv
!!    use M_system, only : system_getenv
!!    implicit none
!!       write(*,'("USER     : ",a)')system_getenv('USER')
!!       write(*,'("LOGNAME  : ",a)')system_getenv('LOGNAME')
!!       write(*,'("USERNAME : ",a)')system_getenv('USERNAME')
!!    end program demo_system_getenv
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function system_getenv(name,default) result(value)

! ident_23="@(#)M_system::system_getenv(3f): call get_environment_variable as a function with a default value(3f)"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
integer                              :: howbig
integer                              :: stat
character(len=:),allocatable         :: value

   if(NAME.ne.'')then
      call get_environment_variable(name, length=howbig, status=stat, trim_name=.true.)  ! get length required to hold value
      if(howbig.ne.0)then
         select case (stat)
         case (1)     ! print *, NAME, " is not defined in the environment. Strange..."
            value=''
         case (2)     ! print *, "This processor doesn't support environment variables. Boooh!"
            value=''
         case default ! make string to hold value of sufficient size and get value
            if(allocated(value))deallocate(value)
            allocate(character(len=max(howbig,1)) :: VALUE)
            call get_environment_variable(name,value,status=stat,trim_name=.true.)
            if(stat.ne.0)VALUE=''
         end select
      else
         value=''
      endif
   else
      value=''
   endif
   if(value.eq.''.and.present(default))value=default

end function system_getenv
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
!! but trying to use portable Fortran.  If assume compiled by certain compilers could
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
character(len=32) :: val
integer           :: length, rc, r
logical           :: file_exists
character(len=80) :: scratch

   call define('UNKNOWN=0', 0)
   call define('LINUX=1', 0)
   call define('MACOS=2', 0)
   call define('WINDOWS=3', 0)
   call define('CYGWIN=4', 0)
   call define('SOLARIS=5', 0)
   call define('FREEBSD=6', 0)
   call define('OPENBSD=7', 0)

   r = OS_UNKNOWN
   ! Check environment variable `OS`.
   call get_environment_variable('OS', val, length, rc)
   if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
       r = OS_WINDOWS
   else
      ! Check environment variable `OSTYPE`.
      call get_environment_variable('OSTYPE', val, length, rc)
      if (rc == 0 .and. length > 0) then
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
   call define(scratch,0)
end subroutine get_os_type
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
   & --noenv            .false.  &
   & --comment          COMMENT  &
   & --ident            .false.  &
   & --width            1024     &
   & '
logical                       :: isscratch

                                                                        ! allow formatting comments for particular post-processors
   call get_environment_variable('PREP_COMMENT_STYLE',G_comment_style)  ! get environment variable for -comment switch
   if(G_comment_style.eq.'')G_comment_style='default'                   ! if environment variable not set set default
   call substitute(cmd,'COMMENT',trim(G_comment_style))                 ! change command line to have correct default
                                                                        ! this would actually allow any parameter after number

   kracken_comment='!'
   call kracken('prep',cmd)                                       ! define command arguments, default values and crack command line

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

   if(out_filename.eq.'')then                              ! open output file
      G_iout=6
   elseif(out_filename.eq.'@')then
      G_iout=6
      G_IHELP=6
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

   keeptabs=lget('prep_keeptabs')
   G_write_what=lget('prep_verbose')                       ! set flag for special mode where lines with @(#) are written to stderr
   G_comment_style=lower(sget('prep_comment'))             ! allow formatting comments for particular post-processors
   G_system_on = lget('prep_system')                       ! allow system commands on $SYSTEM directives

   call get_os_type()
   call defines()                                          ! define named variables declared on the command line
   call includes()                                         ! define include directories supplies on command line
   call opens()                                            ! convert input filenames into $include directives

   READLINE: do                                            ! read loop to read input file
      read(G_file_dictionary(G_iocount)%unit_number,'(a)',end=7) line
      G_io_total_lines=G_io_total_lines+1
      G_file_dictionary(G_iocount)%line_number=G_file_dictionary(G_iocount)%line_number+1

      if(keeptabs)then
         G_source=line
      else
         call notabs(line,G_source,ilast)                  ! expand tab characters and trim trailing ctrl-M from DOS files
      endif

      if(G_expand)then
         call expand_variables(G_source)
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
         if(.not.isscratch.and. (G_file_dictionary(G_iocount)%unit_number.gt.0))then
            close(G_file_dictionary(G_iocount)%unit_number,iostat=ios)
         endif
      endif

      G_iocount=G_iocount-1
      if(G_scratch_lun.ne.-1)then
         ios=filedelete(G_scratch_file//'.out')
         G_scratch_lun=-1
      endif

      if(G_iocount.lt.1)exit
   enddo READLINE

   if (G_nestl.ne.0) then                                           ! check to make sure all if blocks are closed
      call stop_prep('*prep* ERROR(067) - $IF BLOCK NOT CLOSED.')
   endif
   call print_comment_block()
end program prep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
