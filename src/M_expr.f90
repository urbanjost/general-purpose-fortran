module M_expr
! evaluate Fortran-like integer and logical expressions

use iso_fortran_env, only : stderr=>error_unit, stdout=>output_unit,stdin=>input_unit
use M_strings,   only : nospace, v2s, substitute, upper, split, str_replace=>replace, sep, glob
use M_list,      only : dictionary
implicit none
private

public                               :: expr
public                               :: undef
public                               :: get_integer_from_string
integer,public,parameter             :: G_line_length=4096             ! allowed length of input lines
integer,public,parameter             :: G_var_len=63                   ! allowed length of variable names
integer,public,save                  :: G_iout=stdout                  ! output unit
logical,public,save                  :: G_verbose=.false.
logical,public,save                  :: G_debug=.false.

type(dictionary),public,save         :: table
character(len=G_line_length)         :: G_source=''                    ! original source file line
integer,save                         :: G_error=0

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! if def=.true.  define if just a variable name to "1"
! if logical=.true. must return .true. or .false.
! if ierr/=0 an error occurred
recursive subroutine expr(line,value,ierr,def,logical)                !@(#)expr(3f): process '[variablename=]expression' directive
character(len=*),intent(in)           :: line
character(len=G_var_len),intent(out)  :: value              ! returned variable value
integer,intent(out)                   :: ierr
logical,intent(in),optional           :: def
logical,intent(in),optional           :: logical
character(len=:),allocatable          :: array(:)
character(len=G_line_length)          :: expression
!character(len=:),allocatable          :: expression
logical                               :: def_local
integer :: i
   if(present(def))then
      def_local=def
   else
      def_local=.false.
   endif
   G_source=line
   G_error=0
   if(line.eq.'')then
      write(*,*)'BLANKS'
      call show_state(msg='Variables:')
      value=''
      ierr=0
      return
   endif
   call split(nospace(upper(line)),array,delimiters=';')            ! split string to an array parsing on delimiters
   do i=1,size(array)
     expression=nospace(trim(array(i)))
     FIND_DEFINED: do                                               ! find and reduce all DEFINED() functions to ".TRUE." or ".FALSE."
        if (index(expression,'DEFINED(').ne.0) then                 ! find a DEFINED() function
           call ifdefined(expression,index(expression,'DEFINED('))  ! reduce DEFINED() function that was found
           cycle                                                    ! look for another DEFINED() function
        endif
        exit                                                        ! no remaining DEFINED() functions so exit loop
     enddo FIND_DEFINED
   ! normalize logical operators
     expression=str_replace(expression,'==','.EQ.')
     expression=str_replace(expression,'/=','.NE.')
     expression=str_replace(expression,'!=','.NE.')
     expression=str_replace(expression,'>=','.GE.')
     expression=str_replace(expression,'<=','.LE.')
     expression=str_replace(expression,'>','.GT.')
     expression=str_replace(expression,'<','.LT.')
     expression=str_replace(expression,'&&','.AND.')
     expression=str_replace(expression,'||','.OR.')
     expression=str_replace(expression,'!','.NOT.')
     expression=str_replace(expression,'.XOR.','.NEQV.')
     if(index(expression,'=').ne.0)then
        call let(expression,value,def)
     elseif(def_local)then
        call let(expression,value,def)
     else
        if(present(logical))then
           call eval(expression,value,logical)
        else
           call eval(expression,value,.false.)
        endif
     endif
   enddo
   ierr=G_error
end subroutine expr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine let(expression,value,def)                      !@(#)define(3f): process 'variablename[=expression]' directive
character(len=*),intent(in)  :: expression                   ! packed uppercase working copy of input line
character(len=G_var_len)     :: value                        ! returned variable value

character(len=G_line_length) :: temp                         ! scratch
integer                      :: iequ                         ! location of "=" in the directive, if any
integer                      :: iname                        ! length of variable name
logical,intent(in),optional  :: def
logical                      :: def_local
   if(present(def))then; def_local=def; else; def_local=.false.; endif

   if(expression.eq.'')then
      value='0'
      return
   endif

   iequ=index(expression,'=')                                ! find "=" in "variable_name=expression" if any
   iname=merge(len_trim(expression),iequ-1,iequ.eq.0)        ! find end of variable name
   call checkname(expression(:iname))                        ! check that variable name is composed of allowed characters

   if(def_local)then
      if (iequ.eq.0) then                                    ! if no = then variable assumes value of 1
         temp='1'                                            ! no = but a definition so set expression to "1"
      else                                                   ! =value string trails name on directive
         temp=expression(iequ+1:)                            ! get expression
      endif
   else
      temp=expression(iequ+1:)                               ! get expression
   endif

   call eval(temp,value)
   ! check answer
   temp=nospace(value)
   select case(temp)
   case('.FALSE.','.TRUE.')
      call table%set(expression(:iname),temp)
   case default ! assumed a number
      if ( verify(temp(1:1),'0123456789+-').eq.0 .and.  verify(temp(2:len_trim(temp)),'0123456789').eq.0 ) then
         call table%set(expression(:iname),temp)
      elseif (temp(1:1).ge.'A'.and.temp(1:1).le.'Z'.or.temp(1:1).eq.'_')then ! appears to be variable name not number or logical
        value=table%get(temp)                                                ! find defined parameter in dictionary
        if (value.eq.'')then                                                 ! unknown variable name
           call oops('*M_expr* ERROR(003) - Undefined variable name:'//trim(expression))
        else
           if(def_local)then
              call checkname(expression(:iname))                          ! test for legal variable name
              call table%set(expression(:iname),value)
           endif
        endif
      else
         call oops('*M_expr* ERROR(004) - Not logical or integer expression:'//trim(expression))
      endif
   end select

end subroutine let
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine eval(expression,value,logical)
!@(#)eval(3f): evaluate math expression to .TRUE. or .FALSE. or integer value
character(len=*),intent(in)    :: expression
character(len=*),intent(out)   :: value
character(len=G_line_length)   :: temp
logical,intent(in),optional    :: logical
logical                        :: logical_local
integer                        :: iostat

   if(present(logical))then
      logical_local=logical
   else
      logical_local=.false.
   endif

   temp=expression
   if(G_verbose)write(*,*)'*eval*:',trim(temp)
   call parens(temp);                  if(G_verbose)write(*,*)'*eval*:after parens:',trim(temp)
   call math(temp,1,len_trim(temp));   if(G_verbose)write(*,*)'*eval*:after math:',trim(temp)
   call doop(temp,1,len_trim(temp));   if(G_verbose)write(*,*)'*eval*:after doop:',trim(temp)
   call logic(temp,1,len_trim(temp));  if(G_verbose)write(*,*)'*eval*:after logic:',trim(temp)

   ! check answer
   temp=nospace(temp)
   select case(temp)
   case('.FALSE.','.TRUE.','T','F','.T.','.F.')
   case default ! assumed a number
      if ( verify(temp(1:1),'0123456789+-').eq.0 .and.  verify(temp(2:len_trim(temp)),'0123456789').eq.0 ) then
      elseif (logical_local)then
         write(temp,'(g0)',iostat=iostat)true_or_false(temp,1,len_trim(temp)) 
         if(iostat.ne.0)then
            call oops('*M_expr* ERROR(005) - logical expression required:'//trim(expression))
         endif
      elseif (temp(1:1).ge.'A'.and.temp(1:1).le.'Z'.or.temp(1:1).eq.'_')then ! appears to be variable name not number or logical
        temp=table%get(temp)                                                ! find defined parameter in dictionary
        if (temp.eq.'')then                                                 ! unknown variable name
           call oops('*M_expr* ERROR(001) - Undefined variable name:'//trim(expression))
        endif
      else
         call oops('*M_expr* ERROR(002) - Not logical or integer expression:'//trim(expression))
      endif
   end select

   value=temp ! should be variable name to get value of or integer number or logical

end subroutine eval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine parens(line)                       !@(#)parens(3f): find subexpressions in parenthesis and process them
character(len=G_line_length)    :: line       ! line        -
integer                         :: i
integer                         :: j

   TILLDONE: do
      if (index(line,')').ne.0) then          ! closing parens found
         do i=index(line,')'),1,-1            ! find first right paren, then backwards to left paren (find innermost set of parens)
            if (line(i:i).eq.'(') exit
         enddo
         if (i.eq.0) then
            call oops("*M_expr* ERROR(014) - Constant logical expression required:"//trim(G_source))
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
               !   call oops("*M_expr* 1**(-1) NOT IMPLEMENTED YET")
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
            !   !call oops("*M_expr* 1**(-1) Not Implemented Yet")
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
      call oops('*M_expr* ERROR(015) - Constant logical expression required:'//trim(G_source))
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
recursive subroutine domath(line,ipos2)            !@(#)domath(3f): reduce integer expression containing  +-/* and ** operators
character(len=*)                :: line
integer                         :: ipos2

character(len=11)               :: temp
character(len=G_line_length)    :: newl
character(len=2),parameter      :: ops(3)= ['**','*/','+-']
integer                         :: i
integer                         :: j
integer                         :: location
integer                         :: minus1
integer                         :: i1
integer                         :: i2
integer                         :: l
integer                         :: length
integer                         :: numop

  if (ipos2.eq.0) then
     return
  endif
  location=0
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

       length=1                                 ! operator length
       IF (NUMOP.EQ.1) length=2
       IF (I.EQ.len_trim(NEWL)) then            ! if operator is at end of string
          call oops("*M_expr* ERROR(016) - Incomplete statement. Operator (**,/,*,+,-) at string end:"//trim(G_SOURCE))
          exit OVERALL
       endif
       IF (I.EQ.1.AND.NUMOP.NE.3) then          ! if operator at beginning of string and not +-
        call oops("*M_expr* ERROR(017) - Syntax error. Operator (**,*,/) not allowed to prefix expression:"//trim(G_SOURCE))
          exit OVERALL
       endif
       if (.not.(i.eq.1.and.numop.eq.3)) then   ! if processing +- operators and sign at beginning of string skip this
          if (index('*/+-',newl(i-1:i-1)).ne.0.or.index('*/+-',newl(i+length:i+length)).ne.0) then
            call oops('*M_expr* ERROR(018) - Syntax error in domath:'//trim(G_source))
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

       i2=get_integer_from_string(newl(i+length:l-1))

       if (numop.eq.1) then
         i1=i1**i2*minus1
       else
          select case (index('*/+-',newl(i:i)))
          case(1)
             i1=i1*i2*minus1
          case(2)
             if(i2.eq.0)then
                call oops('*M_expr* ERROR(019) - Divide by zero:'//trim(G_source))
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
             call oops('*M_expr* ERROR(020) - Internal program error:'//trim(G_source))
             exit OVERALL
          end select
       endif

       if (i1.le.0) then
         if (j.eq.i-1.and.j.ne.1) then
           minus1=-1
           i1=abs(i1)
           location=j+1
           newl(j+1:j+1)=' '
           l=l-1
           newl=nospace(newl)
         elseif (i.eq.1.and.numop.eq.3) then
           minus1=-1
           i1=abs(i1)
           location=i
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

  if (minus1.eq.-1.and.(location.eq.0.or.location.eq.1)) then
     newl(:G_line_length)='-'//trim(newl)  !x! note potentially trimming a character off the end
  elseif (minus1.eq.-1.and.location.ne.1) then
     newl=newl(:location-1)//'-'//newl(location:)
  endif

  line(:ipos2)=newl(:len_trim(newl))

end subroutine domath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
recursive subroutine doop(line,ipos1,ipos2)             !@(#)doop(3f): find VAL.OP.VAL strings and reduce to .TRUE. or .FALSE.
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

   if(G_verbose)write(*,*)'*doop*:TOP:',trim(line),ipos1,ipos2
   newl=line(ipos1:ipos2)
   if(G_verbose)write(*,*)'*doop*:NEWL:',trim(newl)
   CHECK_EACH_OP_TYPE: do i=1,6
      FIND_MORE_OF: do
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
            call domath(val1,len_trim(val1)) ! instead of a simple integer it could be an expression
            
            ival1=get_integer_from_string(val1)
            ival2=get_integer_from_string(val2)
            temp='.FALSE.'
            select case(i)                                       ! determine truth
            case(1); if (ival1.eq.ival2) temp='.TRUE.' ! .eq.
            case(2); if (ival1.ne.ival2) temp='.TRUE.' ! .ne.
            case(3); if (ival1.ge.ival2) temp='.TRUE.' ! .ge.
            case(4); if (ival1.gt.ival2) temp='.TRUE.' ! .gt.
            case(5); if (ival1.le.ival2) temp='.TRUE.' ! .le.
            case(6); if (ival1.lt.ival2) temp='.TRUE.' ! .lt.
            case default
             temp='.FALSE.'
            end select
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
   if(G_verbose)write(*,*)'*doop*:END:',trim(line)
end subroutine doop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine logic(line,ipos1,ipos2)           !@(#)logic(3f): process .OP. operator strings
character(len=*)             :: line
integer,intent(in)           :: ipos1, ipos2

logical                      :: left, right
character(len=7)             :: temp
character(len=G_line_length) :: newl
integer                      :: i,j,k,l
character(len=6),parameter   :: ops(6)= (/'.NOT. ','.AND. ','.OR.  ','.EQV. ','.NEQV.','.DEF. '/)
integer,parameter            :: opl(6)= [(len_trim(ops(i)),i=1,size(ops))]
integer                      :: ieqv
integer                      :: ineqv
integer                      :: i1
integer                      :: iop
integer                      :: chrs
integer                      :: len1
integer                      :: len2
logical                      :: answer

   newl=line(ipos1:ipos2)
   len1=0
   len2=0
   left=.false.
   LOOP: do i=1,3   ! process .not, .and., .or.
      INFINITE: do
           chrs=len_trim(ops(i))
           IF (INDEX(NEWL,OPS(I)(:chrs)).EQ.0) cycle LOOP
           I1= INDEX(NEWL,OPS(I)(:chrs))-1
           J=I1+1
           LEN1=0
           IF (I.NE.1) then
              OUTER: DO J=I1,1,-1
                INNER: DO K=1,size(ops)
                   LEN1=opl(k)
                   IF (INDEX(NEWL(J:I1),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUTER
                enddo INNER
              enddo OUTER
              IF (J.EQ.0) LEN1=1
              left=true_or_false(NEWL,J+LEN1,I1)
           endif

           OUT: DO L=I1+chrs,len_trim(NEWL)
             IN: DO K=1,size(ops)
                LEN2=opl(k)
                IF (INDEX(NEWL(I1+chrs:L),OPS(K)(:len_trim(OPS(K)))).NE.0) exit OUT
             enddo IN
           enddo OUT

           IF (L.GT.len_trim(NEWL)) LEN2=0
           right=true_or_false(NEWL,I1+chrs+1,L-LEN2)

           select case(i)
           case(1); answer=.not.right
           case(2); answer=left.and.right
           case(3); answer=left.or.right
           case default
              call oops('*M_expr* ERROR(300) - Internal program error:'//trim(G_source))
           end select

           temp='.FALSE.'
           if (answer) temp='.TRUE.'
           call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
        enddo INFINITE
   enddo LOOP

   TILLDONE: do
      ieqv=index(newl,'.EQV.')
      ineqv=index(newl,'.NEQV.')
      if (ieqv*ineqv.eq.0.and.ieqv.ne.ineqv) then ! if one found but not both 
        iop=max(ieqv,ineqv)
      elseif (ieqv.ne.0) then                     ! if found .EQV. 
        iop=min(ieqv,ineqv)
      elseif (ipos1.eq.1) then
        line=newl(:len_trim(newl))//line(ipos2+1:)
        return
      else
        line=line(:ipos1-1)//newl(:len_trim(newl))//line(ipos2+1:)
        return
      endif

      chrs=5
      if (index(newl,'.EQV.').ne.iop) chrs=6
      do j=iop-1,1,-1
         if (newl(j:j+1).eq.'V.') exit
      enddo
      if (j.eq.0) len1=1
      left=true_or_false(newl,j+len1,iop-1)
      do l=iop+chrs,len_trim(newl)
         if (newl(l:l+1).eq.'.E'.or.newl(l:l+1).eq.'.N') exit
      enddo
      if (l.gt.len_trim(newl)) len2=0
      right=true_or_false(newl,iop+chrs,l+len2)
      answer=left.eqv.right
      if (chrs.ne.5) answer=left.neqv.right
      temp='.FALSE.'
      if (answer) temp='.TRUE.'
      call rewrit(newl,temp(:len_trim(temp)),j,j+len1-1,l,l-len2+1)
   enddo TILLDONE
end subroutine logic
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
         call oops('*M_expr* ERROR(021) - Undefined variable. Expression='//trim(G_source)//'. Variable='//trim(substring))
      else
         read(value,'(l4)',iostat=ios) true_or_false         ! try to read a logical from the value for the variable name
         if(ios.ne.0)then                                    ! not successful in reading string as a logical value
            call oops('*M_expr* ERROR(022) - Constant logical expression required.'//trim(G_source))
         endif
      endif

   end select

end function true_or_false
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_integer_from_string(line) !@(#)get_integer_from_string(3f): read integer value from line
                                                                    ! assume string is a variable name or an integer value
character(len=*),intent(in)   :: line                                ! string to read an integer value from
integer                       :: ios                                 ! I/O error value to check to see if internal reads succeeded
integer                       :: get_integer_from_string             ! integer value to return if string is converted successfully
character(len=:),allocatable  :: value
character(len=G_var_len)      :: rendered_value
integer                       :: ierr
   get_integer_from_string=0
   if(len_trim(line).eq.0)then
      get_integer_from_string=0
   elseif (verify(line,'0123456789 +-').eq.0) then                  ! assumed a number
      read(line,'(i11)',iostat=ios) get_integer_from_string         ! try to read integer value from input string
      if(ios.ne.0)then                                              ! failed to convert the string to an integer, so stop
        call oops('*M_expr* ERROR(023) - Must be integer:"'//trim(line)//'" IN '//trim(G_source))
      endif
   else                                                             ! input is not a number, assume it represents a variable name
      value=table%get(line)
      if (value.eq.'')then                                          ! if variable name not found in dictionary, stop
        call oops('*M_expr* ERROR(024) - Undefined variable name:"'//trim(line)//'" IN '//trim(G_source))
      else
         call expr(value,rendered_value,ierr)                       ! recursive call
         value=trim(rendered_value)
         read(value,'(i11)',iostat=ios) get_integer_from_string     ! read integer value from the value associated with name
         if(ios.ne.0)then                                           ! failed reading integer from value, stop
           call oops('*M_expr* ERROR(025) - Must be integer:"'//trim(line)//"="//trim(value)//'" IN '//trim(G_source))
         endif
      endif
   endif                                                            ! return integer value
end function get_integer_from_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine rewrit(line,temp,j,j1,l,l1)           !@(#)rewrit(3f):
character(len=G_line_length)  :: line
character(len=*),intent(in)   :: temp
integer,intent(in)            :: j,j1, l,l1

   if(G_verbose)write(*,*)'*rewrit*:',trim(line),trim(temp),j,j1,l,l1
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
   if(G_verbose)write(*,'(*(g0))')'*rewrit*:END:LINE:',trim(line),':TEMP:',trim(temp),':',j,j1,l,l1
end subroutine rewrit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine checkname(line,ierr)                                                 !@(#)name(3f): test for legal variable name
character(len=*)             :: line
integer,intent(out),optional :: ierr
integer                      :: i

   if (len(line).eq.0)then
   else if (line(1:1).lt.'A'.or.line(1:1).gt.'Z'.and.line(1:1).ne.'_')then                         ! variable names start with a-z
    call oops("*M_expr* ERROR(006) - Name does not start with alphameric or '_' (or general syntax error):"//trim(G_source),ierr)
   elseif(len_trim(line).gt.G_var_len)then
     call oops('*M_expr* ERROR(007) - Variable name exceeds '//v2s(G_var_len)//' characters:'//trim(G_source),ierr)
   endif

   do i=2,len_trim(line)                                                 ! name uses $  _ and letters (A-Z) digits (0-9)
      if(line(i:i).ne.'$'.and.line(i:i).ne.'_'.and.     &
      & (line(i:i).lt.'A'.or.line(i:i).gt.'Z').and.     &
      & (line(i:i).lt.'0'.or.line(i:i).gt.'9')) then
      call oops('*M_expr* ERROR(008) - Variable name contains unallowed character(or general syntax error):'//trim(G_source),ierr)
      endif
   enddo

end subroutine checkname
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
      call oops("*M_expr* ERROR(013) - Incomplete statement."//trim(G_SOURCE))
   endif

   value='.true.'
   line(ipos1:ipos1+6+index(newl,')'))='.TRUE.'
   ifvars= sep(newl(2:index(newl,')')-1),',')

   LIST: do j=1,size(ifvars)

      call checkname(ifvars(j))                          ! test for legal variable name
      value=table%get(ifvars(j))
      if(value.ne.'')cycle LIST
      value='.false.'
      line(ipos1:ipos1+6+index(newl,')'))='.FALSE.'
      exit LIST

   enddo LIST

end subroutine ifdefined
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
integer                                :: ios

   temp=line(ipos1:ipos2)                                                   ! place variable name/value substring into TEMP

   if (temp(1:1).eq.' ')then                                                ! did not find expected variable name or value
      call oops('*M_expr* ERROR(009) - Incomplete statement.'//trim(G_SOURCE))
   endif

   if (temp(1:1).ge.'A'.and.temp(1:1).le.'Z'.or.temp(1:1).eq.'_') then      ! appears to be a variable name (not number or logical)

     value=table%get(temp)                                                  ! find defined parameter in dictionary
     if (value.eq.'')then                                                   ! unknown variable name
        call oops('*M_expr* ERROR(010) - Undefined variable name:'//trim(temp)//' in expression '//trim(G_source))
        do i=1,size(table%key)                                                        ! print variable dictionary
           write(G_iout,'(*(g0,1x))')"!    $DEFINE",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)) )
        enddo
     endif
     return
   else                                                                     ! not a variable name, try as a value
     read(temp(1:11),'(i11)',iostat=ios) ivalue                             ! try string as a numeric integer value
     if(ios.eq.0)then
        write(value,'(i11)') ivalue                                         ! write numeric value into VALUE
        return                                                              ! successfully return numeric VALUE
     endif

     continue                                                               ! failed to read numeric value
     value=temp(:G_var_len)                                                 ! test TEMP as a logical
     if (value.ne.'.FALSE.'.and.value.ne.'.TRUE.')then                      ! if here, value should be a logical
        call oops('*M_expr* ERROR(011) - Syntax error.'//trim(G_source))
     endif
                                                                            ! value is ".TRUE." or ".FALSE."
   endif

   if(temp(1:1).ge.'A')then
      call oops('*M_expr* ERROR(012) - Defined value must be an integer or logical constant.'//trim(G_source))
   endif

end subroutine getval
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
      call oops('*M_expr* ERROR(026) - missing targets:'//trim(G_source))
   endif
   call split(opts,names,delimiters=' ;,')

   do k=1,size(names)
      do i=size(table%key),1,-1                             ! find defined variable to be undefined by searching dictionary
         if (glob(trim(table%key(i)),trim(names(k))))then   ! found the requested variable name
            call table%del(table%key(i))
         endif
      enddo
   enddo

end subroutine undef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine show_state(list,msg)                        !@(#)debug(3f): process $SHOW command or state output when errors occur
character(len=*),intent(in),optional :: list
character(len=*),intent(in) :: msg
integer                     :: i, j, ibug
character(len=:),allocatable   :: array(:)             ! array of tokens
character(len=*),parameter  :: fmt='(*(g0,1x))'
   if(present(list))then
      if(list.ne.'')then
         ! print variables:
         CALL split(list,array,delimiters=' ;,')       ! parse string to an array parsing on delimiters
         do j=1,size(array)
            do i=1,size(table%key)
               if(glob(trim(table%key(i)),trim(array(j))))then ! write variable and corresponding value
                  write(G_iout,fmt)"! ",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)))
               endif
            enddo
         enddo
      endif
   else
      write(G_iout,'(a)')'!==============================================================================='
      write(G_iout,'(a)')'! '//trim(msg)
      ! added UBOUND call because GFORTRAN returns size of 1 when undefined, OK with ifort and nvfortran
      ibug=minval([size(table%key),ubound(table%key)])   ! print variable dictionary
      do i=1,ibug                                     ! print variable dictionary
         write(G_iout,fmt)"!   ",trim(table%key(i)),' = ',adjustl(table%value(i)(:table%count(i)) )
      enddo
      write(G_iout,'(a)')'!-------------------------------------------------------------------------------'
   endif
end subroutine show_state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine oops(message,ierr)   
!@(#) oops(3f): write MESSAGE to stderr if ierr not present and extract error number from ERROR(nnn)
character(len=*),intent(in)  :: message
integer                      :: ios
integer                      :: iwhere
integer,intent(out),optional :: ierr
   write(stderr,'(a)',iostat=ios) trim(G_SOURCE)
   iwhere=index(message,'ERROR(')
   if(iwhere.ne.0)then
      read(message(iwhere+6:iwhere+8),'(i3)')G_error
   else
      write(G_iout,'(a)')'! *M_expr* ERROR(-999) - Message does not contain properly formatted ERROR CODE: '//trim(message)
      G_error=-999
   endif
   if(.not.present(ierr))then
      write(G_iout,'(a)')'! '//trim(message)
      flush(unit=stdout,iostat=ios)
      flush(unit=stderr,iostat=ios)
   endif
end subroutine oops
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_expr
