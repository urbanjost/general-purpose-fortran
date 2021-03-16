

subroutine paranoia(precis)
use M_journal, only : journal
character(len=*),parameter :: ident="@(#)paranoia(3f): call doublprecision or real versions of paranoia"
character(len=*),intent(in) :: precis

   if(precis.eq.'single')then
      call journal('*paranoia" single precision test')
      call sparanoia()
   elseif(precis.eq.'double')then
      call journal('*paranoia" double precision test')
      call dparanoia()
   elseif(precis.eq.'all')then
      call journal('*paranoia" single precision test')
      call sparanoia()
      call journal('*paranoia" double precision test')
      call dparanoia()
   else
      call journal('*paranoia* unknown precision request '//precis)
   endif

end subroutine paranoia
