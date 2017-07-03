!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
function nc_uniqname(name,ierr) !@(#) append a number to the end of the filename that makes it relatively unique
   ! If neccessary, increment the number and try again up to the value 9999.
   ! assumes returned value is 256 characters
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in) :: name
   integer,intent(out)         :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=256)          :: nc_uniqname
   logical                     :: around
   integer,save                :: icount=0      ! counter to generate suffix from
   character(len=256),save     :: lastname=' '  ! name called with last time the routine was called
   integer                     :: ilen
   integer                     :: itimes
   integer                     :: ii
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   itimes=0                                     ! count number of times tried to get a unique name
   nc_uniqname='UNIQ_DID_NOT_WORK'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                     ! if a different input name than last time called reset icount
      lastname=name                             ! a new name to keep for subsequent calls
      icount=0                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                          ! find last non-blank character in file name
   if(ilen.gt.(256-4))then                      ! input filename is too long to add suffix to
      call nc_errmessage('*nc_uniqname* ERROR: unable to append number to filename. Filename is too long')
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do                                 ! top of loop trying for a unique name
      if(itimes.ge.9999)then                    ! if too many tries to be reasonable give up
         call nc_errmessage('*nc_uniqname* unable to find a unique filename. Too many tries')
         ierr=-2
         return
      endif
      if(icount.gt.9999) icount=0               ! reset ICOUNT when it hits arbitrary maximum value
      ii=index(name,'.')
      if(ii.eq.0)then
         nc_uniqname=name
         write(nc_uniqname(ilen+1:),'(i4.4,a)')icount,'.out'      ! create name by adding a numeric string to end
      else
         nc_uniqname=name(:ii-1)
         write(nc_uniqname(ii:),'(i4.4,a)')icount,trim(name(ii:)) ! create name by adding a numeric string before suffix
      endif
      icount=icount+1                           ! increment counter used to come up with suffix
      inquire(file=trim(nc_uniqname),exist=around) ! see if this filename already exists
      if(.not.around)then                       ! found an unused name
         return                                 ! return successfully
      endif
      itimes=itimes+1                           ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function nc_uniqname 
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
