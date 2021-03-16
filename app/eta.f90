program character_frequency ! count frequency of occurrence of characters in an ASCII file
use :: M_io,      only : slurp
use :: M_CLI2,    only : set_args, lget, filenames=>unnamed
use :: M_sort,    only : sort_quick_rx
use :: M_strings, only : describe, lower
implicit none
character(len=1),allocatable :: txt(:) ! array to hold file in memory
integer                      :: i,j,k,ind(0:127),is=127,ie=0,ii
integer,allocatable          :: lets(:)
   call set_args(' --letters:l F') ! option to only count letters ignoring case
   if(size(filenames).eq.0)then
      filenames=['stdin']
   endif
   do j=1,size(filenames)
      if(filenames(j).eq.'-'.or.filenames(j).eq.'stdin')then
         write(*,*)'*eta* cannot read from stdin'
      else
         call slurp(filenames(j),txt) ! allocate character array and copy file into it
      endif
      if(.not.allocated(txt))then
         write(*,*)'*eta* failed to load file '//filenames(j)
      else
         if(lget('letters'))then
            txt=lower(txt)
            is=122;ie=96
         endif
         lets=[(count(char(i).eq.txt),i=ie,is)]
         write(*,'(*(g0))')'filename=',filenames(j),' chars=',size(txt),' lines=',max(count(char(10).eq.txt),count(char(13).eq.txt))
         call sort_quick_rx(real(lets),ind(0:is-ie+1))
         do i=size(lets)-1,0,-1
            if(lets(ind(i)).eq.0)exit
            write(*,'(i9,1x,a)')lets(ind(i)),describe(char(ind(i)+ie-1))
         enddo
         deallocate(txt)  ! release memory
      endif
   enddo
end program character_frequency
