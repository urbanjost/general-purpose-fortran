!===================================================================================================================================
MODULE M_html
private
public h_open
public h_close
public h_array
CONTAINS
!===================================================================================================================================
subroutine h_array(array,iounit)
use M_journal, only : journal
implicit none
character(len=*),parameter :: ident="@(#)M_html::h_array(3f):write table from array"
integer,intent(in) :: iounit
   integer         :: i
   integer         :: j
   real,intent(in) :: array(:,:)
   write(iounit,'(a)')'<table border="1">'
   do i=1,size(array,dim=1)
      write(iounit,*)'<tr>'
      do j=1,size(array,dim=2)
         write(iounit,*)'<td>',array(i,j),'</td>'
      enddo
      write(iounit,'(a)')'</tr>'
   enddo
   write(iounit,'(a)')'</table>'
end subroutine h_array
!===================================================================================================================================
subroutine h_close(iounit)
use M_journal, only : journal
implicit none
character(len=*),parameter :: ident="@(#)M_html::h_close(3f):close HTML file"
integer,intent(in) :: iounit
   write(iounit,*)'</body>'
   write(iounit,*)'</html>'
   close(unit=iounit)
end subroutine h_close
!===================================================================================================================================
subroutine h_open(filename,iounit)
use M_journal, only : journal
implicit none
character(len=*),parameter :: ident="@(#)M_html::h_open(3f):open HTML file"
character(len=*),intent(in) :: filename
integer,intent(in)          :: iounit
end subroutine h_open
!===================================================================================================================================
!===================================================================================================================================
END MODULE M_html
!===================================================================================================================================
!===================================================================================================================================
