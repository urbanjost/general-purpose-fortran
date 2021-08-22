










!>
!!##NAME
!!    M_html(3fm) - [M_html] a module of routines to help write output as HTML documents
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    use M_html, only : h_open, h_close
!!    use M_html, only : h_array
!!
!!##DESCRIPTION
!!
!!     o h_open(lun)          open HTML file and create simple header
!!     o h_close(lun)         close HTML file
!!     o h_array(lun,array)   print a numeric array as an HTML table
!!##EXAMPLE
!!
!!   Sample Usage
!!
!!    program demo_M_html
!!       use M_html
!!       implicit none
!!       integer :: i,j
!!       real    :: arr(3,4)=reshape(         &
!!       & [(((i-1)*3.0+j*2.0,i=1,3),j=1,4)], &
!!       & shape(arr),order=[1,2])
!!       integer :: io=6
!!       call h_open(io,'table.html')
!!       call h_array(io,arr)
!!       call h_close(io)
!!    end program demo_M_html
!!
!!   Expected output
!!
!!    demo_M_html|w3m||lynx
!!
!!    +-------------------------------------------+
!!    |2.00000000|4.00000000|6.00000000|8.00000000|
!!    |----------+----------+----------+----------|
!!    |5.00000000|7.00000000|9.00000000|11.0000000|
!!    |----------+----------+----------+----------|
!!    |8.00000000|10.0000000|12.0000000|14.0000000|
!!    +-------------------------------------------+
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
MODULE M_html
private
public h_open
public h_close
public h_array
public test_suite_M_html
CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_array(3f) - [M_html] print a numeric array as an HTML table
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine h_array(iounit,array)
!!
!!    integer,intent(in) :: iounit
!!    real,intent(in)    :: array(:,:)
!!
!!##DESCRIPTION
!!    Write an array as an HTML table
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_array
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_array
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine h_array(iounit,array)
use M_journal, only : journal
implicit none

! ident_1="@(#)M_html::h_array(3f):write table from array"

integer,intent(in) :: iounit
real,intent(in)    :: array(:,:)
integer            :: i
integer            :: j
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
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_close(3f) - [M_html] close an HTML file
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine h_close(iounit)
!!
!!    integer,intent(in) :: iounit
!!
!!##DESCRIPTION
!!    Write out
!!
!!       </body>
!!       </html>
!!
!!    and then close an HTML file
!!
!!##OPTIONS
!!    lun       The unit number to close
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_close
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_close
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine h_close(iounit)
use M_journal, only : journal
implicit none

! ident_2="@(#)M_html::h_close(3f):close HTML file"

integer,intent(in) :: iounit
   write(iounit,*)'</body>'
   write(iounit,*)'</html>'
   close(unit=iounit)
end subroutine h_close
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    h_open(3f) - [M_html] open an HTML file
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine h_open(iounit)
!!
!!    integer,intent(in) :: iounit
!!    character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!    Open  an HTML output file and then Write out
!!
!!       <html>
!!       <head>
!!       <title>FILENAME </title>
!!       </head>
!!       <body>
!!
!!##OPTIONS
!!    lun       The unit number to open
!!    filename  Name of the file to open
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_h_open
!!    use M_html
!!    implicit none
!!    real    :: arr(10,20)=0.0
!!    integer :: io=20
!!    integer :: i,j
!!    do i=1,10
!!       do j=1,20
!!          arr(i,j)=(i-1)*20+j
!!       enddo
!!    enddo
!!    call h_open(io,'table.html')
!!    call h_array(io,arr)
!!    call h_close(io)
!!
!!    end program demo_h_open
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine h_open(iounit,filename)
use M_journal, only : journal
implicit none

! ident_3="@(#)M_html::h_open(3f):open HTML file"

character(len=*),intent(in) :: filename
integer,intent(in)          :: iounit
   open(unit=iounit,file=filename)
   write(iounit,*)'<html>'
   write(iounit,*)'<head>'
   write(iounit,*)'<title>',trim(filename),'</title>'
   write(iounit,*)'</head>'
   write(iounit,*)'<body>'
end subroutine h_open
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_suite_M_html()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_h_array()
   call test_h_close()
   call test_h_open()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_array()

   call unit_check_start('h_array',msg='')
   !!call unit_check('h_array', 0.eq.0, 'checking',100)
   call unit_check_done('h_array',msg='')
end subroutine test_h_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_close()

   call unit_check_start('h_close',msg='')
   !!call unit_check('h_close', 0.eq.0, 'checking',100)
   call unit_check_done('h_close',msg='')
end subroutine test_h_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_h_open()

   call unit_check_start('h_open',msg='')
   !!call unit_check('h_open', 0.eq.0, 'checking',100)
   call unit_check_done('h_open',msg='')
end subroutine test_h_open
!===================================================================================================================================
end subroutine test_suite_M_html
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE M_html
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
