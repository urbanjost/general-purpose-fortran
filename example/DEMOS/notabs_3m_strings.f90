     program demo_notabs
     use M_strings, only : notabs
     character(len=255)           :: in,out
     character(len=:),allocatable :: string
     character(len=1),parameter   :: t=char(9) ! horizontal tab
     integer                      :: iostat,iout,lun
     call makefile(lun) ! create scratch file
     ! read file and expand tabs
     do
        read(lun,'(A)',iostat=iostat)in
        if(iostat /= 0) exit
        call notabs(in,out,iout)
        write(*,'(a)')out(:iout)
     enddo
     string='one'//t//'two'//t//'three'
     call notabs(string,lgth=iout)
     out=repeat(' ',iout)
     call notabs(string,out)
     write(*,*)'['//string//']'
     contains
     subroutine makefile(lun)
     integer                     :: lun
     integer                     :: i
     character(len=80),parameter :: fakefile(*)=[character(len=80) :: &
     'col1'//t//'col2' ,&
     'a'//t//'one'     ,&
     'bb'//t//'two'    ,&
     'ccc'//t//'three' ,&
     'dddd'//t//'four' ,&
     '']
     ! create input file
        open(newunit=lun,status='scratch')
        write(lun,'(a)')(trim(fakefile(i)),i=1,size(fakefile))
        rewind(lun)
     end subroutine makefile
     end program demo_notabs
