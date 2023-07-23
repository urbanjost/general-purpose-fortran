!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor 
use M_io
use M_framework__msg
use :: M_framework__verify, only : unit_check_start, unit_check, unit_check_done, unit_check_good, unit_check_bad, unit_check_msg
use :: M_framework__verify, only : unit_check_stop
use :: M_framework__verify, only : unit_check_level
use,intrinsic :: iso_fortran_env, only : stdin_lun  => input_unit
use,intrinsic :: iso_fortran_env, only : stderr_lun => error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor

!! setup

   call test_dirname()
   call test_get_tmp()
   call test_print_inquire()
   call test_rd()
   call test_getline()
   call test_read_line()
   call test_read_table()
   call test_filebyte()
   call test_splitpath()
   call test_uniq()
   call test_notopen()
   call test_filename_generator()
   call test_fileread()
   call test_number_of_lines()
   call test_basename()
   call test_joinpath()
   call test_fileopen()
   call test_fileclose()
   call test_filewrite()
   call test_filedelete()
   call test_get_env()
   call test_get_next_char()
!$IFNDEF GITHUB
!   call test_scratch()
!$ENDIF
   call test_separator()
   call test_which()
   call test_getname()
   call test_lookfor()
!! teardown
   call unit_check_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dirname()

call unit_check_start('dirname',msg='')
call unit_check('dirname',  dirname('/usr/bin/')  ==  '/usr', '/usr/bin ==>',dirname('/usr/bin'))
call unit_check('dirname',  dirname('dir1/str/')  ==  'dir1', 'dir1/str ==>',dirname('dir1/str/'))
call unit_check('dirname',  dirname('stdio.h')    ==  '.',    '/stdio.h ==>',dirname('stdio.h'))
call unit_check_done('dirname',msg='')
end subroutine test_dirname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_tmp()

   call unit_check_start('get_tmp',msg='')
   !!call unit_check('get_tmp', 0 == 0, 'checking',100)
   call unit_check_done('get_tmp',msg='')
end subroutine test_get_tmp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filename_generator()

   call unit_check_start('filename_generator',msg='')
   call unit_check_msg('filename_generator','generate a filename containing a whole number')

   call unit_check('filename_generator', filename_generator('head','.tail',100) ==  'head100.tail' )
   call unit_check('filename_generator', filename_generator('head','.tail',1,3) ==  'head001.tail' )

   call unit_check_done('filename_generator',msg='')

end subroutine test_filename_generator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notopen()

integer :: i, bug, ierr, ierr2

   call unit_check_start('notopen',msg='')
   call unit_check_msg('notopen','check for preassigned files from unit 0 to unit 1000')
   call unit_check_msg('notopen','assume 5 and 6 always return -1')

   do i=0,1000
      if(notopen(i,i,ierr)  /=  i)then
         bug=notopen(i,i,ierr2) ! gfortran 11 bug; OK in 9, 10
         call unit_check_msg('notopen','INUSE:',i,ierr,bug )
      endif
   enddo
   call unit_check('notopen', notopen(5,6,ierr)            ==  -1 ,'preassigned')

   do i=10,30,1
     open(unit=i,status="scratch")
   enddo

   close(25)
   close(28)
   call unit_check('notopen', notopen(10,30)            ==  25 )
   call unit_check('notopen', notopen()                 ==  25 )
   call unit_check('notopen', notopen(start=12,end=30)  ==  25 )
   call unit_check('notopen', notopen(26)               ==  28 )
   call unit_check('notopen', notopen(26,99)            ==  28 )

   call unit_check_done('notopen',msg='')

end subroutine test_notopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_inquire()

   call unit_check_start('print_inquire',msg='')
   !!call unit_check('print_inquire', 0 == 0, 'checking',100)
   call unit_check_done('print_inquire',msg='')
end subroutine test_print_inquire
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rd()

   call unit_check_start('rd',msg='')
   !!call unit_check('rd_character', 0 == 0, 'checking',100)
   call unit_check_done('rd',msg='')
end subroutine test_rd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getline()
character(len=:),allocatable :: line, last, expected
integer                      :: lun, ierr, stat, icount
   call unit_check_start('getline',msg='')
   ierr=filewrite('_scratch_getline.txt>',[ character(len=80) :: &
   &achar(9)//'abcdefghij\ ', &
   &'klmnop'//achar(8)//'\' , &
   &'qrstuv\               ', &
   &'wxyz'])
   open(newunit=lun,file='_scratch_getline.txt',pad='yes')
   icount=0
   INFINITE: do while (getline(line,lun,stat) == 0)
      icount=icount+1
      last=line
      if(unit_check_level.gt.0.or..true.) write (*, '(*(g0))') 'getline>>>>',icount,' [',line,']'
   enddo INFINITE
   expected='wxyz'
   call unit_check('getline',is_iostat_end(stat),'last status got',stat,'expected',iostat_end)
   call unit_check('getline',icount.eq.4,'expected ',4,'lines got',icount)
   call unit_check('getline',last.eq.expected,'expected',expected,'got',last)
   ierr=filedelete('_scratch_getline.txt')
   call unit_check_done('getline',msg='')

end subroutine test_getline
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_line()
character(len=:),allocatable :: line, last, expected
integer                      :: lun, ierr, stat, icount
   call unit_check_start('read_line',msg='')
   ierr=filewrite('_scratch_read_line.txt>',[ character(len=80) :: &
   &achar(9)//'abcdefghij\ ', &
   &'klmnop'//achar(8)//'\' , &
   &'qrstuv\               ', &
   &'wxyz'])
   open(newunit=lun,file='_scratch_read_line.txt',pad='yes')
   icount=0
   INFINITE: do while (read_line(line,lun,ios=stat) == 0)
      icount=icount+1
      last=line
      if(unit_check_level.gt.0) write (*, '(*(g0))') 'read_line>>>>',icount,' [',line,']'
   enddo INFINITE
   expected='        abcdefghijklmnop qrstuvwxyz'
   call unit_check('read_line',is_iostat_end(stat),'last status got',stat,'expected',iostat_end)
   call unit_check('read_line',icount.eq.1,'expected ',1,'lines got',icount)
   call unit_check('read_line',last.eq.expected,'expected',expected,'got',last)
   ierr=filedelete('_scratch_read_line.txt')
   call unit_check_done('read_line',msg='')

end subroutine test_read_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_table()
doubleprecision,allocatable :: array(:,:)
integer :: ierr

   call unit_check_start('read_table',msg='')
   ! create test file
   open(file='inputfile',unit=10,action='write')
   write(10,'(a)') [character(len=80):: &
       '#2345678901234567890123456789012345678901234567890123456789012345678901234567890', &
       '# a comment number 10   ', &
       ' #test                  ', &
       '  # table               ', &
       '.-----.-----.-----.     ', &
       '| 1   | -5  | 3e2 |     ', &
       '.-----+-----+-----.     ', &
       '| 4   | 2.0 | 6   |     ', &
       '.-----.-----.-----.     ', &
       '                        ', &
       '# Original sample file: VEALYL_5mm_50um_pH2p50_12p5mM_NewFreezeT30.0384.DAT     ']
   close(unit=10)
   ! read file as a table
   call read_table('inputfile',array,ierr,comment='#')
      ! print values
   call unit_check( 'read_table', size(array      )  ==  6,                 'checking size' )
   call unit_check( 'read_table', size(array,dim=1)  ==  2,                 'checking rows' )
   call unit_check( 'read_table', size(array,dim=2)  ==  3,                 'checking columns' )
   call unit_check( 'read_table', sum(nint(array))   ==  308,               'sum' )
   call unit_check( 'read_table', all([nint(array)]  ==  [1,4,-5,2,300,6]), 'values' )
   ! remove sample file
   open(file='inputfile',unit=10)
   close(unit=10,status='delete')
   call unit_check_done('read_table',msg='')
end subroutine test_read_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filebyte()
character(len=1),allocatable :: data2(:)
character(len=:),allocatable :: line
integer :: ierr
   call unit_check_start('fileread',msg='')
   ierr=filewrite('_scratch.txt>',[ character(len=10) :: &
   &'abcdefghij', &
   &'klmnop    ', &
   &'qrstuv    ', &
   &'wxyz      ', &
   &''])

   call unit_check_start('filebyte',msg='')
   call filebyte('_scratch.txt',data2)
   if(.not.allocated(data2))then
      call unit_check_bad('filebyte','failed to load file','_scratch.txt')
   else
      line=repeat(' ',size(data2))
      write(line,'(*(a))')pack(data2,index('abcdefghijklmnopqrstuvwxyz',data2).ne.0)
      call unit_check('filebyte',line.eq.'abcdefghijklmnopqrstuvwxyz','find all the letters',line)
   endif
   ierr=filedelete('_scratch.txt')
   call unit_check_done('filebyte',msg='')
end subroutine test_filebyte
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileread()
character(len=:),allocatable :: data(:), data2(:)
integer :: ierr
   data=[ character(len=80) :: &
   &'This is the text to write  ', &
   &'into the file. It will be  ', &
   &'trimmed on the right side. ', &
   &'                           ', &
   &'     That is all Folks!    ']
   ierr=filewrite('_scratch.txt',data)
   call fileread('_scratch.txt',data2)
   if(.not.allocated(data2))then
      call unit_check_bad('fileread','failed to load file','_scratch.txt')
   else
      call unit_check('fileread', all(data==data2) , 'check read back file written')
      if(.not.all(data==data2))then
         write(*,'(a)')'DATA:',size(data)
         write(*,'(a)')data
         write(*,'(a)')'DATA2:',size(data2)
         write(*,'(a)')data2
      endif
   endif
   ierr=filedelete('_scratch.txt')
   call unit_check_done('fileread',msg='')
end subroutine test_fileread
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_number_of_lines()
integer,parameter  :: lun=10
character(len=256) :: iomsg
integer            :: iostat
   call unit_check_start('number_of_lines',msg='')
   ! create test file
   open(file='inputfile',unit=LUN,action='write')
   write(LUN,'(a)') [character(len=80):: &
       '1                       ', &
       '2                       ', &
       '3                       ', &
       '4                       ', &
       '5                       ']
   call unit_check('number_of_lines', number_of_lines(LUN) == -1, 'expected -1 lines, got', number_of_lines(LUN))
   close(unit=LUN,iostat=iostat)
   open(file='inputfile',unit=LUN,action='read')
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 1:',trim(iomsg)
   call unit_check('number_of_lines', number_of_lines(LUN) == 5, 'expected 5 lines, got', number_of_lines(LUN))
   close(unit=LUN,iostat=iostat,iomsg=iomsg)
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 2:',trim(iomsg)
   ! read file as a table
   open(file='inputfile',unit=LUN)
   close(unit=LUN,status='delete',iostat=iostat)
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 3:',trim(iomsg)
   call unit_check_done('number_of_lines',msg='')
end subroutine test_number_of_lines
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_basename()
character(len=:),allocatable :: fn
   call unit_check_start('basename',msg='')
   fn='/home/user/src/code.f90'
   call unit_check('basename', basename(fn) == 'code',            ' leaf with any suffix removed'    ,basename(fn) )
   call unit_check('basename', basename(fn,'') == 'code.f90',     ' leaf with suffix retained'       ,basename(fn,'') )
   call unit_check('basename', basename(fn,'.f90') == 'code',     ' with suffix unless it is ".f90"' ,basename(fn,'.f90') )
   call unit_check('basename', basename(fn,'.F90') == 'code.f90', ' with suffix unless it is ".F90"' ,basename(fn,'.F90') )
   call unit_check_done('basename',msg='')
end subroutine test_basename
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_joinpath()
   call unit_check_start('joinpath',msg='')
   !!call unit_check('joinpath', 0 == 0, 'checking',100)
   call unit_check_done('joinpath',msg='')
end subroutine test_joinpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileopen()
   call unit_check_start('fileopen',msg='')
   !!call unit_check('fileopen', 0 == 0, 'checking',100)
   call unit_check_done('fileopen',msg='')
end subroutine test_fileopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileclose()
   call unit_check_start('fileclose',msg='')
   !!call unit_check('fileclose', 0 == 0, 'checking',100)
   call unit_check_done('fileclose',msg='')
end subroutine test_fileclose
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filewrite()
integer :: ierr
character(len=:),allocatable :: data(:), data2(:)
   call unit_check_start('filewrite',msg='')
   data=[ character(len=80) :: &
   &'This is the text to write  ', &
   &'into the file. It will be  ', &
   &'trimmed on the right side. ', &
   &'                           ', &
   &'     That is all Folks!    ']
   ierr=filewrite('_scratch.txt',data)
   ! allocate character array and copy file into it
   call fileread('_scratch.txt',data2)
   if(.not.allocated(data2))then
      call unit_check_bad('filewrite','failed to load file','_scratch.txt')
   else
      call unit_check('filewrite', all(data==data2) , 'check read back file written')
      if(.not.all(data==data2))then
         write(*,'(a)')'DATA:',size(data)
         write(*,'(a)')data
         write(*,'(a)')'DATA2:',size(data2)
         write(*,'(a)')data2
      endif
   endif
   ierr=filedelete('_scratch.txt')
   call unit_check_done('filewrite',msg='')
end subroutine test_filewrite
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filedelete()
   call unit_check_start('filedelete',msg='')
   !!call unit_check('filedelete', 0 == 0, 'checking',100)
   call unit_check_done('filedelete',msg='')
end subroutine test_filedelete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scratch
   call unit_check_start('scratch',msg='')
   !!call unit_check('scratch', 0 == 0, 'checking',100)
   call unit_check_done('scratch',msg='')
end subroutine test_scratch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_separator()
   call unit_check_start('separator',msg='')
   !!call unit_check('separator', 0 == 0, 'checking',100)
   call unit_check_done('separator',msg='')
end subroutine test_separator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_which()
   call unit_check_start('which',msg='')
   !!call unit_check('which', 0 == 0, 'checking',100)
   call unit_check_done('which',msg='')
end subroutine test_which
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lookfor()
   call unit_check_start('lookfor',msg='')
   !!call unit_check('lookfor', 0 == 0, 'checking',100)
   call unit_check_done('lookfor',msg='')
end subroutine test_lookfor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getname()
   call unit_check_start('getname',msg='')
   !!call unit_check('getname', 0 == 0, 'checking',100)
   call unit_check_done('getname',msg='')
end subroutine test_getname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splitpath()
integer,parameter      :: maxlen=4096
character(len=maxlen)  :: dir
character(len=maxlen)  :: name
character(len=maxlen)  :: basename
character(len=maxlen)  :: ext
   call unit_check_start('splitpath',msg='')
   call splitpath('/usr/local/bin/test.exe', dir, name, basename, ext)
   call unit_check('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)
   call unit_check('splitpath', name=='test.exe', 'name','test.exe',name)
   call unit_check('splitpath', basename=='test', 'basename','test',basename)
   call unit_check('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', dir=dir)
   call unit_check('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)

   call splitpath('/usr/local/bin/test.exe', name=name)
   call unit_check('splitpath', name=='test.exe', 'name','test.exe',name)

   call splitpath('/usr/local/bin/test.exe', ext=ext)
   call unit_check('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', basename=basename)
   call unit_check('splitpath', basename=='test', 'basename','test',basename)

   call unit_check_done('splitpath',msg='')

end subroutine test_splitpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniq()

   call unit_check_start('uniq',msg='')
   !!call unit_check('uniq', 0 == 0, 'checking',100)
   call unit_check_done('uniq',msg='')
end subroutine test_uniq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_env()

   call unit_check_start('get_env',msg='')
   !!call unit_check('get_env', 0 == 0, 'checking',100)
   call unit_check_done('get_env',msg='')
end subroutine test_get_env
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_next_char()

   call unit_check_start('get_next_char',msg='')
   !!call unit_check('get_next_char', 0 == 0, 'checking',100)
   call unit_check_done('get_next_char',msg='')
end subroutine test_get_next_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end program runtest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!function read_line(line,lun,ios) result(ier)
!character(len=:),allocatable,intent(out) :: line
!integer,intent(in),optional              :: lun
!integer,optional                         :: ios
!integer                                  :: ier
!DESCRIPTION
!  The input file must have a PAD attribute of YES for the function to work
!  properly, which is typically true but can be set on an open file.
!  •  Append lines that end in a backslash with next line
!  •  Expand tabs
!  •  Replace unprintable characters with spaces
!  •  Remove trailing carriage return characters and white space
!character (len =: ), allocatable :: line
!integer                          :: stat
!integer                          :: icount=0
!         open(unit=stdin,pad='yes')
!         INFINITE: do while (read_line(line,ios=stat) == 0)
!            icount=icount
!            write (*, '(*(g0))') icount,' [',line,']'
!         enddo INFINITE
!         if ( .not.is_iostat_end(stat) ) then
!            write (stderr, '(*(g0))') &
!            & 'error: line ',icount,'==>',trim (line)
!         endif
