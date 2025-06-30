!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program runtest
use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor 
use M_io
use M_framework__msg
use :: M_framework__verify, only : unit_test_start, unit_test, unit_test_done, unit_test_good, unit_test_bad, unit_test_msg
use :: M_framework__verify, only : unit_test_stop
use :: M_framework__verify, only : unit_test_level
use,intrinsic :: iso_fortran_env, only : stdin_lun  => input_unit
use,intrinsic :: iso_fortran_env, only : stderr_lun => error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
character(len=:),allocatable :: tmsg

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
   call test_is_hidden_file()
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
   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dirname()
   tmsg='strip last component from filename'
   call unit_test_start('dirname',msg=tmsg)
   call unit_test('dirname',  dirname('/usr/bin/')  ==  '/usr', '/usr/bin ==>',dirname('/usr/bin'))
   call unit_test('dirname',  dirname('dir1/str/')  ==  'dir1', 'dir1/str ==>',dirname('dir1/str/'))
   call unit_test('dirname',  dirname('stdio.h')    ==  '.',    '/stdio.h ==>',dirname('stdio.h'))
   call unit_test_done('dirname',msg='')
end subroutine test_dirname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_tmp()
   tmsg='Return the name of the scratch directory'
   call unit_test_start('get_tmp',msg=tmsg)
   !!call unit_test('get_tmp', 0 == 0, 'checking',100)
   call unit_test_done('get_tmp',msg='')
end subroutine test_get_tmp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filename_generator()

   tmsg='generate a filename containing a number'
   call unit_test_start('filename_generator',msg=tmsg)
   call unit_test_msg('filename_generator','generate a filename containing a whole number')

   call unit_test('filename_generator', filename_generator('head','.tail',100) ==  'head100.tail' )
   call unit_test('filename_generator', filename_generator('head','.tail',1,3) ==  'head001.tail' )

   call unit_test_done('filename_generator',msg='')

end subroutine test_filename_generator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notopen()

integer :: i, bug, ierr, ierr2

   tmsg='Find a FUN/LUN (Fortran-unit-number) that is not in use'
   call unit_test_start('notopen',msg=tmsg)
   call unit_test_msg('notopen','check for preassigned files from unit 0 to unit 1000')
   call unit_test_msg('notopen','assume 5 and 6 always return -1')

   do i=0,1000
      if(notopen(i,i,ierr)  /=  i)then
         bug=notopen(i,i,ierr2) ! gfortran 11 bug; OK in 9, 10
         call unit_test_msg('notopen','INUSE:',i,ierr,bug )
      endif
   enddo
   call unit_test('notopen', notopen(5,6,ierr)            ==  -1 ,'preassigned')

   do i=10,30,1
     open(unit=i,status="scratch")
   enddo

   close(25)
   close(28)
   call unit_test('notopen', notopen(10,30)            ==  25 )
   call unit_test('notopen', notopen()                 ==  25 )
   call unit_test('notopen', notopen(start=12,end=30)  ==  25 )
   call unit_test('notopen', notopen(26)               ==  28 )
   call unit_test('notopen', notopen(26,99)            ==  28 )

   call unit_test_done('notopen',msg='')

end subroutine test_notopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_inquire()
   tmsg='Do INQUIRE on file by name/number and print results'
   call unit_test_start('print_inquire',msg=tmsg)
   !!call unit_test('print_inquire', 0 == 0, 'checking',100)
   call unit_test_done('print_inquire',msg='')
end subroutine test_print_inquire
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rd()
   tmsg='ask for string from standard input with user-definable prompt'
   call unit_test_start('rd',msg=tmsg)
   !!call unit_test('rd_character', 0 == 0, 'checking',100)
   call unit_test_done('rd',msg='')
end subroutine test_rd
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getline()
character(len=:),allocatable :: line, last, expected
integer                      :: lun, ierr, stat, icount
   tmsg='read a line from specified LUN into allocatable string up to line length limit'
   call unit_test_start('getline',msg=tmsg)
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
      if(unit_test_level.gt.0.or..true.) write (*, '(*(g0))') 'getline>>>>',icount,' [',line,']'
   enddo INFINITE
   expected='wxyz'
   call unit_test('getline',is_iostat_end(stat),'last status got',stat,'expected',iostat_end)
   call unit_test('getline',icount.eq.4,'expected ',4,'lines got',icount)
   call unit_test('getline',last.eq.expected,'expected',expected,'got',last)
   ierr=filedelete('_scratch_getline.txt')
   call unit_test_done('getline',msg='')

end subroutine test_getline
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_line()
character(len=:),allocatable :: line, last, expected
integer                      :: lun, ierr, stat, icount
   tmsg='read a sanitized line from specified LUN into allocatable string'
   call unit_test_start('read_line',msg=tmsg)
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
      if(unit_test_level.gt.0) write (*, '(*(g0))') 'read_line>>>>',icount,' [',line,']'
   enddo INFINITE
   expected='        abcdefghijklmnop qrstuvwxyz'
   call unit_test('read_line',is_iostat_end(stat),'last status got',stat,'expected',iostat_end)
   call unit_test('read_line',icount.eq.1,'expected ',1,'lines got',icount)
   call unit_test('read_line',last.eq.expected,'expected',expected,'got',last)
   ierr=filedelete('_scratch_read_line.txt')
   call unit_test_done('read_line',msg='')

end subroutine test_read_line
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_table()
doubleprecision,allocatable :: array(:,:)
integer :: ierr
integer, allocatable :: expected(:)
integer, allocatable :: answer(:)
   tmsg='read file containing a table of numeric values'
   call unit_test_start('read_table',msg=tmsg)
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
   call unit_test( 'read_table', size(array      )  ==  6,   'checking size' ,  'expected',6,'got',size(array) )
   call unit_test( 'read_table', size(array,dim=1)  ==  2,   'checking rows' ,  'expected',2,'got',size(array,dim=1) )
   call unit_test( 'read_table', size(array,dim=2)  ==  3,   'checking columns','expected',3,'got',size(array,dim=2) )
   call unit_test( 'read_table', sum(nint(array))   ==  308, 'sum' ,'expected',308,'got',sum(nint(array)) )
   expected= [1,4,-5,2,300,6]
   answer=[nint(array)]
   call unit_test( 'read_table', all(answer == expected), 'values' ,'expected',str(expected),'got',str(answer))

   ! remove sample file
   open(file='inputfile',unit=10)
   close(unit=10,status='delete')
   call unit_test_done('read_table',msg='')
end subroutine test_read_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filebyte()
character(len=1),allocatable :: data2(:)
character(len=:),allocatable :: line
integer :: ierr
   ierr=filewrite('_scratch.txt>',[ character(len=10) :: &
   &'abcdefghij', &
   &'klmnop    ', &
   &'qrstuv    ', &
   &'wxyz      ', &
   &''])

   tmsg='read a file into a character array'
   call unit_test_start('filebyte',msg=tmsg)
   call filebyte('_scratch.txt',data2)
   if(.not.allocated(data2))then
      call unit_test_bad('filebyte','failed to load file','_scratch.txt')
   else
      line=repeat(' ',size(data2))
      write(line,'(*(a))')pack(data2,index('abcdefghijklmnopqrstuvwxyz',data2).ne.0)
      call unit_test('filebyte',line.eq.'abcdefghijklmnopqrstuvwxyz','find all the letters',line)
   endif
   ierr=filedelete('_scratch.txt')
   call unit_test_done('filebyte',msg='')
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
   tmsg='read a file into a string array'
   call unit_test_start('fileread',msg=tmsg)
   call fileread('_scratch.txt',data2)
   if(.not.allocated(data2))then
      call unit_test_bad('fileread','failed to load file','_scratch.txt')
   else
      call unit_test('fileread', all(data==data2) , 'check read back file written')
      if(.not.all(data==data2))then
         write(*,'(a)')'DATA:',size(data)
         write(*,'(a)')data
         write(*,'(a)')'DATA2:',size(data2)
         write(*,'(a)')data2
      endif
   endif
   ierr=filedelete('_scratch.txt')
   call unit_test_done('fileread',msg='')
end subroutine test_fileread
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_number_of_lines()
integer,parameter  :: lun=10
character(len=256) :: iomsg
integer            :: iostat
   tmsg='read an open sequential file to get number of lines'
   call unit_test_start('number_of_lines',msg=tmsg)
   ! create test file
   open(file='inputfile',unit=LUN,action='write')
   write(LUN,'(a)') [character(len=80):: &
       '1                       ', &
       '2                       ', &
       '3                       ', &
       '4                       ', &
       '5                       ']
   call unit_test('number_of_lines', number_of_lines(LUN) == -1, 'expected -1 lines, got', number_of_lines(LUN))
   close(unit=LUN,iostat=iostat)
   open(file='inputfile',unit=LUN,action='read')
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 1:',trim(iomsg)
   call unit_test('number_of_lines', number_of_lines(LUN) == 5, 'expected 5 lines, got', number_of_lines(LUN))
   close(unit=LUN,iostat=iostat,iomsg=iomsg)
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 2:',trim(iomsg)
   ! read file as a table
   open(file='inputfile',unit=LUN)
   close(unit=LUN,status='delete',iostat=iostat)
   if(iostat /= 0)write(*,*)'<ERROR>*test_number_of_lines* 3:',trim(iomsg)
   call unit_test_done('number_of_lines',msg='')
end subroutine test_number_of_lines
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_basename()
character(len=:),allocatable :: fn
   tmsg='return last component from filename '
   call unit_test_start('basename',msg=tmsg)
   fn='/home/user/src/code.f90'
   call unit_test('basename', basename(fn) == 'code',            ' leaf with any suffix removed'    ,basename(fn) )
   call unit_test('basename', basename(fn,'') == 'code.f90',     ' leaf with suffix retained'       ,basename(fn,'') )
   call unit_test('basename', basename(fn,'.f90') == 'code',     ' with suffix unless it is ".f90"' ,basename(fn,'.f90') )
   call unit_test('basename', basename(fn,'.F90') == 'code.f90', ' with suffix unless it is ".F90"' ,basename(fn,'.F90') )
   call unit_test_done('basename',msg='')
end subroutine test_basename
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_joinpath()
   tmsg='join parts of a pathname together'
   call unit_test_start('joinpath',msg=tmsg)
   !!call unit_test('joinpath', 0 == 0, 'checking',100)
   call unit_test_done('joinpath',msg='')
end subroutine test_joinpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_hidden_file()
logical,parameter :: F=.false., T=.true.
   call unit_test_start('is_hidden_file', msg='categorize pathname as a hidden filename or not')
   call showit_is_hidden_file('.abc', T)
   call showit_is_hidden_file('./.', F)
   call showit_is_hidden_file('..', F)
   call showit_is_hidden_file('...', T)
   call showit_is_hidden_file('/abc/def/notes.txt', F)
   call showit_is_hidden_file('/abc/def/.hide', T)
   call unit_test_done('is_hidden_file', msg='')
end subroutine test_is_hidden_file
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine showit_is_hidden_file(path, expected)
character(len=*), intent(in) :: path
logical, intent(in) :: expected
   call unit_test('is_hidden_file', is_hidden_file(path) .eqv.expected, 'for', path, 'expected', expected)
end subroutine showit_is_hidden_file
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileopen()
   tmsg='A simple open of a sequential file'
   call unit_test_start('fileopen',msg=tmsg)
   !!call unit_test('fileopen', 0 == 0, 'checking',100)
   call unit_test_done('fileopen',msg='')
end subroutine test_fileopen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fileclose()
   tmsg='A simple close of a sequential file'
   call unit_test_start('fileclose',msg=tmsg)
   !!call unit_test('fileclose', 0 == 0, 'checking',100)
   call unit_test_done('fileclose',msg='')
end subroutine test_fileclose
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filewrite()
integer :: ierr
character(len=:),allocatable :: data(:), data2(:)
   tmsg='A simple write of a CHARACTER array to a file'
   call unit_test_start('filewrite',msg=tmsg)
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
      call unit_test_bad('filewrite','failed to load file','_scratch.txt')
   else
      call unit_test('filewrite', all(data==data2) , 'check read back file written')
      if(.not.all(data==data2))then
         write(*,'(a)')'DATA:',size(data)
         write(*,'(a)')data
         write(*,'(a)')'DATA2:',size(data2)
         write(*,'(a)')data2
      endif
   endif
   ierr=filedelete('_scratch.txt')
   call unit_test_done('filewrite',msg='')
end subroutine test_filewrite
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_filedelete()
   tmsg='A simple close of an open file with STATUS="DELETE"'
   call unit_test_start('filedelete',msg=tmsg)
   !!call unit_test('filedelete', 0 == 0, 'checking',100)
   call unit_test_done('filedelete',msg='')
end subroutine test_filedelete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scratch
   tmsg='Return the name of a scratch file'
   call unit_test_start('scratch',msg=tmsg)
   !!call unit_test('scratch', 0 == 0, 'checking',100)
   call unit_test_done('scratch',msg='')
end subroutine test_scratch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_separator()
   tmsg='try to determine pathname directory separator character'
   call unit_test_start('separator',msg=tmsg)
   !!call unit_test('separator', 0 == 0, 'checking',100)
   call unit_test_done('separator',msg='')
end subroutine test_separator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_which()
   tmsg='find the pathname of a command by searching the directories in $PATH'
   call unit_test_start('which',msg=tmsg)
   !!call unit_test('which', 0 == 0, 'checking',100)
   call unit_test_done('which',msg='')
end subroutine test_which
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lookfor()
   tmsg='look for a filename in directories specified by an environment variable'
   call unit_test_start('lookfor',msg=tmsg)
   !!call unit_test('lookfor', 0 == 0, 'checking',100)
   call unit_test_done('lookfor',msg='')
end subroutine test_lookfor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getname()
   tmsg='get name of the current executable'
   call unit_test_start('getname',msg=tmsg)
   !!call unit_test('getname', 0 == 0, 'checking',100)
   call unit_test_done('getname',msg='')
end subroutine test_getname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splitpath()
integer,parameter      :: maxlen=4096
character(len=maxlen)  :: dir
character(len=maxlen)  :: name
character(len=maxlen)  :: basename
character(len=maxlen)  :: ext
   tmsg='split a Unix pathname into components'
   call unit_test_start('splitpath',msg=tmsg)
   call splitpath('/usr/local/bin/test.exe', dir, name, basename, ext)
   call unit_test('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)
   call unit_test('splitpath', name=='test.exe', 'name','test.exe',name)
   call unit_test('splitpath', basename=='test', 'basename','test',basename)
   call unit_test('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', dir=dir)
   call unit_test('splitpath', dir=='/usr/local/bin', 'directory','/usr/local/bin/',dir)

   call splitpath('/usr/local/bin/test.exe', name=name)
   call unit_test('splitpath', name=='test.exe', 'name','test.exe',name)

   call splitpath('/usr/local/bin/test.exe', ext=ext)
   call unit_test('splitpath', ext=='.exe', 'ext','.exe',ext)

   call splitpath('/usr/local/bin/test.exe', basename=basename)
   call unit_test('splitpath', basename=='test', 'basename','test',basename)

   call unit_test_done('splitpath',msg='')

end subroutine test_splitpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniq()
   tmsg='append a number to the end of filename to make a unique name if name exists'
   call unit_test_start('uniq',msg=tmsg)
   !!call unit_test('uniq', 0 == 0, 'checking',100)
   call unit_test_done('uniq',msg='')
end subroutine test_uniq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_env()
   tmsg='a function returning the value of an environment variable'
   call unit_test_start('get_env',msg=tmsg)
   !!call unit_test('get_env', 0 == 0, 'checking',100)
   call unit_test_done('get_env',msg='')
end subroutine test_get_env
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_get_next_char()
   tmsg='read from a file one character at a time'
   call unit_test_start('get_next_char',msg=tmsg)
   !!call unit_test('get_next_char', 0 == 0, 'checking',100)
   call unit_test_done('get_next_char',msg='')
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
