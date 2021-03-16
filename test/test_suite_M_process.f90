program runtest
use M_msg
use M_verify
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_verify, only : unit_check_level
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_process()
contains

end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_M_process()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
use M_process

!! setup
   call test_process_open_read()
   call test_process_open_write()
   call test_process_readall()
   call test_process_readline()
   call test_process_writeline_array()
   call test_process_writeline_scalar()
   call test_process_close()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_close()
type(streampointer) :: fp              ! C file pointer returned by process_open()
integer :: ierr                        ! check status of calls to process module routines
character(len=:),allocatable :: string ! hold results, assuming sufficient memory is available
character(len=4096) :: line            ! long enough to hold any expected line
   call unit_check_start('process_close',msg='')
   string=''
   !!call process_close(fp,ierr)             ! not open yet
   !!call unit_check('process_close', ierr.ne.0, 'close process before opening it',ierr)
   call process_open_read('echo A;echo B;echo C',fp,ierr)    ! open process to read from
   do                                      ! read output of process till end
      call process_readline(line,fp,ierr)
      if(ierr.ne.0)exit
      string=string//trim(line)//' '       ! append output lines together
   enddo
   call unit_check_msg('process_close','string=',string )
   call process_close(fp,ierr)             ! Wrap up
   call unit_check('process_close', ierr.eq.0, 'close process ',ierr)
   call process_open_write('cat',fp,ierr)    ! open process to write to that is not terminated
   call process_close(fp,ierr)
   !!call unit_check('process_close', ierr.eq.0, 'close process that is open',ierr)
   call unit_check_done('process_close',msg='')
end subroutine test_process_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_open_read()
type(streampointer) :: fp                                                  ! C file pointer returned by process_open()
integer :: ierr                                                            ! check status of calls to process module routines
character(len=:),allocatable :: string                                     ! hold results, assuming sufficient memory is available
character(len=4096) :: line                                                ! long enough to hold any expected line
  call unit_check_start('process_open_read',msg='')
  string=''
  call process_open_read('echo a;echo b;echo c',fp,ierr)                   ! open process to read from
  call unit_check('process_open_read', ierr.eq.0, 'open ierr=',ierr)
  do                                                                       ! read output of process till end
     call process_readline(line,fp,ierr)
     if(ierr.ne.0)exit
     string=string//trim(line)//'+'                                        ! append output lines together
  enddo
  call unit_check('process_open_read', string.eq.'a+b+c+', string)
  call process_open_read('echo a;echo b;echo c',fp,ierr)                   ! open process to read from
  call unit_check('process_open_read', string.eq.'a+b+c+', 'openn already open process,ierr=',ierr)
  call process_close(fp,ierr)                                              ! Wrap up
  !!call unit_check('process_open_read', ierr.eq.0, 'close ierr=',ierr)
  call unit_check_done('process_open_read',msg='')
end subroutine test_process_open_read
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_open_write()
type(streampointer)          :: fp            ! C file pointer returned by process_open()
integer                      :: ierr          ! check status of calls to process module routines
integer                      :: lun
integer                      :: ios
character(len=256)           :: line
   call unit_check_start('process_open_write',msg='')
   ! clear scratch file
   open(newunit=lun,file='_scratch_.txt',iostat=ios)
   close(unit=lun,iostat=ios,status='delete')
   ! start shell with command that finishes immediately (special case, would just use execute_command_line(3f) intrinsic)
   call process_open_write('echo one >_scratch_.txt;echo two >>_scratch_.txt',fp,ierr)    ! open process to write to
   call unit_check('process_open_write', ierr.eq.0, 'ierr=',ierr)
   call process_close(fp,ierr)
   call unit_check('process_open_write', ierr.eq.0, 'no error on close, ierr=',ierr)
   ! check expected file
   open(newunit=lun,file='_scratch_.txt')
   read(lun,'(a)',iostat=ios)line
   call unit_check('process_open_write', line.eq.'one', 'line 1:',line)
   read(lun,'(a)',iostat=ios)line
   call unit_check('process_open_write', line.eq.'two', 'line 2:',line)
   close(unit=lun,iostat=ios,status='delete')
   ! start shell that waits to read commands
   call process_open_write('bash||cmd',fp,ierr)    ! open process to write to
   call unit_check('process_open_write', ierr.eq.0, 'ierr=',ierr)
   call process_writeline('echo three >_scratch_.txt',fp,ierr)
   call unit_check('process_open_write', ierr.ge.0, 'write of "echo three >_scratch_.txt", ierr=',ierr)
   call process_writeline('echo four >>_scratch_.txt',fp,ierr)
   call unit_check('process_open_write', ierr.ge.0, 'write of "echo four >>_scratch_.txt", ierr=',ierr)
   call process_close(fp,ierr)
   call unit_check('process_open_write', ierr.eq.0, 'should now be closed, ierr=',ierr)
   ! check expected file
   open(newunit=lun,file='_scratch_.txt')
   read(lun,'(a)',iostat=ios)line
   call unit_check('process_open_write', line.eq.'three',  'line 1:',line)
   read(lun,'(a)',iostat=ios)line
   call unit_check('process_open_write', line.eq.'four',   'line 2:',line)
   close(unit=lun,iostat=ios,status='delete')
   !
   call unit_check_done('process_open_write',msg='')
end subroutine test_process_open_write
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_readall()
integer :: ierr
character(len=:),allocatable :: string
   string=process_readall('echo A;echo B;echo C',ierr=ierr)
   call unit_check_start('process_readall',msg='')
   call unit_check('process_readall', string.eq.'A B C', string)
   call unit_check_done('process_readall',msg='')
end subroutine test_process_readall
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_readline()
type(streampointer) :: fp                                                   ! C file pointer returned by process_open()
integer :: ierr                                                             ! check status of calls to process module routines
character(len=:),allocatable :: string                                      ! hold results, assuming sufficient memory is available
character(len=4096) :: line                                                 ! long enough to hold any expected line
   call unit_check_start('process_readline',msg='')
   string=''
   call process_open_read('echo a;echo b;echo c',fp,ierr)                   ! open process to read from
   do                                                                       ! read output of process till end
      call process_readline(line,fp,ierr)
      if(ierr.ne.0)exit
      string=string//trim(line)//'+'                                        ! append output lines together
   enddo
   call unit_check('process_readline', string.eq.'a+b+c+', string)
   call process_close(fp,ierr)                                              ! Wrap up
   call unit_check_done('process_readline',msg='')
end subroutine test_process_readline
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_writeline_array()
type(streampointer)          :: fp            ! C file pointer returned by process_open()
integer                      :: ierr          ! check status of calls to process module routines
integer                      :: lun
integer                      :: ios
integer                      :: i
character(len=:),allocatable :: text(:)
character(len=*),parameter   :: lines(*)=[character(len=10) :: 'one','two','three','four']
character(len=256)           :: line
   call unit_check_start('process_writeline_array',msg='')
   ! clear scratch file
   open(newunit=lun,file='_scratch_.txt',iostat=ios)
   close(unit=lun,iostat=ios,status='delete')
   ! start shell
   call process_open_write('bash||cmd',fp,ierr)    ! open process to write to
   ! feed commands to shell that redirect output to _scratch_.txt file
   text=[character(len=128) ::      &
      "echo one   >_scratch_.txt",  &
      "echo two   >>_scratch_.txt", &
      "echo three >>_scratch_.txt", &
      "echo four  >>_scratch_.txt"]
   call process_writeline(text,fp,ierr)       ! multiple lines
   call unit_check('process_writeline_array',ierr.ge.0,'wrote four lines, ierr=',ierr)
   call process_close(fp,ierr)
   ! check expected file
   open(newunit=lun,file='_scratch_.txt')
   do i=1,5
      read(lun,'(a)',iostat=ios)line
      if(ios.ne.0)exit
      call unit_check('process_writeline_array',line.eq.lines(i),'got ',line,'expected',line)
   enddo
   close(unit=lun,iostat=ios,status='delete')
   call unit_check('process_writeline_array',i.eq.5,'number of lines',i-1)
   call unit_check_done('process_writeline_array',msg='')
end subroutine test_process_writeline_array
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_process_writeline_scalar()
type(streampointer)          :: fp            ! C file pointer returned by process_open()
integer                      :: ierr          ! check status of calls to process module routines
integer                      :: lun
integer                      :: ios
integer                      :: i
character(len=*),parameter   :: lines(*)=[character(len=10) :: 'one','two','three','four']
character(len=256)           :: line
   call unit_check_start('process_writeline_scalar',msg='')
   ! clear scratch file
   open(newunit=lun,file='_scratch_.txt',iostat=ios)
   close(unit=lun,iostat=ios,status='delete')
   ! start shell
   call process_open_write('bash||cmd',fp,ierr)    ! open process to write to (ie. start gnuplot(1) program)
   ! feed commands to shell that redirect output to _scratch_.txt file
   call process_writeline('echo one    >_scratch_.txt',fp,ierr)
   call process_writeline('echo two   >>_scratch_.txt',fp,ierr)
   call process_writeline('echo three >>_scratch_.txt',fp,ierr)
   call process_writeline('echo four  >>_scratch_.txt',fp,ierr)
   call process_close(fp,ierr)
   ! check expected file
   open(newunit=lun,file='_scratch_.txt')
   do i=1,5
      read(lun,'(a)',iostat=ios)line
      if(ios.ne.0)exit
      call unit_check('process_writeline_scalar',line.eq.lines(i),line)
   enddo
   close(unit=lun,iostat=ios,status='delete')
   call unit_check('process_writeline_scalar',i.eq.5,'number of lines',i)

   call unit_check_done('process_writeline_scalar',msg='')
end subroutine test_process_writeline_scalar
!===================================================================================================================================
end subroutine test_suite_M_process
