program runtest
use M_framework__msg
use M_system,  only : system_getcwd
use M_framework__verify,  only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify,  only : unit_test_level
use M_framework__verify, only : unit_test_stop
use M_path,    only : path
implicit none
   unit_test_level=0
!! setup
   if(unit_test_level.ne.0)then
     write(*,*)'unit_test_LEVEL=',unit_test_level
   endif
   call test_init_path()
   call test_construct_from_dat()
   call test_branch()
   call test_leaf()
   call test_stem()
   call test_bud()
   call test_path_exists()
   call test_path_isdir()
   call test_path_readable()
   call test_path_writable()
   call test_path_executable()
   call test_path_realpath()
   call test_path_stat()
   call unit_test_stop()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_branch()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('branch',msg='')
   call unit_test('branch', file%branch().eq.'/home/user', file%name,'branch',file%branch())
   call unit_test_done('branch',msg='')
end subroutine test_branch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_leaf()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('leaf',msg='')
   call unit_test('leaf', file%leaf().eq.'file.txt', file%name,'leaf',file%leaf())
   call unit_test_done('leaf',msg='')
end subroutine test_leaf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stem()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('stem',msg='')
   call unit_test('stem', file%stem().eq.'file', file%name,'stem',file%stem())
   call unit_test_done('stem',msg='')
end subroutine test_stem
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bud()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('bud',msg='')
   call unit_test('bud', file%bud().eq.'.txt', file%name,'bud',file%bud())
   call unit_test_done('bud',msg='')
end subroutine test_bud
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_construct_from_dat()
type(path)    :: file
   file%name='/home/user/file.txt'
   call unit_test_start('construct_from_dat',msg='')
   call unit_test('construct_from_dat', file%name.eq.'/home/user/file.txt', '/home/user/file.txt',file%name)
   call unit_test_done('construct_from_dat',msg='')
end subroutine test_construct_from_dat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_init_path()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('init_path',msg='')
   call unit_test('init_path', file%name.eq.'/home/user/file.txt', '/home/user/file.txt',file%name)
   call unit_test_done('init_path',msg='')
end subroutine test_init_path
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_executable()
type(path)    :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'executable.. ',file%executable()
   call unit_test_start('path_executable',msg='')
   !!call unit_test('path_executable', 0.eq.0, file%name,file%executable())
   call unit_test_done('path_executable',msg='')
end subroutine test_path_executable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_exists()
type(path)    :: file
   call file%init('.')
   call unit_test_start('path_exists',msg='')
   call unit_test('path_exists', file%exists(), file%name,file%exists())
   file%name='_notthere_/_notthere_'
   call unit_test('path_exists', .not.file%exists(), file%name,file%exists())
   call unit_test_done('path_exists',msg='')
end subroutine test_path_exists
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_isdir()
type(path)                   :: file
character(len=:),allocatable :: dirname
integer                      :: ierr
   call unit_test_start('path_isdir',msg='')
   call system_getcwd(dirname,ierr)
   call unit_test('path_isdir', ierr.eq.0, 'getting current directory',dirname)
   call file%init(dirname)
   call unit_test('path_isdir', file%is_dir(), 'current directory',file%name,'isdir',file%is_dir())
   call unit_test_done('path_isdir',msg='')
end subroutine test_path_isdir
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_readable()
type(path)    :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'readable.... ',file%readable()
   call unit_test_start('path_readable',msg='')
   !!call unit_test('path_readable', 0.eq.0, file%name,file%readable())
   call unit_test_done('path_readable',msg='')
end subroutine test_path_readable
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_realpath()
type(path)                   :: file
character(len=:),allocatable :: dirname
integer                      :: ierr
   call unit_test_start('path_realpath',msg='')

   call system_getcwd(dirname,ierr)
   call unit_test('path_isdir', ierr.eq.0, 'getting current directory',dirname)

   call file%init('.')
   call unit_test('path_realpath',file%realpath().eq.dirname,'pathname=',file%name,'=>',file%realpath(),'cwd=',dirname)

   call unit_test_done('path_realpath',msg='')

end subroutine test_path_realpath
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_stat()
type(path)    :: file
   call file%init('/home/user/file.txt')
   call unit_test_start('path_stat',msg='')
   !!call unit_test('path_stat', 0.eq.0, file%name,100)
   call unit_test_done('path_stat',msg='')
end subroutine test_path_stat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_path_writable()
type(path)    :: file
   call file%init('/home/user/file.txt')
!         write(*,*)'writable.... ',file%writable()
   call unit_test_start('path_writable',msg='')
   !!call unit_test('path_writable', 0.eq.0, file%name,file%writeable())
   call unit_test_done('path_writable',msg='')
end subroutine test_path_writable
!===================================================================================================================================
end program runtest
