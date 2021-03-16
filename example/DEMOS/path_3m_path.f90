          program demo_path
          use M_path, only   : path
          use M_system, only : system_getpwuid, system_getgrgid
          use M_time,   only : fmtdate, u2d
          use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
          character(len=*),parameter               :: fmt_date='year-month-day hour:minute:second'
          type(path)                               :: file
          character(len=:),allocatable             :: filename
          integer(kind=int64)                      :: buff(14)
          integer                                  :: i
             do i = 1 , max(1,command_argument_count())
              if(command_argument_count().eq.0)then
                 filename='.'
              else
                 call getname(i,filename)
              endif

              file%name=filename
              ! or
              call file%init(filename)

              write(*,*)'name........ ',file%name
              write(*,*)'branch...... ',file%branch()
              write(*,*)'leaf........ ',file%leaf()
              write(*,*)'stem........ ',file%stem()
              write(*,*)'bud......... ',file%bud()
              write(*,*)'is_dir...... ',file%is_dir()
              write(*,*)'readable.... ',file%readable()
              write(*,*)'writable.... ',file%writable()
              write(*,*)'executable.. ',file%executable()
              write(*,*)'exists...... ',file%exists()
              write(*,*)'realpath.... ',file%realpath()
              write(*,*)'stat........ '
              buff=file%stat()
              if(buff(14) == 0) then
               write (*,"(9x,'Device ID(hex/decimal):', T40,Z0,'h/',I0,'d')")&
               & buff(1),buff(1)
               write (*,"(9x,'Inode number:', T40, I0)")&
               & buff(2)
               write (*,"(9x,'File mode (octal):', T40, O19)")&
               & buff(3)
               write (*,"(9x,'Number of links:', T40, I0)")&
               & buff(4)
               write (*,"(9x,'Owner''s uid/username:', T40, I0,1x, A)")&
               & buff(5), system_getpwuid(buff(5))
               write (*,"(9x,'Owner''s gid/group:', T40, I0,1x, A)")&
               & buff(6), system_getgrgid(buff(6))
               write (*,"(9x,'Device where located:', T40, I0)")&
               & buff(7)
               write (*,"(9x,'File size(bytes):', T40, I0)")&
               & buff(8)
               write (*,"(9x,'Last access time:', T40, I0,1x, A)")&
               & buff(9), fmtdate(u2d(int(buff(9))),fmt_date)
               write (*,"(9x,'Last modification time:', T40, I0,1x, A)")&
               & buff(10),fmtdate(u2d(int(buff(10))),fmt_date)
               write (*,"(9x,'Last status change time:', T40, I0,1x, A)")&
               & buff(11),fmtdate(u2d(int(buff(11))),fmt_date)
               write (*,"(9x,'Preferred block size(bytes):', T40, I0)")&
               & buff(12)
               write (*,"(9x,'No. of blocks allocated:', T40, I0)")&
               & buff(13)
              else
               write (*,*) '*path%stat* error: ',file%name,' status= ',status
              endif
              write(*,*)
             enddo

          contains
          subroutine getname(i,fn)
          integer,intent(in)                       :: i
          character(len=:),allocatable,intent(out) :: fn
          integer                                  :: fn_length
          ! get pathname from command line arguments
             call get_command_argument (i , length=fn_length)
             if(allocated(filename))deallocate(filename)
             allocate(character(len=fn_length) :: fn)
             call get_command_argument (i , value=fn)
          end subroutine getname
          end program demo_path
