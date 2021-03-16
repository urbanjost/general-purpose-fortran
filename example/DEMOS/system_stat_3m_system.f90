       program demo_system_stat

          use M_system, only : system_stat, system_getpwuid, system_getgrgid
          use M_time, only :   fmtdate, u2d
          use, intrinsic :: iso_fortran_env, only : int32, int64
          implicit none

          integer(kind=int64)  :: buff(13)
          integer(kind=int32)  :: status
          character(len=*),parameter :: fmt_date='year-month-day hour:minute:second'

          integer(kind=int64) :: &
           Device_ID, Inode_number,     File_mode, Number_of_links, Owner_uid,        &
           Owner_gid, Directory_device, File_size, Last_access,     Last_modification,&
           Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
          equivalence                                    &
             ( buff(1)  , Device_ID                  ) , &
             ( buff(2)  , Inode_number               ) , &
             ( buff(3)  , File_mode                  ) , &
             ( buff(4)  , Number_of_links            ) , &
             ( buff(5)  , Owner_uid                  ) , &
             ( buff(6)  , Owner_gid                  ) , &
             ( buff(7)  , Directory_device           ) , &
             ( buff(8)  , File_size                  ) , &
             ( buff(9)  , Last_access                ) , &
             ( buff(10) , Last_modification          ) , &
             ( buff(11) , Last_status_change         ) , &
             ( buff(12) , Preferred_block_size       ) , &
             ( buff(13) , Number_of_blocks_allocated )

          CALL SYSTEM_STAT("/etc/hosts", buff, status)

          if (status == 0) then
             write (*, FMT="('Device ID(hex/decimal):',      &
             & T30, Z0,'h/',I0,'d')") buff(1),buff(1)
             write (*, FMT="('Inode number:',                &
             & T30, I0)") buff(2)
             write (*, FMT="('File mode (octal):',           &
             & T30, O19)") buff(3)
             write (*, FMT="('Number of links:',             &
             & T30, I0)") buff(4)
             write (*, FMT="('Owner''s uid/username:',       &
             & T30, I0,1x, A)") buff(5), system_getpwuid(buff(5))
             write (*, FMT="('Owner''s gid/group:',          &
             & T30, I0,1x, A)") buff(6), system_getgrgid(buff(6))
             write (*, FMT="('Device where located:',        &
             & T30, I0)") buff(7)
             write (*, FMT="('File size(bytes):',            &
             & T30, I0)") buff(8)
             write (*, FMT="('Last access time:',            &
             & T30, I0,1x, A)") buff(9), fmtdate(u2d(int(buff(9))),fmt_date)
             write (*, FMT="('Last modification time:',      &
             & T30, I0,1x, A)") buff(10),fmtdate(u2d(int(buff(10))),fmt_date)
             write (*, FMT="('Last status change time:',     &
             & T30, I0,1x, A)") buff(11),fmtdate(u2d(int(buff(11))),fmt_date)
             write (*, FMT="('Preferred block size(bytes):', &
             & T30, I0)") buff(12)
             write (*, FMT="('No. of blocks allocated:',     &
             & T30, I0)") buff(13)
          endif

          end program demo_system_stat
