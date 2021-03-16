            program demo_system_rename
            use M_system, only : system_rename
            use M_system, only : system_remove
            use M_system, only : system_perror
            implicit none
            character(len=256) :: string
            integer            :: ios, ierr

            ! try to remove junk files just in case
            ierr=system_remove('_scratch_file_')
            write(*,'(a,i0)') 'should not be zero ',ierr
            call system_perror('*demo_system_rename*')
            ierr=system_remove('_renamed_scratch_file_')
            write(*,'(a,i0)') 'should not be zero ',ierr
            call system_perror('*demo_system_rename*')

            ! create scratch file to rename
            open(unit=10,file='_scratch_file_',status='new')
            write(10,'(a)') 'Test by renaming "_scratch_file_" to "_renamed_scratch_file_"'
            write(10,'(a)') 'IF YOU SEE THIS ON OUTPUT THE RENAME WORKED'
            close(10)
            ! rename scratch file
            ierr=system_rename('_scratch_file_','_renamed_scratch_file_')
            if(ierr.ne.0)then
               write(*,*)'ERROR RENAMING FILE ',ierr
            endif
            ! read renamed file
            open(unit=11,file='_renamed_scratch_file_',status='old')
            INFINITE: do
               read(11,'(a)',iostat=ios)string
               if(ios.ne.0)exit INFINITE
               write(*,'(a)')trim(string)
            enddo INFINITE
            close(unit=11)

            ! clean up
            ierr=system_remove('_scratch_file_')
            write(*,'(a,i0)') 'should not be zero ',ierr
            ierr=system_remove('_renamed_scratch_file_')
            write(*,'(a,i0)') 'should be zero ',ierr

            end program demo_system_rename
