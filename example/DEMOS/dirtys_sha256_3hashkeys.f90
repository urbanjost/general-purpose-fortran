           program demo_dirty_sha256
           use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
           use M_hashkeys,                   only : sha256, dirty_sha256
           use M_io,                         only : slurp
           use M_strings,                    only : switch
           implicit none
           character(len=1),allocatable :: text(:) ! array to hold file in memory
           character(len=:),allocatable :: string
           integer                      :: i
           character(len=4096)          :: filename
              do i=1,command_argument_count()  ! step through filenames on command line
                 call get_command_argument(i, filename)
                 call slurp(filename,text) ! allocate character array and copy file into it
                 if(.not.allocated(text))then
                    write(ERROR_UNIT,*)'*rever* ERROR: failed to load file '//trim(filename)
                 else
                    string=switch(text) ! switch array to a single character variable
                    deallocate(text)    ! release memory
                    write(*,*)dirty_sha256(string),len(string),trim(filename) ! write digest value
                 endif
              enddo
           end program demo_dirty_sha256
