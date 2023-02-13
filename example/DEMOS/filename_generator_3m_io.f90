        program demo_filename_generator
        use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
        use M_io, only : filename_generator
        implicit none

            ! no zero-fill
            write(*,*) filename_generator("file_",".dat",11)
            ! zero-fill till 3 digits
            write(*,*) filename_generator("file_",".dat",11,3)
            ! zero-fill till 9 digits
            write(*,*) filename_generator("file_",".dat",11,9)
            ! same as default (no zero-fill)
            write(*,*) filename_generator("file_",".dat",11,0)

        end program demo_filename_generator
