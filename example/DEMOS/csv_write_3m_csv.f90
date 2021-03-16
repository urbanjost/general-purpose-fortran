          program  demo_csv_write
          !
          use M_csv,     only : csv_write
          use M_csv,     only : CSV_lun, CSV_TRUE, CSV_FALSE
          implicit none
          integer                :: i
          integer                :: v
          integer                :: iarr(10)=[(i*10,i=1,size(iarr))]
          real,dimension(3,4)    :: rand2d
          integer,dimension(3,4) :: array2d

             open(newunit=CSV_lun,file='csv_test.csv',action='write')
             CSV_true='TRUE'
             CSV_false='FALSE'

             ! a number of scalar values in a row
             do i = 0,8
                v = 10**i
                call csv_write( v )
             enddo
             call csv_write() ! end line

             ! strings, some with double-quotes in them
             call csv_write( 'Aha','"Aha"','Aha "!"')
             call csv_write() ! end line

             ! lots of types
             call csv_write('string',.true.,.false.,111,23.45,10.20e15)
             call csv_write(3456.78901234d0,cmplx(huge(0.0),tiny(0.0)))
             call csv_write() ! end line

             call csv_write(1.234)                   ! scalars
             call csv_write(1.234d0)
             call csv_write([1,2,3,4,5,6,7,8,9,10])  ! a vector
             call csv_write()                        ! end line

             call csv_write(iarr)         ! a vector
             call csv_write() ! end line  ! even a vector needs a line end

             call random_number( rand2d ) ! a table is written one row per line
             array2d = int( rand2d*100.0)
             call csv_write( array2d)

             close( unit=CSV_lun)

              end program demo_csv_write
