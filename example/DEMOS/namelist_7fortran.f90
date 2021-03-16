             program demo_namelist
             implicit none
             integer           :: lun

             ! create a namelist and initialize the values
             logical           :: l=.true.
             character(len=10) :: c='XXXXXXXXXX'
             real              :: r=12.3456
             integer           :: i=789
             complex           :: x=(12345.6789,9876.54321)
             doubleprecision   :: d= 123456789.123456789d0
             integer           :: a(5)=[1,2,3,4,5]
             type point
              integer           :: x=0
              integer           :: y=0
              character(len=10) :: color='red'
             endtype point
             type(point) :: dot
             namelist /nlist/ l,c,r,i,x,d,a,dot

             open(file='_tmp_',newunit=lun)

                write(*,*)'initial nlist'
                write(*,nlist)
                write(lun,nlist)

                write(*,*)'change values and print nlist again'
                a=[10,20,30,40,50]
                dot%color='orange'
                write(lun,nlist)

                write(*,*)'read back values. Can have multiple sets in a file'
                rewind(lun)
                read(lun,nlist)
                read(lun,nlist)
                write(*,nlist)

              end program demo_namelist
