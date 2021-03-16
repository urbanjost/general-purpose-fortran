          ! program demo_contourlines and user routine
          program TestCase
          use M_ContourPlot, only : contourlines
          implicit none
          !
          integer,parameter    :: NPTS=121
          real,parameter,dimension(8):: c = &
                [0.1, 0.2, 0.5, 1.0, 2.0, 4.0, 6.0, 8.0]
          real                 :: eps
          integer              :: errCode
          integer,parameter    :: DBG=2
          integer              :: ierr
          !integer              :: iexp=0, jexp=0, ism=0
          integer              :: iexp=2, jexp=3, ism=1
          integer              :: i,j,k
          real,dimension(NPTS) :: x,y,z
          external             :: my_CntCrv
          !
             k=0
             do j=0,10
                do i=0,10
                   k=k+1
                   x(k)=0.1*real(i)
                   y(k)=0.1*real(j)
                end do
             end do
             !
             z=(x-0.5)**2 + (y-0.5)**2
             z(:)=16.0*z(:)
             !
             ! write out the input values for inspection
             open(unit=dbg, file='test.log', status='replace', &
                & iostat=errCode, action='write', position='rewind')
             write(DBG,'(I4,3F12.6)') (k,x(k),y(k),z(k),k=1,npts)
             !
             call ContourLines(x,y,z, ism,iexp,jexp, c, eps,ierr,my_CntCrv)
          END PROGRAM TestCase
          ! ----------------------------------------------------------------
          subroutine my_CntCrv(x,y,n,z)
          ! User-supplied routine used to plot or process the contour lines
          implicit none
          integer,intent(in)          :: n
          real,intent(in),dimension(n):: x,y
          real,intent(in)             :: z
          !
          integer,save                :: gnu=0
          integer                     :: k
          integer                     :: errCode
             if(gnu.eq.0)then
                ! on first call, set up output file
                gnu=1
                open(unit=gnu, file='test1.gnu', status='replace', &
                  & iostat=errCode, action='write', position='rewind')
                write(*,*) "File test1.gnu added to your directory"
             endif
             ! write a contour line out to a file followed by a blank line
             write(gnu,'("# level ",g0)') z
             write(gnu,'(2f12.5)') (x(k),y(k),k=1,n)
             write(gnu,'(a)') " "
          !
          end subroutine my_CntCrv
          ! end program demo_contourlines and user routine
