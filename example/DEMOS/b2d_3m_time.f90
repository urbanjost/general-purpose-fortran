      program demo_b2d
      use M_time, only : b2d, d2b, fmtdate, realtime, BAStime
      !BAStime includes operator(+), operator(-)
      implicit none
      integer,parameter :: dp=kind(0.0d0)
      type(BAStime)     :: today
      type(BAStime)     :: aday
      type(BAStime)     :: newday, yesterday, tomorrow
      integer           :: dat(8)
      character(len=*),parameter :: g='(*(g0,1x))'

         write(*,g)'b2d:'
         call date_and_time(values=dat) ! get the date using intrinsic
         today=d2b(dat)                 ! convert DAT to BAS
         aday=BAStime(1,0.0_dp)         ! a value of one day
         write(*,g)'Today=',fmtdate(b2d(today))

         write(*,g)'BAStime +- BAStime'
         write(*,g)'Yesterday=',fmtdate(b2d(today+BAStime(-1,0.0_dp)))
         write(*,g)'Tomorrow= ',fmtdate(b2d(today+BAStime(+1,0.0_dp)))

         write(*,g)'Yesterday=',fmtdate(b2d(today+BAStime(0,-86400.0_dp)))
         write(*,g)'Tomorrow= ',fmtdate(b2d(today+BAStime(0,+86400.0_dp)))

         write(*,g)'Yesterday=',fmtdate(b2d(today-aday))
         write(*,g)'Tomorrow= ',fmtdate(b2d(today+aday))

         yesterday=today-aday
         write(*,g)'Yesterday=',fmtdate(b2d(yesterday))
         tomorrow=today+aday
         write(*,g)'Tomorrow=',fmtdate(b2d(tomorrow))

         write(*,g)'BAStime +- value_in_seconds'
         write(*,g)'Yesterday=',fmtdate(b2d(today-86400))
         write(*,g)'Tomorrow= ',fmtdate(b2d(today+86400))

         write(*,g)'BAStime comparisons'
         newday=today+(aday/2)
         write(*,g)'today=',today%format()
         write(*,g)'newday=',newday%format()
         call pr(today,newday)
         call pr(newday,today)
         call pr(today,today)

         write(*,g)'BAStime compound expressions'
         write(*,g) (today+86400/2).eq.newday,fmtdate(b2d(newday))
      contains
         subroutine pr(left,right)
         type(BAStime),intent(in) :: left, right
         write(*,g) 'eq',left.eq.right, &
                    'gt',left.gt.right, &
                    'lt',left.lt.right, &
                    'ge',left.ge.right, &
                    'le',left.le.right, &
                    'ne',left.ne.right
         end subroutine pr
      end program demo_b2d
