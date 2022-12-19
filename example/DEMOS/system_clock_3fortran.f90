      program demo_system_clock
      use, intrinsic :: iso_fortran_env, only : wp=>real64,int32,int64
      implicit none
      character(len=*),parameter :: g='(1x,*(g0,1x))'
      integer(kind=int64) :: count64, count_rate64, count_max64
      integer(kind=int64) :: start64, finish64
      integer(kind=int32) :: count32, count_rate32, count_max32
      integer(kind=int32) :: start32, finish32
      real(kind=wp)       :: time_read
      real(kind=wp)       :: sum
      integer             :: i

        print g,'accuracy may vary with argument type!'
        call system_clock(count_rate=count_rate64)
        print g,'COUNT RATE FOR INT64:',count_rate64
        call system_clock(count_rate=count_rate32)
        print g,'COUNT RATE FOR INT32:',count_rate32

        print g,'query all arguments'
        call system_clock(count64, count_rate64, count_max64)
        print g, 'COUNT_MAX=',count_max64
        print g, 'COUNT_RATE=',count_rate64
        print g, 'CURRENT COUNT=',count64

        print g,'time some computation'
        call system_clock(start64)

        ! some code to time
        sum=0.0_wp
        do i=-huge(0)-1, huge(0)-1, 10
           sum=sum+sqrt(real(i))
        enddo
        print g,'SUM=',sum

        call system_clock(finish64)

        time_read=(finish64-start64)/real(count_rate64,wp)
        write(*,'(a30,1x,f7.4,1x,a)') 'time : ', time_read, ' seconds'

      end program demo_system_clock
