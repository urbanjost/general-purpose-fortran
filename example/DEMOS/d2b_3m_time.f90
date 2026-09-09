      program demo_d2b
      use M_time, only : d2b, BAStime, d2j, d2m
      implicit none
      integer :: dat(8)
      type(BAStime) :: bas
      !                            Modified Julian Dates
      !
      !   To use this table, add the day-of-month to the tabulated entry.
      !   For example, 30 Jan 2000 = MJD 51573.
      ! __________________________________________________________________
      !  2000  2001  2002  2003  2004  2005  2006  2007  2008  2009
      integer,parameter :: array(1:12,2000:2009)=reshape([ &
       51543,51909,52274,52639,53004,53370,53735,54100,54465,54831, & ! Jan
       51574,51940,52305,52670,53035,53401,53766,54131,54496,54862, & ! Feb
       51603,51968,52333,52698,53064,53429,53794,54159,54525,54890, & ! Mar
       51634,51999,52364,52729,53095,53460,53825,54190,54556,54921, & ! Apr
       51664,52029,52394,52759,53125,53490,53855,54220,54586,54951, & ! May
       51695,52060,52425,52790,53156,53521,53886,54251,54617,54982, & ! Jun
       51725,52090,52455,52820,53186,53551,53916,54281,54647,55012, & ! Jul
       51756,52121,52486,52851,53217,53582,53947,54312,54678,55043, & ! Aug
       51787,52152,52517,52882,53248,53613,53978,54343,54709,55074, & ! Sep
       51817,52182,52547,52912,53278,53643,54008,54373,54739,55104, & ! Oct
       51848,52213,52578,52943,53309,53674,54039,54404,54770,55135, & ! Nov
       51878,52243,52608,52973,53339,53704,54069,54434,54800,55165],& ! Dec
       shape=shape(array),order=[2,1])
       integer :: i,j
         call date_and_time(values=dat)
         write(*,'(" Today is:",*(i0:,":"))')dat
         bas=d2b(dat)
         write(*,*)'Baseday and Seconds is',bas
         write(*,*)'Baseday is', bas%base_day ! whole days since the MJD Epoch date
         write(*,*)'Seconds is', bas%secs     ! offset in seconds from start of BASE_DAY
         ! print any date that does not match regression test values
         do i=2000,2009
          do j=1,12
           !dat=[ year,month,day,timezone,hour,minutes,seconds,milliseconds]
           dat=[i,j,1,0,0,0,0,0]   ! first day of month
           bas=d2b(dat)
           if(array(j,i)+1.ne.bas%base_day)then
              write(*,*)i,j,array(j,i)+1,d2b(dat),d2m(dat),d2j(dat)-2400000.5
           endif
          enddo
         enddo
      end program demo_d2b
