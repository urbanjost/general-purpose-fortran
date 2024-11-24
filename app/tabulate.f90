program tabulate
use M_messages, only : tabgraph
use M_kracken,  only : kracken, sget, iget, rget, lget
use M_strings,  only : s2vs
character(len=5000)         :: ctmp
character(len=5000)         :: line
character(len=1)            :: fill
character(len=256)          :: delims
real,allocatable            :: array(:)
integer                     :: ios
integer                     :: isize=-1
integer                     :: ilen
real                        :: rmin, rmax

   call kracken('tabulate',' -fill -min 0 -max 100 -delims -len 0 -version .f. -help .f.')
   call help_usage(lget('tabulate_help'))      ! if -help option is present, display help text and exit
   call help_version(lget('tabulate_version')) ! if -version option is present, display version text and exit
   fill=sget('tabulate_fill')
   delims=sget('tabulate_delims')
   ilen=iget('tabulate_len')
   rmin=rget('tabulate_min')
   rmax=rget('tabulate_max')

   write(*,*)'MIN=',rmin,',MAX=',rmax
   INFINITE: do
      read(*,'(a)',iostat=ios)line
      if(ios.ne.0)exit INFINITE
      array=s2vs(line,delims)
      if(isize.le.0)then
         isize=size(array)
      endif
      call tabgraph(ctmp,array,isize,rmin,rmax,fill,ilen)
      write(*,'(a)')trim(ctmp)
   enddo INFINITE

contains

subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'     tabulate(1f) - [M_messages] write out a row of numbers and a text-based scaled graph',&
'     (LICENSE:PD)                                                               ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'     tabulate -min 0 -max 100 -fill '' '' -len 0                                ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   tabulate(1f) makes it very easy to find values in particular ranges in printed output.',&
'   It works particularly well with the once ubiquitous fan-fold paper.          ',&
'                                                                                ',&
'   It can be used in scripts to make terminal-based monitors of resources such as memory, CPU load, and file space.',&
'                                                                                ',&
'   Given a max and min value for setting up a range read columns of             ',&
'   numbers and write the numbers back out with a text-based graph scale appended.',&
'   Each column of numbers is written with a format of "1x,g14.7", taking up 15 characters per number.',&
'                                                                                ',&
'   The first column is assumed to be the X values.                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    -min     low value  of range to place values in                             ',&
'    -max     high value of range to place values in                             ',&
'    -len     is the number of characters to use for the scale region.           ',&
'             If set to 0, it pads out to 132 columns unless the scale would be  ',&
'             less than 15 characters wide; then it pads out to 255 characters   ',&
'    -fill    the character to fill the graph background with. Defaults to space.',&
'    -delims  list of characters to use as delimiters. Defaults to a space       ',&
'                                                                                ',&
'   Each number takes up 15 columns on output.                                   ',&
'                                                                                ',&
'                                                                                ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
' The purpose of this routine becomes much clearer when looking at a sample      ',&
' output.                                                                        ',&
'                                                                                ',&
'  0.1200000      0.1197122      0.9928086    #                           1                     2#',&
'  0.2400000      0.2377026      0.9713380    #                              1                 2 #',&
'  0.3600000      0.3522742      0.9358968    #                                 1             2  #',&
'  0.4800000      0.4617792      0.8869949    #                                    1         2   #',&
'  0.6000000      0.5646425      0.8253356    #                                      1      2    #',&
'  0.7200000      0.6593847      0.7518057    #                                        1  2      #',&
'  0.8400000      0.7446431      0.6674628    #                                         2 1      #',&
'  0.9600000      0.8191915      0.5735200    #                                      2     1     #',&
'   1.080000      0.8819578      0.4713283    #                                    2         1   #',&
'   1.200000      0.9320391      0.3623577    #                                 2             1  #',&
'   1.320000      0.9687151      0.2481754    #                              2                 1 #',&
'   1.440000      0.9914584      0.1304237    #                           2                     1#',&
'   1.560000      0.9999417      0.1079617E-01#                        2                        1#',&
'   1.680000      0.9940432     -0.1089867    #                     2                           1#',&
'   1.800000      0.9738476     -0.2272020    #                  2                             1 #',&
'   1.920000      0.9396455     -0.3421496    #               2                               1  #',&
'   2.040000      0.8919287     -0.4521761    #             2                                1   #',&
'   2.160000      0.8313834     -0.5556992    #          2                                  1    #',&
'   2.280000      0.7588807     -0.6512296    #        2                                  1      #',&
'   2.400000      0.6754631     -0.7373938    #      2                                  1        #',&
'   2.520000      0.5823306     -0.8129520    #    2                                  1          #',&
'   2.640000      0.4808225     -0.8768179    #  2                                 1             #',&
'   2.760000      0.3723991     -0.9280727    # 2                               1                #',&
'   2.880000      0.2586192     -0.9659793    #2                             1                   #',&
'   3.000000      0.1411200     -0.9899925    #2                           1                     #',&
'   3.120000      0.2159109E-01 -0.9997669    #2                        1                        #',&
'   3.240000     -0.9824860E-01 -0.9951619    #2                     1                           #',&
'   3.360000     -0.2166750     -0.9762438    #2                  1                              #',&
'   3.480000     -0.3319852     -0.9432846    #2               1                                 #',&
'   3.600000     -0.4425204     -0.8967585    #  2          1                                    #',&
'   3.720000     -0.5466911     -0.8373344    #   2      1                                       #',&
'   3.840000     -0.6429987     -0.7658673    #     2  1                                         #',&
'   3.960000     -0.7300584     -0.6833848    #      12                                          #',&
'   4.080000     -0.8066177     -0.5910735    #    1    2                                        #',&
'   4.200000     -0.8715757     -0.4902610    #  1         2                                     #',&
'   4.320000     -0.9239982     -0.3823968    # 1            2                                   #',&
'   4.440000     -0.9631310     -0.2690330    #1                2                                #',&
'   4.560000     -0.9884112     -0.1517999    #1                   2                             #',&
'   4.680000     -0.9994755     -0.3238349E-01#1                      2                          #',&
'   4.800000     -0.9961646      0.8749917E-01#1                         2                       #',&
'   4.920000     -0.9785261      0.2061229    #1                            2                    #',&
'   5.040000     -0.9468138      0.3217820    #1                               2                 #',&
'   5.160000     -0.9014837      0.4328130    # 1                                 2              #',&
'   5.280000     -0.8431876      0.5376194    #   1                                 2            #',&
'   5.400000     -0.7727644      0.6346930    #     1                                  2         #',&
'   5.520000     -0.6912268      0.7226379    #       1                                  2       #',&
'   5.640000     -0.5997474      0.8001894    #         1                                  2     #',&
'   5.760000     -0.4996417      0.8662322    #            1                                 2   #',&
'   5.880000     -0.3923501      0.9198160    #              1                                2  #',&
'   6.000000     -0.2794155      0.9601703    #                 1                              2 #',&
'   6.120000     -0.1624621      0.9867148    #                    1                            2#',&
'   6.240000     -0.4317211E-01  0.9990677    #                       1                         2#',&
'   6.360000      0.7673930E-01  0.9970512    #                          1                      2#',&
'   6.480000      0.1955465      0.9806944    #                             1                   2#',&
'   6.600000      0.3115413      0.9502326    #                                1               2 #',&
'   6.720000      0.4230552      0.9061039    #                                   1           2  #',&
'   6.840000      0.5284849      0.8489427    #                                     1       2    #',&
'   6.960000      0.6263130      0.7795717    #                                        1  2      #',&
'   7.080000      0.7151332      0.6989882    #                                         21       #',&
'   7.200000      0.7936678      0.6083515    #                                       2    1     #',&
'   7.320000      0.8607875      0.5089645    #                                     2        1   #',&
'   7.440000      0.9155264      0.4022577    #                                  2            1  #',&
'   7.560000      0.9570977      0.2897654    #                               2                1 #',&
'   7.680000      0.9849033      0.1731054    #                            2                    1#',&
'   7.800000      0.9985434      0.5395523E-01#                         2                       1#',&
'   7.920000      0.9978216     -0.6597050E-01#                      2                          1#',&
'   8.040000      0.9827484     -0.1849474    #                   2                             1#',&
'   8.160000      0.9535407     -0.3012642    #                2                               1 #',&
'   8.280000      0.9106185     -0.4132481    #              2                                1  #',&
'   8.400000      0.8545991     -0.5192883    #           2                                 1    #',&
'   8.520000      0.7862877     -0.6178606    #         2                                  1     #',&
'   8.640000      0.7066678     -0.7075455    #      2                                   1       #',&
'   8.760000      0.6168842     -0.7870539    #    2                                  1          #',&
'   8.880000      0.5182281     -0.8552424    #   2                                 1            #',&
'   9.000000      0.4121185     -0.9111302    # 2                                1               #',&
'   9.120000      0.3000815     -0.9539136    #2                               1                 #',&
'   9.240000      0.1837285     -0.9829770    #2                            1                    #',&
'   9.360000      0.6473301E-01 -0.9979026    #2                         1                       #',&
'   9.480000     -0.5519352E-01 -0.9984757    #2                      1                          #',&
'   9.600000     -0.1743271     -0.9846878    #2                   1                             #',&
'   9.720000     -0.2909525     -0.9567375    #2                1                                #',&
'   9.840000     -0.4033932     -0.9150267    # 2            1                                   #',&
'   9.960000     -0.5100321     -0.8601554    #  2        1                                      #',&
'   10.08000     -0.6093352     -0.7929127    #    2    1                                        #',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!      tabulate(1f) - [M_messages] write out a row of numbers and a text-based scaled graph
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!      tabulate -min 0 -max 100 -fill ' ' -len 0
!!
!!##DESCRIPTION
!!    tabulate(1f) makes it very easy to find values in particular ranges in printed output.
!!    It works particularly well with the once ubiquitous fan-fold paper.
!!
!!    It can be used in scripts to make terminal-based monitors of resources such as memory, CPU load, and file space.
!!
!!    Given a max and min value for setting up a range read columns of
!!    numbers and write the numbers back out with a text-based graph scale appended.
!!    Each column of numbers is written with a format of "1x,g14.7", taking up 15 characters per number.
!!
!!    The first column is assumed to be the X values.
!!
!!##OPTIONS
!!     -min     low value  of range to place values in
!!     -max     high value of range to place values in
!!     -len     is the number of characters to use for the scale region.
!!              If set to 0, it pads out to 132 columns unless the scale would be
!!              less than 15 characters wide; then it pads out to 255 characters
!!     -fill    the character to fill the graph background with. Defaults to space.
!!     -delims  list of characters to use as delimiters. Defaults to a space
!!
!!    Each number takes up 15 columns on output.
!!
!!
!!
!!##EXAMPLE
!!
!!
!!  The purpose of this routine becomes much clearer when looking at a sample
!!  output.
!!
!!   0.1200000      0.1197122      0.9928086    #                           1                     2#
!!   0.2400000      0.2377026      0.9713380    #                              1                 2 #
!!   0.3600000      0.3522742      0.9358968    #                                 1             2  #
!!   0.4800000      0.4617792      0.8869949    #                                    1         2   #
!!   0.6000000      0.5646425      0.8253356    #                                      1      2    #
!!   0.7200000      0.6593847      0.7518057    #                                        1  2      #
!!   0.8400000      0.7446431      0.6674628    #                                         2 1      #
!!   0.9600000      0.8191915      0.5735200    #                                      2     1     #
!!    1.080000      0.8819578      0.4713283    #                                    2         1   #
!!    1.200000      0.9320391      0.3623577    #                                 2             1  #
!!    1.320000      0.9687151      0.2481754    #                              2                 1 #
!!    1.440000      0.9914584      0.1304237    #                           2                     1#
!!    1.560000      0.9999417      0.1079617E-01#                        2                        1#
!!    1.680000      0.9940432     -0.1089867    #                     2                           1#
!!    1.800000      0.9738476     -0.2272020    #                  2                             1 #
!!    1.920000      0.9396455     -0.3421496    #               2                               1  #
!!    2.040000      0.8919287     -0.4521761    #             2                                1   #
!!    2.160000      0.8313834     -0.5556992    #          2                                  1    #
!!    2.280000      0.7588807     -0.6512296    #        2                                  1      #
!!    2.400000      0.6754631     -0.7373938    #      2                                  1        #
!!    2.520000      0.5823306     -0.8129520    #    2                                  1          #
!!    2.640000      0.4808225     -0.8768179    #  2                                 1             #
!!    2.760000      0.3723991     -0.9280727    # 2                               1                #
!!    2.880000      0.2586192     -0.9659793    #2                             1                   #
!!    3.000000      0.1411200     -0.9899925    #2                           1                     #
!!    3.120000      0.2159109E-01 -0.9997669    #2                        1                        #
!!    3.240000     -0.9824860E-01 -0.9951619    #2                     1                           #
!!    3.360000     -0.2166750     -0.9762438    #2                  1                              #
!!    3.480000     -0.3319852     -0.9432846    #2               1                                 #
!!    3.600000     -0.4425204     -0.8967585    #  2          1                                    #
!!    3.720000     -0.5466911     -0.8373344    #   2      1                                       #
!!    3.840000     -0.6429987     -0.7658673    #     2  1                                         #
!!    3.960000     -0.7300584     -0.6833848    #      12                                          #
!!    4.080000     -0.8066177     -0.5910735    #    1    2                                        #
!!    4.200000     -0.8715757     -0.4902610    #  1         2                                     #
!!    4.320000     -0.9239982     -0.3823968    # 1            2                                   #
!!    4.440000     -0.9631310     -0.2690330    #1                2                                #
!!    4.560000     -0.9884112     -0.1517999    #1                   2                             #
!!    4.680000     -0.9994755     -0.3238349E-01#1                      2                          #
!!    4.800000     -0.9961646      0.8749917E-01#1                         2                       #
!!    4.920000     -0.9785261      0.2061229    #1                            2                    #
!!    5.040000     -0.9468138      0.3217820    #1                               2                 #
!!    5.160000     -0.9014837      0.4328130    # 1                                 2              #
!!    5.280000     -0.8431876      0.5376194    #   1                                 2            #
!!    5.400000     -0.7727644      0.6346930    #     1                                  2         #
!!    5.520000     -0.6912268      0.7226379    #       1                                  2       #
!!    5.640000     -0.5997474      0.8001894    #         1                                  2     #
!!    5.760000     -0.4996417      0.8662322    #            1                                 2   #
!!    5.880000     -0.3923501      0.9198160    #              1                                2  #
!!    6.000000     -0.2794155      0.9601703    #                 1                              2 #
!!    6.120000     -0.1624621      0.9867148    #                    1                            2#
!!    6.240000     -0.4317211E-01  0.9990677    #                       1                         2#
!!    6.360000      0.7673930E-01  0.9970512    #                          1                      2#
!!    6.480000      0.1955465      0.9806944    #                             1                   2#
!!    6.600000      0.3115413      0.9502326    #                                1               2 #
!!    6.720000      0.4230552      0.9061039    #                                   1           2  #
!!    6.840000      0.5284849      0.8489427    #                                     1       2    #
!!    6.960000      0.6263130      0.7795717    #                                        1  2      #
!!    7.080000      0.7151332      0.6989882    #                                         21       #
!!    7.200000      0.7936678      0.6083515    #                                       2    1     #
!!    7.320000      0.8607875      0.5089645    #                                     2        1   #
!!    7.440000      0.9155264      0.4022577    #                                  2            1  #
!!    7.560000      0.9570977      0.2897654    #                               2                1 #
!!    7.680000      0.9849033      0.1731054    #                            2                    1#
!!    7.800000      0.9985434      0.5395523E-01#                         2                       1#
!!    7.920000      0.9978216     -0.6597050E-01#                      2                          1#
!!    8.040000      0.9827484     -0.1849474    #                   2                             1#
!!    8.160000      0.9535407     -0.3012642    #                2                               1 #
!!    8.280000      0.9106185     -0.4132481    #              2                                1  #
!!    8.400000      0.8545991     -0.5192883    #           2                                 1    #
!!    8.520000      0.7862877     -0.6178606    #         2                                  1     #
!!    8.640000      0.7066678     -0.7075455    #      2                                   1       #
!!    8.760000      0.6168842     -0.7870539    #    2                                  1          #
!!    8.880000      0.5182281     -0.8552424    #   2                                 1            #
!!    9.000000      0.4121185     -0.9111302    # 2                                1               #
!!    9.120000      0.3000815     -0.9539136    #2                               1                 #
!!    9.240000      0.1837285     -0.9829770    #2                            1                    #
!!    9.360000      0.6473301E-01 -0.9979026    #2                         1                       #
!!    9.480000     -0.5519352E-01 -0.9984757    #2                      1                          #
!!    9.600000     -0.1743271     -0.9846878    #2                   1                             #
!!    9.720000     -0.2909525     -0.9567375    #2                1                                #
!!    9.840000     -0.4033932     -0.9150267    # 2            1                                   #
!!    9.960000     -0.5100321     -0.8601554    #  2        1                                      #
!!    10.08000     -0.6093352     -0.7929127    #    2    1                                        #
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        tabulate>',&
'@(#)DESCRIPTION:    print values alongside a text graph of the values>',&
'@(#)VERSION:        Thu Nov 23,  2000>',&
'@(#)AUTHORS:        John S. Urban>',&
'@(#)LANGUAGE:       Fortran>',&
'@(#)COMPILED:       2024-11-24 04:45:32 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program tabulate
