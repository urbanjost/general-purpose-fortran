     program demo_M_roman_numbers
     ! Test case is courtesy of Jeanne Martin
     use M_roman_numbers
     implicit none

     character(len=16), dimension(3999) :: table
     type(roman)                :: r
     integer                    :: i

     type(roman)                :: year_2000, cornerstone, &
     & bad_place, bad_dig, bad_dig2, long_dig, BC
     type(roman)                :: bad_place2, too_big
     type(roman), dimension(10) :: errors
     type(roman), dimension(5)  :: arith
     integer                    :: centuries, ix, iy, iz, iterate_runs
     character(len=16)          :: long
     character(len=3)           :: short

        do iterate_runs = 1,1000

     ! test r = i
     ! print table of all roman numbers,
     ! save roman values for 2nd part of test
           write (unit=*, fmt="(a)") "Integer  Roman Number"
     ! do i = 1, 3999
           do i = 1900, 2000  ! shortened to reduce output
              r = i
              write (unit=*, fmt="(/, tr4, i4, tr2)", advance = "NO") i
              call print_roman (r)
              table(i) = r
           enddo
           write (unit=*, fmt="(2/)")

     ! test r = c
     ! print table again converting roman to integer
           write (unit=*, fmt="(a,/)") "Integer  Roman Number"
     ! do i = 1, 3999
           do i = 1985, 1995  ! shortened to reduce output
              r = table(i)
              write (unit=*, fmt="(tr4, i4, tr2, 16a, /)") int(r), table(i)
           enddo
           write (unit=*, fmt="(/)")

     ! test c = r
           long = r
           short = r
           write (unit=*, fmt="(a, 2a17)")  " short and long ", short, long

     ! test i = r
           ix = r
           write (unit=*, fmt="(/, a, i4)") " ix = ", ix

     ! test len
           ix = len(r)
           write (unit=*, fmt="(a, i4, /)") " len(r) = ", ix

     ! test roman_number
           iy = roman_number(25)
           iz = roman_number("XXIX")
           write (unit=*, fmt="(a, 2i4, /)") " iy and iz ", iy, iz

     ! test error procedures, arithmetic, and comparison

           year_2000 = "MM"
           too_big = 2 * year_2000
           cornerstone = 1913
           BC = -12
           bad_place = "XXIC"
           bad_dig = "MCM XXX III"
           long_dig = "MCMXXXIII  "
           write (unit=*, fmt="(/, a)", advance = "NO") "long_dig = "
           call print_roman (long_dig)

           centuries = int(cornerstone/100)
           if (cornerstone==1913) then
              write (unit=*, fmt="(/,a)") "good == test"
           else
              write (unit=*, fmt="(/,a)") "bad == test"
           end if
           if (cornerstone == "MCMXIII") then
              write (unit=*, fmt="(/,a)") "good == test"
           else
              write (unit=*, fmt="(/,a)") "bad == test"
           end if
           if (long_dig > 1900) then
              write (unit=*, fmt="(/,a)") "good > test"
           else
              write (unit=*, fmt="(/,a)") "bad > test"
           end if

           bad_dig2 = "MQM"
           bad_place2 = "MMIVX"

           write (unit=*, fmt="(a, i4,/)") "centuries = ", centuries
           write (unit=*, fmt="(a)", advance = "NO") "cornerstone = "
           call print_roman (cornerstone)
           write (unit=*, fmt="(/, a)", advance = "NO") "year_2000 = "
           call print_roman (year_2000)
           write (unit=*, fmt="(/, a)", advance = "NO") "bad_place = "
           call print_roman (bad_place)
           write (unit=*, fmt="(/, a)", advance = "NO") "bad_dig = "
           call print_roman (bad_dig)
           write (unit=*, fmt="(/)")

           errors(1) = "MCCCCX"
           errors(2) = "MDDCX"
           errors(3) = "LXIVI"
           write (unit=*, fmt="(/, a, i4, /)") "LXIVI = ", int(errors(3))
           errors(4) = "LIXIV"
           errors(5) = "MCMDXX"
           errors(6) = "MCMXXXXI"
           errors(7) = "MXLX"
           write (unit=*, fmt="(/, a, i4, /)") "MXLX = ", int(errors(7))
           errors(8) = "MCMCXX"
           errors(9) = "MXLXI"

           arith(1) = 2
           arith(2) = arith(1) * "X"
           arith(3) = arith(2) / "IV"
           arith(4) = arith(3) + cornerstone
           arith(5) = year_2000 - "CIII"
           write (unit=*, fmt="(/, a, 5i6, /)") "arith = ", &
           & ((int(arith(i))), i = 1, 5)

        enddo

     end program demo_M_roman_numbers
