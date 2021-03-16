          program demo_M_big_integer
          use M_big_integer
          type(big_integer) :: a, b
          a = "1234567890"
          b = a ** 34
          call print_big(b)
          end program demo_M_big_integer
