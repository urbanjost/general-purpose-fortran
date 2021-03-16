                    program demo_associate
                    real :: myreal, x, y, theta, a
                    x = 0.42
                    y = 0.35
                    myreal = 9.1
                    theta = 1.5
                    a = 0.4
                    associate ( z => exp(-(x**2+y**2)) * cos(theta), v => myreal)
                       print *, a+z, a-z, v
                       v = v * 4.6
                    end associate
                    print *, myreal
                    end program demo_associate
