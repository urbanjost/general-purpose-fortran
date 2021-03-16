          program demo_M_units
          use M_units, only : r2d, d2r
          use M_units, only : f2c, c2f
          use M_units, only : sind, cosd, tand
          use M_units, only : asind, acosd, atand, atan2d
          !!
          use M_units, only : pi8=>pi
          use M_units, only : e,euler,golden_ratio,deg_per_rad,rad_per_deg
          use M_units, only : c__m_per_sec, c__ft_per_sec
          !!
          implicit none
          real pi
          pi=pi8
          write(*,*)r2d([0.0,PI/4.0,PI/2.0,3.0*PI/2.0,PI])
          write(*,*)d2r([0.0,45.0,90.0,135.0,180.0])
          write(*,*)f2c([-40.0,32.0,212.0])
          write(*,*)c2f([-40.0,0.0,100.0])
          write(*,*)PI
          write(*,*)E
          !!
          write(*,101) "Napier's constant (e) is about ",e
          write(*,101) "The Euler-Mascheroni constant (euler or gamma) is about ",euler
          write(*,101) "pi (pi) is about ",pi8
          write(*,101) "The Golden Ratio (golden_ratio) is about ",golden_ratio
          write(*,101) "Deg_Per_Rad is about ",Deg_Per_Rad
          write(*,101) "Rad_Per_Deg is about ",Rad_Per_Deg
          !!
          write(*,101) "Speed of light in a vacuum (m/sec)       ", c__m_per_sec
          write(*,101) "Speed of light in a vacuum (ft/sec)      ", c__ft_per_sec
          !!
          101 format(a,t57,g0)
          !!
          end program demo_M_units
