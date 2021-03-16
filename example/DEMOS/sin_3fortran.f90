                  program demo_sin
                  implicit none
                  real :: d
                      d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX
                      print '(A,F9.4,A)', 'distance: ',d,' km'
                  contains
                  function haversine(latA,lonA,latB,lonB) result (dist)
                  !
                  ! calculate great circle distance in kilometers
                  ! given latitude and longitude in degrees
                  !
                  real,intent(in) :: latA,lonA,latB,lonB
                  real :: a,c,dist,delta_lat,delta_lon,lat1,lat2
                  real,parameter :: radius = 6371 ! mean earth radius in kilometers,
                  ! recommended by the International Union of Geodesy and Geophysics
                  real, parameter :: deg_to_rad = atan(1.0)/45.0 ! generate constant pi/180
                     delta_lat = deg_to_rad*(latB-latA)
                     delta_lon = deg_to_rad*(lonB-lonA)
                     lat1 = deg_to_rad*(latA)
                     lat2 = deg_to_rad*(latB)
                     a = (sin(delta_lat/2))**2 + cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2
                     c = 2*asin(sqrt(a))
                     dist = radius*c
                  end function haversine
                  end program demo_sin
