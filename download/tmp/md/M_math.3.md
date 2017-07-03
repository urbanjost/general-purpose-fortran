[CLI Home Page]

NAME

M_math(3fp) - [M_math]module collecting various general math-related procedures together

DESCRIPTION

GEOMETRY

  * citer(3f) determine various geometric properties of circle segment given radius and area of the segment.
  * envelope(3f) Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
  * inpolygon(3f) Subroutine to determine whether or not a point is in a polygon
  * locpt(3f) find if a point is inside a polygonal path
  * poly_intercept(3f) find points where a line intersects a polygon
  * polyarea(3f) find area of a polygon

FIT

  * julfit(1f) linear least square fit
  * julfit1(3f) linear least square fit(y=a*x+b)
  * lowess(3f) data smoothing using locally weighted regression
  * splift(3f) fits a spline to the n data points given in x and y
  * splint(3f) interpolates and twice differentiates a cubic spline
  * linearint(3f) linear interpolation

STATISTICS

  * extremum(3f) find the minimum and maximum value in a real array
  * bds(3f) basic descriptive statistics
  * skekurx(3f) skew and kurtosis variant
  * skekur1(3f) skew and kurtosis variant
  * stddef(3f) standard deviation

COMPARING AND ROUNDING FLOATING POINT VALUES

  * accdig(3f) compare two real numbers only up to a specified number of digits
  * almost(3f) compare two real numbers only up to a specified number of digits
  * dp_accdig(3f) compare two double numbers only up to a specified number of digits
  * round(3f) round val to specified number of significant digits
  * scale1(3f) given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist
  * scale3(3f) find nice log range, typically for an axis

MATRICES

  * invert_2x2(3f) directly invert 2x2 matrix
  * invert_3x3(3f) directly invert 3x3 matrix
  * invert_4x4(3f) directly invert 4x4 matrix

