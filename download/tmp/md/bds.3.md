[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                 Manual Reference Pages  - bds (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    bds(3f) - [M_math:statistics] Basic Statistical Measures

CONTENTS

    Synopsis
    Description
    Options
    Returns
    Definitions
    Examples

SYNOPSIS



    subroutine bds(x,n,stat)


        integer,intent(in) :: n
        real,intent(in)    :: x(n)
        real,intent(out)   :: STAT(13)

DESCRIPTION

    Given a vector of real values calculate common basic statistical meansures for the array.

OPTIONS

          x REAL input vector of values to determine statistical properties for

          n size of input vector

RETURNS

        stat array of statistical measurements calculated

            1. mean

            2. second moment about the mean

            3. third moment about the mean

            4. fourth moment about the mean

            5. variance

            6. standard deviation

            7. skewness

            8. kurtosis

            9. sum

        10. largest value

        11. smallest value

        12. location of largest value

        13. location of smallest value

DEFINITIONS

    MEAN

    A type of average, calculated by dividing the sum of a set of values by the number of values.

                   mean = Sum(Xi)/N



    MEDIAN

    A type of average, found by arranging the values in order and then selecting the one in the middle. If the total number of
    values in the sample is even, then the median is the mean of the two middle numbers.

    MODE

    The most frequent value in a group of values.

    VARIANCE

    The average of the square of the distance of each data point from the mean

                   variance = Sum((Xi-mean)^2))/N



    for a population, or more commonly, for a sample the unbiased value is

                   variance = Sum((Xi-mean)^2))/(N-1)



    STANDARD DEVIATION

    The standard deviation is the square root of the variance.

                   sd = sqrt(variance)



    It is the most commonly used measure of spread.

    SKEWNESS

    Skewness is a measure of symmetry, or more precisely, the lack of symmetry. A distribution, or data set, is symmetric if it
    looks the same to the left and right of the center point.

                   skewness = Sum{(X(i)-mean)^3} /((N-1)*SD^3)



    Where SD is the standard deviation, and N is the number of samples. Some sources will use N instead of N-1 or they might
    present the formula in a slightly different mathematically equivalent format.

                   The skewness of symmetric data is zero



    KURTOSIS

    Kurtosis is a measure of whether the data are peaked or flat relative to a normal distribution. That is, data sets with high
    kurtosis tend to have a distinct peak near the mean, decline rather rapidly, and have heavy tails. Data sets with low kurtosis
    tend to have a flat top near the mean rather than a sharp peak. A uniform distribution would be the extreme case.

                   kurtosis = ( SUM{(X(i)-mean)^4} ) / ((N-1)*SD^4) -3



    The standard normal distribution has a kurtosis of zero. Positive kurtosis indicates a "peaked" distribution and negative
    kurtosis indicates a "flat" distribution.

Although often called kurtosis, historically the above expression is for "excess kurtosis" because three is subtracted from the
value. The purpose of this is to give the normal distribution a kurtosis of 0. In recent times, the term "excess kurtosis" is often
simply called "kurtosis", so consider that whether to subtract 3 or not is merely a convention, not a right or wrong answer. When
using a particular program, you just need to be aware of which convention


    Again, another freqent difference is whether they use N in the denominator or the bias corrected N-1.

    The formulas for skewness and kurtosis are treated in

                 Sokal, R. R., & Rohlf, F. J. (1995).
                 Biometry: The principles and practice of statistics in biological
                 research. (3rd ed.) New York: W. H. Freeman, pp. 114-115.



    Similar formulas are given in

                 Zar, J. H., Biostatistical analysis (3rd ed)., Prentice
                 Hall, 1996.



    which refers to "machine formulas" and cites

                 Bennett and Franklin, Statistical analysis in chemistry and
                 the chemical industry. NY: Wiley, 1954, at p. 81.

EXAMPLES

-----------------------------------------------------------------------------------------------------------------------------------

                                                              bds (3)                                                 July 02, 2017

Generated by manServer 1.08 from 7210595e-6101-4976-9b77-856ddcf2b988 using man macros.
                                                               [bds]
