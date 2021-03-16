          program demo_sum
          ! John Mahaffy  2/16/96
          implicit none
          integer nd, ndh, nduh, j
          parameter (nd=10,ndh=nd/2, nduh=nd-ndh)
          real csum, cpsum, cbpsum
          real, dimension(nd):: c=[(j, j=-1,nd-2)], b
          data b/ndh*-1.0, nduh*2.0/
             csum= sum(c(1:nd))
             cpsum= sum (c(1:nd), mask=c.gt.0)
             cbpsum= sum(c(1:nd), mask=b.gt.0.0)
             print *, 'Sum of all elements in c = ' , csum
             print *, 'Sum of Positive elements in c = ', cpsum
             print *, 'Sum of elements in c when corresponding elements in b>0', &
             & ' =', cbpsum
          end program demo_sum
