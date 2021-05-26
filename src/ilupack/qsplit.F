#include <ilupack_fortran.h>
#include <ilupackmacros.h>





        subroutine QSPLIT(a,ind,n,ncut)
        integer n,ind(n), ncut
        FLOAT a(n)
c-----------------------------------------------------------------------
c     does a quick-sort split of a real array.
c     on input a(1:n). is a real array
c     on output a(1:n) is permuted such that its elements satisfy:
c
c     |a(i)| >= |a(ncut)| for i < ncut and
c     |a(i)| <= |a(ncut)| for i > ncut
c
c     ind(1:n) is an integer array which permuted in the same way as a(*).
c
c     code taken from SPARSKIT of Yousef Saad.
c     adapted by Matthias Bollhoefer for the complex case
c-----------------------------------------------------------------------
        FLOAT tmp
        REALS abskey, ABSFNAME
        integer itmp, first, last, mid, j
c-----
        first = 1
        last = n
        if (ncut .lt. first .or. ncut .gt. last) return
c
c     outer loop -- while mid .ne. ncut do
c
 1      mid = first
        abskey = ABS(a(mid))
        do 2 j=first+1, last
           if (ABS(a(j)) .gt. abskey) then
              mid = mid+1
c     interchange
              tmp = a(mid)
              itmp = ind(mid)
              a(mid) = a(j)
              ind(mid) = ind(j)
              a(j)  = tmp
              ind(j) = itmp
           endif
 2      continue
c
c     interchange
c
        tmp = a(mid)
        a(mid) = a(first)
        a(first)  = tmp
c
        itmp = ind(mid)
        ind(mid) = ind(first)
        ind(first) = itmp
c
c     test for while loop
c
        if (mid .eq. ncut) return
        if (mid .gt. ncut) then
           last = mid-1
        else
           first = mid+1
        endif
        goto 1
c----------------end-of-qsplit------------------------------------------
c-----------------------------------------------------------------------
        end
