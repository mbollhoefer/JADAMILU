#include <ilupack_fortran.h>
#include <ilupackmacros.h>



        subroutine QQSORTI(ind,stack,n)
        integer n, ind(n),stack(n)
c-----------------------------------------------------------------------
c     does a quick-sort of an integer array 
c     on input  ind(1:n) is an integer array
c     on output ind(1:n) is permuted such that its elements are in 
c               increasing order. 
c 
c     QQSORT computes a total ordering 
c     written by Matthias Bollhoefer, 2005
c-----------------------------------------------------------------------
        integer indtmp, key, first, last,top, mid, j
c-----
        top=0
        first=1
        last =n
c
c     outer loop -- while first<last or top>0 do
c
 1      if (first.ge.last .and. top.eq.0) goto 999
        mid = first
        key = ind(mid)
        do 2 j=first+1, last
           if (ind(j) .lt. key) then
              mid = mid+1
c     interchange entries at position j and mid
              indtmp  = ind(mid)
              ind(mid)= ind(j)
              ind(j)  = indtmp
           endif
 2      continue
c
c     interchange
        indtmp    = ind(mid)
        ind(mid)  = ind(first)
        ind(first)= indtmp
c
c     test for while loop
        if (first.lt.mid-1 .or. last.gt.mid+1) then
           if (first .lt. mid-1) then
              top=top+1
              stack(top)=mid
              last=mid-1
           else
              first=mid+1
           endif
        else
           if (top.gt.0) then
              first=stack(top)+1
              stack(top)=0
              top=top-1
              if (top.gt.0) then
                 last=stack(top)-1
              else
                 last=n
              endif
           else
              first=last
           endif
        end if
        goto 1
 999    return
        end
c----------------end-of-qqsort------------------------------------------
c-----------------------------------------------------------------------
