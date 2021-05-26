#include <ilupack_fortran.h>
      double precision function ddot2(n,dx,incx)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
c     changes done by Matthias Bollhoefer, October 2003 
c     copy of ddot when x=y. This version avoids conflicts
c     with the optimization when alias of x and y are used
c     
      implicit none
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot2 = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dx(ix)
        ix = ix + incx
   10 continue
      ddot2 = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dx(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dx(i) + dx(i + 1)*dx(i + 1) +
     *   dx(i + 2)*dx(i + 2) + dx(i + 3)*dx(i + 3) + dx(i + 4)*dx(i + 4)
   50 continue
   60 ddot2 = dtemp
      return
      end
