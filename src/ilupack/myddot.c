#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <blas.h>
#include <lapack.h>

#include <ilupack.h>
#include <ilupackmacros.h>
#include <namesilupack.h>


#define STDERR          stderr
#define STDOUT          stdout


FLOAT MYDDOTC(integer *n, FLOAT *x, integer *ix, FLOAT *y, integer *iy)
{
  FLOAT falpha;
  
  // scalar product (x,y)
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_		     
  falpha=DISTDOT(n,x,ix,y,iy);
#else
 #ifdef _USE_MKL_
   DISTDOT(*n, x,*ix, y,*iy, &falpha);
 #else
   falpha=DISTDOT(n,x,ix,y,iy);
 #endif       
#endif

   return falpha;
} // end MYDDOTC

