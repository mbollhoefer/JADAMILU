#include <stdio.h>
#include <ilupack.h>

#include <ilupackmacros.h>


void CSRMATVEC(CSRMAT A, FLOAT *x, FLOAT *y)
{
/*---------------------------------------------------------------------
|
| This routine does the matrix vector product y = A x.
|
|----------------------------------------------------------------------
| on entry:
| x     = a vector
| A  = the matrix (in SparRow form)
|
| on return
| y     = the product A * x
|
|     code taken from ARMS of Yousef Saad.
|     adapted by Matthias Bollhoefer to fit with the data structures
|     of ILUPACK including the complex case
|--------------------------------------------------------------------*/
/*   local variables    */

   integer i, j, *ia, *ja;
   FLOAT *a;

   x--;
   ia=A.ia;
   ja=A.ja-1;
   a =A.a -1;
   for (i=0; i<A.nr; i++,y++,ia++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
       *y = 0.0;
#else
       y->r=y->i=0.0;
#endif
       for (j=*ia; j<*(ia+1); j++){
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
           *y += a[j] * x[ja[j]];
#else
           y->r += a[j].r*x[ja[j]].r - a[j].i*x[ja[j]].i;
           y->i += a[j].r*x[ja[j]].i + a[j].i*x[ja[j]].r;
#endif
       } // end for j
   } // end for i
}
/*---------------end of CSRmatvec---------------------------------------
----------------------------------------------------------------------*/


