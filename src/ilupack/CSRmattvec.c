#include <stdio.h>
#include <ilupack.h>

#include <ilupackmacros.h>




void CSRMATTVEC(CSRMAT A, FLOAT *x, FLOAT *y)
{
/*---------------------------------------------------------------------
|
| This routine does the matrix vector product y = A^T x.
|
|----------------------------------------------------------------------
| on entry:
| x     = a vector
| A  = the matrix (in SparRow form)
|
| on return
| y     = the product A^T * x
|
|     code taken from ARMS of Yousef Saad.
|     adapted by Matthias Bollhoefer to fit with the data structures
|     of ILUPACK including the complex case
|--------------------------------------------------------------------*/
/*   local variables    */

   integer i, j, *ia, *ja;
   FLOAT *a;

   /* clear y */
   for (i=0; i<A.nc; i++) 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
       y[i] = 0.0;
#else
       y[i].r=y[i].i=0.0;
#endif

   y--;
   ia=A.ia;
   ja=A.ja-1;
   a =A.a -1;
   for (i=0; i<A.nr; i++,x++,ia++) {
       for (j=*ia; j<*(ia+1); j++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
           y[ja[j]] += a[j] * *x;
#else
           y[ja[j]].r += a[j].r*x->r - a[j].i*x->i;
           y[ja[j]].i += a[j].r*x->i + a[j].i*x->r;
#endif
       } // end for j
   } // end for i
}
/*---------------end of CSRmattvec--------------------------------------
----------------------------------------------------------------------*/
