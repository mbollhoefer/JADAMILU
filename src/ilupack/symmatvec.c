#include <stdlib.h>
#include <stdio.h>
#include <ilupack.h>

#include <ilupackmacros.h>


#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYMATVEC       DSSMmatvec
#else
#define MYMATVEC       SSSMmatvec
#endif

#define SKEW(A)      (-(A))

#else

#ifdef _DOUBLE_REAL_
#define MYMATVEC       DSYMmatvec
#else
#define MYMATVEC       SSYMmatvec
#endif

#define SKEW(A)      (A)
#endif
// end _SKEW_MATRIX_


#else

#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYMATVEC       CSSMmatvec
#else
#define MYMATVEC       ZSSMmatvec
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYMATVEC       CSYMmatvec
#else
#define MYMATVEC       ZSYMmatvec
#endif

#endif
// end _SKEW_MATRIX_


#else
#define CONJG(A)     (-(A))

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYMATVEC       CSHRmatvec
#else
#define MYMATVEC       ZSHRmatvec
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYMATVEC       CHERmatvec
#else
#define MYMATVEC       ZHERmatvec
#endif

#endif
// end _SKEW_MATRIX_

#endif
// end _COMPLEX_SYMMETRIC_

#endif
// end _DOUBLE_REAL_




void MYMATVEC(CSRMAT A, FLOAT *x, FLOAT *y)
{
/*---------------------------------------------------------------------
|
| This routine does the matrix vector product y = A x for a symmetric
| matrix, where only the upper triangular part is stored
|
|----------------------------------------------------------------------
| on entry:
| x     = a vector
| A  = the matrix (in SparRow form)
|
| on return
| y     = the product A * x
|
|  adaption of matvec for symmetric matrices, where only the upper
|  triangular part is stored 
|  adaption by Matthias Bollhoefer, 2003
|--------------------------------------------------------------------*/
/*   local variables    */

   integer i, j,k, *ki;
   FLOAT *kr;
   REALS val;

   /* y=U*x, U strict upper triangular part of A */
   //kn=A->nnzrow;
   // adjust x to match FORTRAN indexing
   x--;
   for (i=0; i<A.nr; i++,y++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
       *y = 0.0;
#else
       y->r=y->i=0.0;
#endif
       ki = A.ja+A.ia[i]-1;
       kr = A.a +A.ia[i]-1;

       for (k=0; k<A.ia[i+1]-A.ia[i]; k++) {
	   /* exclude the diagonal part if present */
	   if (*ki==i+1) {
	      kr++;
	      ki++;
	   } // end if
	   else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	      *y += *kr++ * x[*ki++];
#else
	      y->r += kr->r*x[*ki].r-kr->i*x[*ki].i;
	      y->i += kr->r*x[*ki].i+kr->i*x[*ki].r;
	      kr++; ki++;
#endif
	   } // end if-else
       } // end for k
   } // end for i
   y-=A.nr;
   x++;


   /* y=y+(D+U^T)*x=(U+D+U^T)*x, where D is the diagonal part of A */
   //kn=A->nnzrow;
   // adjust y to match FORTRAN indexing
   y--;
   for (i=0; i<A.nr; i++,x++) {
       ki = A.ja+A.ia[i]-1;
       kr = A.a +A.ia[i]-1;
       for (k=0; k<A.ia[i+1]-A.ia[i]; k++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
           y[*ki++] += SKEW(*kr++ * (*x));
#else
           y[*ki].r += SKEW(kr->r*x->r-CONJG(kr->i)*x->i);
           y[*ki].i += SKEW(kr->r*x->i+CONJG(kr->i)*x->r);
	   kr++; ki++;
#endif
       } // end for k

   } // end for i
}
/*---------------end of symmatvec---------------------------------------
----------------------------------------------------------------------*/


