#include <ilupack.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
//#define PRINT_INFO
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))

#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMAMGEXTRACT       DSSMAMGextract
#else
#define MYSYMAMGEXTRACT       SSSMAMGextract
#endif

#define SKEW(A)      (-(A))

#else

#ifdef _DOUBLE_REAL_
#define MYSYMAMGEXTRACT       DSYMAMGextract
#else
#define MYSYMAMGEXTRACT       SSYMAMGextract
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
#define MYSYMAMGEXTRACT       CSSMAMGextract
#else
#define MYSYMAMGEXTRACT       ZSSMAMGextract
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMAMGEXTRACT       CSYMAMGextract
#else
#define MYSYMAMGEXTRACT       ZSYMAMGextract
#endif

#endif
// end _SKEW_MATRIX_


#else
#define CONJG(A)     (-(A))

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMAMGEXTRACT       CSHRAMGextract
#else
#define MYSYMAMGEXTRACT       ZSHRAMGextract
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMAMGEXTRACT       CHERAMGextract
#else
#define MYSYMAMGEXTRACT       ZHERAMGextract
#endif

#endif
// end _SKEW_MATRIX_

#endif
// end _COMPLEX_SYMMETRIC_

#endif
// end _DOUBLE_REAL_








/* Given an nxn (skew-)SYMMETRIC matrix A, extract from
           /  B  F \
   Q^TAQ = |       |
           \ F^T C /
   the matrix E, where B is of size nBxnB
   Only half of the matrix A is stored
*/
void MYSYMAMGEXTRACT(CSRMAT *F, CSRMAT A, integer *q,integer *invq,  integer nB)
{
/*
   A        given matrix
   q,invq   permutation vectors associated with the permutation matrix Q
   nB       size of B
   F        on output, matrix F is extracted from Q^TAQ

   written by Matthias Bollhoefer, November 2003
*/

   integer i,ii,j,jj,cnt, cntj,n, *ia, *ja;
   FLOAT *a;

   n=A.nr;
   ia=A.ia;
   ja=A.ja;
   a=A.a;
   
   /* Block F */
   /* 1. pass, detect  length of any row and total number of nonzeros */
   F->nr=nB;
   F->nc=n-nB;
   F->ia=(integer *)MALLOC((size_t)(nB+1)*sizeof(integer),"AMGextract:F->ia");
   F->ia[0]=1;
   for (ii=0; ii<nB; ii++) {
       // extract row q[ii]
       i=q[ii]-1;
       cnt=0;
       for (jj=ia[i]-1; jj<ia[i+1]-1;jj++) {
	   j=ja[jj]-1;
	   if (invq[j]>nB) cnt++;
       } // end for jj
       F->ia[ii+1]=cnt;
   } // end for ii
   for (ii=nB; ii<n; ii++) {
       // extract row q[ii]
       i=q[ii]-1; 
       for (jj=ia[i]-1; jj<ia[i+1]-1;jj++) {
	   j=ja[jj]-1;
	   cnt=invq[j];
	   if (cnt<=nB) (F->ia[cnt])++;
       } // end for jj
   } // end for ii
   // switch from counter to pointer
   for (ii=0; ii<nB; ii++) 
       F->ia[ii+1]+=F->ia[ii];

   /* number of nonzeros */
   cnt=F->ia[nB]-1;
   F->ja=(integer *)  MALLOC((size_t)cnt*sizeof(integer),  "AMGextract:F->ja");
   F->a =(FLOAT *)MALLOC((size_t)cnt*sizeof(FLOAT),"AMGextract:F->a");
   /* 2. pass, extract values and indices */
   for (ii=0; ii<nB; ii++) {
       // extract row q[ii] 
       i=q[ii]-1;
       // get current pointer
       cnt=F->ia[ii];
       for (jj=ia[i]-1; jj<ia[i+1]-1;jj++) {
	   j=ja[jj]-1;
	   if (invq[j]>nB) {
	      F->ja[cnt-1]=invq[j]-nB;
	      F->a[cnt-1]=a[jj];
	      cnt++;
	   }
       } // end for jj
       // store new pointer
       F->ia[ii]=cnt;
   } // end for ii

   for (ii=nB; ii<n; ii++) {
       // extract row q[ii] 
       i=q[ii]-1;
       for (jj=ia[i]-1; jj<ia[i+1]-1;jj++) {
	   j=ja[jj]-1;
	   cnt=invq[j];
	   if (cnt<=nB) {
	      // get current pointer
 	      cntj=F->ia[cnt-1];
	      F->ja[cntj-1]=ii+1-nB;
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	      F->a[cntj-1]=SKEW(a[jj]);
#else
	      // use conjugate transpose
	      F->a[cntj-1].r=SKEW(      a[jj].r);
	      F->a[cntj-1].i=SKEW(CONJG(a[jj].i));
#endif
	      // store new pointer
 	      F->ia[cnt-1]=cntj+1;
	   }
       } // end for jj
   } // end for ii

   // shift pointers back
   for (ii=nB; ii>0; ii--) 
       F->ia[ii]=F->ia[ii-1];
   F->ia[0]=1;

} /* end symAMGextract */


