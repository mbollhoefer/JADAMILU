#include <string.h>
#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO

#define STDOUT stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))



#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKNNZ     dsymamgnnz


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKNNZ     ssymamgnnz


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKNNZ     csymamgnnz
#else
#define MYSYMILUPACKNNZ     cheramgnnz
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKNNZ     zsymamgnnz
#else
#define MYSYMILUPACKNNZ     zheramgnnz
#endif
#endif


size_t MYSYMILUPACKNNZ(size_t *Fparam, 
		    size_t *FPREC,
		    integer   *nlev) {
   /*
     ILUPACK FORTRAN interface for displaying the multilevel structure

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     n           size of the system

     ia          pointer         \
     ja          column indices  | of an NxN matrix in sparse row format
     a           numerical values/


     nlev        number of AMG levels

   */

  CSRMAT       A;
  ILUPACKPARAM *param;
  AMGLEVELMAT *PRE, *next;
  integer i,j,k,l,m, tmp,tmp1,tmp2, nz;
  size_t nnzU;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));





  
  next=PRE;
  nnzU=0;
  tmp=0;
  tmp1=0;
  tmp2=0;

  for (i=1; i<=*nlev; i++) {
      // fill-in LU
      l=nnzU;
      if (i<*nlev || next->LU.ja!=NULL) {
	 k=0;
	 for (m=0; m<next->LU.nr; ) {
	     // 2x2 pivot
	     if (next->LU.ja[next->LU.nr+1+m]>0) {
	        k+=2;
	        nnzU+=2*(next->LU.ja[m+1]-next->LU.ja[m]);
		m+=2;
	     }
	     else {
	        nnzU+=next->LU.ja[m+1]-next->LU.ja[m];
		m++;
	     }
	 } // end for m
	 tmp1+=k;
	 tmp2+=next->LU.nr;
      }
      if (i==*nlev) {
	 if (next->LU.ja==NULL) {
	    tmp=-1;
	    j=next->LU.nr;
	    nnzU+=(j*(j-1))/2;
	 }
      }

      if (i<*nlev) {
	 // fill-in F
	 nnzU+=next->F.ia[next->F.nr]-1;
      }
      next=next->next;
  }
  tmp1=tmp2=0;

  if (tmp) {
     // nnzU-j*(j+1)/2+n-j memory for sparse data structures
     //                    indices (weight 1/3) and values (weight 2/3)
     // j*(j+1)/2          memory for dense data, no indices (weight 2/3)
     nnzU=nnzU-(j*(j+1))/2+2*PRE->n-j+(j*(j+1))/3.0;
  }
  else {
     nnzU=nnzU+2*PRE->n;
  }


  return (nnzU);

} // MYSYMILUPACKNNZ
