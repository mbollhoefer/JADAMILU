#include <string.h>
#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO

#define STDOUT stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))



#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKINFO     dsymamginfo


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKINFO     ssymamginfo


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINFO     csymamginfo
#else
#define MYSYMILUPACKINFO     cheramginfo
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINFO     zsymamginfo
#else
#define MYSYMILUPACKINFO     zheramginfo
#endif
#endif


void MYSYMILUPACKINFO(size_t *Fparam, 
		    size_t *FPREC,
		    integer   *nlev,
		    integer   *n,
		    integer   *ia,
		    integer   *ja,
		    FLOAT     *a) {
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
  integer i,j,k,l,m, tmp,tmp1,tmp2, nnzU, nz;


  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));


  /* finally release memory of the preconditioner */
  A.nc=A.nr=*n;
  A.ia=ia;
  A.ja=ja;
  A.a =a;
  nz=A.ia[*n]-1;



  
  next=PRE;
  nnzU=0;
  tmp=0;
  tmp1=0;
  tmp2=0;

  for (i=1; i<=*nlev; i++) {
      // fill-in LU
      printf("level %3d, block size %7d\n",i,next->LU.nr); fflush(stdout);
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
	 printf("           2x2 pivots %5.1f%%\n",(100.0*k)/next->LU.nr); fflush(stdout);
      }
      if (i==*nlev) {
	 if (next->LU.ja==NULL) {
	    printf("switched to full matrix processing\n");fflush(STDOUT);
	    tmp=-1;
	    j=next->LU.nr;
	    nnzU+=(j*(j-1))/2;
	 }
      }
      printf("  local fill-in %7d(%5.1lfav)\n",
	     nnzU-l+2*next->LU.nr,(1.0*(nnzU-l+2*next->LU.nr))/next->LU.nr);

      if (i<*nlev) {
	 // fill-in F
	 nnzU+=next->F.ia[next->F.nr]-1;
	 printf("level %3d->%3d, block size (%7d,%7d)\n",i,i+1,next->LU.nr,next->F.nc);
	 printf("  local fill-in F %7d(%5.1lfav pr)\n",
		next->F.ia[next->F.nr]-1,(1.0*(next->F.ia[next->F.nr]-1))/next->LU.nr);
      }
      next=next->next;
  }
  printf("\ntotal fill-in sum%8d(%5.1lfav)\n",
	 nnzU+2**n,(1.0*(nnzU+2**n))/(*n));
  printf("fill-in factor:      %5.1lf\n",(1.0*nnzU+2**n)/nz);
  printf("total number of sparse  2x2 pivots %5.1f%%\n",(100.0*tmp1)/tmp2); 
  fflush(stdout);
  tmp1=tmp2=0;

  if (tmp) {
     // nnzU-j*(j+1)/2+n-j memory for sparse data structures
     //                    indices (weight 1/3) and values (weight 2/3)
     // j*(j+1)/2          memory for dense data, no indices (weight 2/3)
     printf("memory usage factor: %5.1lf\n",(1.0*(nnzU-(j*(j+1))/2+2**n-j))/nz
	    +(j*(j+1))/(3.0*nz));
  }
  else {
    printf("memory usage factor: %5.1lf\n",(1.0*(nnzU+2**n))/nz);
  }
  printf("total time: %8.1le [sec]\n\n",  (double)ILUPACK_secnds[7]); 

  printf("refined timings for   ILUPACK multilevel factorization\n"); 

  printf("initial preprocessing:         %8.1le [sec]\n",ILUPACK_secnds[0]); 
  printf("reorderings remaining levels:  %8.1le [sec]\n",ILUPACK_secnds[1]); 
  printf("SYMPILUC (sum over all levels):%8.1le [sec]\n",ILUPACK_secnds[2]); 
  printf("SYMILUC (if used):             %8.1le [sec]\n",ILUPACK_secnds[3]); 
  printf("SPTRF, LAPACK (if used):       %8.1le [sec]\n",ILUPACK_secnds[4]); 
  printf("remaining parts:               %8.1le [sec]\n\n",MAX(0.0,(double)ILUPACK_secnds[7]
							       -ILUPACK_secnds[0]
							       -ILUPACK_secnds[1]
							       -ILUPACK_secnds[2]
							       -ILUPACK_secnds[3]
							       -ILUPACK_secnds[4]));

  fflush(STDOUT);



} // MYSYMILUPACKINFO
