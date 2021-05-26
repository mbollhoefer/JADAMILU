#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


REALS SYMILUPACKSAVEDIAGGEP(size_t *Fparam, integer *n, 
			    integer *ia, integer *ja, FLOAT *a,
			    integer *ib, integer *jb, FLOAT *b)
{
  integer i,j,k,ki;
  REALS   val,ti,tmp;
  REALS   *disc, *diag;
  ILUPACKPARAM *IPparam;
  
  diag=(REALS *)MALLOC(*n*sizeof(REALS),"symamgsavediaggep:diag");
  disc=(REALS *)MALLOC(*n*sizeof(REALS),"symamgsavediaggep:disc");

  for (i=0; i<*n; i++)
      disc[i]=diag[i]=0.0;

  // matrix B
  // Compute diagonal lumping
  // for any b_{ij}!=0 replace B by
  //               / |b_{ij}|*delta  -b_{ij}        \
  // B +[e_i e_j]  |                                |  [e_i e_j]^T
  //               \ -b_{ij}         |b_{ij}|/delta /
  // where delta =sqrt(b_{ii}/b_{jj})
  // This is essentially borrowed from Ajiz & Jennings dropping
  // The choice of delta ensures not only that the small matrix is 
  // positive semidefinite and rank-1, but also that
  // b_{ii}+|b_{ij}|*delta = b_{ii} * (1+|b_{ij}|/sqrt(b_{ii}*b_{jj}))
  // b_{jj}+|b_{ij}|/delta = b_{jj} * (1+|b_{ij}|/sqrt(b_{ii}*b_{jj})),
  // i.e., the relative perturbation for both diagonal entries is
  // the same.

  // note that replacing B by D s.t. D>=B implies that
  // the smallest eigenvalue min_{x!=0} x^TAx/(x^TBx) only gets smaller
  // when using min_{x!=0} x^TAx/(x^TDx) instead, which is related to
  // the eigenvalue problem D^{-1/2}AD^{-1/2}x=lambda x

  // pass 1. Compute B's diagonal entries
  for (i=0; i<*n; i++) {
      for (j=ib[i]; j<ib[i+1]; j++) {
	  k=jb[j-1]-1;
	  if (k==i) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     diag[i]=b[j-1];
#else
	     diag[i]=b[j-1].r;
#endif
	  }
      }
  }
  // pass 2. diagonal lumping for B
  for (i=0; i<*n; i++) {
      for (j=ib[i]; j<ib[i+1]; j++) {
	  k=jb[j-1]-1;
	  if (k!=i) {
	     val=sqrt(diag[i]/diag[k]);
	     diag[i]+=FABS(b[j-1])*val;
	     diag[k]+=FABS(b[j-1])/val;
	  }
      }
  }
  // pass 3. invert diagonal part and take sqrt
  for (i=0; i<*n; i++)
      diag[i]=1/sqrt(diag[i]);



  // matrix DAD, compute smallest diagonal entries and absolute
  // off-diagonal sum
  ki=0;
  ti=0;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     val=diag[i]*a[j-1]*diag[k];
#else
	     val=diag[i]*a[j-1].r*diag[k];
#endif
	     if (i==0)
	        ti=val;
	     else {
	        if (val<ti) {
		   ti=val;
		   ki=i;
		}
	     }
	  }
	  else {
	     val=diag[i]*FABS(a[j-1])*diag[k];
	     disc[i]+=val;
	     // remember that only the upper triangular part is stored
	     disc[k]+=val;
	  }
      }
  }
  // matrix DAD, compute diagonal entries
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     diag[i]=diag[i]*a[j-1]*diag[i];
#else
	     diag[i]=diag[i]*a[j-1].r*diag[i];
#endif
	  }
      }
  }


  val=ti-disc[ki];
  for (i=0; i<*n; i++) {
      tmp=diag[i]-disc[i]; 
      if (tmp<val) {
	 if (diag[i]-ti < disc[ki]) 
	    val=tmp;
      }
  }
  free(disc);
  free(diag);

  return (val);
}
