#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


void SYMILUPACKRESTOREDIAG(size_t *Fparam, integer *n, integer *ia, 
		       integer *ja, FLOAT *a)
{
  integer i,j,k;
  REALS   val;
  ILUPACKPARAM *IPparam;
  
  memcpy(&IPparam, Fparam, sizeof(size_t));

  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     a[j-1]=IPparam->diag[i];
#else
	     a[j-1].r=IPparam->diag[i];
	     a[j-1].i=0.0;
#endif
	  }
      }
  }
  free(IPparam->diag);
}
