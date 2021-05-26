#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


#ifdef _SINGLE_REAL_ 
void ilupackdummy10007() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10008() {
}

#elif defined _DOUBLE_REAL_
void sdsymamgrestorediag(size_t *Fparam, integer *n, integer *ia, 
			 integer *ja, doubleprecision *a) {
  integer i,j,k;
  doubleprecision *buff;
  SILUPACKparam   *IPparam;
  
  memcpy(&IPparam, Fparam, sizeof(size_t));
  buff=(doubleprecision *)IPparam->diag;

  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
	     a[j-1]=buff[i];
	  }
      }
  }
  free(IPparam->diag);
}
#else
void czheramgrestorediag(size_t *Fparam, integer *n, integer *ia, 
			 integer *ja, doublecomplex *a)
{
  integer i,j,k;
  doubleprecision *buff;
  CILUPACKparam   *IPparam;
  
  memcpy(&IPparam, Fparam, sizeof(size_t));
  buff=(doubleprecision *)IPparam->diag;

  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
	     a[j-1].r=buff[i];
	     a[j-1].i=0.0;
	  }
      }
  }
  free(IPparam->diag);
}

#endif
