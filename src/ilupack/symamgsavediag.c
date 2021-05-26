#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


REALS SYMILUPACKSAVEDIAG(size_t *Fparam, integer *n, integer *ia, 
			 integer *ja, FLOAT *a)
{
  integer i,j,k,ki;
  REALS   val,ti,tmp;
  REALS   *disc;
  ILUPACKPARAM *IPparam;
  
  IPparam=(ILUPACKPARAM *)MALLOC((size_t)1*sizeof(ILUPACKPARAM),"ilupackfactor:param");
  memcpy(Fparam, &IPparam, sizeof(size_t));

  IPparam->diag=(REALS *)MALLOC(*n*sizeof(REALS),"symamgsavediag:IPparam->diag");
  disc=(REALS *)MALLOC(*n*sizeof(REALS),"symamgsavediag:disc");

  for (i=0; i<*n; i++)
      disc[i]=IPparam->diag[i]=0.0;

  ki=0;
  ti=0;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     IPparam->diag[i]=a[j-1];
#else
	     IPparam->diag[i]=a[j-1].r;
#endif
	     if (i==0) 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		 ti=a[j-1];
#else
		 ti=a[j-1].r;
#endif
	     else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		 if (a[j-1] < ti)  
		     ti=a[j-1];
#else
		 if (a[j-1].r < ti)  
		     ti=a[j-1].r;
#endif
		 ki=i;
	     }
	  }
	  else {
	      val=FABS(a[j-1]);
	      disc[i]+=val;
	      // remember that only the upper triangular part is stored
	      disc[k]+=val;
	  }
      }
  }


  val=ti-disc[ki];
  for (i=0; i<*n; i++) {
      tmp=IPparam->diag[i]-disc[i]; 
      if (tmp<val) {
	  if (IPparam->diag[i]-ti < disc[ki]) 
	      val=tmp;
      }
  }
  free(disc);

  return (val);
}
