#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


#ifdef _SINGLE_REAL_ 
void ilupackdummy10001() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10002() {
}

#elif defined _DOUBLE_REAL_
doubleprecision sdsymamgsavediag(size_t *Fparam, integer *n, 
				 integer *ia, integer *ja, 
				 doubleprecision *a)
{
  integer i,j,k,ki;
  doubleprecision *buff, *disc, val,ti,tmp;
  SILUPACKparam *IPparam;

  IPparam=(SILUPACKparam *)MALLOC((size_t)1*sizeof(SILUPACKparam),
				  "singlesymamgsavediag:param");
  memcpy(Fparam, &IPparam, sizeof(size_t));

  buff=(doubleprecision *)MALLOC(*n*sizeof(doubleprecision),
				 "singlesymamgsavediag:IPparam->diag");
  disc=(doubleprecision *)MALLOC(*n*sizeof(doubleprecision),
				 "singlesymamgsavediag:disc");
  IPparam->diag=(real *)buff;

  for (i=0; i<*n; i++)
      disc[i]=buff[i]=0.0;

  ki=1;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
	     buff[i]=a[j-1];
	     if (i==0) 
	        ti=a[j-1];
	     else {
	        if (a[j-1] < ti)  
		   ti=a[j-1];
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
  for (i=1; i<*n; i++) {
      tmp=buff[i]-disc[i]; 
      if (tmp<val) {
	 if (buff[i]-ti < disc[ki]) 
	    val=tmp;
      }
  }
  free(disc);

  return (val);
}



#else
doubleprecision czheramgsavediag(size_t *Fparam, integer *n, 
				 integer *ia, integer *ja, 
				 doublecomplex *a)
{
  integer i,j,k,ki;
  doubleprecision *buff, *disc,  val,ti,tmp;

  CILUPACKparam *IPparam;

  IPparam=(CILUPACKparam *)MALLOC((size_t)1*sizeof(CILUPACKparam),
				  "singlesymamgsavediag:param");
  memcpy(Fparam, &IPparam, sizeof(size_t));

  buff=(doubleprecision *)MALLOC(*n*sizeof(doubleprecision),
				 "singlesymamgsavediag:IPparam->diag");
  disc=(doubleprecision *)MALLOC(*n*sizeof(doubleprecision),
				 "singlesymamgsavediag:disc");
  IPparam->diag=(real *)buff;

  for (i=0; i<*n; i++)
      disc[i]=buff[i]=0.0;

  ki=1;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j-1]-1;
	  if (k==i) {
	     buff[i]=a[j-1].r;
	     if (i==0) 
	        ti=a[j-1].r;
	     else {
	        if (a[j-1].r < ti)  
		   ti=a[j-1].r;
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
  for (i=1; i<*n; i++) {
      tmp=buff[i]-disc[i]; 
      if (tmp<val) {
	  if (buff[i]-ti < disc[ki]) 
	      val=tmp;
      }
  }
  free(disc);

  return (val);
}

#endif

