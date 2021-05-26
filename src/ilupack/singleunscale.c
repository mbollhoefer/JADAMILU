#include <string.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _SINGLE_REAL_ 
void ilupackdummy10009() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10010() {
}

#elif defined _DOUBLE_REAL_

void sdamgundoscaling(size_t *Fparam, 
		      size_t *FPREC,
		      integer   *nlev,
		      integer   *n,
		      integer   *ia,
		      integer   *ja,
		      doubleprecision *a) {
   /*
     ILUPACK FORTRAN interface for initialization

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

     n           size of the system

     ia          pointer         \
     ja          column indices  | of an NxN matrix in sparse row format
     a           numerical values/

   */

  SILUPACKparam *param;
  SAMGlevelmat  *PRE;
  real  *r, *c;
  integer      i,j,k;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));

    
  /* finally release memory of the preconditioner */
  r=PRE->rowscal;
  c=PRE->colscal;

  // adjust arrays
  ja--;
  a--;
  c--;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j];
	  a[j]/=r[i]*c[k];
      }
  }

} // unscale

#else
void czamgundoscaling(size_t *Fparam, 
		      size_t *FPREC,
		      integer   *nlev,
		      integer   *n,
		      integer   *ia,
		      integer   *ja,
		      doublecomplex *a) {
   /*
     ILUPACK FORTRAN interface for initialization

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

     n           size of the system

     ia          pointer         \
     ja          column indices  | of an NxN matrix in sparse row format
     a           numerical values/

   */

  CILUPACKparam *param;
  CAMGlevelmat  *PRE;
  complex       *r, *c;
  integer       i,j,k;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));

    
  /* finally release memory of the preconditioner */
  r=PRE->rowscal;
  c=PRE->colscal;

  // adjust arrays
  ja--;
  a--;
  c--;
  for (i=0; i<*n; i++) {
      for (j=ia[i]; j<ia[i+1]; j++) {
	  k=ja[j];
	  a[j].r/=r[i].r*c[k].r;
	  a[j].i/=r[i].r*c[k].r;
      }
  }

} // unscale
#endif
