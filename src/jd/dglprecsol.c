#include <string.h>
#include <ilupack.h>
#include <ilupackmacros.h>

#define MAX(A,B)   (((A)>=(B))?(A):(B))
#define MIN(A,B)   (((A)<=(B))?(A):(B))

void  DGLPRECSOL(size_t    *IPdiag, 
		 integer   *n,
		 FLOAT     *target,
		 FLOAT     *source) {

  REALS *diag;
  integer i;

  memcpy(&diag, IPdiag, sizeof(size_t));

  for (i=0; i<*n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
      target[i]=diag[i]*source[i];
#else
      target[i].r=diag[i]*source[i].r;
      target[i].i=diag[i]*source[i].i;
#endif
  }
}
