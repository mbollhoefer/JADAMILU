#include <string.h>
#include <ilupack.h>
#include <ilupackmacros.h>

#define MAX(A,B)   (((A)>=(B))?(A):(B))
#define MIN(A,B)   (((A)<=(B))?(A):(B))

void  DGLPRECDELETE(size_t    *IPdiag) {

  REALS *diag;

  memcpy(&diag, IPdiag, sizeof(size_t));

  free(diag);
}
