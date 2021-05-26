#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <ilupack.h>

#include <ilupackmacros.h>

void *MALLOC(size_t nbytes, char *msg )
{ /* allocates space -- or exits in case of
     failure */
  void *ptr;
  if (nbytes == 0)
    return NULL;

  ptr = (void *) malloc(nbytes);
  if (ptr == NULL) {
    fprintf(stderr,"Mem. alloc. ERROR in %s. Requested bytes: %ld bytes\n",
            msg, (long)nbytes );
    fflush(stderr);
     exit( -1 );
  }
  return ptr;
}
