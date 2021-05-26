#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <ilupack.h>

#include <ilupackmacros.h>

void *REALLOC(void *ptr, size_t nbytes, char *msg )
{ /* allocates space -- or exits in case of
     failure */

  if (nbytes == 0)
    return NULL;

  if (ptr==NULL)
     ptr = (void *) malloc(nbytes);
  else
     ptr = (void *) realloc(ptr,nbytes);
  if (ptr == NULL) {
    fprintf(stderr,"Mem. re-alloc. ERROR in %s. Requested bytes: %ld bytes\n",
            msg, (long)nbytes );
    fflush(stderr);
     exit( -1 );
  }
  return ptr;
}
