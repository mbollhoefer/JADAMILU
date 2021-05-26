#include <ilupack.h>

#include <ilupackmacros.h>


void SWAPM(REALS v[], integer i, integer j)
{
   REALS temp;

   temp = v[i];
   v[i] = v[j];
   v[j] = temp;
}
