#include <ilupack.h>


void swapj(integer v[], integer i, integer j)
{
   integer temp;

   temp = v[i];
   v[i] = v[j];
   v[j] = temp;
}
