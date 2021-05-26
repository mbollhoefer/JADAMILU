#include <stdlib.h>

#include <ilupack.h>
#include <ilupackmacros.h>

void ipsrandom(unsigned integer *seed)
{
  srand((int) *seed);
}
