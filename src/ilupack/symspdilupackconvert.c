#include <string.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO



integer SYMSPDILUPACKCONVERT(size_t *Fparam, 
			     size_t *FPREC,
			     integer   *nlev) {
   /*
     ILUPACK FORTRAN interface for converting an symmetric preconditioner to an SPD preconditioner

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

   */

  CSRMAT       A;
  integer      flags,i;
  REALS        droptols[2];
  integer      myelbow,mymaxit,mynrestart,ierr,
               (*perm0)(),(*perm)(),(*permf)();
  REALS        mycondest,myrestol;
  ILUPACKPARAM *param;
  AMGLEVELMAT *PRE;
		       

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));


  return (SYMSPDCONVERT(PRE,*nlev));

} // SYMSPDILUPACKCONVERT
