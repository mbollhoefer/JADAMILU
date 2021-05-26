#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include <amd.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))




/*
 scaling and permutation driver, here for AMD, approximate minimum degree
 from UMFPACK (Patrick Amestoy, Tim Davis and Iain Duff)

 Given an n x n matrix A in compressed sparse row format this routine computes
 row and column scalings as well as row and column permutations such that

 A -> Dr * A * Dc,
 
 where Dr and Dc are diagonal matrices stored in the vectors proscal and 
 pcoscal. The scaling is explicitly applied to A.

 The permutation p,invq refer to permutation matrices P and Q^{-1} such  that

  P^T*(Dr * A * Dc)*Q 

 is hopefully better suited for the ILU.

 The routine returns a leading block size nB<=n
                        /B F\ 
  P^T*(Dr * A * Dc)*Q = |   | 
                        \E C/
 In this case the permutation recommends that the ILU is only applied to the
 leading block B of size nB x nB


 In particular AMD uses a symmetric reordering that reorders the system
 such that nodes from the elimination graph with lowest apprixmate degree
 precede those with higher degree which may be advantageous for direct solvers.
 nB=n
 */
integer PERMAMD(CSRMAT A, FLOAT *prowscale, FLOAT *pcolscale,
	    integer *p, integer *invq, integer *nB, ILUPACKPARAM *param)

{
/*
    A          n x n matrix in compressed sparse row format
               A is altered by the routine since the scalings are directly
               applied to A. Hopefully the scaling are only powers of 2,
               which is the case for all scaling routines from ILUPACK.

    proscal,   vectors of length n that store the row and column scalings Dr
    pcoscal    and Dc
               A -> Dr*A*Dc

    p,invq     permutation vectors p and q^{-1} of length n which refer to
               row / column permutation matrices P and Q^{-1}.
               P^T*(Dr * A * Dc)*Q hopefully better suited for ILU

    nB         leading blocksize nB
                                     /B F\ 
               P^T*(Dr * A * Dc)*Q = |   |,  B matrix of size nB x nB
                                     \E C/
               nB is the recommended block size for the application of an ILU

    param      ILUPACK parameters
               param->ipar[7] gives information about the requested scaling

               param->ipar[7] &  512       indicates initial preprocessing,
                                           initial permutation routine in
                                           the main ILU driver called AMGFACTOR

               param->ipar[7] & 1024       indicates regular reordering,
                                           the second permutation routine in
                                           the main ILU driver called AMGFACTOR

               param->ipar[7] &  512+1024  indicates final pivoting,
                                           the third permutation routine in
                                           the main ILU driver called AMGFACTOR


               depending on the circumstances (initial preprocessing, regular
               reordering, final pivoting)

               param->ipar[7] & ( 1+  2+  4)   initial preprocessing

               param->ipar[7] & ( 8+ 16+ 32)   regular reordering

               param->ipar[7] & (64+128+256)   final pivoting

               give information on whether row scaling (lowest bit 1,8,64), 
               column scaling (medium bit 2,16,128) should be applied.
               The highest bit (4,32,256) defines the order in which the 
               scalings should be perfomed. If the highest bit is cleared, then
               we start with row scaling.

    
               param->ipar[8] defines the norm that should be used

               param->ipar[8] & (   1+   2+   4+   8+   16)  initial preprocessing

               param->ipar[8] & (  32+  64+ 128+ 256+  512)  regular reordering

               param->ipar[8] & (1024+2048+4096+8192+16384)  final pivoting

               The five bits (values [0,...,31] up to shifts) are used for
               0,1,2          infinity norm, 1-norm, 2-norm
               3              spd scaling using the square root of the diagonal
                              entries

               The scaling routines that are defined with ILUPACK only use
               the nearest powers of 2 for scaling


   interface driver written by Matthias Bollhoefer, July/November 2003, 2005
 */
 
   integer i,j,k,l,scale,nrm,ierr=0,*ia,*ja,nz, iflag;
   size_t mem;
   double Control[AMD_CONTROL], Info[AMD_INFO];

   // start by rescaling the system, param defines the norms
#include "scaleprefix.c"


   ILUPACK_mem[12]=0;
   ILUPACK_mem[13]=0;

   // setup matrix for AMD
   param->nibuff=MAX(param->nibuff,(size_t)A.nc+1);
   param->ibuff=(integer *) REALLOC(param->ibuff,param->nibuff*sizeof(integer),
				"perm_amd:ibuff");
   ia=param->ibuff;
   nz=A.ia[A.nc]-1;
   if (nz<=param->niaux) {
      ja=param->iaux;
      ILUPACK_mem[12]=nz;
   }
   else {
      ja=(integer *) MALLOC((size_t)nz*sizeof(integer),"perm_amd:ja");
      ILUPACK_mem[10]=MAX(ILUPACK_mem[10],nz);
   }

   // convert from FORTRAN style to C-style
   for (i=0; i<=A.nc; i++)
       A.ia[i]--;
   for (i=0; i<nz; i++)
       A.ja[i]--;
#ifdef _LONG_INTEGER_
   amd_l_preprocess(A.nc,A.ia,A.ja,ia,ja);
#else
   amd_preprocess(A.nc,A.ia,A.ja,ia,ja);
#endif
   // conversion back
   for (i=0; i<=A.nc; i++)
       A.ia[i]++;
   for (i=0; i<nz; i++)
       A.ja[i]++;

   // reorder the system using approximate minimum degree
#ifdef _LONG_INTEGER_
   amd_l_defaults(Control);
   ierr=amd_l_order(A.nc,ia,ja,p,Control,Info);
#else
   amd_defaults(Control);
   ierr=amd_order(A.nc,ia,ja,p,Control,Info);
#endif
   if (nz>param->niaux)
      free(ja);

   for (i=0; i<A.nc; i++) {
       invq[p[i]]=i+1;
       p[i]++;
   } // end for

   /*
   for (i=0; i<A.nc; i++)
     printf("%8d",p[i]);
   printf("\n");
   for (i=0; i<A.nc; i++)
     printf("%8d",invq[i]);
   printf("\n");
   fflush(stdout);
   */


   *nB=A.nc;


   return (ierr);
}
