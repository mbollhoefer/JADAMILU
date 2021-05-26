#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


//#define PRINT_INFO

#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _DOUBLE_REAL_
#define MYSYMAMGDELETE       DSYMAMGdelete

#else
#define MYSYMAMGDELETE       SSYMAMGdelete
#endif



#else



#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMAMGDELETE       CSYMAMGdelete
#else
#define MYSYMAMGDELETE       ZSYMAMGdelete
#endif

#else

#ifdef _SINGLE_COMPLEX_
#define CONJG(A)     (conjg(A))
#define MYSYMAMGDELETE       CHERAMGdelete
#else
#define CONJG(A)     (dconjg(A))
#define MYSYMAMGDELETE       ZHERAMGdelete
#endif

#endif



#endif



#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))

// #define PRINT_INLINE





/*
  delete components of an existing multilevel ILU
*/
void MYSYMAMGDELETE(CSRMAT A, AMGLEVELMAT PRE, integer nlev, ILUPACKPARAM *IPparam)
{
/*
  A        input matrix, matrix is altered by the function
           diagonal scaling is directly applied to A
           only the diagonal part of A and its upper triangular part
           are stored
  PRE      Linked list of partial preconditioners
  nlev     number of levels(blocks)
  IPparam  ILUPACK parameter list

  Code written by Matthias Bollhoefer February 2005
*/


  integer lev, param=IPparam->ipar[6];
  AMGLEVELMAT *current, *prev;


  current=&PRE;
  lev=0;
  while (lev<nlev) {
	lev++;
	if (lev<nlev)
	   current=current->next;
  } // end while
  // now "current" points to the last level

  while (lev) {
        // printf("lev=%d\n",lev); fflush(stdout);
	/* give up row and column scaling */
	free(current->colscal);
	free(current->p);
	free(current->invq);
	
	// skip F (and E=F^T)
	if (lev<nlev) {
	  free(current->F.ia);
	  free(current->F.ja);
	  free(current->F.a);
	}
	if (current->absdiag!=NULL && current->LU.ja[0]<0)
	   free(current->absdiag);

	prev=current;
	current=current->prev;
	if (lev>1) 
	   free(prev);

	lev--;
  } /* end while */

  if (!(param&RE_FACTOR)) {
     // printf("elbow space factor discarded\n");fflush(stdout);
     free(IPparam->jlu);
     free(IPparam->alu);
     IPparam->nju=0;
     IPparam->njlu=0;
     IPparam->nalu=0;

     free(IPparam->ibuff);
     free(IPparam->dbuff);
     IPparam->nibuff=0;
     IPparam->ndbuff=0;
     
  }
  else {
     // printf("elbow space factor kept\n");fflush(stdout);
  }
  // printf("nalu=%ld\n",IPparam->nalu);fflush(stdout);

} /* end symAMGdelete */


