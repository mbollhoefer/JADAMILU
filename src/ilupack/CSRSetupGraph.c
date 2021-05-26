#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define STDERR   stderr
#define STDOUT   stdout
#define IABS(A)         (((A)>=0)?(A):(-(A)))
#define MAX(A,B)        (((A)>=(B))?(A):(B))






/* compute pattern of B=|A|+|A|^T */
void SETUPGRAPH(CSRMAT A, CSRMAT *B, integer *ibuff, integer *iaux, size_t niaux)
{
  /*
    A       input matrix in compressed sparse row format
    B       output matrix in compressed sparse row format
            such that the nonzero pattern of B corresponds to |A|+|A|^T
            The numerical values are NOT stored

    ibuff   integer buffer of size max(A.nr,A.nc)+1

    
    code written by Matthias Bollhoefer, 2000, 2003, 2005
  */
   integer i, j, k, n, nnz, shift, oldshift, *ind;


   n=MAX(A.nc,A.nr);
   B->nr=A.nc;
   B->nc=A.nr;
   
   nnz=A.ia[A.nr]-1;
   B->ia=ibuff;
   // only allocate memory if the buffer is not big enough
   if (2*(size_t)nnz<=niaux)
      B->ja=iaux;
   else
      B->ja=(integer *)MALLOC(2*(size_t)nnz*sizeof(integer),"CSRSetupGraph:B->ja");
   
   // counter for the number of nonzeros for every row of B
   ind=B->ia;
   for (i=0; i<=A.nr; i++)
       ind[i]=0;

   // run through A in order to find the nnz for each column of A
   // do not adjust ind to fit with FORTRAN indexing!
   for (j=0; j<nnz; j++) 
       ind[A.ja[j]]++;
   // change ind such that ind[i] holds the start of column i
   // ind[0] has not been used due to different indexing in FORTRAN
   ind[0]=1;
   for (i=0; i<n; i++) 
       ind[i+1]+=ind[i];

   // increment by the number of nonzeros in every row of A
   for (i=0; i<=n; i++) 
       ind[i]+=A.ia[i]-1;

   /*--------------------  now do the actual copying  */
   for (i=0; i<n; i++) {
       // copy indices
       for (j=A.ia[i]; j<A.ia[i+1]; j++) {
	   // current column index of row i in FORTRAN style
	   k=A.ja[j-1]-1;
	   B->ja[ind[i]-1]=k+1;
	   // advance pointer
	   ind[i]++;
	   B->ja[ind[k]-1]=i+1;
	   // advance pointer
	   ind[k]++;
       } // end for j
   } // end for i
   
   
   // shift ind back
   for (i=n; i>0; i--) 
       ind[i]=ind[i-1];
   ind[0]=1;

   // Now every row my have duplicate entries
   // abuse ind also as sign pattern, the entries of ind are at least 1
   shift=0;
   for (i=0; i<n; i++) {
       oldshift=shift;
       for (j=IABS(ind[i]); j<IABS(ind[i+1]); j++) {
	   k=B->ja[j-1]-1;
	   B->ja[j-1-shift]=B->ja[j-1];
	   // this entry already exists in the current row
	   if (ind[k]<0)
	      shift++;
	   else 
	      ind[k]=-ind[k];
       } // end for j
       for (j=IABS(ind[i])-oldshift; j<IABS(ind[i+1])-shift; j++) {
	   k=B->ja[j-1]-1;
	   ind[k]=IABS(ind[k]);
       } // end for j
       ind[i]-=oldshift;
   } // end for i
   ind[n]-=shift;


   // indicate that we needed to allocate memory
   if (2*(size_t)nnz>niaux)
      B->ia[B->nc]=-B->ia[B->nc];
} /* end CSRSetupGraph */
