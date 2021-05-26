#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <blas.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define STDERR   stderr
#define STDOUT   stdout
#define IABS(A)         (((A)>=0)?(A):(-(A)))
#define MAX(A,B)        (((A)>=(B))?(A):(B))
#define MIN(A,B)        (((A)<=(B))?(A):(B))






/* compute pattern of B=|A|+|A|^T */
void SETUPGRAPH_EPSILON(CSRMAT A, CSRMAT *B, REALS epsilon, 
			REALS *dbuff, integer *ibuff, integer *iaux, size_t niaux)
{
  /*
    A       input matrix in compressed sparse row format
    B       output matrix in compressed sparse row format
            such that the nonzero pattern of B corresponds to |A|+|A|^T
            The numerical values are NOT stored
    epsilon threshold. Entries a_ij such that 
            |a_ij| < epsilon * min(max_k|a_kj|,max_l|a_il|)
            are being ignored when setting up the graph
    dbuff   real buffer of size max(A.nr,A.nc)
    ibuff   integer buffer of size max(A.nr,A.nc)

    
    code written by Matthias Bollhoefer, 2000, 2003, 2005
  */
   integer i, j, k, l, n, m=1, nnz, shift, oldshift, *ind;
   FLOAT swap;
   REALS absval;

   n=MAX(A.nc,A.nr);
   B->nr=A.nc;
   B->nc=A.nr;

   // compute the maximum entry in each row/column
   for (i=0; i<n; i++) 
       dbuff[i]=0.0;
   for (i=0; i<A.nr; i++) {
       j=A.ia[i]-1;
       l=A.ia[i+1]-1-j;
       k=I_AMAX(&l,A.a+j,&m);
       absval=FABS(A.a[j+k-1]);
       dbuff[i]=MAX(dbuff[i],absval);
       l+=j;
       for (; j<l; j++) {
	   // column index k
	   k=A.ja[j]-1;
	   absval=FABS(A.a[j]);
	   dbuff[k]=MAX(dbuff[k],absval);
       } // end for j
   } // end for i


   // now for every row, shuffle those entries to the back which are below
   // the threshold
   nnz=0;
   for (i=0; i<A.nr; i++) {
       j=A.ia[i]-1;
       l=A.ia[i+1]-1;
       // initially assume that all entries are above the threshold
       for (; j<l; ) {
	   // column index k
	   k=A.ja[j]-1;
	   // shuffle entry to the end
	   if (k!=i && FABS(A.a[j])<epsilon*MIN(dbuff[i],dbuff[k])) {
	      m=A.ja[l-1];
	      A.ja[l-1]=A.ja[j];
	      A.ja[j]=m;
	      swap=A.a[l-1];
	      A.a[l-1]=A.a[j];
	      A.a[j]=swap;
	      l--;
	   } // end if
	   else
	     j++;
       } // end for j
       ibuff[i]=l+1;
       nnz+=ibuff[i]-A.ia[i];
   } // end for i

 
   B->ia=ibuff+n;
   // only allocate memory if the buffer is not big enough
   if (2*(size_t)nnz<=niaux) {
     B->ja=iaux;
   }
   else {
     B->ja=(integer *)MALLOC(2*(size_t)nnz*sizeof(integer),"setupgraph_epsilon:B->ja");
   }
   // counter for the number of nonzeros for every row of B
   ind=B->ia;
   for (i=0; i<=A.nr; i++)
       ind[i]=0;

   // run through A in order to find the nnz for each column of A
   // do not adjust ind to fit with FORTRAN indexing!
   for (i=0; i<A.nr; i++) 
       for (j=A.ia[i]-1; j<ibuff[i]-1; j++) 
	   ind[A.ja[j]]++;
   // change ind such that ind[i] holds the start of column i
   // ind[0] has not been used due to different indexing in FORTRAN
   ind[0]=1;
   for (i=0; i<n; i++) 
       ind[i+1]+=ind[i];

   // increment by the number of nonzeros in every row of A
   k=0;
   for (i=0; i<n; i++) {
       k+=ibuff[i]-A.ia[i];
       ind[i+1]+=k;
   }


   /*--------------------  now do the actual copying  */
   for (i=0; i<n; i++) {
       // copy indices
       for (j=A.ia[i]; j<ibuff[i]; j++) {
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

   
   // Now every row may have duplicate entries
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


} /* end setupgraph_epsilon */
