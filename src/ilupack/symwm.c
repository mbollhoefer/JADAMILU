#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <blas.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))
#define STDERR          stderr
#define STDOUT          stdout


// #define PRINT_INFO

#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMMWM       DSSMsmwm
#else
#define MYSYMMWM       SSSMsmwm
#endif

#define SKEW(A)      (-(A))

#else

#ifdef _DOUBLE_REAL_
#define MYSYMMWM       DSYMsmwm
#else
#define MYSYMMWM       SSYMsmwm
#endif

#define SKEW(A)      (A)
#endif
// end _SKEW_MATRIX_


#else

#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM       CSSMsmwm
#else
#define MYSYMMWM       ZSSMsmwm
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM       CSYMsmwm
#else
#define MYSYMMWM       ZSYMsmwm
#endif

#endif
// end _SKEW_MATRIX_


#else
#define CONJG(A)     (-(A))

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM       CSHRsmwm
#else
#define MYSYMMWM       ZSHRsmwm
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM       CHERsmwm
#else
#define MYSYMMWM       ZHERsmwm
#endif

#endif
// end _SKEW_MATRIX_

#endif
// end _COMPLEX_SYMMETRIC_

#endif
// end _DOUBLE_REAL_



/* compute symmetric weighted matching, i.e. break cycles into
   1x1 and 2x2 cycles

   A     is a (skew-)symmetric/Hermitian matrix
   p     is a vector of length A.nc carrying the old and the new
         permutation
   ibuff is a buffer of length A.nc
   dbuff is a buffer of length 2*A.nc
*/
integer MYSYMMWM(CSRMAT A, integer *p, integer *ibuff, FLOAT *dbuff)
{
  integer i,j,k,l=1,n=A.nc, m, flag, next;
  REALS weight, weighte, weighto, val, *rbuff=(REALS *)(dbuff+A.nc);
  FLOAT aij;
  
  for (i=0; i<n; i++) {
      rbuff[i]=0.0;
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
      dbuff[i]=0.0;
#else
      dbuff[i].r=0.0;
      dbuff[i].i=0.0;
#endif
      ibuff[i]=0;
  }

  // compute 1-norm and extract diagonal entries
  for (i=0; i<n; i++) {
      j=A.ia[i]-1;
      k=A.ia[i+1]-1-j;
      l=1;
      rbuff[i]+=ASUM(&k,A.a+j,&l);
      k+=j;
      for (; j<k; j++) {
	  l=A.ja[j]-1;
	  if (l!=i)
	     rbuff[l]+=FABS(A.a[j]);
	  else {
	     rbuff[l]-=FABS(A.a[j]);
	     dbuff[i]=A.a[j];
	  }
      } // end for j
  } // end for i

  
  // break cycles into 1x1 and 2x2 cycles
  // first element of a cycle
  l=0;
  while (l<n) {

        flag=-1;
	// move to the next undiscovered cycle
	while (flag) 
	      if (l>=n || ibuff[l]==0)
		 flag=0;
	      else
		 l++;

	// mark entries of cycle k
	if (l<n) {


#ifdef PRINT_INFO
	   printf("%8d",l+1);
#endif


	   ibuff[l]=-1;
	   // successor in this cycle
	   i=p[l]-1;
	   // length of the cycle
	   j=1;
	   while (i!=l) {


#ifdef PRINT_INFO
	         printf("%8d",i+1);
#endif


	         ibuff[i]=-1;
		 i=p[i]-1;
		 j++;
	   } // end while


#ifdef PRINT_INFO
	   printf("\n");
	   fflush(stdout);
#endif

	   // even cycle of length greater than 2
	   if (j>2 && j%2==0) {
	      /* break even cycle into a product of subsequent 2-cycles.
		 There are two isomorphic possibilities to break up the cycle.
		 The first sequence starts with a p[l], the second one with p[p[l]-1].
		 We break the cycle such that the more diagonal dominant sequence of
		 2-cycles is taken
	      */
	      weighte=0.0;
	      weighto=0.0;
	      flag=0;
	      i=l;
	      next=p[i]-1;
	      k=0;
	      while (k<j) {
		    // find off-diagonal entries A(i,next), A(next,i)
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		    aij=0.0;
#else
		    aij.r=aij.i=0.0;
#endif
		    // scan row i for A(i,next)
		    for (m=A.ia[i]-1; m<A.ia[i+1]-1; m++)
		        if (A.ja[m]-1==next)
			   aij=A.a[m];
		    // scan row 'next' for A(next,i)
		    for (m=A.ia[next]-1; m<A.ia[next+1]-1; m++)
		        if (A.ja[m]-1==i)
			   aij=A.a[m];

		    // compute maximum off-diagonal 1-norm of the associated rows
		    // remove |aij| and |aji| which have the same absolute value
		    val=FABS(aij);
		    weight=MAX(rbuff[i],rbuff[next])-val;
		    if (weight<0.0)
		       weight=0.0;
		    /* To measure the block diagonal dominance we use
		       ||/aii aij\^{-1}||          ||/ ajj -aij\||      weight
		       |||       |     ||*weight = |||         |||* ---------------
 		       ||\aji ajj/     ||          ||\-aji  aii/|| |aii*ajj-aij*aji|  

                         MAX(|ajj|+|-aij|,|-aji|+|aii|)*weight
                       = -------------------------------------
                                   |aii*ajj-aij*aji|  

                         (MAX(|ajj|,|aii|)+|aij|)*weight
                       = -------------------------------
                                |aii*ajj-aij*aji|
		    */
		    weight*=MAX(FABS(dbuff[i]),FABS(dbuff[next]))+val;


		    // build determinant aii*ajj-aij*aji 
		    // To do this form product A(i,next)*A(next,i)
		    // for symmetry reasons this can be reduced
		    // to a product only based on 'aij'
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		    // aij*aji
		    aij*=SKEW(aij);
		    
		    // compute determinant
		    aij=dbuff[i]*dbuff[next]-aij;
#else
		    // aij*aji
		    val=aij.r;
		    aij.r=val  *SKEW(val)-aij.i*SKEW(CONJG(aij.i));
		    aij.i=aij.i*SKEW(val)+val  *SKEW(CONJG(aij.i));

		    // compute determinant
		    aij.r=dbuff[i].r*dbuff[next].r-dbuff[i].i*dbuff[next].i-aij.r;
		    aij.i=dbuff[i].r*dbuff[next].i+dbuff[i].i*dbuff[next].r-aij.i;
#endif

		    // catch the case that the 2x2 block is singular
		    weight/=FABS(aij)+1.0e-30;

		    if (flag) {
		       weighto+=weight;


#ifdef PRINT_INFO
		       printf("o%8.1le,%8.1le\n",weight,weighto);
#endif


		       flag=0;
		    }
		    else {
		       weighte+=weight;


#ifdef PRINT_INFO
		       printf("e%8.1le,%8.1le\n",weight,weighte);
#endif


		       flag=-1;
		    }
		    i=next;
		    next=p[i]-1;
		    k++;
	      } // end while (k<j)

	      // we prefer the sequence of 2-cycles that is more block diagonal dominant
	      if (weighte<=weighto)
		 i=l;
	      else
		 i=p[l]-1;
	      next=p[i]-1;
	      k=0;
	      while (k<j) {


#ifdef PRINT_INFO
	            printf("%8d%7d,",i+1,next+1);
#endif


		    // store i
		    m=i;
		    // advance i by two steps
		    i=p[next]-1;
		    // 2-cycle (i,next)
		    p[next]=m+1;
		    next=p[i]-1;
		    k+=2;
	      } // end while (k<j)


#ifdef PRINT_INFO
	      printf("\n\n");
	      fflush(stdout);
#endif


	   } // end if (j>2 && j%2==0)


	   // odd cycle of length greater than 2
	   else if (j>2) {
	      // find 1x1 cycle that is most diagonal dominant
	      i=l;
	      next=p[i]-1;
	      weight=1e30;
	      m=-1;
	      k=0;
	      while (k<j) {
	            val=FABS(dbuff[i]);
		    if (val!=0.0) {
		       val=rbuff[i]/val;


#ifdef PRINT_INFO
		       printf("%8.1le\n",val);
#endif


		       if (val<weight) {
			  weight=val;
			  m=i;
		       }
		    }
		    i=next;
		    next=p[i]-1;
		    k++;
	      } // end while

	      // did we find at least one nonzero diagonal entry?
	      if (m>=0) {
		 // we take the most diagonal dominant diagonal entry as 1x1 cycle
		 // break the cycle into a sequence of 2-cycles behind the singleton
		 i=p[m]-1;
		 // singleton
		 p[m]=m+1;


#ifdef PRINT_INFO
		 printf("%7d,",m+1);
#endif


		 next=p[i]-1;
		 k=1;
		 while (k<j) {


#ifdef PRINT_INFO
		       printf("%8d%7d,",i+1,next+1);
#endif


		       // store i
		       m=i;
		       // advance i by two steps
		       i=p[next]-1;
		       // 2-cycle p[i] and p[next]
		       p[next]=m+1;
		       next=p[i]-1;
		       k+=2;
		 } // end while


#ifdef PRINT_INFO
		 printf("\n\n");
		 fflush(stdout);
#endif


		 
	      } // end if (m>=0)
	      else { // all diagonal entries are zero 
		 /* only check two subsequent cycle sequences similar to the even case
		    break cycle such that the more diagonal dominant sequence is taken.
		    We ignore the final diagonal entry (which is zero in any case)
		 */
		 weighte=0.0;
		 weighto=0.0;
		 flag=0;
		 i=l;
		 next=p[i]-1;
		 k=1;
		 while (k<j) {
		       // find off-diagonal entries A(i,next), A(next,i)
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		       aij=0.0;
#else
		       aij.r=aij.i=0.0;
#endif
		       // scan row i for A(i,next)
		       for (m=A.ia[i]-1; m<A.ia[i+1]-1; m++)
			   if (A.ja[m]-1==next)
			      aij=A.a[m];
		       // scan row 'next' for A(next,i)
		       for (m=A.ia[next]-1; m<A.ia[next+1]-1; m++)
		           if (A.ja[m]-1==i)
			      aij=A.a[m];

		       // compute maximum off-diagonal 1-norm of the associated rows
		       // remove |aij| and |aji| which have the same absolute value
		       val=FABS(aij);
		       weight=MAX(rbuff[i],rbuff[next])-val;
		       if (weight<0.0)
			  weight=0.0;
		       /* To measure the block diagonal dominance we use
			  ||/aii aij\^{-1}||          ||/ ajj -aij\||      weight
			  |||       |     ||*weight = |||         |||* ---------------
			  ||\aji ajj/     ||          ||\-aji  aii/|| |aii*ajj-aij*aji|  

                             MAX(|ajj|+|-aij|,|-aji|+|aii|)*weight
			  =  -------------------------------------
                                       |aii*ajj-aij*aji|  

                            (MAX(|ajj|,|aii|)+|aij|)*weight
			  = -------------------------------
                                   |aii*ajj-aij*aji|
		       */
		       weight*=MAX(FABS(dbuff[i]),FABS(dbuff[next]))+val;

		       
		       // build determinant aii*ajj-aij*aji 
		       // To do this form product A(p[i],p[next])*A(p[next],p[i])
		       // for symmetry reasons this can be reduced
		       // to a product only based on 'aij'
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		       // aij*aji
		       aij*=SKEW(aij);
		    
		       // compute determinant
		       aij=dbuff[i]*dbuff[next]-aij;
#else
		       // aij*aji
		       val=aij.r;
		       aij.r=val  *SKEW(val)-aij.i*SKEW(CONJG(aij.i));
		       aij.i=aij.i*SKEW(val)+val  *SKEW(CONJG(aij.i));
		       
		       // compute determinant
		       aij.r=dbuff[i].r*dbuff[next].r-dbuff[i].i*dbuff[next].i-aij.r;
		       aij.i=dbuff[i].r*dbuff[next].i+dbuff[i].i*dbuff[next].r-aij.i;
#endif

		       // catch the case that the 2x2 block is singular
		       weight/=FABS(aij)+1.0e-30;

		       if (flag) {
			  weighto+=weight;


#ifdef PRINT_INFO
		       printf("o%8.1le,%8.1le\n",weight,weighto);
#endif


			  flag=0;
		       }
		       else {
			  weighte+=weight;


#ifdef PRINT_INFO
		       printf("e%8.1le,%8.1le\n",weight,weighte);
#endif


			  flag=-1;
		       }
		       i=next;
		       next=p[i]-1;
		       k++;
		 } // end while (k<j)

		 // we choose the cycle that is more block diagonal dominant
		 if (weighte<=weighto)
		    i=l;
		 else
		    i=p[l]-1;
		 next=p[i]-1;
		 k=1;
		 while (k<j) {


#ifdef PRINT_INFO
		       printf("%8d%7d,",i+1,next+1);
#endif


		       // store i
		       m=i;
		       // advance i by two steps
		       i=p[next]-1;
		       // 2-cycle (i,next)
		       p[next]=m+1;
		       next=p[i]-1;
		       k+=2;
		 } // end while
		 // finally set singleton
		 p[i]=i+1;


#ifdef PRINT_INFO
		 printf("%7d,\n\n",i+1);
		 fflush(stdout);
#endif


	      } // end if-else (m>=0)
	   } // end if-else-if (j>2)
	} // end if  (l<n)
  } // end while (l<n)
  
  // finally rearrange the permutation such that the main weight is moved
  // to the tridiagonal part of A
  for (i=0; i<n; i++)
      ibuff[i]=p[i];

  j=0;
  for (i=0; i<n; i++) {
      k=ibuff[i];
      if (k-1==i)
	 p[j++]=k;
  } // end for i
  l=j;
  for (i=0; i<n; i++) {
      k=ibuff[i];
      if (k-1!=i) {
	 p[j++]=k;
	 p[j++]=i+1;
	 ibuff[k-1]=k;
      }
  } // end for i
  return (l);
} // end symwm

