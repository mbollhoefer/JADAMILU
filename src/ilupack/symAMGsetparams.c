#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <blas.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stdout
#define STDOUT          stdout
#define PRINT_INFO
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))
#define RESTOL_FUNC(A)  sqrt(A)











void SYMAMGSETPARAMS(CSRMAT A, ILUPACKPARAM *param,
		    integer flags, REALS elbow, REALS *droptols, REALS condest,
		    REALS restol, integer max_it)
{
  /*
    extract some of the default parameters from ipar and fpar

    ipar        integer parameters ipar[0,...,33]
                ipar[0]     elbow space factor. Predict the fill-in required
                            by the multilevel ILU by
                            ipar[0]*nnz(A)
                            default: 5
                ipar[1]     
                            
                            
                            
                            
		            
                ipar[2]     
                            
                            
                            
                            
		            
                ipar[3]     lfil parameter. Limit the number of nonzeros per
                            column in L / row in U by ipar[3]
                            default: n+1
                ipar[4]     flag that indicates different types of transposed
                            systems
                            Bit 0:  A^T      instead of A is stored  
                            Bit 1:  CONJG(A) instead of A is stored

                            Bit 2:  solve A^Tx=b      instead of Ax=b  
                            Bit 3:  solve CONJG(A)x=b instead of Ax=b  

                            REMARK: Note that P is derived from A. So if A^T is
			    stored instead of A, it follows that P^T is stored
			    instead of P. Similar relations hold for CONJG(A)

                ipar[5]     reserved for choice of iterative solver 
		            currently unused
			    default: 8

		ipar[6]     flags for the configuration of the multilevel ILU
                            available flags
			    inverse based dropping:                   DROP_INVERSE
			    don't shift away zero pivots(iluc):       NO_SHIFT
			    Tismentsky update:                        TISMENETSKY_SC
			    repeated ILU(iluc):                       REPEAT_FACT
			    improved estimate ||L^{-1}||,||U^{-1}||:  IMPROVED_ESTIMATE
			    diagonal compensation:                    DIAGONAL_COMPENSATION
			    reduce the partial LU to non-coarse
			    part(piluc,mpiluc):                       COARSE_REDUCE
			    use different pivoting strategy, if 
			    the regular reordering fails:             FINAL_PIVOTING
			    initial system should be reordered
			    using an initial strategy:                PREPROCESS_INITIAL_SYSTEM
			    subsequent systems should be reordered
			    using the regular strategy:               PREPROCESS_SUBSYSTEMS
			    use mpiluc as template instead of piluc:  MULTI_PILUC
			    
			    default:  DROP_INVERSE
                                     |PREPROCESS_INITIAL_SYSTEM
				     |PREPROCESS_SUBSYSTEMS
				     |IMPROVED_ESTIMATE
				     |FINAL_PIVOTING

		ipar[7]     decide which kind of scaling should be combined
                            with the three permutations
			    Bit 0-2 initial preprocessing
                            Bit 0
                                     (no)  left scaling        0/  1
                            Bit 1
                                     (no)  right scaling       0/  2
                            Bit 2    row/column scaling first  0/  4
			    default: 3

			    Bit 3-5 regular reordering
                            Bit 3
                                     (no)  left scaling        0/  8
                            Bit 4
                                     (no)  right scaling       0/ 16
                            Bit 5    row/column scaling first  0/ 32
			    default: 24

			    Bit 6-8 final pivoting
                            Bit 6
                                     (no)  left scaling        0/ 64
                            Bit 7
                                     (no)  right scaling       0/128
                            Bit 8    row/column scaling first  0/256
			    default: 192

                            Bit 9-10 indicate which ordering is currently
                                     in use
                             512     initial preprocessing
                            1024     regular reordering
                            1536     final pivoting
			    default: 0 (no preference)
			    sum default: 3+24+192


		ipar[8]     decide which kind of norm should be used
                            with the three permutations
			    Bit  0- 4 initial preprocessing
                            0         maximum norm
                            1         1-norm
                            2         2-norm 
                            3         square root of the diagonal entries
                            4         weighted row norm?
			    default: 3

			    Bit  5- 9 regular reordering
                            0         maximum norm
                            32*1      1-norm
                            32*2      2-norm 
                            32*3      square root of the diagonal entries
                            32*4      weighted row norm?
			    default: 32*3

			    Bit 10-14 final pivoting
                            0         maximum norm
                            1024*1    1-norm
                            1024*2    2-norm 
                            1024*3    square root of the diagonal entries
                            1024*4    weighted row norm?
			    default: 1024*3
			    sum default: 3+32*3+1024*3

                ipar[9]     lfil parameter for the approximate Schur 
		            complement. Limit the number of nonzeros per
                            row in S by ipar[9]
                            default: n+1

		ipar[10]    
		ipar[11]    
		ipar[12]    

		ipar[13,...,19] currently unused

                ipar[20,...33] parameters used by SPARSKIT solvers
	        ipar[20]    needed to init the iterative solver
		            default: 0
		ipar[21]    preconditioning side (left 1, right 2, both 3)
		            default: 2
		ipar[22]    stopping criteria 
		            default: 1 (relative residual)
		ipar[23]    workspace size requested by SPARSKIT solver
		            dependent on ipar[5]. Currently only GMRES is
                            supported.
			    defautl: (n+3)*(ipar[4]+2)+(ipar[4]+1)*ipar[4]/2;

  	        ipar[24]    restart length for GMRES,FOM,... 
		            default: 30
		ipar[25]    maximum number of iteration steps 
		            default: MIN(1.1*n+10,500)
		ipar[26,...33] further parameters used by SPARSKIT solvers
		            default: 0


    fpar        floating point parameters 
                fpar[0]     drop tolerance for triangular factors
                            default:  1e-2
                fpar[1]     drop tolerance for the approximate Schur complement
                            default:  1e-2
                fpar[2]     norm bound for the inverse triangular factors
                            default:  1e+2
                fpar[3]     define when a matrix is dense enough to switch
                            to full matrix processing 
                            fpar[3]*nnz(S) > n^2 
                            default:  3.0
                fpar[4]     Define the minimum block size which allows the
                            the multilevel ILU to continue with the default 
                            reordering strategy (including scaling and 
                            pivoting) before switching to a different strategy
                            if a matrix A of size "n" should be factored at the
                            current level, then piluc yiels up to a permutation
                                 / B  F \
                            A -> |      | such that B is factored
                                 \ E  C /
                            We continue the multilevel factorization if
                            n-nB<=n*fpar[4]
			    default: 0.75
                fpar[5]     Provided that a final static pivoting strategy
                            is requested, we may encounter the situation when
                            the regular reodering strategy becomes ineffective,
                            i.e.
                                 / B  F \
                            A -> |      | such that B is factored
                                 \ E  C /
                            with B of tiny size (n-nB > n*fpar[4]). In this
                            situation we may switch to a different final 
                            pivoting strategy. Like the regular reordering
                            strategy, we may also encounter a situation when
                            the final reordering strategy fails due to
                            a small block B
                            We continue the multilevel factorization with the
                            final pivoting strategy as long as
                            n-nB<=n*fpar[5]
			    default: 0.75
                fpar[6]     ONLY used if MULTI_PILUC is requested. Multiple,
                            repeated use of piluc without switching to a new 
                            level is continued as long as
                            "number of entries skipped" < 
                            "number of entries skipped previously" * fpar[6]
			    default: 0.75

		fpar[7]     drop tolerance for SYMILUC, used relative to 
                            fpar[0].	    
			    default: sqrt(eps)

                fpar[8]     drop tolerance for the remaining Schur complement
                            default: 1e-4 (squared drop tolerance) 

                fpar[9,...,19] currently unused

                fpar[20,...,30] floating parameters required for SPARSKIT 
		            solvers
		fpar[20]    relative error tolerance
		            default: sqrt(MACHEPS)
		fpar[21]    absolute error tolerance
		            default: 0
			    fpar[22]=
		ipar[23,...30] further parameters used by SPARSKIT solvers
		            default: 0


     written by Matthias Bollhoefer, October 2003
   */

  integer i;

  param->ipar[6]=flags     ;
  // param->ipar[0]=elbow     ;
  param->elbow=elbow;
  param->fpar[0]=droptols[0];
  param->fpar[1]=droptols[1];
  param->fpar[2]=condest   ;
  param->fpar[7]=param->fpar[1]/param->fpar[2];
  param->fpar[20]=restol    ;
  param->ipar[25]=max_it    ;


  // adapt the drop tolerance for the remaining Schur complement
  // with respect to the order of the approximate Schur complement
  if ((param->ipar[6]&TISMENETSKY_SC) || !(param->ipar[6]&SIMPLE_SC))
     param->fpar[8]=droptols[1]*droptols[1];
  else
     param->fpar[8]=droptols[1];


   // allocate memory for iterative solver depending on our choice
   i=param->ipar[24];
   switch (param->ipar[5]) {
   case  1:   // pcg 
     i=5*A.nr;
     break;
   case  2:   // sbcg
     i=5*A.nr;
     break;
   case  3:   // bcg 
     i=7*A.nr;
     break;
   case  4:   // sqmr 
     i=6*A.nr;
     break;
   case  5:   // bcgstab 
     i=8*A.nr;
     break;
   case  6:   // tfqmr 
     i=11*A.nr;
     break;
   case  7:   // fom 
     i=(A.nr+3)*(i+2)+(i+1)*i/2;
     break;
   case  8:   // gmres 
     i=(A.nr+3)*(i+2)+(i+1)*i/2;
     break;
   case  9:   // fgmres 
     i=2*A.nr*(i+1)+((i+1)*i)/2 + 3*i + 2;
     break;
   case 10:   // dqgmres 
     i=A.nr+(i+1)*(2*A.nr+4);
     break;
   } // end switch 
   param->ipar[23]=i;

} // end AMGsetparams

