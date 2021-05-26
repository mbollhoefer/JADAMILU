#include <ilupack_fortran.h>
#include <ilupackmacros.h>
*======================================================================*
*
       SUBROUTINE PJD_EV( N, A, JA, IA, EIGS, RES, X, LX, NEIG, 
     +                    SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     +                    SHIFT, DROPTOL, MEM, ICNTL, IPRINT, INFO, 
     +                    GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, ICNTL(5), IPRINT, INFO
      REALS              SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer            JA(*), IA(*)
      FLOAT   A(*), X(*)
      REALS   EIGS(*), RES(*)
*     ..
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of the SSSS matrix A 
*  passed in arrays A, JA, IA, using built-in ILU preconditioning.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER.
*          The dimension of the matrix. Should be larger than 1.
*
*  A       (input/output) numerical values
*  IA      (input/output) pointers for every column
*  JA      (input/output) column indices, if JA(1)<0, then A is assumed 
*          to be diagonal.
*
*          Detailed description of A,IA,JA
* 
*              Note first that no more than the diagonal part of A and 
*              EITHER values from the strict upper triangular part OR 
*              values from the strict lower triangular part are needed 
*              to define the full matrix. 
*              PJD_EV/PJDREVCOM_EV ASSUMES THAT EACH OFF-DIAGONAL ENTRY IS
*              REFERENCED ONLY ONCE, in either the upper triangular part
*              or in the lower triangular part.
*              (Thus, for instance, A,IA,JA may contain either the upper
*              triangular part or the lower triangular part - in each
*              case including diagonal).
*
*              PJD_EV ASSUMES IN ADDITION THAT ALL DIAGONAL ELEMENTS ARE
*              REFERENCED, EVEN WHEN SOME OF THEM ARE EQUAL TO ZERO.
*              HHHH
*
*              OTHERWISE THE CODE WILL NOT RUN PROPERLY
*
*              On input, IA(I), I=1,...,N+1 refers to the physical start
*              of row I. In this case the entries of row I are located 
*              in A(K), where K=IA(I),...IA(I+1)-1. JA(K) carries the 
*              associated column indices. According what is written
*              above, PJD_EV assumes that some of these JA(K) (for 
*              IA(I)<= K < IA(I+1) ) is equal to I with corresponding 
*              A(K) carrying the value of the diagonal element, possibly
*              equal to zero.
*
*              A,IA,JA are "output" parameters because on exit the 
*              entries of each row may occur in a different order (The
*              matrix is mathematically the same, but stored in 
*              different way).
*              
*  EIGS    (input/output) REALS array, dimension NEIG.
*          On input, eigenvalue estimates corresponding to provided
*              initial guesses (EIGS(i) corresponds to approximate 
*              eigenvector number i); used only if NINIT>(MADSPACE+1)/2
*              to make sure that initial approximate eigenvectors are
*              processed in the right order. 
*              Sorting is skipped if EIGS(1)=EIGS(2)= ... =EIGS(NINIT).
*              Then, if NINIT > (MADSPACE+1)/2 , initial approximations
*              should be in stored in increasing order of eigenvalues if
*              ISEARCH <= 1 , or in increasing distance of eigenvalues
*              to SIGMA if ISEARCH >= 2
*          On output, eigenvalues as they are computed
*              (EIGS(i) corresponds to approximate eigenvector number i).
*
*  RES     (output) REALS array, dimension NEIG.
*          Residual norms: RES(i)=|| A*x(i)-EIGS(i)*x(i) ||, 
*              where A is the matrix, || is the two norm, and x(i) is
*              the approximate eigenvector number i .
*
*  X       (input/output+workspace) FLOAT array, dimension LX.
*          On input, the initial guess(es) (not required, see NINIT). 
*          On output, the iterated approximate eigenvector(s). 
*              On output (input), approximate eigenvector number i is
*              (or should be) stored in X(1+N*(i-1):N*i), 
*              for i=1,...,NINIT
*
*  LX      (input) INTEGER
*          Dimension of X. Should be at least
*              N*(2*MADSPACE+NEIG+4)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          If MADSPACE >= 3, use LX not smaller than
*              N*(3*MADSPACE+NEIG+1)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          to guarantee optimal performance.
*
*  NEIG    (input/output) INTEGER
*          On input, the number of eigenvalue(s) to be computed; 
*                    should be positive.
*          On output, the number of eigenvalues effectively computed
*                    with the required accuracy.
*
*  SIGMA   (input) REALS
*          If ISEARCH <= 0: not used
*          If ISEARCH  = 1: estimation of the smallest eigenvalue
*                  (may speed up somewhat the algorithm if not too
*                   inaccurate)
*          If ISEARCH >= 2: the "target", see ISEARCH
*
*  ISEARCH (input) INTEGER
*          If ISEARCH <= 0: compute the smallest eigenvalue(s)
*          If ISEARCH  = 1: compute the smallest eigenvalue(s) and use
*                   SIGMA as initial guess. If one searches for the
*                   smallest eigenvalue(s) and has to rerun the
*                   algorithm for the same problem (or a problem with
*                   similar eigenvalues at the lower end), it is a good
*                   idea to set ISEARCH=1 and SIGMA=EIGS(1) (as obtained
*                   from the first run).
*          If ISEARCH >= 2: compute the eigenvalues closest to SIGMA
*
*  NINIT   (input) INTEGER
*          Number of initial guess(es) provided. May be set to 0.
*
*  MADSPACE (input) INTEGER
*           Maximal dimension of the search space (usually between 10 
*           and 20). Should be at least 2.
*
*  ITER    (input/output) INTEGER
*          On input, the maximum number of matrix vector multiplications; 
*                    should be positive.
*          On output, actual number of matrix vector multiplications.
*
*  TOL     (input) REALS
*          The tolerance on residual norm. Iterations to compute
*               eigenvector number i are stopped whenever
*               || A*x(i)-EIGS(i)*x(i) || <= TOL, where  ||x(i)||=1.
*               Should be positive.
*
*  SHIFT   (input/output) REALS
*          used only if ISEARCH = 1
*          On input, SHIFT is used to shift the input matrix by a
*              multiple of the identity matrix in order to compute the 
*              preconditioner. A good heuristic is obtained by setting
*              SHIFT equal to SIGMA (the smallest eigenvalue estimate)
*              minus the estimated gap between this smallest eigenvalue
*              and the next one (i.e., SHIFT approximates 2 lambda_1 -
*              lambda_2, where lambda_1 (lambda_2) is smallest (second 
*              smallest) eigenvalue). If one has no idea of this gap,
*              SHIFT may be set equal to SIGMA.
*          On output: suggested new value for the SHIFT parameter (not
*              necessarily equal to the current estimation of 2 lambda_1
*              - lambda_2). If one searches for the smallest eigenvalue(s) 
*              and has to rerun the algorithm  for the same problem (or
*              a problem with similar eigenvalues at the lower end), it 
*              is a good idea to set ISEARCH=1, SIGMA=EIGS(1) and SHIFT
*              equal to the value on output from PJD_EV/PJDREVCOM_EV.
*
*  DROPTOL (input/output) REALS
*          On input, drop tolerance for the multilevel incomplete 
*              factorization. A small drop tolerance will typically lead
*              to more fill-in, i.e. more memory will be consumed and 
*              the application of the preconditioner is more costly. On 
*              the other hand, the number of iteration steps is expected
*              to be less for a smaller drop tolerance.
*          On output: suggested new value for the DROPTOL parameter, 
*              that might be useful to if one has to rerun the algorithm
*              for a similar problem.
*
*  MEM     (input) REALS
*          MEM prescribes the amount of memory the user is willing to 
*              spend for the preconditioner. MEM is relative to the
*              number of nonzero of the input matrix. If it turns out
*              the preconditioner that is computed does not fit into the
*              memory area that is offered by the user, PJD_EV will
*              terminate with an error message. In this case one can
*              either increase MEM (if there is more memory available)
*              or one has to increase DROPTOL.
*
*  ICNTL   (input) INTEGER
*          some control parameters
*          ICNTL(1) Not used (kept for compatibility with PJDREVCOM_EV).
*          ICNTL(2) If equal to zero, then adaptive preconditioning is
*                   used, i.e., during the eigenvector computation the
*                   ILU may be recomputed (with different SHIFT and
*                   DROPTOL), if useful and possible.
*                   If not equal to zero, then the preconditioner
*                   computed initially is kept throughout. If, in
*                   addition, ICNTL(2) is equal to -1, the existing
*                   preconditioner is reused in a static way (this 
*                   option presumes that PJD_EV was called previously and
*                   successful for the same problem). Finally, ICNTL(2)
*                   set to -2 will cause that a previously existing
*                   preconditioner will be reused in a adaptive fashion.
*          ICNTL(3) If ICNTL(2) is equal to zero and ISEARCH <= 2,
*                   then ICNTL(3) states whether negative diagonal
*                   entries that show up in the ILU will be changed to
*                   positive ones. If set to zero (default), then up to
*                   1% of the negative diagonal entries are converted.
*                   If more negative diagonal are discovered then the
*                   algorithm searches for a new shift (and possibly a
*                   different DROPTOL if adaptive preconditioning is
*                   used).
*                   If ICNTL(3) is set to 1, then no negative diagonal
*                   entries are permitted forcing the algorithm to
*                   compute a different shift.  
*          ICNTL(4) If set to zero, default estimate for norm of the
*                   inverse factor is used. Otherwise use ICNTL(4) as
*                   bound.
*          ICNTL(5) AAAZ
*                   AAZZ
*                   AZZZ
*
*  IPRINT  (input) INTEGER
*          Its absolute value indicates the unit number where 
*              information is to be printed (N.B.: 5 is converted to 6).
*              If zero, only error messages are printed on standard
*              output. Otherwise, its sign indicates the level of output:
*              if negative, extensive information (for expert) is
*              provided; most users should be satisfied with the 
*              information provided for positive IPRINT.
*          
*  INFO    (output) INTEGER
*              INFO=0 if normal termination. 
*              INFO>0 if allowed maximum number of matrix vector 
*                        multplications performed without finding all
*                        wanted eigenvalues & eigenvectors. 
*              INFO<0 if an error occurred - see printed output for
*                        details
*              (INFO=-54321: the computation of the preconditioner
*                        failed).
*
*  GAP     (output) REALS
*          The estimated distance between the set of NEIG computed
*          eigenvalues and the remaining part of the spectrum; may be
*          inaccurate.
*
** ===========================================================
*
       integer keep, IJOB, NDX1, NDX2, NDX3, JB, IB
       REALS b
*
        keep=ICNTL(1)
        ICNTL(1)=1
        IJOB=0
*
        CALL PJDRVCOM( N, A, JA, IA, B, JB, IB, EIGS, RES, X, LX,
     +                 NEIG, SIGMA, ISEARCH, NINIT, MADSPACE, ITER,
     +                 TOL, SHIFT, DROPTOL, MEM, ICNTL, IJOB,
     +                 NDX1, NDX2, NDX3, IPRINT, INFO, GAP, .FALSE.)
*
        ICNTL(1)=keep
*
      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE PJDREVCOM_EV( N, A, JA, IA, EIGS, RES, X, LX, NEIG,
     +                          SIGMA, ISEARCH, NINIT, MADSPACE, ITER,
     +                          TOL, SHIFT, DROPTOL, MEM, ICNTL,
     +                          IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE, INFO
      integer            ITER, ICNTL(5), IJOB, NDX1, NDX2, IPRINT
      REALS              SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer            JA(*), IA(*)
      FLOAT              A(*), X(*)
      REALS              EIGS(*), RES(*)
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of a SSSS matrix A 
*  using built-in ILU or diagonal preconditioning.
*  Matrix multiplication by A is carried out in a user defined routine
*  that is called through a reverse communication protocol.
*
*  Arguments
*  =========
*
*    N,A,JA,IA,                        |  see comments in subroutine PJD_EV
*
*       In addition:
*
*            A,JA,IA need not to define exactly the matrix whose
*               eigenvalues are wanted; instead it may be some 
*               approximation; this is consistent because the matrix 
*               passed to PJDREVCOM_EV is only used to define a
*               preconditioner, whereas matrix vector multiplications
*               are performed by a user provided routine via the reverse
*               communication protocol (see below).
*
*            A,JA,IA should be compliant with the format described in PJD_EV
*            However: 
*                  zero diagonal entries need not to be referenced in
*                        the structure (although they may);
*                  if a diagonal preconditioning is wanted, one
*                        should set JA(1) negative;
*                        if JA(1)<0, A(1),...,A(N) is supposed
*                        to carry the diagonal of the matrix;
*                        then, JA does not need to have a length greater
*                        than 1, IA is neither referenced, and A,JA,IA
*                        are unchanged on output. 
*
*
*    EIGS, RES, X, LX, NEIG,           |
*    SIGMA, ISEARCH, NINIT, MADSPACE,  |  
*    ITER, TOL, SHIFT, DROPTOL, MEM,   |  see comments in subroutine PJD_EV
*                                      |
*    IPRINT, INFO, GAP                 |
*
*
*  ICNTL   (input/output) INTEGER
*          some control parameters
*          ICNTL(1) should be set to zero (default value), except if X 
*                   overwrites the arrays in A,JA,IA, in which case one
*                   should set ICNTL(1)=2 (this tells that the matrix
*                   cannot be refactored once the eigenvalue computation
*                   started)
*          other entries in ICNTL: see comments in subroutine PJD_EV
*
*
*  IJOB    (input/output) INTEGER. 
*          Used to communicate job code between the levels.
*          Input: one should use IJOB=0 on the first call, 
*                 and leave IJOB unchanged on subsequent calls
*          Output:
*               IJOB=0: work done - terminate
*               IJOB=1: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_EV leaving IJOB
*                       (and other parameters) unchanged.
*                       
*  NDX1    (output) INTEGER.
*  NDX2    Indicate indices into X() for the needed MATVEC when IJOB=1.
*
** ============================================================
*
       integer NDX3, JB, IB
       REALS B
*
       CALL PJDRVCOM( N, A, JA, IA, B, JB, IB, EIGS, RES, X, LX,
     +                NEIG, SIGMA, ISEARCH, NINIT, MADSPACE, ITER, 
     +                TOL, SHIFT, DROPTOL, MEM, ICNTL, IJOB,
     +                NDX1, NDX2, ndx3, IPRINT, INFO, GAP, .FALSE.)
       IF (IJOB.EQ.3) IJOB=1
*
      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE JDREVCOM_EV( N, EIGS, RES, X, LX, NEIG, SIGMA,
     +                         ISEARCH, NINIT, MADSPACE, ITER, TOL,
     +                         IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, IJOB, NDX1, NDX2, IPRINT, INFO
      REALS   SIGMA, TOL, GAP
*     ..
*     .. Array Arguments ..
      FLOAT   X( * )
      REALS   EIGS( *), RES(*)
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of a SSSS matrix A. 
*  Matrix multiplication by A and preconditioner solve are carried out
*  in user defined routines that are called through a reverse
*  communication protocol.
*
*  Arguments
*  =========
*
*    N, EIGS, RES, X, LX, NEIG,        |
*    SIGMA, ISEARCH, NINIT,            |  
*    MADSPACE, ITER, TOL,              |  see comments in subroutine PJD_EV
*                                      |
*    IPRINT, INFO, GAP                 |
*
*
*  IJOB    (input/output) INTEGER. 
*          Used to communicate job code between the levels.
*          Input: one should use IJOB=0 on the first call, and leave
*                 IJOB unchanged on subsequent calls
*          Output:
*               IJOB=0: work done - terminate
*               IJOB=1: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1) 
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_EV leaving IJOB
*                       (and other parameters) unchanged.
*               IJOB=2: solve Prec*X(NDX1:NDX1+N-1) = X(NDX2:NDX2+N-1)
*                       (call to preconditioner solve routine: 
*                       PSOLVE) and return to PJDREVCOM_EV leaving IJOB 
*                       (and other parameters) unchanged.
*
*  NDX1    (output) INTEGER.
*  NDX2    Indicate indices into X() for the needed MATVEC when IJOB=1,
*          or for the needed PSOLVE when IJOB=2.
*
** ============================================================
*
       integer NDX3
*
       CALL JDRVCOM( N, EIGS, RES, X, LX, NEIG, SIGMA,
     +               ISEARCH, NINIT, MADSPACE, ITER, TOL, IJOB,
     +               NDX1, NDX2, NDX3, IPRINT, INFO, GAP, .FALSE.)
       IF (IJOB.EQ.3) IJOB=1
*
      RETURN
      END
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE PJDCLEANUP_EV
*
*  Purpose
*  =======
*
*  Releases memory allocated by JADAMILU routines.
*
** ============================================================
      implicit none
      DOUBLE PRECISION  timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  condest0
      INTEGER*8         IPparam,IPPREC,IPdiag
      integer           IPnlev,factdgl,factspd,IUNIT,PR,
     +                  prvdr
      logical           reenterfirsttime,flagsingle
      common/PJDINITPJD/timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  IPparam,IPPREC,IPdiag,IPnlev,factdgl,factspd,
     +                  prvdr,IUNIT,PR,condest0,reenterfirsttime,
     +                  flagsingle
      integer ilumem  
      common/ILUPACKMEM/ilumem
      external DGLPRECDELETE,SYMILUPACKDELETE
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
      external SINGLESYMILUPACKDELETE
#endif

c     make sure that PJDCLEANUP_EV is not really executed before PJD_EV/
c     PJDREVCOM_EV has been called for the first time
      if (ilumem.ne.-1) then        
         if (factdgl.gt.0) then
            call DGLPRECDELETE(IPdiag)
         else if (factdgl.lt.0) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
               call SINGLESYMILUPACKDELETE(IPparam,IPPREC,IPnlev,0)
            else
               call SYMILUPACKDELETE(IPparam,IPPREC,IPnlev,0)
            end if
#else
            call SYMILUPACKDELETE(IPparam,IPPREC,IPnlev,0)
#endif
         end if
c        ensure that twice calling PJDCLEANUP_EV is captured
         ilumem=-1
      end if
      factdgl=0
c
      return
      end
*
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE PJD_GEP( N, A, JA, IA, B, JB, IB, EIGS, RES, X, LX, 
     +                      NEIG, SIGMA, ISEARCH, NINIT, MADSPACE, ITER,
     +                      TOL, SHIFT, DROPTOL, MEM, ICNTL, IPRINT, 
     +                      INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, ICNTL(5), IPRINT, INFO
      REALS   SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer            JA(*), IA(*), JB(*), IB(*)
      FLOAT   A(*), B(*), X(*)
      REALS   EIGS(*), RES(*)
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of the generalized
*  eigenvalue problem defined by the SSSS matrix A and the SSSS
*  positive definite mass matrix B, passed in arrays A, JA, IA, and 
*  arrays B, JB, IB, respectively; built-in ILU preconditioning is used.
*
*
*  Arguments
*  =========
*
*    N,A,JA,IA,                        |  see comments in subroutine PJD_EV
*
*  B, JB, IB (input/output) define the mass matrix B, using the same
*            storage scheme as for A (see comments in subroutine PJD_EV).
*            The same restriction on diagonal entries apply.
*            In addition, B should be positive definite. 
*
*              
*    EIGS, RES, X                      |
*                                      |
*    NEIG, SIGMA, ISEARCH, NINIT,      |
*    MADSPACE, ITER,                   |  see comments in subroutine PJD_EV 
*                                      | 
*    SHIFT, DROPTOL, MEM,              |  
*    ICNTL, IPRINT, INFO, GAP          |
*
*              
*  LX      (input) INTEGER
*          Dimension of X. If ISEARCH <= 1, should be at least
*             N*(3*MADSPACE+2*NEIG+4)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          and, if ISEARCH >= 2, should be at least
*             N*(3*MADSPACE+2*NEIG)+MAX(4*N,6*MADSPACE)+3*MADSPACE**2
*                                   +MAX(MADSPACE**2,NEIG)
*          (which amounts the same except for very small N)
*          If MADSPACE >= 3, use LX not smaller than
*             N*(4*MADSPACE+2*NEIG+1)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          to guarantee optimal performance.
*
*  TOL     (input) REALS
*          The tolerance on residual norm. Iterations to compute
*               eigenvector number i are stopped whenever
*               || A*x(i)-EIGS(i)*B*x(i) || <= TOL * ||B*x(i)||.
*               Should be positive.
*
** ===========================================================
*
       integer keep, ijob, ndx1, ndx2, ndx3
*
        keep=ICNTL(1)
        ICNTL(1)=1
        ijob=0
*
        CALL PJDRVCOM( N, A, JA, IA, B, JB, IB, EIGS, RES, X, LX,  
     +                  NEIG, SIGMA, ISEARCH, NINIT, MADSPACE, ITER,
     +                  TOL, SHIFT, DROPTOL, MEM, ICNTL, ijob,
     +                  ndx1, ndx2, ndx3, IPRINT, INFO, GAP, .TRUE.)
*
        ICNTL(1)=keep
*
      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE PJDREVCOM_GEP( N, A, JA, IA, B, JB, IB, EIGS, RES, X,
     +                            LX, NEIG, SIGMA, ISEARCH, NINIT, 
     +                            MADSPACE, ITER, TOL, SHIFT, DROPTOL,
     +                            MEM, ICNTL, IJOB, NDX1, NDX2, NDX3,
     +                            IPRINT, INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer   N, LX, NEIG, ISEARCH, NINIT, MADSPACE, ITER
      integer   ICNTL(5), IJOB, NDX1, NDX2, NDX3, IPRINT, INFO
      REALS     SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer   JA(*), IA(*), JB(*), IB(*)
      FLOAT     A(*), B(*), X(*)
      REALS     EIGS(*), RES(*)
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of the generalized
*  eigenvalue problem defined by a SSSS matrix A and a SSSS
*  positive definite mass matrix B, using a built-in ILU preconditioning.
*  Matrix multiplication by A and B is carried out in user defined
*   routine(s) that are called through a reverse communication protocol.
*
*
*  Arguments
*  =========
*
*    N,A,JA,IA,                       |   see comments in subroutine PJD_EV
*
*  B, JB, IB (input/output) define the mass matrix B, using the same
*            storage scheme as for A (see comments in subroutine PJD_EV).
*            The same restriction on diagonal entries apply.
*            In addition, B should be positive definite. 
*
*       In addition:
*
*            A,JA,IA & B,JB,IB need not to define exactly the matrices 
*               A and B; this is consistent because the matrix 
*               passed to PJDREVCOM_GEP is only used to define a
*               preconditioner, whereas matrix vector multiplications
*               are performed by a user provided routine via the reverse
*               communication protocol (see below).
*
*            A,JA,IA and B,JB,IB  should be compliant with the format 
*            described in PJD_EV.However: 
*                  zero diagonal entries need not to be referenced in
*                        the structure (although they may);
*                  if a diagonal preconditioning is wanted, one
*                        should set ja(1) negative;
*                        if JA(1)<0, A(1),...,A(N) is supposed
*                        to carry the diagonal of the matrix A, and 
*                        B(1),...,B(N) the diagonal of the matrix B;
*                        then, JA does not need to have a length greater
*                        than 1, IA, IB, JB are neither referenced, and 
*                        A,JA,IA & B,JB,IB are unchanged on output. 
*
*
*    EIGS, RES, X,                     |
*                                      |
*    NEIG, SIGMA, ISEARCH, NINIT,      |
*    MADSPACE, ITER,                   |  see comments in subroutine PJD_EV 
*                                      | 
*    SHIFT, DROPTOL, MEM,              |  
*                                      |
*    IPRINT, INFO, GAP                 |
*
*
*  LX      (input) INTEGER
*          Dimension of X. If ISEARCH <= 1, should be at least
*             N*(3*MADSPACE+2*NEIG+4)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          and, if ISEARCH >= 2, should be at least
*             N*(3*MADSPACE+2*NEIG)+MAX(4*N,6*MADSPACE)+3*MADSPACE**2
*                                   +MAX(MADSPACE**2,NEIG)
*          (which amounts the same except for very small N)
*          If MADSPACE >= 3, use LX not smaller than
*             N*(4*MADSPACE+2*NEIG+1)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          to guarantee optimal performance.
*              
*  TOL     (input) REALS
*          The tolerance on residual norm. Iterations to compute
*               eigenvector number i are stopped whenever
*               || A*x(i)-EIGS(i)*B*x(i) || <= TOL * ||B*x(i)||.
*               Should be positive.
*
*  ICNTL   (input/output) INTEGER
*          some control parameters
*          ICNTL(1) should be set to zero (default value), except if X 
*                   overwrites the arrays in A,JA,IA, in which case one
*                   should set ICNTL(1)=2 (this tells that the matrix
*                   cannot be refactored once the eigenvalue computation
*                   started)
*          other entries in ICNTL: see comments in subroutine PJD_EV
*
*  IJOB    (input/output) INTEGER. 
*          Used to communicate job code between the levels.
*          Input: one should use IJOB=0 on the first call, 
*                 and leave IJOB unchanged on subsequent calls
*          Output:
*               IJOB=0: work done - terminate
*               IJOB=1: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1)
*                       AND     X(NDX3:NDX3+N-1)= B*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*               IJOB=3: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*               IJOB=4: compute X(NDX3:NDX3+N-1)= B*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*                       
*  NDX1    (output) INTEGER.
*  NDX2    Indicate indices into X() for the needed MATVEC when 
*  NDX3    IJOB=1, 3 or 4. 
*
** ============================================================
*
       CALL PJDRVCOM( N, A, JA, IA, B, JB, IB, EIGS, RES, X, LX, NEIG, 
     +                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     +                 SHIFT, DROPTOL, MEM, ICNTL, IJOB,
     +                 NDX1, NDX2, NDX3, IPRINT, INFO, GAP, .TRUE.)
      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE JDREVCOM_GEP(N, EIGS, RES, X, LX, NEIG, SIGMA, 
     +                          ISEARCH, NINIT, MADSPACE, ITER, TOL,  
     +                          IJOB, NDX1, NDX2, NDX3, IPRINT, INFO,
     +                          GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, IJOB, NDX1, NDX2, NDX3, IPRINT, INFO
      REALS   SIGMA, TOL, GAP
*     ..
*     .. Array Arguments ..
      FLOAT   X( * )
      REALS   EIGS( *), RES(*)
*
*  Purpose
*  =======
*
*  Computes selected eigenvalues and eigenvectors of the generalized
*  eigenvalue problem defined by a SSSS matrix A and a SSSS
*  positive definite mass matrix B, using a built-in ILU preconditioning.
*  Matrix multiplication by A and B and preconditioner solve are
*  carried out in user defined routines that are called through a 
*  reverse communication protocol.
*
*
*  Arguments
*  =========
*
*
*    EIGS, RES, X                      |
*                                      |
*    NEIG, SIGMA, ISEARCH, NINIT,      |
*    MADSPACE, ITER,                   |  see comments in subroutine PJD_EV 
*                                      | 
*    IPRINT, INFO, GAP                 |
*
*
*  LX      (input) INTEGER
*          Dimension of X. If ISEARCH <= 1, should be at least
*             N*(3*MADSPACE+2*NEIG+4)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          and, if ISEARCH >= 2, should be at least
*             N*(3*MADSPACE+2*NEIG)+MAX(4*N,6*MADSPACE)+3*MADSPACE**2
*                                   +MAX(MADSPACE**2,NEIG)
*          (which amounts the same except for very small N)
*          If MADSPACE >= 3, use LX not smaller than
*             N*(4*MADSPACE+2*NEIG+1)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG)
*          to guarantee optimal performance.
*              
*  TOL     (input) REALS
*          The tolerance on residual norm. Iterations to compute
*               eigenvector number i are stopped whenever
*               || A*x(i)-EIGS(i)*B*x(i) || <= TOL * ||B*x(i)||.
*               Should be positive.
*
*  IJOB    (input/output) INTEGER. 
*          Used to communicate job code between the levels.
*          Input: one should use IJOB=0 on the first call, and leave
*                 IJOB unchanged on subsequent calls
*          Output:
*               IJOB=1: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1)
*                       AND     X(NDX3:NDX3+N-1)= B*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*               IJOB=2: solve Prec*X(NDX1:NDX1+N-1) = X(NDX2:NDX2+N-1)
*                       (call to preconditioner solve routine: 
*                       PSOLVE) and return to PJDREVCOM_GEP leaving IJOB 
*                       (and other parameters) unchanged.
*               IJOB=3: compute X(NDX2:NDX2+N-1)= A*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*               IJOB=4: compute X(NDX3:NDX3+N-1)= B*X(NDX1:NDX1+N-1)
*                       (call to matrix vector multiplication routine:
*                        MATVEC) and return to PJDREVCOM_GEP leaving IJOB
*                       (and other parameters) unchanged.
*                       
*  NDX1    (output) INTEGER.
*  NDX2    Indicate indices into X() for the needed MATVEC when 
*  NDX3    IJOB=1, 3 or 4, or for the needed PSOLVE when IJOB=2.
*
** ============================================================
*
        CALL JDRVCOM( N, EIGS, RES, X, LX, NEIG, SIGMA,
     +                 ISEARCH, NINIT, MADSPACE, ITER, TOL, IJOB, 
     +                 NDX1, NDX2, NDX3, IPRINT, INFO, GAP, .TRUE.)
*
      RETURN
      END
*======================================================================*
***
