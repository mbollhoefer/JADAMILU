      PROGRAM EXAMPLE11
*     simple program to illustrate basic usage of DPJD with
*     diagonal preconditioning
      IMPLICIT NONE
*     needed parameters: 
*              N: size of the problem
*         MAXEIG: max. number of wanteg eig (NEIG<=MAXEIG)
*          MAXSP: max. value of MADSPACE
      INTEGER    N, MAXEIG, MAXSP
      PARAMETER (N=1000,MAXEIG=5,MAXSP=20)
*     optimal workspace (here MAXSP*MAXSP>MAXEIG)
      INTEGER LX
      PARAMETER (LX=N*(3*MAXSP+MAXEIG+1)+4*MAXSP*MAXSP)
      DOUBLE PRECISION   EIGS(MAXEIG), RES(MAXEIG), X(LX)
*     arguments to pass to the routines
      INTEGER            NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER            ITER, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, GAP,MEM, DROPTOL,SHIFT

*     some local variables
      INTEGER I,K
*     let us define a very simple matrix:
*
*              1     5      0      ...
*              5     2      5       0     ...
*         A =  0     5      3       5      0     ...
*              0     0      5       4      5      0     ...
*             ...   ...   
*
*     we use the storage scheme needed by DPJD
*     (only the upper triangular part is referenced)
      DOUBLE PRECISION A(2*N)
      INTEGER IA(N+1), JA(2*N)
      K=1
      DO I=1,N
         IA(I)=K
         JA(K)=I
         A(K)=DBLE(I)
         K=K+1
         IF (I .LT. N) THEN
            JA(K)=I+1
            A(K)=5.0D0
            K=K+1
         END IF
      END DO
      IA(N+1)=K
*
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with ILU preconditioning'
      PRINT *, 
     $'  First low and next high accuracy'
      PRINT *, 
     $'-----------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
c     we want the smallest eigenvalues
      ISEARCH=0
c     (then, SIGMA, SHIFT need not to be set)
c     
c     elbow space factor for the fill computed during the ILU
      MEM=20.0
c     tolerence for discarded fill
      DROPTOL=1.d-3
c
c     number of wanted eigenvalues
      NEIG=maxeig
c     no initial approximate eigenvectors
      NINIT=0
c     desired size of the search space
      MADSPACE=maxsp
c     maximum number of iteration steps
      ITER=1000
c     LOW tolerance for the eigenvector residual
      TOL=1.0d-3
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0

      CALL DPJD(N,A,JA,IA, EIGS, RES, X, LX, NEIG, 
     $          SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $          SHIFT, DROPTOL, MEM, ICNTL,
     $          IPRINT, INFO, GAP)
c
c     turn off adaptivity and recycle preconditioner
      ICNTL(2)=-1
c     on input pass the computed eigenvectors
      NINIT=NEIG
c     improve accuracy of the eigenvectors
      TOL=1.0d-10
c     re-init number of iteration steps, because it was overwritten
      ITER=1000
c     remember also that SIGMA and SHIFT were changed on
c     on output, re-use these values
      ISEARCH=1
c     number of wanted eigenvalues
      NEIG=maxeig
c
      CALL DPJD(N,A,JA,IA, EIGS, RES, X, LX, NEIG, 
     $          SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $          SHIFT, DROPTOL, MEM, ICNTL,
     $          IPRINT, INFO, GAP)

c
c     release internal memory and discard preconditioner
      CALL DPJDCLEANUP
c
      END
