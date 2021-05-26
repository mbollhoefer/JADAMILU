      PROGRAM EXAMPLE1
*     simple program to illustrate basic usage of PJD
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
      REAL               SEIGS(MAXEIG),SRES(MAXEIG),SX(LX)
*     arguments to pass to the routines
      INTEGER            NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER            ITER, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, DELTA, SHIFT, GAP, MEM, DROPTOL
      REAL               SSIGMA,STOL,SDELTA,SSHIFT,SGAP,SMEM,SDROPTOL
*
*     some local variables
      INTEGER I,K,I2,J,I3
*     let us define a very simple matrix:
*
*              0     5      0      ...
*              5     1      5       0     ...
*         A =  0     5      2       5      0     ...
*              0     0      5       3      5      0     ...
*             ...   ...   
*
*     we use the storage scheme needed by PJD
*     (only the upper triangular part is referenced)
      DOUBLE PRECISION A(2*N)
      REAL             SA(2*N)
      INTEGER          IA(N+1), JA(2*N)
      K=1
      DO I=1,N
         IA(I)=K
         JA(K)=I
         A(K) =DBLE(I-1)
         SA(K)=REAL(I-1)
         K=K+1
         IF (I .LT. N) THEN
            JA(K)=I+1
            A(K) =5.0D0
            SA(K)=5.0E0
            K=K+1
         END IF
      END DO
      IA(N+1)=K
*
*  EXAMPLE 1
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the eigenvalues closest to origin ILU preconditioning'
      PRINT *, 
     $'---------------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
c     we want the eigenvalues near target sigma=0         
      ISEARCH=2
      SIGMA =0.0d0
      SSIGMA=SIGMA
c     (then, SHIFT need not to be set)
c     
c     elbow space factor for the fill computed during the ILU
      MEM =20.0
      SMEM=MEM
c     tolerence for discarded fill
      DROPTOL =1.d-3
      SDROPTOL=DROPTOL
c
c     number of wanted eigenvalues
      NEIG=maxeig
c     no initial approximate eigenvectors
      NINIT=0
c     desired size of the search space
      MADSPACE=maxsp
c     maximum number of iteration steps
      ITER=1000
c     tolerance for the eigenvector residual
      TOL =1.0d-10
      STOL=1.0E-7
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0
c     
c     We have the exact matrix: we may call PJD, single precision version
c
      write (6,'(A)') 'call single driver'

      CALL SPJD( N,sa,ja,ia, SEIGS, SRES, SX, LX, NEIG, 
     $           SSIGMA, ISEARCH, NINIT, MADSPACE, ITER, STOL,
     $           SSHIFT, SDROPTOL, SMEM, ICNTL,
     $           IPRINT, INFO, GAP)
      CALL SPJDCLEANUP

      write (6,'(A)') 'single driver completed'


c     now call double precision JD with single precsion PREC
      ICNTL(5)=1
      NEIG=5
      NINIT=NEIG
      do j=1,NEIG
         EIGS(j)=SEIGS(j)
      end do
      do j=1,N*NEIG
         X(j)=SX(j)
      end do
      write (6,'(A)') 'call mixed double/single driver'
      CALL DPJD( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $           SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $           SHIFT, DROPTOL, MEM, ICNTL,
     $           IPRINT, INFO, GAP)
      write (6,'(A)') 'mixed double/single driver completed'

c
c
c     release the memory allocated for preconditioning
c     (not needed if the computation really terminates here)
      CALL DPJDCLEANUP
c
      END
