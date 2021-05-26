      PROGRAM EXAMPLE7
*     simple program to illustrate basic usage of DPJD
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
      DOUBLE PRECISION   SIGMA, TOL, SHIFT, GAP, MEM, DROPTOL, T

*     some local variables
      INTEGER I,K,L
      DOUBLE PRECISION A(2*N)
      INTEGER IA(N+1), JA(2*N)


*
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the eigenvalues closest to 0 with ILU preconditioning'
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
      SIGMA=0.0d0
c     (ISEARCH.eq.2: SHIFT need not to be set)
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
c     maximum number of iteration steps
      ITER=1000
c     desired size of the search space
      MADSPACE=maxsp
c     tolerance for the eigenvector residual
      TOL=1.0d-10
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0


      do L=0,2
c        dynamically create matrix entries
         T=1.0D0+5.0d-2*L
         write (6,'(A,1P,E12.4)') 't=',T
         K=1
         DO I=1,N
            IA(I)=K
            JA(K)=I
            A(K)=T*DBLE(I-1)
            K=K+1
            IF (I .LT. N) THEN
               JA(K)=I+1
               A(K)=5.0D0
               K=K+1
            END IF
         END DO
         IA(N+1)=K

c        always re-init the following parameters since they are
c        overwritten by DPJD
c
c        iteration steps needed by the method
c        (ITER set by PJD to number of iteration steps actually needed)
         ITER=1000
c        number of wanted eigenvalues
c        (NEIG set by PJD to number of eigenvalues effectively computed)
         NEIG=maxeig
         CALL DPJD( N,A,JA,IA, EIGS, RES, X, LX, NEIG, 
     $              SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $              SHIFT, DROPTOL, MEM, ICNTL,
     $              IPRINT, INFO, GAP)

c        next time we enter DPJD, we continue with adaptive 
c        preconditioning based on the preconditioner that has been used
c        so far
         ICNTL(2)=-2
c        use previously computed eigenvectors as initial approximation
         NINIT=NEIG
      end do
c
c     release internal memory and discard preconditioner
      CALL DPJDCLEANUP
c
      END
