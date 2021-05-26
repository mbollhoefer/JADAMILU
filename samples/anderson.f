      program laplace
      implicit none


c     general parameters
      integer m,n
      parameter (m=100,n=m*m*m)
      integer ia(N+1),ja(4*n),nz
      doubleprecision a(4*n)
      intrinsic dsqrt, MAX
      REAL tic,toc, TIMEARRAY(2), totaltime


c     ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     ILUPACK parameters
c     cccccccccccccccccc
c     ILUPACK external parameters
      integer  k,l

      doubleprecision     droptol,  MEM

      external PJDINIT, PJDREVCOM, PJDCLEANUP, matveca

c     Jacobi-Davidson parameters
c     maxeig: maximum nmber of eigenvalues. Here 5
c     maxsp:  size of the search space.     Here maxeig*5
c     LX:     size of memory for array X
      integer maxeig, maxsp, LX
      parameter (maxeig=5,maxsp=25)
      parameter (LX=N*(3*maxsp+maxeig+1)+3*maxsp**2+maxsp**2)

c     NEIG:     number of desired eigenvalues
c     MADSPACE: desired size of the search space
c     ISEARCH:  Flag for JD control
c     NINIT:    Flag for initial approximate eigenvectors
      INTEGER            NEIG, NEIGOLD, MADSPACE, ISEARCH, NINIT

c     ITER: maximum number of iteration steps
c     INFO: error flag
c     IJOB: flag to control reverse communication
c     NDX1: position of source vector     \ for matrix-vector
c     NDX2: position of destination vector/ and preconditioning
c     i,i1,i2,j,i3: auxilliary variables
      INTEGER            ITER, INFO, IJOB, NDX1, NDX2, i,i1,i2,j,i3,
     +                   ICNTL(5), IERR, ITOL, IPRINT

c     SIGMA: shift to indicate the location of the desired eigenvalues
c     TOL:   tolerance for the eigenvector residual
      DOUBLE PRECISION   SIGMA, TOL, SHIFT

c     EIGS: vector of approximate eigenvalues
c     RES:  vector of eigenvector residual norms
c     X:    matrix of eigenvectors and buffer
      DOUBLE PRECISION   EIGS(maxeig), RES(maxeig), X(LX), GAP

******************************************************************************
****** EXAMPLE 1: The Anderson matrix, symmetric and highly indefinite
c     sample matrix for the Anderson model of localization
      call MakeAndersonMatrix(m,n,ia,ja,a,nz)
******************************************************************************
c


c     set input variables
c     the matrix is already in the required format
c
c          standard report on standard output
           IPRINT=-6
c
           ISEARCH=2
           SHIFT=0.0d0
c          provide an eigenvalue target (ignored if ISEARCH=0)
           SIGMA=0.0d0
c
c          elbow space factor for the fill computed during the ILU
           MEM=15.0
c          tolerence for discarded fill
           DROPTOL=1.d-3
c
           NEIG=5
           TOL=1.0d-10

c       no initial approximate eigenvectors
        NINIT=0
c       desired size of the search space
        MADSPACE=maxsp
c       maximum number of iteration steps
        ITER=5000

c       additional parameters set to default
        ICNTL(1)=0
        ICNTL(2)=1
        ICNTL(3)=0
        ICNTL(4)=0
        ICNTL(5)=0
c
 30   CONTINUE
       CALL DPJD( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $                       SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                       SHIFT, DROPTOL, MEM, ICNTL,
     $                       IPRINT, INFO, GAP)

 100  continue
*     come here to terminate
c     release memory that was allocated by ILUPACK

      call DPJDCLEANUP

      end

******************
c     set up matrix for the Anderson model of localization
      subroutine MakeAndersonMatrix(m,n,ia,ja,a,nz)
      integer y,ni,i,j,k,ElementsPut,n,m,nz
      integer ia(0:N),ja(0:4*N-1)
      real*8 a(0:4*N-1), bb
      integer nn(0:6)
      doubleprecision iprandom
      external iprandom, ipsrandom

      call ipsrandom(0)

      ElementsPut=0

      do k=0,M-1
       do j=0,M-1
        do i=0,M-1

         y=i+j*M+k*M*M

         nn(0)=y
         nn(1)=mod(i+1,M)+j*M+k*M*M
         nn(2)=mod(M+i-1,M)+j*M+k*M*M
         nn(3)=i+mod(j+1,M)*M+k*M*M
         nn(4)=i+mod(M+j-1,M)*M+k*M*M
         nn(5)=i+j*M+mod(k+1,M)*M*M
         nn(6)=i+j*M+mod(M+k-1,M)*M*M

         ElementsPut=ElementsPut+1
         ia(y)=ElementsPut
         ja(ElementsPut-1)=nn(0)+1
         bb = (iprandom(0)-0.5)
         if (bb.ge.0.0) then
            a(ElementsPut-1)=8.25 - bb*16.5
         else
            a(ElementsPut-1)=-8.25 - bb*16.5
         endif

         do ni=1,6
          if(nn(ni).gt.y)then
           ElementsPut=ElementsPut+1
           ja(ElementsPut-1)=nn(ni)+1
           a(ElementsPut-1)=1.0
          endif
         enddo

        enddo
       enddo
      enddo

      ia(N)=ElementsPut+1
      nz=ElementsPut
      return
      end
******************

