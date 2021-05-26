*======================================================================*
*
       SUBROUTINE PJD( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                 SHIFT, DROPTOL, MEM, ICNTL, IPRINT, INFO, 
     $                 GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, ICNTL(5), IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer            JA(*), IA(*)
      DOUBLE PRECISION   A(*), X(*)
      DOUBLE PRECISION   EIGS(*), RES(*)


      integer myicntl(5)

      myicntl(1)=icntl(1)
      myicntl(2)=icntl(2)
      myicntl(3)=icntl(3)
      myicntl(4)=icntl(4)
      myicntl(5)=0
      CALL dpjd( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $           SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $           SHIFT, DROPTOL, MEM, myicntl, IPRINT, INFO, GAP)
      icntl(1)=myicntl(1)
      icntl(2)=myicntl(2)
      icntl(3)=myicntl(3)
      icntl(4)=myicntl(4)

      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
       SUBROUTINE PJDREVCOM( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $                       SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                       SHIFT, DROPTOL, MEM, ICNTL,
     $                       IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, ICNTL(5), IJOB, NDX1, NDX2, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
*     ..
*     .. Array Arguments ..
      integer            ja(*), ia(*)
      DOUBLE PRECISION   a(*), X(*)
      DOUBLE PRECISION   EIGS(*), RES(*)

      integer myicntl(5)

      myicntl(1)=icntl(1)
      myicntl(2)=icntl(2)
      myicntl(3)=icntl(3)
      myicntl(4)=icntl(4)
      myicntl(5)=0
      call dpjdrevcom( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                 SHIFT, DROPTOL, MEM, myicntl,
     $                 IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      icntl(1)=myicntl(1)
      icntl(2)=myicntl(2)
      icntl(3)=myicntl(3)
      icntl(4)=myicntl(4)

      RETURN
      END
*
*======================================================================*
*
*======================================================================*
*
      SUBROUTINE JDREVCOM( N, EIGS, RES, X, LX, NEIG, 
     $                     SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                     IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, ISEARCH, NINIT, MADSPACE
      integer            ITER, IJOB, NDX1, NDX2, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, GAP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
      DOUBLE PRECISION   EIGS( *), RES(*)
      call djdrevcom( N, EIGS, RES, X, LX, NEIG, 
     $                SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
      RETURN
      END
*======================================================================*
*
*======================================================================*
*
      SUBROUTINE PJDCLEANUP
      call dpjdcleanup
      return
      end
*
*
*=============================================================================*




