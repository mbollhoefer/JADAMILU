#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c-----------------------------------------------------------------------
      subroutine DIAMUA (nrow, a, ja, ia, diag)
      implicit none
      integer nrow, ja(*),ia(nrow+1)
      FLOAT a(*), diag(nrow)
c-----------------------------------------------------------------------
c performs the matrix by matrix product A = Diag * A  (in place) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c diag = diagonal matrix stored as a vector dig(1:n)
c
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c
c     code taken from SPARSKIT of Yousef Saad.
c     adapted by Matthias Bollhoefer for the complex case
c-----------------------------------------------------------------
      FLOAT scal
      integer ii,k,k1,k2

      do 1 ii=1,nrow
c     
c     normalize each row 
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         scal = diag(ii) 
         do 2 k=k1, k2
            a(k) = a(k)*scal
 2       continue
 1    continue
c     
      return
c----------end-of-diamua------------------------------------------------
c-----------------------------------------------------------------------
      end 
c----------------------------------------------------------------------- 



c----------------------------------------------------------------------- 
      subroutine AMUDIA (nrow, a, ja, ia, diag)
      implicit none
      integer nrow, ja(*), ia(nrow+1)
      FLOAT a(*), diag(nrow) 
c-----------------------------------------------------------------------
c performs the matrix by matrix product A = A * Diag  (in place) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c diag = diagonal matrix stored as a vector dig(1:n)
c
c	    
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c-----------------------------------------------------------------
      integer ii,k,k1,k2

      do 1 ii=1,nrow
c     
c     scale each element 
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            a(k) = a(k)*diag(ja(k)) 
 2       continue
 1    continue
      return
c-----------------------------------------------------------------------
c-----------end-of-amudiag----------------------------------------------
      end 
c----------------------------------------------------------------------- 



c-----------------------------------------------------------------------
      subroutine RNRMS(nrow, nrm, a, ja, ia, diag) 
      implicit none
      integer nrow, nrm
      FLOAT a(*), diag(nrow)
      integer ja(*), ia(nrow+1) 
c-----------------------------------------------------------------------
c gets the norms of each row of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c
c-----------------------------------------------------------------
      integer ii, k,k1,k2
      REALS scal, SQRTFNAME, ABSFNAME
      REALS zero, one, two
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_
      parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
#else
      parameter(zero=0.0D0, one=1.0D0, two=2.0D0)
#endif

      do 1 ii=1,nrow
c
c     compute the norm if each element.
c     
         scal = zero
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         if (nrm .eq. 0) then
            do 2 k=k1, k2
               scal = max(scal,ABS(a(k))) 
 2          continue
         elseif (nrm .eq. 1) then
            do 3 k=k1, k2
               scal = scal + ABS(a(k)) 
 3          continue
         else
            do 4 k=k1, k2
               scal = scal+ABS(a(k))**2
 4          continue
         endif 
         if (nrm .eq. 2) scal = SQRT(scal) 
         diag(ii) = scal
 1    continue
      return
c-----------------------------------------------------------------------
c-------------end-of-rnrms----------------------------------------------
      end 
c----------------------------------------------------------------------- 



c----------------------------------------------------------------------- 
      subroutine CNRMS   (nrow, nrm, a, ja, ia, diag) 
      implicit none
      integer nrow, nrm
      FLOAT a(*), diag(nrow) 
      integer ja(*), ia(nrow+1) 
c-----------------------------------------------------------------------
c gets the norms of each column of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c
c-----------------------------------------------------------------
      integer ii, j,k,k1,k2
      REALS scal, SQRTFNAME, ABSFNAME
      
      do 10 k=1, nrow 
         diag(k) = FNULL
 10   continue
      do 1 ii=1,nrow
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            j = ja(k) 
c     update the norm of each column
            scal=ABS(diag(j))
            if (nrm .eq. 0) then
               scal = max(scal,ABS(a(k))) 
            elseif (nrm .eq. 1) then
               scal = scal + ABS(a(k)) 
            else
               scal = scal + ABS(a(k))**2
            endif 
            diag(j)=scal
 2       continue
 1    continue
      if (nrm .ne. 2) return
      do 3 k=1, nrow
         diag(k) = SQRT(CTOD(diag(k)))
 3    continue
      return
c-----------------------------------------------------------------------
c------------end-of-cnrms-----------------------------------------------
      end 
c----------------------------------------------------------------------- 



c----------------------------------------------------------------------- 
      subroutine ROWSCALE(nrow,nrm,a,ja,ia,diag,ierr) 
      implicit none
      integer nrow,nrm,ja(*),ia(nrow+1),ierr 
      FLOAT a(*), diag(nrow) 
c-----------------------------------------------------------------------
c scales the rows of A such that their norms are one on return
c 3 choices of norms: 1-norm, 2-norm, max-norm.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the rows have been scaled, i.e., on return 
c        we have A = Diag*A.
c
c	    
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Row number i is a zero row.
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c-----------------------------------------------------------------
      integer i,j,pow

      REALS zero, one, two
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_
      parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
#else
      parameter(zero=0.0D0, one=1.0D0, two=2.0D0)
#endif
      REALS log2, fpow, LOGFNAME, ABSFNAME

      log2=one/LOG(two)

      call RNRMS (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. FNULL) then
            ierr = j 
            return
         else
            fpow=LOG(ABS(diag(j)))*log2
            if (fpow.lt.zero) then
               pow=fpow-0.5
               fpow=one
               do i=1,-pow
                  fpow=fpow*two
               enddo
               diag(j)=fpow
            else
               pow=fpow+0.5
               fpow=one
               do i=1,pow
                  fpow=fpow*two
               enddo
               diag(j) = FONE/fpow
            endif
         endif
 1    continue
      call DIAMUA(nrow,a,ja,ia,diag)
      return
c-------end-of-rowscale-------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------



c----------------------------------------------------------------------- 
      subroutine COLSCALE(nrow,nrm,a,ja,ia,diag,ierr) 
c----------------------------------------------------------------------- 
      implicit none
      integer nrow,nrm,ja(*),ia(nrow+1),ierr 
      FLOAT a(*),diag(nrow) 
c-----------------------------------------------------------------------
c scales the columns of A such that their norms are one on return
c result matrix written on b, or overwritten on A.
c 3 choices of norms: 1-norm, 2-norm, max-norm. in place.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the columns have been scaled, i.e., on return 
c        we have A = A * Diag
c
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Column number i is a zero row.
c Notes:
c-------
c 1)     The column dimension of A is not needed. 
c-----------------------------------------------------------------
      integer i,j,pow

      REALS log2, fpow, LOGFNAME, ABSFNAME
      REALS zero, one, two
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_
      parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
#else
      parameter(zero=0.0D0, one=1.0D0, two=2.0D0)
#endif

      log2=one/LOG(two)

      call CNRMS (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. FNULL) then
            ierr = j 
            return
         else
            fpow=LOG(ABS(diag(j)))*log2
            if (fpow.lt.zero) then
               pow=fpow-0.5
               fpow=one
               do i=1,-pow
                  fpow=fpow*two
               enddo
               diag(j)=fpow
            else
               pow=fpow+0.5
               fpow=one
               do i=1,pow
                  fpow=fpow*two
               enddo
               diag(j) = FONE/fpow
            endif
         endif
 1    continue
      call AMUDIA (nrow,a,ja,ia,diag)
      return
c--------end-of-colscale------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
