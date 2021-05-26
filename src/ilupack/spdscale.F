#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c----------------------------------------------------------------------- 
      subroutine SPDSCALE(n,a,ja,ia,diag,ierr)  
      implicit none
      integer n,ja(*),ia(n+1),ierr 
      FLOAT a(*), diag(n) 
c-----------------------------------------------------------------------
c scales an SPD matrix such that it becomes (approximately) unit diagonal
c-----------------------------------------------------------------------
c on entry:
c ---------
c n    = size of the matrix A
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the rows/columns have been scaled, i.e., on return 
c        we have A = Diag*A*Diag.
c
c	    
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Row number i is a zero row.
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c
c     code adapted from DIAMUA, AMUDIA of Yousef Saad's SPARSKIT.
c     adapted by Matthias Bollhoefer 
c-----------------------------------------------------------------
      integer i,j,pow

      REALS log2, fpow
      REALS SQRTFNAME, ABSFNAME, LOGFNAME
      REALS zero, one, two
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_
      parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
#else
      parameter(zero=0.0D0, one=1.0D0, two=2.0D0)
#endif

      

      log2=one/LOG(two)

      do j=1, n
         diag(j)=FNULL
         do i=ia(j),ia(j+1)-1
            if (ja(i).eq.j) diag(j)=SQRT(ABS(a(i)))
         enddo
      enddo
      ierr = 0
      do 1 j=1, n
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
      call DIAMUA(n,a,ja,ia,diag)
      call AMUDIA(n,a,ja,ia,diag)
      return
c--------end-of-spdscale------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
