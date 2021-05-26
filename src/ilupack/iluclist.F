#include <ilupack_fortran.h>
#include <ilupackmacros.h>

      subroutine ILUCLIST(n,k,startk,jlu,jnext,Ulist,Ufirst)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,startk
      integer jlu(*),jnext(n),Ulist(n),Ufirst(k)
c-----------------------------------------------------------------------
c     update linked lists of U (resp. L)
c
c     n         size of the problem
c     k         current step of the update procedure
c     startk    start of row k of U
c     jlu       index array
c     jnext     pointers to the first space behind any row of U
c     Ulist     linked list for the nonzeros of U in column k
c     Ufirst    first nonzero entry in U(i,k:n)

      integer i,j,l
      
c     position of the first nonzero entry U(i,k) in column k. Note
c     that U is stored by rows!
      i=Ulist(k)
c     while i>0
 10      if (i.le.0) goto 20

c        does there exist an entry in U(i,k:n)?
         Ufirst(i)=Ufirst(i)+1
         l=Ufirst(i)
c        are there nonzeros leftover?
         if (l.lt.jnext(i)) then
            j=jlu(l)
c           save the old successor of i
            l=Ulist(i)
c           add i to the linked list of column j
            Ulist(i)=Ulist(j)
            Ulist(j)=i
c           restore the old successor
            i=l
         else
            i=Ulist(i)
         end if
      goto 10
c     end while

c     first nonzero in new row k of U
 20   i=k
      Ufirst(i)=startk
      l=Ufirst(i)
c     if there are nonzeros leftover
      if (l.lt.jnext(i)) then
         j=jlu(l)
c        add i to the linked list of column j
         Ulist(i)=Ulist(j)
         Ulist(j)=i
      end if
      return
c----------------end-of-ILUCLIST----------------------------------------
c-----------------------------------------------------------------------
      end
