#include <ilupack_fortran.h>
#include <ilupackmacros.h>




      subroutine piluclist(n,k,jlu,jnext,Llist,Lfirst)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k
      integer jlu(*),jnext(n),Llist(n),Lfirst(k)
c-----------------------------------------------------------------------
c     update linked lists of L 
c
c     n         size of the problem
c     k         current step of the update procedure
c     jlu       index array
c     jnext     pointers to the first space behind any column of L
c     Llist     linked list for the nonzeros of L in row k
c     Lfirst    first nonzero entry in L(i,k:n)

      integer i,j,l
      
c     position of the first nonzero entry L(i,k) in column k. Note
c     that L is stored by columns!
      i=Llist(k)
c     while i>0
 10      if (i.le.0) goto 20

c        does there exist an entry in L(i,k:n)?
         Lfirst(i)=Lfirst(i)+1
         l=Lfirst(i)
c        are there nonzeros leftover?
         if (l.lt.jnext(i)) then
            j=jlu(l)
c           save the old successor of i
            l=Llist(i)
c           add i to the linked list of row j
            Llist(i)=Llist(j)
            Llist(j)=i
c           restore the old successor
            i=l
         else
            i=Llist(i)
         end if
      goto 10
c     end while

 20   return
c----------------end-of-piluclist---------------------------------------
c-----------------------------------------------------------------------
      end



      subroutine GNLAGGRESSIVEDROPPING(n,k,alu,jlu,ju,Lnext,Unext,
     +                                 Lfirst,Lfirste,Ufirst,Ufirste,
     +                                 Llen,Llene,Ulen,Ulene,condestL,
     +                                 condestU,droptol,flag)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,jlu(*),ju(n),Lnext(n),Unext(n),
     +        Lfirst(n),Lfirste(n),Ufirst(n),Ufirste(n),
     +        Llen(n),Llene(n),Ulen(n),Ulene(n),flag 
      FLOAT   alu(*)
      REALS   droptol,condestL,condestU
c----------------------------------------------------------------------*
c
c     n          size of the problem
c     k          current finished step of the ILU
c     alu        numerical values
c     jlu        associated indices
c     ju         pointers to the beginning of each row of U in the 
c                matrix alu,jlu.
c
c     Lnext      pointers to the first space behind any column of L
c                (behind the regular entries)
c     Unext      pointers to the first space behind any row of U 
c                (behind the regular entries)
c
c     Lfirst     first nonzero entry in L(k:n,i), regular part
c     Lfirste    first nonzero entry in L(k:n,i), epsilon size part
c     Ufirst     first nonzero entry in U(i,k:n), regular part
c     Ufirste    first nonzero entry in U(i,k:n), epsilon size part
c
c     Llen       number of skipped entries, located at the start of
c                every column of L
c     Llene      number of skipped entries, located at the start of
c                every column of L, epsilon size part
c     Ulen       number of skipped entries, located at the start of
c                every row of U
c     Ulene      number of skipped entries, located at the start of
c                every row of U, epsilon size part
c
c     condestL   estimate or bound for ||L^{-1}||
c     condestU   estimate or bound for ||U^{-1}||
c     droptol    drop tolerance
c     flag       if nonzero, then Tismenetsky Schur complement is used

      integer i,j,shift,m,jj,p,count,q
      REALS   droptolLU



#ifdef PRINT_INFO
      write (6,'(A)') 'apply aggressive dropping'
      write (6,'(A)') 'old L'
      do i=1,k
         write (6,'(A,I4)') 'column ',i
         write (6,'(10I8)') (jlu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(10I8)') (jlu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Lnext(i)+Llene(i),ju(i)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Lnext(i)+Llene(i),ju(i)-1)
         end if      
      end do

      write (6,'(A)') 'old U'
      do i=1,k
         write (6,'(A,I4)') 'row ',i
         write (6,'(10I8)') (jlu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(10I8)') (jlu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
         end if      
      end do

      write (6,'(A)') 'old Lfirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirst(i)
      end do
      write (6,'(A)') 'old Ufirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirst(i)
      end do

      write (6,'(A)') 'old jlu'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',jlu(i)
      end do
      write (6,'(A)') 'old ju'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',ju(i)
      end do

      if (flag.eq.0) goto 99
      write (6,'(A)') 'old Lfirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirste(i)
      end do
      write (6,'(A)') 'old Ufirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirste(i)
      end do
 99   continue
#endif


      count=0
      shift=0
      do i=1,k
         p=shift
c        shift skipped part of L(i+1:n,i)
         do j=jlu(i),jlu(i)+Llen(i)-1
            jlu(j-shift)=jlu(j)
            alu(j-shift)=alu(j)
         end do

         droptolLU=droptol/condestL
         q=Lnext(i)-jlu(i)-Llen(i)
         if (q.gt.0) droptolLU=droptolLU/q
#ifdef PRINT_INFO
         write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolL=',droptolLU
#endif

c        aggressive dropping applied to L(1:k,i)
         m=0
         do j=jlu(i)+Llen(i),Lnext(i)-1
            if (ABS(alu(j)).le.droptolLU .and. jlu(j).le.k) then
c               write (6,'(A,I4,A,I4,A)')'discard (',jlu(j),',',i,')'
               count=count+1
               shift=shift+1
               m=m+1
            else
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end if
         end do           
         Lfirst(i)=Lfirst(i)-shift
         jlu(i)=jlu(i)-p

c        epsilon size part
         p=shift
         if (flag.gt.0) then
c           shift skipped part of L(i+1:n,i)
            do j=Lnext(i),Lnext(i)+Llene(i)-1
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end do

c           aggressive dropping applied to L(1:k,i)
            m=0
            droptolLU=droptol/condestL
            q=ju(i)-Lnext(i)-Llene(i)
            if (q.gt.0) droptolLU=droptolLU/q
            do j=Lnext(i)+Llene(i),ju(i)-1
               if (ABS(alu(j)).le.droptolLU .and. jlu(j).le.k)then
c                  write (6,'(A,I4,A,I4,A)')
c     +                  'discard (',jlu(j),',',i,')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jlu(j-shift)=jlu(j)
                  alu(j-shift)=alu(j)
               end if
            end do          
            Lfirste(i)=Lfirste(i)-shift
         end if
         Lnext(i)=Lnext(i)-p        


         p=shift
c        shift skipped part of U(i,i+1:n)
         do j=ju(i),ju(i)+Ulen(i)-1
            jlu(j-shift)=jlu(j)
            alu(j-shift)=alu(j)
         end do

         droptolLU=droptol/condestU
         q=Unext(i)-ju(i)-Ulen(i)
         if (q.gt.0) droptolLU=droptolLU/q
#ifdef PRINT_INFO
         write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolLU
#endif

c        aggressive dropping applied to U(i,1:k)
         m=0
         do j=ju(i)+Ulen(i),Unext(i)-1
            if (ABS(alu(j)).le.droptolLU .and. jlu(j).le.k) then
c               write (6,'(A,I4,A,I4,A)')
c     +               'discard (',i,',',jlu(j),')'
               count=count+1
               shift=shift+1
               m=m+1
            else
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end if
         end do           
         Ufirst(i)=Ufirst(i)-shift
         ju(i)=ju(i)-p

c        epsilon size part
         p=shift
         if (flag.gt.0) then
c           shift skipped part of U(i,i+1:n)
            do j=Unext(i),Unext(i)+Ulene(i)-1
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end do

c           aggressive dropping applied to U(i,1:k)
            m=0
            droptolLU=droptol/condestU
            q=jlu(i+1)-Unext(i)-Ulene(i)
            if (q.gt.0) droptolLU=droptolLU/q
            do j=Unext(i)+Ulene(i),jlu(i+1)-1
               if (ABS(alu(j)).le.droptolLU .and. jlu(j).le.k)then
c                  write (6,'(A,I4,A,I4,A)')
c     +                  'discard (',i,',',jlu(j),')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jlu(j-shift)=jlu(j)
                  alu(j-shift)=alu(j)
               end if
            end do          
            Ufirste(i)=Ufirste(i)-shift
         end if
         Unext(i)=Unext(i)-p        

      end do
      jlu(i)=jlu(i)-shift

#ifdef PRINT_INFO
      write (6,'(A)') 'aggressive dropping applied'
      write (6,'(A)') 'new L'
      do i=1,k
         write (6,'(A,I4)') 'column ',i
         write (6,'(10I8)') (jlu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(10I8)') (jlu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Lnext(i)+Llene(i),ju(i)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Lnext(i)+Llene(i),ju(i)-1)
         end if      
      end do

      write (6,'(A)') 'new U'
      do i=1,k
         write (6,'(A,I4)') 'row ',i
         write (6,'(10I8)') (jlu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(10I8)') (jlu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
         end if      
      end do

      write (6,'(A)') 'new Lfirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirst(i)
      end do
      write (6,'(A)') 'new Ufirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirst(i)
      end do

      write (6,'(A)') 'new jlu'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',jlu(i)
      end do
      write (6,'(A)') 'new ju'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',ju(i)
      end do

      if (flag.eq.0) goto 199
      write (6,'(A)') 'new Lfirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirste(i)
      end do
      write (6,'(A)') 'new Ufirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirste(i)
      end do
 199  continue
#endif

    
 900  write (6,'(I6,A)') count,' entries discarded'
 901  return
c----------------end-of-GNLAGGRESSIVEDROPPING----------------------------
c---------------------------------------------------------------------
      end



      subroutine GNLFAGGRESSIVEDROPPING(n,k,alu,jlu,ju,Lnext,Unext,
     +                                  Lfirst,Lfirste,Ufirst,Ufirste,
     +                                  Llen,Llene,Ulen,Ulene,condestL,
     +                                  condestU,droptol,flag)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,jlu(*),ju(n),Lnext(n),Unext(n),
     +        Lfirst(n),Lfirste(n),Ufirst(n),Ufirste(n),
     +        Llen(n),Llene(n),Ulen(n),Ulene(n),flag 
      FLOAT   alu(*),condestL(n),condestU(n)
      REALS   droptol
c----------------------------------------------------------------------*
c
c     n          size of the problem
c     k          current finished step of the ILU
c     alu        numerical values
c     jlu        associated indices
c     ju         pointers to the beginning of each row of U in the 
c                matrix alu,jlu.
c
c     Lnext      pointers to the first space behind any column of L
c                (behind the regular entries)
c     Unext      pointers to the first space behind any row of U 
c                (behind the regular entries)
c
c     Lfirst     first nonzero entry in L(k:n,i), regular part
c     Lfirste    first nonzero entry in L(k:n,i), epsilon size part
c     Ufirst     first nonzero entry in U(i,k:n), regular part
c     Ufirste    first nonzero entry in U(i,k:n), epsilon size part
c
c     Llen       number of skipped entries, located at the start of
c                every column of L
c     Llene      number of skipped entries, located at the start of
c                every column of L, epsilon size part
c     Ulen       number of skipped entries, located at the start of
c                every row of U
c     Ulene      number of skipped entries, located at the start of
c                every row of U, epsilon size part
c
c     condestL   estimate or bound for ||L^{-1}||
c     condestU   estimate or bound for ||U^{-1}||
c     droptol    drop tolerance
c     flag       if nonzero, then Tismenetsky Schur complement is used

      integer i,j,shift,m,jj,p,count,q
      REALS   droptolLU



#ifdef PRINT_INFO
      write (6,'(A)') 'apply aggressive dropping'
      write (6,'(A)') 'old L'
      do i=1,k
         write (6,'(A,I4)') 'column ',i
         write (6,'(10I8)') (jlu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(10I8)') (jlu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Lnext(i)+Llene(i),ju(i)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Lnext(i)+Llene(i),ju(i)-1)
         end if      
      end do

      write (6,'(A)') 'old U'
      do i=1,k
         write (6,'(A,I4)') 'row ',i
         write (6,'(10I8)') (jlu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(10I8)') (jlu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
         end if      
      end do

      write (6,'(A)') 'old Lfirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirst(i)
      end do
      write (6,'(A)') 'old Ufirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirst(i)
      end do

      write (6,'(A)') 'old jlu'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',jlu(i)
      end do
      write (6,'(A)') 'old ju'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',ju(i)
      end do

      if (flag.eq.0) goto 99
      write (6,'(A)') 'old Lfirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirste(i)
      end do
      write (6,'(A)') 'old Ufirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirste(i)
      end do
 99   continue
#endif


      count=0
      shift=0
      do i=1,k
         p=shift
c        shift skipped part of L(i+1:n,i)
         do j=jlu(i),jlu(i)+Llen(i)-1
            jlu(j-shift)=jlu(j)
            alu(j-shift)=alu(j)
         end do

         droptolLU=droptol
         q=Lnext(i)-jlu(i)-Llen(i)
         if (q.gt.0) droptolLU=droptolLU/q
#ifdef PRINT_INFO
         write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolL=',droptolLU
#endif

c        aggressive dropping applied to L(1:k,i)
         m=0
         do j=jlu(i)+Llen(i),Lnext(i)-1
c           column index q of L(q,i)
            q=jlu(j)
            if (ABS(alu(j))*CTOD(condestL(q)).le.droptolLU 
     +          .and. jlu(j).le.k) then
c               write (6,'(A,I4,A,I4,A)')'discard (',jlu(j),',',i,')'
               count=count+1
               shift=shift+1
               m=m+1
            else
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end if
         end do           
         Lfirst(i)=Lfirst(i)-shift
         jlu(i)=jlu(i)-p

c        epsilon size part
         p=shift
         if (flag.gt.0) then
c           shift skipped part of L(i+1:n,i)
            do j=Lnext(i),Lnext(i)+Llene(i)-1
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end do

c           aggressive dropping applied to L(1:k,i)
            m=0
            droptolLU=droptol
            q=ju(i)-Lnext(i)-Llene(i)
            if (q.gt.0) droptolLU=droptolLU/q
            do j=Lnext(i)+Llene(i),ju(i)-1
c              row index q of L(q,i)
               q=jlu(j)
               if (ABS(alu(j))*CTOD(condestL(q)).le.droptolLU 
     +              .and. jlu(j).le.k) then
c                  write (6,'(A,I4,A,I4,A)')
c     +                  'discard (',jlu(j),',',i,')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jlu(j-shift)=jlu(j)
                  alu(j-shift)=alu(j)
               end if
            end do          
            Lfirste(i)=Lfirste(i)-shift
         end if
         Lnext(i)=Lnext(i)-p        


         p=shift
c        shift skipped part of U(i,i+1:n)
         do j=ju(i),ju(i)+Ulen(i)-1
            jlu(j-shift)=jlu(j)
            alu(j-shift)=alu(j)
         end do

         droptolLU=droptol
         q=Unext(i)-ju(i)-Ulen(i)
         if (q.gt.0) droptolLU=droptolLU/q
#ifdef PRINT_INFO
         write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolLU
#endif

c        aggressive dropping applied to U(i,1:k)
         m=0
         do j=ju(i)+Ulen(i),Unext(i)-1
c              column index q of U(i,q)
               q=jlu(j)
               if (ABS(alu(j))*CTOD(condestU(q)).le.droptolLU 
     +             .and. jlu(j).le.k) then
c               write (6,'(A,I4,A,I4,A)')
c     +               'discard (',i,',',jlu(j),')'
               count=count+1
               shift=shift+1
               m=m+1
            else
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end if
         end do           
         Ufirst(i)=Ufirst(i)-shift
         ju(i)=ju(i)-p

c        epsilon size part
         p=shift
         if (flag.gt.0) then
c           shift skipped part of U(i,i+1:n)
            do j=Unext(i),Unext(i)+Ulene(i)-1
               jlu(j-shift)=jlu(j)
               alu(j-shift)=alu(j)
            end do

c           aggressive dropping applied to U(i,1:k)
            m=0
            droptolLU=droptol
            q=jlu(i+1)-Unext(i)-Ulene(i)
            if (q.gt.0) droptolLU=droptolLU/q
            do j=Unext(i)+Ulene(i),jlu(i+1)-1
c              column index q of U(i,q)
               q=jlu(j)
               if (ABS(alu(j))*CTOD(condestU(q)).le.droptolLU 
     +             .and. jlu(j).le.k) then
c                  write (6,'(A,I4,A,I4,A)')
c     +                  'discard (',i,',',jlu(j),')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jlu(j-shift)=jlu(j)
                  alu(j-shift)=alu(j)
               end if
            end do          
            Ufirste(i)=Ufirste(i)-shift
         end if
         Unext(i)=Unext(i)-p        

      end do
      jlu(i)=jlu(i)-shift

#ifdef PRINT_INFO
      write (6,'(A)') 'aggressive dropping applied'
      write (6,'(A)') 'new L'
      do i=1,k
         write (6,'(A,I4)') 'column ',i
         write (6,'(10I8)') (jlu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=jlu(i),jlu(i)+Llen(i)-1)
         write (6,'(10I8)') (jlu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=jlu(i)+Llen(i),Lnext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Lnext(i),Lnext(i)+Llene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Lnext(i)+Llene(i),ju(i)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Lnext(i)+Llene(i),ju(i)-1)
         end if      
      end do

      write (6,'(A)') 'new U'
      do i=1,k
         write (6,'(A,I4)') 'row ',i
         write (6,'(10I8)') (jlu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(1P,10E8.1)')
     +        (alu(j), j=ju(i),ju(i)+Ulen(i)-1)
         write (6,'(10I8)') (jlu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         write (6,'(1P,10E8.1)') 
     +        (alu(j), j=ju(i)+Ulen(i),Unext(i)-1)
         if (flag.ne.0) then
            write (6,'(10I8)') (jlu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(1P,10E8.1)')
     +           (alu(j), j=Unext(i),Unext(i)+Ulene(i)-1)
            write (6,'(10I8)') (jlu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +           (alu(j), j=Unext(i)+Ulene(i),jlu(i+1)-1)
         end if      
      end do

      write (6,'(A)') 'new Lfirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirst(i)
      end do
      write (6,'(A)') 'new Ufirst'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirst(i)
      end do

      write (6,'(A)') 'new jlu'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',jlu(i)
      end do
      write (6,'(A)') 'new ju'
      do i=1,k+1
         write (6,'(I2,A,I4)') i,',    ',ju(i)
      end do

      if (flag.eq.0) goto 199
      write (6,'(A)') 'new Lfirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Lfirste(i)
      end do
      write (6,'(A)') 'new Ufirste'
      do i=1,k
         write (6,'(I2,A,I4)') i,',    ',Ufirste(i)
      end do
 199  continue
#endif

    
 900  write (6,'(A,I6,A)') 'additional ',count,' entries discarded'     
 901  return
c----------------end-of-GNLFAGGRESSIVEDROPPING----------------------------
c---------------------------------------------------------------------
      end
