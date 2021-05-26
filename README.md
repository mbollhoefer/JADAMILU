THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED 
OR IMPLIED. ANY USE IS AT YOUR OWN RISK. 

Using JADAMILU is free for non commercial applications 
(For commercial use, please contact the authors).  

You can acknowledge, using reference 

M. Bollhoefer, Y. Notay,
  JADAMILU: a software code for computing selected eigenvalues 
  of large sparse symmetric matrices, 
  Computer Physics Communications, 2007.

the contribution of this package in any scientific publication
dependent upon the use of the package. 
You shall use reasonable endeavors to notify the authors of 
the package of this publication. 

----------------------------------------------------------------------

You read this file because you have successfully downloaded the
jadamilu.tgz file corresponding to your compiler and architecture,
and used the command

> tar zxvf jadamilu.tgz

to uncompress the package.
Assumed you did this in directory /MYDIR 
(for instance, your home directory)
This created a directory /MYDIR/JADAMILU and the present file is
/MYDIR/JADAMILU/README

You will find the documentation in /MYDIR/JADAMILU/Doc/ (a userguide giving
additional information to the CPC paper referred above).

In /MYDIR/JADAMILU/Doc/DPJD.f (resp. SPJD.f, CPJD.f, ZPJD.f) you will 
find the source of the main driver routines, that is, of the routines 
your application program may call to use JADAMILU. 

The user guide refers to several examples. The related sources are located
in /MYDIR/JADAMILU/samples/

The file /MYDIR/JADAMILU/Doc/DPJD.f (resp. SPJD.f, CPJD.f, ZPJD.f) is 
only given to you for illustration purpose. All routines and subroutines
have been precompiled for you and stored in a library 
/MYDIR/JADAMILU/lib/ArchComp/libjadamilu.a , where ArchComp reflects the 
architecture of the processor, the operating system, the storage mode 
for integers and the compiler you plan to use, according the table at 
the bottom of this README file.
Besides, you will find /MYDIR/JADAMILU/lib/ArchComp/libmylapack.a and
/MYDIR/JADAMILU/lib/ArchComp/libmyblas.a , which contain the part of the
LAPACK  and BLAS libraries needed by JADAMILU.

To compile any program using JADAMILU, for instance the examples in
/MYDIR/JADAMILU/samples/ , go to the directory where the program is 
located and use the command

> mycompiler myprog.f -L/MYDIR/JADAMILU/lib/ArchComp -ljadamilu -lmylapack -lmyblas

where mycompiler stands for the Fortran compiler matching ArchComp.  
For instance, if you have an Intel processor with 32 bit architecture
and use gfortran compiler, EXAMPLE1.f will be compiled with

> gfortran EXAMPLE1.f -L/MYDIR/JADAMILU/lib/INT32GNU -ljadamilu -lmylapack -lmyblas

Of course, you may insert your favorite options, like "-O" for optimization
or "-o myprog" so that the executable has name "myprog" instead of a.out

Note that if the compiler suite contains a Fortran 90 compiler, 
mycompiler may be this Fortran 90 compiler and myprog may have 
extension .f90 ; for instance, with pgf:

> pgf90 myprog.f90 -L/MYDIR/JADAMILU/lib/ArchComp -ljadamilu -lmylapack -lmyblas

Note that LAPACK and BLAS are standard libraries, and optimized versions
are installed on many systems. If this is true for your system, you
may compile your code with

> mycompiler myprog.f -L/MYDIR/JADAMILU/lib/ArchComp -ljadamilu -llapack -lblas

Then, the system version of LAPACK and BLAS will be used instead of
the version provided with JADAMILU. Possibly (according the way system
versions have been installed), this requires an additional
-L/LAPACKBLASDIR option to tell the compiler where to find these
system LAPACK and BLAS (see your system administrator for details).

Of course, these are simple examples. JADAMILU may also be used in complex
applications with programs split in several files and possibly built
with makefile. It is easy to deduce how to proceed from the examples
above, in particular how to adapt possible Makefile

________________________________________________________________

TABLE of ArchComp
-----------------

To know your OS, enter
> uname -o

To know your processor, ask your system administrator or check the Website
describing your system confifuration.

To know whether your system use 32 or 64 bit addressing, type
> uname -p
If the answer ends with _64, you have 64 bit addressing, otherwise 32.

When your processor use 64 bit addressing, you may use long integer
(integer*8, i.e., 64 bit integer) in your application; 
sometimes this is done with the -i8 option flag given to the compiler. 
If the integer passed to JADAMILU top level routines are such long integer
(for instance if you use the -i8 flag), select the version of the library
with yes in the fourth column; otherwise, select the version with no.


   OS 	processor 	32/64 bit   integer=	  compiler	ArchComp 
                        addressing   integer*8       
  
Linux 	Intel/AMD	   32		no	     gfortran	INT32GNU
Linux 	Intel/AMD	   32		no	     ifort	INT32GNU
Linux 	Intel/AMD	   32		no	     pgf	INT32pgf
 
Linux 	Intel EM64T	   64		no     	     ifort 	EM64Nifort
Linux 	Intel EM64T	   64		yes 	     ifort 	EM64Yifort
 
Linux 	AMD Opteron	   32	 	no 	     gfortran 	OPT32GNU
Linux 	AMD Opteron	   64		no 	     gfortran 	OPT64NGNU
 
Linux 	AMD Opteron	   32		no 	     pgf 	OPT32pgf
Linux 	AMD Opteron	   64		no 	     pgf 	OPT64Npgf
Linux 	AMD Opteron	   64		yes 	     pgf 	OPT64Ypgf
 
Linux 	AMD Opteron	   32		no 	     ifort 	OPT32ifort
Linux 	AMD Opteron	   64		no 	     ifort 	OPT64Nifort
Linux 	AMD Opteron	   64		yes 	     ifort 	OPT64Yifort
 
AIX 	Power		   64		no  	     xlf90 	AIXxlf90


________________________________________________________________


Advanced usage
---------------

If you have problems to use one of the precompiled libraries or if the list
of compilers / operating systems does not match your requirements, then you
can manually recompile the JADAMILU library.

To do that, enter the directory /MYDIR/JADAMILU/src/

There you will find a 

       makefile 

a file called 

       user.mk

and several platform-depend suggestions in /MYDIR/JADAMILU/src/makefiles called

       makefile.ArchComp

where "ArchComp" refers to the existing platforms, mentioned earlier.
Here you can preselect you own compiler and settings. The file 
"makefile.ArchComp" is included from "makefile". 
In the easiest case you simply choose your platform and compiler from 
"user.mk". If necessary you may alter "makefile.ArchComp" make your 
private settings.



However note that we can only distribute our own source codes. External
software is not included.




Here is a list of the following external software codes.


1. MC64
C COPYRIGHT (c) 1999 Council for the Central Laboratory
*                    of the Research Councils
CCCCC PACKAGE MC64A/AD
CCCCC AUTHORS Iain Duff (i.duff@rl.ac.uk) and Jacko Koster (jak@ii.uib.no)
CCCCC LAST UPDATE 20/09/99
CCCCC
C *** Conditions on external use ***
C
C The user shall acknowledge the contribution of this
C package in any publication of material dependent upon the use of
C the package. The user shall use reasonable endeavours to notify
C the authors of the package of this publication.
C
C The user can modify this code but, at no time
C shall the right or title to all or any part of this package pass
C to the user. The user shall make available free of charge
C to the authors for any purpose all information relating to any
C alteration or addition made to this package for the purposes of
C extending the capabilities or enhancing the performance of this
C package.
C
C The user shall not pass this code directly to a third party without
C the express prior consent of the authors.  Users wanting to licence
C their own copy of these routines should send email to hsl@aeat.co.uk
C
C None of the comments from the Copyright notice up to and including
C this one shall be removed or altered in any way.



2. AMD
   AMD is a set of routines for ordering a sparse matrix prior to Cholesky 
   factorization (or for LU factorization with diagonal pivoting). 

   Copyright (c) 2004-2006 by Timothy A. Davis, Patrick R. Amestoy, and
   Iain S. Duff. All Rights Reserved. Distributed under the GNU LGPL license. 



3. BLAS
   The reference BLAS is a freely-available software package. It is available
   from netlib via anonymous ftp and the World Wide Web. Thus, it can be 
   included in commercial software packages (and has been). We only ask that 
   proper credit be given to the authors. 

   Like all software, it is copyrighted. It is not trademarked, but we do ask
   the following: 

   If you modify the source for these routines we ask that you change the name
   of the routine and comment the changes made to the original. 

   The authors of BLAS  will gladly answer any questions regarding the 
   software. If a modification is done, however, it is the responsibility of
   the person who modified the routine to provide support. 



4. LAPACK
   The complete LAPACK package or individual routines from LAPACK are freely
   available on netlib and can be obtained via the World Wide Web or anonymous
   ftp. 

   The LAPACK homepage can be accessed on the World Wide Web via the URL 
   address:

             http://www.netlib.org/lapack/ 



