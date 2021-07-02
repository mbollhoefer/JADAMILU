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

For more information please visit http://metronu.ulb.ac.be/JADAMILU/

----------------------------------------------------------------------



You will find the documentation in JADAMILU/Doc/ (a userguide giving
additional information to the CPC paper referred above).

In JADAMILU/Doc/DPJD_prototype.f (resp. SPJD_prototype.f, CPJD_prototype.f,
ZPJD_prototype.f) you will find for your information a copy of the source of the
main driver routines, that is, of the routines your application program may call
to use JADAMILU. 

The user guide refers to several examples. The related sources are located
in JADAMILU/samples/

The file JADAMILU/Doc/DPJD_prototype.f (resp. SPJD_prototype.f, CPJD_prototype.f,
ZPJD_prototype.f) is only given to you for illustration purpose. All routines and
subroutines have been precompiled for you and stored in a library 
JADAMILU/lib/ArchComp/libjadamilu.a , where ArchComp reflects the 
architecture of the processor, the operating system, the storage mode 
for integers and the compiler you plan to use, according the table at 
the bottom of this README file.



building the library
--------------------

To build the JADAMILU library, go to the JADAMILU/src directory and modify
"user.mk" in order to set up your most favoured compiler and architecture
according the table at the bottom of this README file. After that, type "make".
JADAMILU will compute the library "libjadamilu.a" which will be located at
JADAMILU/lib/ArchComp/.

Before you compile any sample code, you need to get the AMD library which is
part of SuiteSparse library of Tim Davis (Texas A&M UNiversity). Downloading and
compiling the SuiteSparse library will also provide libraries "libamd.a" and
"libsuitesparseconfig.a" (These are typically located in some subdirectory such
as SuiteSparse/AMD/Lib/ and SuiteSparse/SuiteSparse_config/). You could copy
these libraries (for simplicity) to JADAMILU/lib/ArchComp/ or refer differently
to them.

Furthermore you need your own BLAS and LAPACK library. Simple precompiled F77
sources are could be used, but these are likely to be slow. You better use some
vendor-specific libraries which highly optimized (if in doubt, ask your system
administrator for details).


Last but not least you need to get MC64 and MC21 from the HSL Mathematical
Software Library (https://www.hsl.rl.ac.uk/). For copyrights, terms of use, etc.,
we kindly refer to read carefully the their conditions and make sure that they
apply to you. You could compile these sources in the same way as the library has
been built including options for long integer, position independent code, memory
model, "-c" etc. Then copy the object files MC*.o to  JADAMILU/lib/ArchComp/
For example, if you are using the GNU compiler and you want to use 64 bit
long integer, then use a command such as
> gfortran -O -fPIC -m64 -fdefault-integer-8 -mcmodel=medium -c MC64D.f
This refers to the INT64YGNU option

If you are using 32 bit integer in large memory space use
> gfortran -O -fPIC -m64 -mcmodel=medium -c MC64D.f
This refers to the INT64NGNU option

If you have no special requirements, simply use
> gfortran -O -fPIC -c MC64D.f
This refers to the INT32GNU option

Other compilers and options are treated accordingly. You may take a look at
JADAMILU/src/makefiles for some suggestions.



compiling the main program
--------------------------

To compile any program using JADAMILU, for instance the examples in
JADAMILU/samples/ , go to the directory where the program is 
located and use the command

> mycompiler OPTIONS myprog.f -L ../lib/ArchComp -ljadamilu -lamd -lsuitesparseconfig -llapack -lblas ../lib/ArchComp/MC*.o

where mycompiler stands for the Fortran compiler matching ArchComp.  
For instance, if you have an Intel processor with 32 bit architecture
and use gfortran compiler, EXAMPLE1.f will be compiled with

> gfortran EXAMPLE1.f  -L ../lib/INT32GNU -ljadamilu -lamd -lsuitesparseconfig -llapack -lblas ../lib/INT32GNU/MC*.o

Similarly, 
> gfortran -m64 -fdefault-integer-8 -mcmodel=medium EXAMPLE1.f  -L ../lib/INT64YGNU -ljadamilu -lamd -lsuitesparseconfig -llapack -lblas ../lib/INT64YGNU/MC*.o
would refer to long address space and 64 bit long integer

Of course, you may insert your favorite options, like "-O" for optimization
or "-o myprog" so that the executable has name "myprog" instead of a.out

Note that if the compiler suite contains a Fortran 90 compiler, 
mycompiler may be this Fortran 90 compiler and myprog may have 
extension .f90 ; for instance, with pgf:

> pgf90 myprog.f90  -L ../lib/ArchComp -ljadamilu -lamd -lsuitesparseconfig -llapack -lblas ../lib/ArchComp/MC*.o

Of course, these are simple examples. JADAMILU may also be used in complex
applications with programs split in several files and possibly built
with makefile. It is easy to deduce how to proceed from the examples
above, in particular how to adapt possible Makefile



Compiling the CMEX interfaces for MATLAB
----------------------------------------
go to the subdirectory JADAMILU/matlabsrc and type "make".
After that, you will find in the subdirectory JADAMILU/matlab the associated
binaries (which have an ending like *.mex*) along with JADAMILU's two
MATLAB drivers "PJDinit.m", which is used to init your parameters to the
default values and "PJD.m" which is JADAMILU's eigensolver call. "PJD.m"
is set up along the MATLAB "eigs" function. There are two sample codes
"example1.m",...,"example3.m" which demonstrate the usage of PJDinit and PJD



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



