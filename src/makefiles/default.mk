
# default preconditioned Jacobi-Davidson main driver
MAIN=PJD

# decide whether to use Intel MKL
MKL=-D_USE_MKL_


# -----------------------------------------------------

ILUPACK=$(DIRILUPACK)$(PRECISION)symAMGextract.o          \
        $(DIRILUPACK)$(PRECISION)sympiluc.o               \
        $(DIRILUPACK)$(PRECISION)sympilucsol.o            \
        $(DIRILUPACK)$(PRECISION)sympiluclsol.o           \
        $(DIRILUPACK)$(PRECISION)sympilucusol.o           \
        $(DIRILUPACK)$(PRECISION)symAMGsetup.o            \
        $(DIRILUPACK)$(PRECISION)symAMGsol.o              \
        $(DIRILUPACK)$(PRECISION)symiluc.o                \
        $(DIRILUPACK)$(PRECISION)iluclist.o               \
        $(DIRILUPACK)$(PRECISION)piluclist.o              \
        $(DIRILUPACK)$(PRECISION)permamd.o                \
        $(DIRILUPACK)$(PRECISION)sympermMC64amd.o         \
        $(DIRILUPACK)$(PRECISION)symwm.o                  \
        $(DIRILUPACK)$(PRECISION)symAMGinit.o             \
        $(DIRILUPACK)$(PRECISION)symAMGgetparams.o        \
        $(DIRILUPACK)$(PRECISION)symAMGsetparams.o        \
        $(DIRILUPACK)$(PRECISION)symAMGdelete.o           \
        $(DIRILUPACK)$(PRECISION)symspd.o                 \
        $(DIRILUPACK)$(PRECISION)qsplit.o                 \
        $(DIRILUPACK)$(PRECISION)qsplit2.o                \
        $(DIRILUPACK)$(PRECISION)scale.o                  \
        $(DIRILUPACK)$(PRECISION)spdscale.o               \
        $(DIRILUPACK)$(PRECISION)symscale.o               \
        $(DIRILUPACK)$(PRECISION)Malloc.o                 \
        $(DIRILUPACK)$(PRECISION)Realloc.o                \
        $(DIRILUPACK)$(PRECISION)geteps.o                 \
        $(DIRILUPACK)$(PRECISION)qsort.o                  \
        $(DIRILUPACK)$(PRECISION)qsort2.o                 \
        $(DIRILUPACK)$(PRECISION)qqsort.o                 \
        $(DIRILUPACK)$(PRECISION)qqsorti.o                \
        $(DIRILUPACK)$(PRECISION)qqsort2.o                \
        $(DIRILUPACK)$(PRECISION)qqsorts.o                \
        $(DIRILUPACK)$(PRECISION)qqsorts2.o               \
        $(DIRILUPACK)$(PRECISION)swapm.o                  \
        $(DIRILUPACK)$(PRECISION)swapj.o                  \
        $(DIRILUPACK)$(PRECISION)CSRmatvec.o              \
        $(DIRILUPACK)$(PRECISION)CSRmattvec.o             \
        $(DIRILUPACK)$(PRECISION)CSRmathvec.o             \
        $(DIRILUPACK)$(PRECISION)CSRSetupGraph.o          \
        $(DIRILUPACK)$(PRECISION)CSRSetupGraph_epsilon.o  \
        $(DIRILUPACK)$(PRECISION)clear.o                  \
        $(DIRILUPACK)$(PRECISION)symmatvec.o              \
        $(DIRILUPACK)$(PRECISION)ddot2.o                  \
        $(DIRILUPACK)$(PRECISION)symilupackinit.o         \
        $(DIRILUPACK)$(PRECISION)symilupackfactorgep.o    \
        $(DIRILUPACK)$(PRECISION)symilupackfactor.o       \
        $(DIRILUPACK)$(PRECISION)symilupacksol.o          \
        $(DIRILUPACK)$(PRECISION)symilupackdelete.o       \
        $(DIRILUPACK)$(PRECISION)symilupackinfo.o         \
        $(DIRILUPACK)$(PRECISION)symilupacknnz.o          \
        $(DIRILUPACK)$(PRECISION)symspdilupackconvert.o   \
        $(DIRILUPACK)$(PRECISION)cc_etimes.o              \
        $(DIRILUPACK)$(PRECISION)unscale.o                \
        $(DIRILUPACK)$(PRECISION)symamgsavediag.o         \
        $(DIRILUPACK)$(PRECISION)symamgsavediaggep.o      \
        $(DIRILUPACK)$(PRECISION)symamgrestorediag.o      \
        $(DIRILUPACK)$(PRECISION)cc_etimes2.o             \
        $(DIRILUPACK)$(PRECISION)iprandom.o               \
        $(DIRILUPACK)$(PRECISION)ipsrandom.o              \
        $(DIRILUPACK)$(PRECISION)singlesymamgsavediag.o   \
        $(DIRILUPACK)$(PRECISION)singlesymilupacksol.o    \
        $(DIRILUPACK)$(PRECISION)singlesymilupackinit.o   \
        $(DIRILUPACK)$(PRECISION)singlesymamgrestorediag.o \
        $(DIRILUPACK)$(PRECISION)singleunscale.o           \
        $(DIRILUPACK)$(PRECISION)singlesymilupackfactorgep.o\
        $(DIRILUPACK)$(PRECISION)singlesymilupackfactor.o\
        $(DIRILUPACK)$(PRECISION)myddot.o




JD=$(DIRJD)$(PRECISION)PJD.o          $(DIRJD)$(PRECISION)dglprecsol.o   \
   $(DIRJD)$(PRECISION)dglprecsetup.o $(DIRJD)$(PRECISION)dglprecdelete.o\
   $(DIRJD)$(PRECISION)JDaux.o        $(DIRJD)PJD_compatible.o


BLASLIKE=$(DIRBLASLIKE)common.o          \
         $(DIRBLASLIKE)sprivatesptrs.o $(DIRBLASLIKE)cprivatehptrs.o \
         $(DIRBLASLIKE)dprivatesptrs.o $(DIRBLASLIKE)zprivatehptrs.o





SAMPLES=$(MAIN).o



STARTDIR=$(PWD)

MYSTARTDIR=$(STARTDIR)

# Linux
#LIBS=-ljadamilu -lamd -llapack -lblas -lm -lc 

# HP alpha
#LIBS=-ljadamilu -lamd -llapack -lcxml -lblas -lm -lc -lfor

# IBM AIX
#LIBS=-ljadamilu -lamd -llapack -lblas  -lm -lc

# SGI Altix
#LIBS=-ljadamilu -lamd  -lmkl_lapack -lmkl  -lm -lc  -lifcore -lguide



# where are the headers
INCDIR=$(MYSTARTDIR)/include

# where are the libraries
LIBDIR=$(MYSTARTDIR)/lib/$(MYPLATFORM)


.SUFFIXES: .c .f .F .o .a
.DEFAULT: main

#%.o: %.c
#	$(CC)  $(CCFLAGS)  -I$(INCDIR)  $(FORTRANNAMES) $(ARITHMETIC) $(LONGINTEGER) -c -o $(PRECISION)$@ $<
#
#
#%.o: %.F
#	$(FCOMPILE)

%.o: %.f
	$(FF)  $(FFFLAGS)  -c -o $@ $<


$(PRECISION)%.o: %.c
	$(CC)  $(CCFLAGS)  -I$(INCDIR)  $(FORTRANNAMES) $(ARITHMETIC) $(LONGINTEGER) -c -o $@ $<


$(PRECISION)%.o: %.F
	$(FCOMPILE)



