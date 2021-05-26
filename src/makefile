#!/bin/csh
#
# makefile for JADAMILU
# 
STARTDIR=$(PWD)
MYSTARTDIR=$(STARTDIR)

include user.mk
MYPLATFORM=$(PLATFORM)
include makefiles/platforms.mk
include makefiles/default.mk

DIRILUPACK=ilupack/
DIRJD=jd/
DIRAMD=amd/
DIRMC64=mc64/
DIRBLASLIKE=blaslike/
DIRBLAS=blas/
DIRLAPACK=lapack/


.PHONY:  srilupack rilupack scilupack cilupack srjd rjd scjd cjd\
         srourlibs rourlibs scourlibs courlibs clean clear mc64 amd \
         blas lapack srlibs rlibs sclibs clibs blaslike

ourpart: srourlibs rourlibs scourlibs courlibs

real: srlibs rlibs

all: srlibs rlibs sclibs clibs


srilupack:
	@ echo build single real ILUPACK preconditioners
	@ cd ilupack; \
        $(MAKE) "ARITHMETIC=-D_SINGLE_REAL_" "PRECISION=s" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"

rilupack:
	@ echo build double real ILUPACK preconditioners
	@ cd ilupack; \
        $(MAKE) "ARITHMETIC=-D_DOUBLE_REAL_" "PRECISION=d" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"

scilupack:
	@ echo build single complex ILUPACK preconditioners
	@ cd ilupack; \
        $(MAKE) "ARITHMETIC=-D_SINGLE_COMPLEX_" "PRECISION=c" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"

cilupack:
	@ echo build double complex ILUPACK preconditioners
	@ cd ilupack; \
        $(MAKE) "ARITHMETIC=" "PRECISION=z" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"


srjd:
	@ echo build single real Jacobi-Davidson solver
	@ cd jd;\
        $(MAKE) "ARITHMETIC=-D_SINGLE_REAL_" "PRECISION=s" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"
	@ echo sample executable program can be found in the current directory

rjd:
	@ echo build double real Jacobi-Davidson solver
	@ cd jd;\
        $(MAKE) "ARITHMETIC=-D_DOUBLE_REAL_" "PRECISION=d" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"
	@ echo sample executable program can be found in the current directory

scjd:
	@ echo build single complex Jacobi-Davidson solver
	@ cd jd;\
        $(MAKE) "ARITHMETIC=-D_SINGLE_COMPLEX_" "PRECISION=c" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"
	@ echo sample executable program can be found in the current directory

cjd:
	@ echo build double complex Jacobi-Davidson solver
	@ cd jd;\
        $(MAKE) "ARITHMETIC=" "PRECISION=z" "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"
	@ echo sample executable program can be found in the current directory


srourlibs: srilupack srjd blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) ourmain "MYPLATFORM=$(PLATFORM)" "PRECISION=s" \
        "MYSTARTDIR=$(STARTDIR)" \
        "AMD=" \
        "MC64="\
        "BLAS=" \
        "LAPACK="

rourlibs: rilupack rjd blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) ourmain "MYPLATFORM=$(PLATFORM)" "PRECISION=d" \
        "MYSTARTDIR=$(STARTDIR)" \
        "AMD=" \
        "MC64="\
        "BLAS=" \
        "LAPACK="

scourlibs: scilupack scjd blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) ourmain "MYPLATFORM=$(PLATFORM)" "PRECISION=c" \
        "MYSTARTDIR=$(STARTDIR)" \
        "AMD=" \
        "MC64="\
        "BLAS=" \
        "LAPACK="

courlibs: cilupack cjd blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) ourmain "MYPLATFORM=$(PLATFORM)" "PRECISION=z" \
        "MYSTARTDIR=$(STARTDIR)" \
        "AMD=" \
        "MC64="\
        "BLAS=" \
        "LAPACK="


mc64: $(MC64)
	@ echo build MC64 matching
	@ cd mc64; \
        $(MAKE)  "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"


amd:
	@ echo build AMD reordering
	@ cd amd; \
        $(MAKE)  "MYPLATFORM=$(PLATFORM)" \
        "MYSTARTDIR=$(STARTDIR)"


blaslike:
	@ echo build blas like routine needed for JADAMILU
	@ cd blaslike;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)" "MYSTARTDIR=$(STARTDIR)"


blas:
	@ echo build blas library
	@ cd blas;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)" "MYSTARTDIR=$(STARTDIR)"


lapack:
	@ echo build lapack library
	@ cd lapack;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)" "MYSTARTDIR=$(STARTDIR)"


srlibs: srilupack srjd mc64 amd blas lapack blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)"  "PRECISION=s"\
        "MYSTARTDIR=$(STARTDIR)"

rlibs: rilupack rjd mc64 amd blas lapack blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)"  "PRECISION=d" \
        "MYSTARTDIR=$(STARTDIR)"

sclibs: scilupack scjd mc64 amd blas lapack blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)"  "PRECISION=c"\
        "MYSTARTDIR=$(STARTDIR)"

clibs: cilupack cjd mc64 amd blas lapack blaslike
	@ echo bind libraries
	@ cd ../lib;\
        $(MAKE) "MYPLATFORM=$(PLATFORM)"  "PRECISION=z" \
        "MYSTARTDIR=$(STARTDIR)"


clean:
	rm -rf   */*.o


clear:
	rm -rf */*.o */*/*.o */D/* */Z/* */S/* */C/*;\
        rm -rf ../lib/$(PLATFORM)/lib*.a
