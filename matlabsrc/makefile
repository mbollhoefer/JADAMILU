#!/bin/csh
#

STARTDIR=$(PWD)/../src
MYSTARTDIR=$(STARTDIR)

include $(STARTDIR)/user.mk
MYPLATFORM=$(PLATFORM)
include $(STARTDIR)/makefiles/platforms.mk
include $(STARTDIR)/makefiles/default.mk

# where is your "mex" script located?
# even when matlab is installed, the mex command is not necessarily
# installed as well
# usually "mex" is in the same place as the "matlab" command
# CMEX=/usr/local/MATLAB/R2011a/bin/mex
# if your ar lucky, the command is installed
CMEX=mex

MEXDIR=../matlab

DOBJECTS=$(MEXDIR)/DSYMjadamilu.$(EXT) \
         $(MEXDIR)/DSYMjadamilu_gep.$(EXT) \
         $(MEXDIR)/DSYMjadamilurevcom.$(EXT)
         

ZOBJECTS=$(MEXDIR)/ZHERjadamilu.$(EXT) \
         $(MEXDIR)/ZHERjadamilurevcom.$(EXT)

OBJECTS=$(DOBJECTS) $(ZOBJECTS)

all: $(OBJECTS)

clean:
	rm -f $(OBJECTS)

.SUFFIXES: .c .$(EXT)

$(DOBJECTS): ../lib/$(PLATFORM)/libjadamilu.a
$(ZOBJECTS): ../lib/$(PLATFORM)/libjadamilu.a 
$(MEXDIR)/%.$(EXT): %.c ../lib/$(PLATFORM)/libjadamilu.a
# 64 BIT ------------------------------
# gcc/gfortran with mexopts77_gfortran.sh 
	$(CMEX) -O -largeArrayDims -D_LONG_INTEGER_ $(FORTRANNAMES) -I../src/include -outdir $(MEXDIR)  $<  -L../lib/$(PLATFORM) -ljadamilu -lmwlapack -lmwblas -lm -lc -lgfortran
# 64 BIT ------------------------------


$(MEXDIR)/D*.$(EXT): ../lib/$(PLATFORM)/libjadamilu.a  
$(MEXDIR)/D*.$(EXT): ../lib/$(PLATFORM)/libjadamilu.a  
$(MEXDIR)/Z*.$(EXT): ../lib/$(PLATFORM)/libjadamilu.a 
$(MEXDIR)/Z*.$(EXT): ../lib/$(PLATFORM)/libjadamilu.a 
