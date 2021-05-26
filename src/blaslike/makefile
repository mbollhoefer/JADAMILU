
include ../makefiles/platforms.mk
include ../user.mk
include ../makefiles/default.mk

.c.o:
	$(CC)  $(CCFLAGS)  -I$(INCDIR)  $(FORTRANNAMES) $(ARITHMETIC) -c -o $@ $<

.f.o:
	$(FF)  $(FFFLAGS)  -I$(INCDIR) -c -o $@ $<

.F.o:
	$(FF)  $(FFFLAGS)  -I$(INCDIR)  $(LONGINTEGER) -c -o $@ $<

main : $(BLASLIKE)


*.o: ../include/ilupack_fortran.h
