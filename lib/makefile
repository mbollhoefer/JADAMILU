include ../src/makefiles/platforms.mk
include ../src/user.mk
include ../src/makefiles/default.mk

DIRILUPACK=../src/ilupack/
DIRAMD=../src/amd/
DIRJD=../src/jd/
DIRMC64=../src/mc64/
DIRBLASLIKE=../src/blaslike/
DIRBLAS=../src/blas/
DIRLAPACK=../src/lapack/

main: $(MYPLATFORM)/libjadamilu.a \
      $(MYPLATFORM)/libmyblas.a    $(MYPLATFORM)/libmylapack.a \
      $(ILUPACK) $(JD)

ourmain: $(MYPLATFORM)/libjadamilu.a $(ILUPACK) $(JD) $(BLASLIKE)


$(MYPLATFORM)/libjadamilu.a: $(ILUPACK) $(AMD) $(JD) $(MC64) $(BLASLIKE)
	$(AR) $(ARFLAGS) $@ $?
	$(RL) $(RLFLAGS) $@

$(MYPLATFORM)/libmyblas.a:  $(BLAS)
	$(AR) $(ARFLAGS) $@ $?
	$(RL) $(RLFLAGS) $@

$(MYPLATFORM)/libmylapack.a:  $(LAPACK1) $(LAPACK2)
	$(AR) $(ARFLAGS) $@ $(LAPACK1)
	$(AR) $(ARFLAGS) $@ $(LAPACK2)
	$(RL) $(RLFLAGS) $@

