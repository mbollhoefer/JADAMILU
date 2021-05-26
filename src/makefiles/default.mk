
# default preconditioned Jacobi-Davidson main driver
MAIN=PJD

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
        $(DIRILUPACK)$(PRECISION)sympermmwm_amd.o         \
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
        $(DIRILUPACK)$(PRECISION)singlesymilupackfactor.o


MC64=$(DIRMC64)MC64D.o $(DIRMC64)MC21D.o \
     $(DIRMC64)MC64S.o $(DIRMC64)MC21S.o 


AMDSRC=$(DIRAMD)amd_aat       $(DIRAMD)amd_defaults   \
       $(DIRAMD)amd_1         $(DIRAMD)amd_order      \
       $(DIRAMD)amd_2         $(DIRAMD)amd_control    \
       $(DIRAMD)amd_dump      $(DIRAMD)amd_info       \
       $(DIRAMD)amd_postorder $(DIRAMD)amd_valid      \
       $(DIRAMD)amd_post_tree $(DIRAMD)amd_preprocess   


AMDI = $(addsuffix .o, $(subst amd_,amd_i_,$(AMDSRC)))
AMDL = $(addsuffix .o, $(subst amd_,amd_l_,$(AMDSRC)))
AMD = $(AMDI) $(AMDL)


JD=$(DIRJD)$(PRECISION)PJD.o          $(DIRJD)$(PRECISION)dglprecsol.o   \
   $(DIRJD)$(PRECISION)dglprecsetup.o $(DIRJD)$(PRECISION)dglprecdelete.o\
   $(DIRJD)$(PRECISION)JDaux.o        $(DIRJD)PJD_compatible.o


BLASLIKE=$(DIRBLASLIKE)common.o          \
         $(DIRBLASLIKE)sprivatesptrs.o $(DIRBLASLIKE)cprivatehptrs.o \
         $(DIRBLASLIKE)dprivatesptrs.o $(DIRBLASLIKE)zprivatehptrs.o

BLAS=$(DIRBLAS)caxpy.o  $(DIRBLAS)zhpr2.o  $(DIRBLAS)izamax.o \
     $(DIRBLAS)ccopy.o  $(DIRBLAS)zhpr.o   $(DIRBLAS)lsame.o  \
     $(DIRBLAS)cdotc.o  $(DIRBLAS)zrotg.o  $(DIRBLAS)sasum.o  \
     $(DIRBLAS)cdotu.o  $(DIRBLAS)zscal.o  $(DIRBLAS)saxpy.o  \
     $(DIRBLAS)cgbmv.o  $(DIRBLAS)zswap.o  $(DIRBLAS)scasum.o \
     $(DIRBLAS)cgemm.o  $(DIRBLAS)zsymm.o  $(DIRBLAS)scnrm2.o \
     $(DIRBLAS)cgemv.o  $(DIRBLAS)zsyr2k.o $(DIRBLAS)scopy.o  \
     $(DIRBLAS)cgerc.o  $(DIRBLAS)zsyrk.o  $(DIRBLAS)sdot.o   \
     $(DIRBLAS)cgeru.o  $(DIRBLAS)ztbmv.o  $(DIRBLAS)sdsdot.o \
     $(DIRBLAS)chbmv.o  $(DIRBLAS)ztbsv.o  $(DIRBLAS)sgbmv.o  \
     $(DIRBLAS)chemm.o  $(DIRBLAS)ztpmv.o  $(DIRBLAS)sgemm.o  \
     $(DIRBLAS)chemv.o  $(DIRBLAS)ztpsv.o  $(DIRBLAS)sgemv.o  \
     $(DIRBLAS)cher2.o  $(DIRBLAS)ztrmm.o  $(DIRBLAS)sger.o   \
     $(DIRBLAS)cher2k.o $(DIRBLAS)ztrmv.o  $(DIRBLAS)snrm2.o  \
     $(DIRBLAS)cher.o   $(DIRBLAS)ztrsm.o  $(DIRBLAS)srot.o   \
     $(DIRBLAS)cherk.o  $(DIRBLAS)ztrsv.o  $(DIRBLAS)srotg.o  \
     $(DIRBLAS)chpmv.o  $(DIRBLAS)dcopy.o  $(DIRBLAS)srotm.o  \
     $(DIRBLAS)chpr2.o  $(DIRBLAS)ddot.o   $(DIRBLAS)srotmg.o \
     $(DIRBLAS)chpr.o   $(DIRBLAS)dgbmv.o  $(DIRBLAS)ssbmv.o  \
     $(DIRBLAS)crotg.o  $(DIRBLAS)dgemm.o  $(DIRBLAS)sscal.o  \
     $(DIRBLAS)cscal.o  $(DIRBLAS)dgemv.o  $(DIRBLAS)zhpmv.o  \
     $(DIRBLAS)csrot.o  $(DIRBLAS)dger.o   $(DIRBLAS)zherk.o  \
     $(DIRBLAS)csscal.o $(DIRBLAS)dnrm2.o  $(DIRBLAS)strmm.o  \
     $(DIRBLAS)cswap.o  $(DIRBLAS)drot.o   $(DIRBLAS)strmv.o  \
     $(DIRBLAS)csymm.o  $(DIRBLAS)drotg.o  $(DIRBLAS)strsm.o  \
     $(DIRBLAS)csyr2k.o $(DIRBLAS)drotm.o  $(DIRBLAS)strsv.o  \
     $(DIRBLAS)csyrk.o  $(DIRBLAS)drotmg.o $(DIRBLAS)xerbla.o \
     $(DIRBLAS)ctbmv.o  $(DIRBLAS)dsbmv.o  $(DIRBLAS)zaxpy.o  \
     $(DIRBLAS)ctbsv.o  $(DIRBLAS)dscal.o  $(DIRBLAS)zcopy.o  \
     $(DIRBLAS)ctpmv.o  $(DIRBLAS)dsdot.o  $(DIRBLAS)zdotc.o  \
     $(DIRBLAS)ctpsv.o  $(DIRBLAS)dspmv.o  $(DIRBLAS)zdotu.o  \
     $(DIRBLAS)ctrmm.o  $(DIRBLAS)dspr2.o  $(DIRBLAS)zdrot.o  \
     $(DIRBLAS)ctrmv.o  $(DIRBLAS)dspr.o   $(DIRBLAS)zdscal.o \
     $(DIRBLAS)ctrsm.o  $(DIRBLAS)dswap.o  $(DIRBLAS)zgbmv.o  \
     $(DIRBLAS)ctrsv.o  $(DIRBLAS)dsymm.o  $(DIRBLAS)zgemm.o  \
     $(DIRBLAS)dasum.o  $(DIRBLAS)dsymv.o  $(DIRBLAS)zgemv.o  \
     $(DIRBLAS)daxpy.o  $(DIRBLAS)dsyr2.o  $(DIRBLAS)zgerc.o  \
     $(DIRBLAS)dtrmm.o  $(DIRBLAS)dsyr2k.o $(DIRBLAS)zgeru.o  \
     $(DIRBLAS)dcabs1.o $(DIRBLAS)dsyr.o   $(DIRBLAS)zhbmv.o  \
     $(DIRBLAS)dsyrk.o  $(DIRBLAS)sspmv.o  $(DIRBLAS)zhemv.o  \
     $(DIRBLAS)dtbmv.o  $(DIRBLAS)zhemm.o  $(DIRBLAS)zher2.o  \
     $(DIRBLAS)dtbsv.o  $(DIRBLAS)sspr.o   $(DIRBLAS)zher2k.o \
     $(DIRBLAS)dtpmv.o  $(DIRBLAS)sswap.o  $(DIRBLAS)zher.o   \
     $(DIRBLAS)dtpsv.o  $(DIRBLAS)ssymm.o  $(DIRBLAS)icamax.o \
     $(DIRBLAS)sspr2.o  $(DIRBLAS)ssymv.o  $(DIRBLAS)idamax.o \
     $(DIRBLAS)dtrmv.o  $(DIRBLAS)ssyr2.o  $(DIRBLAS)isamax.o \
     $(DIRBLAS)dtrsm.o  $(DIRBLAS)ssyr2k.o $(DIRBLAS)stbsv.o  \
     $(DIRBLAS)dtrsv.o  $(DIRBLAS)ssyr.o   $(DIRBLAS)stpmv.o  \
     $(DIRBLAS)dzasum.o $(DIRBLAS)ssyrk.o  $(DIRBLAS)stpsv.o  \
     $(DIRBLAS)dznrm2.o $(DIRBLAS)stbmv.o   


LAPACK1=$(DIRLAPACK)cbdsqr.o $(DIRLAPACK)dtzrqf.o $(DIRLAPACK)dlaqtr.o\
       $(DIRLAPACK)cgbbrd.o $(DIRLAPACK)dtzrzf.o $(DIRLAPACK)dlar1v.o\
       $(DIRLAPACK)cgbcon.o $(DIRLAPACK)dzsum1.o $(DIRLAPACK)dlar2v.o\
       $(DIRLAPACK)cgbequ.o $(DIRLAPACK)icmax1.o $(DIRLAPACK)dlarfb.o\
       $(DIRLAPACK)cgbrfs.o $(DIRLAPACK)ieeeck.o $(DIRLAPACK)dlarf.o \
       $(DIRLAPACK)cgbsv.o  $(DIRLAPACK)ilaenv.o $(DIRLAPACK)dlarfg.o\
       $(DIRLAPACK)cgbsvx.o $(DIRLAPACK)izmax1.o $(DIRLAPACK)dlarft.o\
       $(DIRLAPACK)cgbtf2.o $(DIRLAPACK)lsame.o  $(DIRLAPACK)dlarfx.o\
       $(DIRLAPACK)cgbtrf.o $(DIRLAPACK)lsamen.o $(DIRLAPACK)dlargv.o\
       $(DIRLAPACK)cgbtrs.o $(DIRLAPACK)sbdsdc.o $(DIRLAPACK)dlarnv.o\
       $(DIRLAPACK)cgebak.o $(DIRLAPACK)sbdsqr.o $(DIRLAPACK)dlarrb.o\
       $(DIRLAPACK)cgebal.o $(DIRLAPACK)scsum1.o $(DIRLAPACK)dlarre.o\
       $(DIRLAPACK)cgebd2.o $(DIRLAPACK)sdisna.o $(DIRLAPACK)dlarrf.o\
       $(DIRLAPACK)cgebrd.o $(DIRLAPACK)second.o $(DIRLAPACK)dlarrv.o\
       $(DIRLAPACK)cgecon.o $(DIRLAPACK)sgbbrd.o $(DIRLAPACK)dlartg.o\
       $(DIRLAPACK)cgeequ.o $(DIRLAPACK)sgbcon.o $(DIRLAPACK)dlartv.o\
       $(DIRLAPACK)cgees.o  $(DIRLAPACK)sgbequ.o $(DIRLAPACK)dlaruv.o\
       $(DIRLAPACK)cgeesx.o $(DIRLAPACK)sgbrfs.o $(DIRLAPACK)dlarzb.o\
       $(DIRLAPACK)cgeev.o  $(DIRLAPACK)sgbsv.o  $(DIRLAPACK)dlarz.o \
       $(DIRLAPACK)cgeevx.o $(DIRLAPACK)sgbsvx.o $(DIRLAPACK)dlarzt.o\
       $(DIRLAPACK)cgegs.o  $(DIRLAPACK)sgbtf2.o $(DIRLAPACK)dlas2.o \
       $(DIRLAPACK)cgegv.o  $(DIRLAPACK)sgbtrf.o $(DIRLAPACK)dlascl.o\
       $(DIRLAPACK)cgehd2.o $(DIRLAPACK)sgbtrs.o $(DIRLAPACK)dlasd0.o\
       $(DIRLAPACK)cgehrd.o $(DIRLAPACK)sgebak.o $(DIRLAPACK)dlasd1.o\
       $(DIRLAPACK)cgelq2.o $(DIRLAPACK)sgebal.o $(DIRLAPACK)dlasd2.o\
       $(DIRLAPACK)cgelqf.o $(DIRLAPACK)sgebd2.o $(DIRLAPACK)dlasd3.o\
       $(DIRLAPACK)cgelsd.o $(DIRLAPACK)sgebrd.o $(DIRLAPACK)dlasd4.o\
       $(DIRLAPACK)cgels.o  $(DIRLAPACK)sgecon.o $(DIRLAPACK)dlasd5.o\
       $(DIRLAPACK)cgelss.o $(DIRLAPACK)sgeequ.o $(DIRLAPACK)dlasd6.o\
       $(DIRLAPACK)cgelsx.o $(DIRLAPACK)sgees.o  $(DIRLAPACK)dlasd7.o\
       $(DIRLAPACK)cgelsy.o $(DIRLAPACK)sgeesx.o $(DIRLAPACK)dlasd8.o\
       $(DIRLAPACK)cgeql2.o $(DIRLAPACK)sgeev.o  $(DIRLAPACK)dlasd9.o\
       $(DIRLAPACK)cgeqlf.o $(DIRLAPACK)sgeevx.o $(DIRLAPACK)dlasda.o\
       $(DIRLAPACK)cgeqp3.o $(DIRLAPACK)sgegs.o  $(DIRLAPACK)dlasdq.o\
       $(DIRLAPACK)cgeqpf.o $(DIRLAPACK)sgegv.o  $(DIRLAPACK)dlasdt.o\
       $(DIRLAPACK)cgeqr2.o $(DIRLAPACK)sgehd2.o $(DIRLAPACK)dlaset.o\
       $(DIRLAPACK)cgeqrf.o $(DIRLAPACK)sgehrd.o $(DIRLAPACK)dlasq1.o\
       $(DIRLAPACK)cgerfs.o $(DIRLAPACK)sgelq2.o $(DIRLAPACK)dlasq2.o\
       $(DIRLAPACK)cgerq2.o $(DIRLAPACK)sgelqf.o $(DIRLAPACK)dlasq3.o\
       $(DIRLAPACK)cgerqf.o $(DIRLAPACK)sgelsd.o $(DIRLAPACK)dlasq4.o\
       $(DIRLAPACK)cgesc2.o $(DIRLAPACK)sgels.o  $(DIRLAPACK)dlasq5.o\
       $(DIRLAPACK)cgesdd.o $(DIRLAPACK)sgelss.o $(DIRLAPACK)dlasq6.o\
       $(DIRLAPACK)cgesvd.o $(DIRLAPACK)sgelsx.o $(DIRLAPACK)dlasr.o \
       $(DIRLAPACK)cgesv.o  $(DIRLAPACK)sgelsy.o $(DIRLAPACK)dlasrt.o\
       $(DIRLAPACK)cgesvx.o $(DIRLAPACK)sgeql2.o $(DIRLAPACK)dlassq.o\
       $(DIRLAPACK)cgetc2.o $(DIRLAPACK)sgeqlf.o $(DIRLAPACK)dlasv2.o\
       $(DIRLAPACK)cgetf2.o $(DIRLAPACK)sgeqp3.o $(DIRLAPACK)dlaswp.o\
       $(DIRLAPACK)cgetrf.o $(DIRLAPACK)sgeqpf.o $(DIRLAPACK)dlasy2.o\
       $(DIRLAPACK)cgetri.o $(DIRLAPACK)sgeqr2.o $(DIRLAPACK)dlasyf.o\
       $(DIRLAPACK)cgetrs.o $(DIRLAPACK)sgeqrf.o $(DIRLAPACK)dlatbs.o\
       $(DIRLAPACK)cggbak.o $(DIRLAPACK)sgerfs.o $(DIRLAPACK)dlatdf.o\
       $(DIRLAPACK)cggbal.o $(DIRLAPACK)sgerq2.o $(DIRLAPACK)dlatps.o\
       $(DIRLAPACK)cgges.o  $(DIRLAPACK)sgerqf.o $(DIRLAPACK)dlatrd.o\
       $(DIRLAPACK)cggesx.o $(DIRLAPACK)sgesc2.o $(DIRLAPACK)dlatrs.o\
       $(DIRLAPACK)cggev.o  $(DIRLAPACK)sgesdd.o $(DIRLAPACK)dlatrz.o\
       $(DIRLAPACK)cggevx.o $(DIRLAPACK)sgesvd.o $(DIRLAPACK)dlatzm.o\
       $(DIRLAPACK)cggglm.o $(DIRLAPACK)sgesv.o  $(DIRLAPACK)dlauu2.o\
       $(DIRLAPACK)cgghrd.o $(DIRLAPACK)sgesvx.o $(DIRLAPACK)dlauum.o\
       $(DIRLAPACK)cgglse.o $(DIRLAPACK)sgetc2.o $(DIRLAPACK)dopgtr.o\
       $(DIRLAPACK)cggqrf.o $(DIRLAPACK)sgetf2.o $(DIRLAPACK)dopmtr.o\
       $(DIRLAPACK)cggrqf.o $(DIRLAPACK)sgetrf.o $(DIRLAPACK)dorg2l.o\
       $(DIRLAPACK)cggsvd.o $(DIRLAPACK)sgetri.o $(DIRLAPACK)dorg2r.o\
       $(DIRLAPACK)cggsvp.o $(DIRLAPACK)sgetrs.o $(DIRLAPACK)dorgbr.o\
       $(DIRLAPACK)cgtcon.o $(DIRLAPACK)sggbak.o $(DIRLAPACK)dorghr.o\
       $(DIRLAPACK)cgtrfs.o $(DIRLAPACK)sggbal.o $(DIRLAPACK)dorgl2.o\
       $(DIRLAPACK)cgtsv.o  $(DIRLAPACK)sgges.o  $(DIRLAPACK)dorglq.o\
       $(DIRLAPACK)cgtsvx.o $(DIRLAPACK)sggesx.o $(DIRLAPACK)dorgql.o\
       $(DIRLAPACK)cgttrf.o $(DIRLAPACK)sggev.o  $(DIRLAPACK)dorgqr.o\
       $(DIRLAPACK)cgttrs.o $(DIRLAPACK)sggevx.o $(DIRLAPACK)dorgr2.o\
       $(DIRLAPACK)cgtts2.o $(DIRLAPACK)sggglm.o $(DIRLAPACK)dorgrq.o\
       $(DIRLAPACK)chbevd.o $(DIRLAPACK)sgghrd.o $(DIRLAPACK)dorgtr.o\
       $(DIRLAPACK)chbev.o  $(DIRLAPACK)sgglse.o $(DIRLAPACK)dorm2l.o\
       $(DIRLAPACK)chbevx.o $(DIRLAPACK)sggqrf.o $(DIRLAPACK)dorm2r.o\
       $(DIRLAPACK)chbgst.o $(DIRLAPACK)sggrqf.o $(DIRLAPACK)dormbr.o\
       $(DIRLAPACK)chbgvd.o $(DIRLAPACK)sggsvd.o $(DIRLAPACK)dormhr.o\
       $(DIRLAPACK)chbgv.o  $(DIRLAPACK)sggsvp.o $(DIRLAPACK)dorml2.o\
       $(DIRLAPACK)chbgvx.o $(DIRLAPACK)sgtcon.o $(DIRLAPACK)dormlq.o\
       $(DIRLAPACK)chbtrd.o $(DIRLAPACK)sgtrfs.o $(DIRLAPACK)dormql.o\
       $(DIRLAPACK)checon.o $(DIRLAPACK)sgtsv.o  $(DIRLAPACK)dormqr.o\
       $(DIRLAPACK)cheevd.o $(DIRLAPACK)sgtsvx.o $(DIRLAPACK)dormr2.o\
       $(DIRLAPACK)cheev.o  $(DIRLAPACK)sgttrf.o $(DIRLAPACK)dormr3.o\
       $(DIRLAPACK)cheevr.o $(DIRLAPACK)sgttrs.o $(DIRLAPACK)dormrq.o\
       $(DIRLAPACK)cheevx.o $(DIRLAPACK)sgtts2.o $(DIRLAPACK)dormrz.o\
       $(DIRLAPACK)chegs2.o $(DIRLAPACK)shgeqz.o $(DIRLAPACK)dormtr.o\
       $(DIRLAPACK)chegst.o $(DIRLAPACK)shsein.o $(DIRLAPACK)dpbcon.o\
       $(DIRLAPACK)chegvd.o $(DIRLAPACK)shseqr.o $(DIRLAPACK)dpbequ.o\
       $(DIRLAPACK)chegv.o  $(DIRLAPACK)slabad.o $(DIRLAPACK)dpbrfs.o\
       $(DIRLAPACK)chegvx.o $(DIRLAPACK)slabrd.o $(DIRLAPACK)dpbstf.o\
       $(DIRLAPACK)cherfs.o $(DIRLAPACK)slacon.o $(DIRLAPACK)dpbsv.o \
       $(DIRLAPACK)chesv.o  $(DIRLAPACK)slacpy.o $(DIRLAPACK)dpbsvx.o\
       $(DIRLAPACK)chesvx.o $(DIRLAPACK)sladiv.o $(DIRLAPACK)dpbtf2.o\
       $(DIRLAPACK)chetd2.o $(DIRLAPACK)slae2.o  $(DIRLAPACK)dpbtrf.o\
       $(DIRLAPACK)chetf2.o $(DIRLAPACK)slaebz.o $(DIRLAPACK)dpbtrs.o\
       $(DIRLAPACK)chetrd.o $(DIRLAPACK)slaed0.o $(DIRLAPACK)dpocon.o\
       $(DIRLAPACK)chetrf.o $(DIRLAPACK)slaed1.o $(DIRLAPACK)dpoequ.o\
       $(DIRLAPACK)chetri.o $(DIRLAPACK)slaed2.o $(DIRLAPACK)dporfs.o\
       $(DIRLAPACK)chetrs.o $(DIRLAPACK)slaed3.o $(DIRLAPACK)dposv.o \
       $(DIRLAPACK)chgeqz.o $(DIRLAPACK)slaed4.o $(DIRLAPACK)dposvx.o\
       $(DIRLAPACK)chpcon.o $(DIRLAPACK)slaed5.o $(DIRLAPACK)dpotf2.o\
       $(DIRLAPACK)chpevd.o $(DIRLAPACK)slaed6.o $(DIRLAPACK)dpotrf.o\
       $(DIRLAPACK)chpev.o  $(DIRLAPACK)slaed7.o $(DIRLAPACK)dpotri.o\
       $(DIRLAPACK)chpevx.o $(DIRLAPACK)slaed8.o $(DIRLAPACK)dpotrs.o\
       $(DIRLAPACK)chpgst.o $(DIRLAPACK)slaed9.o $(DIRLAPACK)dppcon.o\
       $(DIRLAPACK)chpgvd.o $(DIRLAPACK)slaeda.o $(DIRLAPACK)dppequ.o\
       $(DIRLAPACK)chpgv.o  $(DIRLAPACK)slaein.o $(DIRLAPACK)dpprfs.o\
       $(DIRLAPACK)chpgvx.o $(DIRLAPACK)slaev2.o $(DIRLAPACK)dppsv.o \
       $(DIRLAPACK)chprfs.o $(DIRLAPACK)slaexc.o $(DIRLAPACK)dppsvx.o\
       $(DIRLAPACK)chpsv.o  $(DIRLAPACK)slag2.o  $(DIRLAPACK)dpptrf.o\
       $(DIRLAPACK)chpsvx.o $(DIRLAPACK)slags2.o $(DIRLAPACK)dpptri.o\
       $(DIRLAPACK)chptrd.o $(DIRLAPACK)slagtf.o $(DIRLAPACK)dpptrs.o\
       $(DIRLAPACK)chptrf.o $(DIRLAPACK)slagtm.o $(DIRLAPACK)dptcon.o\
       $(DIRLAPACK)chptri.o $(DIRLAPACK)slagts.o $(DIRLAPACK)dpteqr.o\
       $(DIRLAPACK)chptrs.o $(DIRLAPACK)slagv2.o $(DIRLAPACK)dptrfs.o\
       $(DIRLAPACK)chsein.o $(DIRLAPACK)slahqr.o $(DIRLAPACK)dptsv.o \
       $(DIRLAPACK)chseqr.o $(DIRLAPACK)slahrd.o $(DIRLAPACK)dptsvx.o\
       $(DIRLAPACK)clabrd.o $(DIRLAPACK)slaic1.o $(DIRLAPACK)dpttrf.o\
       $(DIRLAPACK)clacgv.o $(DIRLAPACK)slaln2.o $(DIRLAPACK)dpttrs.o\
       $(DIRLAPACK)clacon.o $(DIRLAPACK)slals0.o $(DIRLAPACK)dptts2.o\
       $(DIRLAPACK)clacp2.o $(DIRLAPACK)slalsa.o $(DIRLAPACK)drscl.o \
       $(DIRLAPACK)clacpy.o $(DIRLAPACK)slalsd.o $(DIRLAPACK)dsbevd.o\
       $(DIRLAPACK)clacrm.o $(DIRLAPACK)slamch.o $(DIRLAPACK)dsbev.o \
       $(DIRLAPACK)clacrt.o $(DIRLAPACK)slamrg.o $(DIRLAPACK)dsbevx.o\
       $(DIRLAPACK)cladiv.o $(DIRLAPACK)slangb.o $(DIRLAPACK)dsbgst.o\
       $(DIRLAPACK)claed0.o $(DIRLAPACK)slange.o $(DIRLAPACK)dsbgvd.o\
       $(DIRLAPACK)claed7.o $(DIRLAPACK)slangt.o $(DIRLAPACK)dsbgv.o \
       $(DIRLAPACK)claed8.o $(DIRLAPACK)slanhs.o $(DIRLAPACK)dsbgvx.o\
       $(DIRLAPACK)claein.o $(DIRLAPACK)slansb.o $(DIRLAPACK)dsbtrd.o\
       $(DIRLAPACK)claesy.o $(DIRLAPACK)slansp.o $(DIRLAPACK)dsecnd.o\
       $(DIRLAPACK)claev2.o $(DIRLAPACK)slanst.o $(DIRLAPACK)dspcon.o\
       $(DIRLAPACK)clags2.o $(DIRLAPACK)slansy.o $(DIRLAPACK)dspevd.o\
       $(DIRLAPACK)clagtm.o $(DIRLAPACK)slantb.o $(DIRLAPACK)dspev.o \
       $(DIRLAPACK)clahef.o $(DIRLAPACK)slantp.o $(DIRLAPACK)dspevx.o\
       $(DIRLAPACK)clahqr.o $(DIRLAPACK)slantr.o $(DIRLAPACK)dspgst.o\
       $(DIRLAPACK)clahrd.o $(DIRLAPACK)slanv2.o $(DIRLAPACK)dspgvd.o\
       $(DIRLAPACK)claic1.o $(DIRLAPACK)slapll.o $(DIRLAPACK)dspgv.o \
       $(DIRLAPACK)clals0.o $(DIRLAPACK)slapmt.o $(DIRLAPACK)dspgvx.o\
       $(DIRLAPACK)clalsa.o $(DIRLAPACK)slapy2.o $(DIRLAPACK)dsprfs.o\
       $(DIRLAPACK)clalsd.o $(DIRLAPACK)slapy3.o $(DIRLAPACK)dspsv.o \
       $(DIRLAPACK)clangb.o $(DIRLAPACK)slaqgb.o $(DIRLAPACK)dspsvx.o\
       $(DIRLAPACK)clange.o $(DIRLAPACK)slaqge.o $(DIRLAPACK)dsptrd.o\
       $(DIRLAPACK)clangt.o $(DIRLAPACK)slaqp2.o $(DIRLAPACK)dsptrf.o\
       $(DIRLAPACK)clanhb.o $(DIRLAPACK)slaqps.o $(DIRLAPACK)dsptri.o\
       $(DIRLAPACK)clanhe.o $(DIRLAPACK)slaqsb.o $(DIRLAPACK)dsptrs.o\
       $(DIRLAPACK)clanhp.o $(DIRLAPACK)slaqsp.o $(DIRLAPACK)dstebz.o\
       $(DIRLAPACK)clanhs.o $(DIRLAPACK)slaqsy.o $(DIRLAPACK)dstedc.o\
       $(DIRLAPACK)clanht.o $(DIRLAPACK)slaqtr.o $(DIRLAPACK)dstegr.o\
       $(DIRLAPACK)clansb.o $(DIRLAPACK)slar1v.o $(DIRLAPACK)dstein.o\
       $(DIRLAPACK)clansp.o $(DIRLAPACK)slar2v.o $(DIRLAPACK)dsteqr.o\
       $(DIRLAPACK)clansy.o $(DIRLAPACK)slarfb.o $(DIRLAPACK)dsterf.o\
       $(DIRLAPACK)clantb.o $(DIRLAPACK)slarf.o  $(DIRLAPACK)dstevd.o\
       $(DIRLAPACK)clantp.o $(DIRLAPACK)slarfg.o $(DIRLAPACK)dstev.o \
       $(DIRLAPACK)clantr.o $(DIRLAPACK)slarft.o $(DIRLAPACK)dstevr.o\
       $(DIRLAPACK)clapll.o $(DIRLAPACK)slarfx.o $(DIRLAPACK)dstevx.o\
       $(DIRLAPACK)clapmt.o $(DIRLAPACK)slargv.o $(DIRLAPACK)dsycon.o\
       $(DIRLAPACK)claqgb.o $(DIRLAPACK)slarnv.o $(DIRLAPACK)dsyevd.o\
       $(DIRLAPACK)claqge.o $(DIRLAPACK)slarrb.o $(DIRLAPACK)dsyev.o \
       $(DIRLAPACK)claqhb.o $(DIRLAPACK)slarre.o $(DIRLAPACK)dsyevr.o\
       $(DIRLAPACK)claqhe.o $(DIRLAPACK)slarrf.o $(DIRLAPACK)dsyevx.o\
       $(DIRLAPACK)claqhp.o $(DIRLAPACK)slarrv.o $(DIRLAPACK)dsygs2.o\
       $(DIRLAPACK)claqp2.o $(DIRLAPACK)slartg.o $(DIRLAPACK)dsygst.o\
       $(DIRLAPACK)claqps.o $(DIRLAPACK)slartv.o $(DIRLAPACK)dsygvd.o\
       $(DIRLAPACK)claqsb.o $(DIRLAPACK)slaruv.o $(DIRLAPACK)dsygv.o \
       $(DIRLAPACK)claqsp.o $(DIRLAPACK)slarzb.o $(DIRLAPACK)dsygvx.o\
       $(DIRLAPACK)claqsy.o $(DIRLAPACK)slarz.o  $(DIRLAPACK)dsyrfs.o\
       $(DIRLAPACK)clar1v.o $(DIRLAPACK)slarzt.o $(DIRLAPACK)dsysv.o \
       $(DIRLAPACK)clar2v.o $(DIRLAPACK)slas2.o  $(DIRLAPACK)dsysvx.o\
       $(DIRLAPACK)clarcm.o $(DIRLAPACK)slascl.o $(DIRLAPACK)dsytd2.o\
       $(DIRLAPACK)clarfb.o $(DIRLAPACK)slasd0.o $(DIRLAPACK)dsytf2.o\
       $(DIRLAPACK)clarf.o  $(DIRLAPACK)slasd1.o $(DIRLAPACK)dsytrd.o\
       $(DIRLAPACK)clarfg.o $(DIRLAPACK)slasd2.o $(DIRLAPACK)dsytrf.o\
       $(DIRLAPACK)clarft.o $(DIRLAPACK)slasd3.o $(DIRLAPACK)dsytri.o\
       $(DIRLAPACK)clarfx.o $(DIRLAPACK)slasd4.o $(DIRLAPACK)dsytrs.o\
       $(DIRLAPACK)clargv.o $(DIRLAPACK)slasd5.o $(DIRLAPACK)dtbcon.o\
       $(DIRLAPACK)clarnv.o $(DIRLAPACK)slasd6.o $(DIRLAPACK)dtbrfs.o\
       $(DIRLAPACK)clarrv.o $(DIRLAPACK)slasd7.o $(DIRLAPACK)dtbtrs.o\
       $(DIRLAPACK)clartg.o $(DIRLAPACK)slasd8.o $(DIRLAPACK)dtgevc.o\
       $(DIRLAPACK)clartv.o $(DIRLAPACK)slasd9.o $(DIRLAPACK)dtgex2.o\
       $(DIRLAPACK)clarzb.o $(DIRLAPACK)slasda.o $(DIRLAPACK)dtgexc.o\
       $(DIRLAPACK)clarz.o  $(DIRLAPACK)slasdq.o $(DIRLAPACK)dtgsen.o\
       $(DIRLAPACK)clarzt.o $(DIRLAPACK)slasdt.o $(DIRLAPACK)dtgsja.o\
       $(DIRLAPACK)clascl.o $(DIRLAPACK)slaset.o $(DIRLAPACK)dtgsna.o\
       $(DIRLAPACK)claset.o $(DIRLAPACK)slasq1.o $(DIRLAPACK)dtgsy2.o\
       $(DIRLAPACK)clasr.o  $(DIRLAPACK)slasq2.o $(DIRLAPACK)dtgsyl.o\
       $(DIRLAPACK)classq.o $(DIRLAPACK)slasq3.o $(DIRLAPACK)dtpcon.o\
       $(DIRLAPACK)claswp.o $(DIRLAPACK)slasq4.o $(DIRLAPACK)dtprfs.o\
       $(DIRLAPACK)clasyf.o $(DIRLAPACK)slasq5.o $(DIRLAPACK)dtptri.o
LAPACK2=$(DIRLAPACK)clatbs.o $(DIRLAPACK)slasq6.o $(DIRLAPACK)dtptrs.o\
       $(DIRLAPACK)clatdf.o $(DIRLAPACK)slasr.o  $(DIRLAPACK)dtrcon.o\
       $(DIRLAPACK)clatps.o $(DIRLAPACK)slasrt.o $(DIRLAPACK)dtrevc.o\
       $(DIRLAPACK)clatrd.o $(DIRLAPACK)slassq.o $(DIRLAPACK)dtrexc.o\
       $(DIRLAPACK)clatrs.o $(DIRLAPACK)slasv2.o $(DIRLAPACK)dtrrfs.o\
       $(DIRLAPACK)clatrz.o $(DIRLAPACK)slaswp.o $(DIRLAPACK)dtrsen.o\
       $(DIRLAPACK)clatzm.o $(DIRLAPACK)slasy2.o $(DIRLAPACK)dtrsna.o\
       $(DIRLAPACK)clauu2.o $(DIRLAPACK)slasyf.o $(DIRLAPACK)dtrsyl.o\
       $(DIRLAPACK)clauum.o $(DIRLAPACK)slatbs.o $(DIRLAPACK)dtrti2.o\
       $(DIRLAPACK)cpbcon.o $(DIRLAPACK)slatdf.o $(DIRLAPACK)dtrtri.o\
       $(DIRLAPACK)cpbequ.o $(DIRLAPACK)slatps.o $(DIRLAPACK)dtrtrs.o\
       $(DIRLAPACK)cpbrfs.o $(DIRLAPACK)slatrd.o $(DIRLAPACK)zunmbr.o\
       $(DIRLAPACK)cpbstf.o $(DIRLAPACK)slatrs.o $(DIRLAPACK)zunmhr.o\
       $(DIRLAPACK)cpbsv.o  $(DIRLAPACK)slatrz.o $(DIRLAPACK)zunml2.o\
       $(DIRLAPACK)cpbsvx.o $(DIRLAPACK)slatzm.o $(DIRLAPACK)zunmlq.o\
       $(DIRLAPACK)cpbtf2.o $(DIRLAPACK)slauu2.o $(DIRLAPACK)zunmql.o\
       $(DIRLAPACK)cpbtrf.o $(DIRLAPACK)slauum.o $(DIRLAPACK)zunmqr.o\
       $(DIRLAPACK)cpbtrs.o $(DIRLAPACK)sopgtr.o $(DIRLAPACK)zupmtr.o\
       $(DIRLAPACK)cpocon.o $(DIRLAPACK)sopmtr.o $(DIRLAPACK)zhetrs.o\
       $(DIRLAPACK)cpoequ.o $(DIRLAPACK)sorg2l.o $(DIRLAPACK)zhgeqz.o\
       $(DIRLAPACK)cporfs.o $(DIRLAPACK)sorg2r.o $(DIRLAPACK)zhpcon.o\
       $(DIRLAPACK)cposv.o  $(DIRLAPACK)sorgbr.o $(DIRLAPACK)zhpevd.o\
       $(DIRLAPACK)cposvx.o $(DIRLAPACK)sorghr.o $(DIRLAPACK)zhpev.o \
       $(DIRLAPACK)cpotf2.o $(DIRLAPACK)sorgl2.o $(DIRLAPACK)zhpevx.o\
       $(DIRLAPACK)cpotrf.o $(DIRLAPACK)sorglq.o $(DIRLAPACK)zhpgst.o\
       $(DIRLAPACK)cpotri.o $(DIRLAPACK)sorgql.o $(DIRLAPACK)zhpgvd.o\
       $(DIRLAPACK)cpotrs.o $(DIRLAPACK)sorgqr.o $(DIRLAPACK)zhpgv.o \
       $(DIRLAPACK)cppcon.o $(DIRLAPACK)sorgr2.o $(DIRLAPACK)zhpgvx.o\
       $(DIRLAPACK)cppequ.o $(DIRLAPACK)sorgrq.o $(DIRLAPACK)zhprfs.o\
       $(DIRLAPACK)cpprfs.o $(DIRLAPACK)sorgtr.o $(DIRLAPACK)zhpsv.o \
       $(DIRLAPACK)cppsv.o  $(DIRLAPACK)sorm2l.o $(DIRLAPACK)zhpsvx.o\
       $(DIRLAPACK)cppsvx.o $(DIRLAPACK)sorm2r.o $(DIRLAPACK)zhptrd.o\
       $(DIRLAPACK)cpptrf.o $(DIRLAPACK)sormbr.o $(DIRLAPACK)zhptrf.o\
       $(DIRLAPACK)cpptri.o $(DIRLAPACK)sormhr.o $(DIRLAPACK)zhptri.o\
       $(DIRLAPACK)cpptrs.o $(DIRLAPACK)sorml2.o $(DIRLAPACK)zhptrs.o\
       $(DIRLAPACK)cptcon.o $(DIRLAPACK)sormlq.o $(DIRLAPACK)zhsein.o\
       $(DIRLAPACK)cpteqr.o $(DIRLAPACK)sormql.o $(DIRLAPACK)zhseqr.o\
       $(DIRLAPACK)cptrfs.o $(DIRLAPACK)sormqr.o $(DIRLAPACK)zlabrd.o\
       $(DIRLAPACK)cptsv.o  $(DIRLAPACK)sormr2.o $(DIRLAPACK)zlacgv.o\
       $(DIRLAPACK)cptsvx.o $(DIRLAPACK)sormr3.o $(DIRLAPACK)zlacon.o\
       $(DIRLAPACK)cpttrf.o $(DIRLAPACK)sormrq.o $(DIRLAPACK)zlacp2.o\
       $(DIRLAPACK)cpttrs.o $(DIRLAPACK)sormrz.o $(DIRLAPACK)zlacpy.o\
       $(DIRLAPACK)cptts2.o $(DIRLAPACK)sormtr.o $(DIRLAPACK)zlacrm.o\
       $(DIRLAPACK)crot.o   $(DIRLAPACK)spbcon.o $(DIRLAPACK)zlacrt.o\
       $(DIRLAPACK)cspcon.o $(DIRLAPACK)spbequ.o $(DIRLAPACK)zladiv.o\
       $(DIRLAPACK)cspmv.o  $(DIRLAPACK)spbrfs.o $(DIRLAPACK)zlaed0.o\
       $(DIRLAPACK)cspr.o   $(DIRLAPACK)spbstf.o $(DIRLAPACK)zlaed7.o\
       $(DIRLAPACK)csprfs.o $(DIRLAPACK)spbsv.o  $(DIRLAPACK)zlaed8.o\
       $(DIRLAPACK)cspsv.o  $(DIRLAPACK)spbsvx.o $(DIRLAPACK)zlaein.o\
       $(DIRLAPACK)cspsvx.o $(DIRLAPACK)spbtf2.o $(DIRLAPACK)zlaesy.o\
       $(DIRLAPACK)csptrf.o $(DIRLAPACK)spbtrf.o $(DIRLAPACK)zlaev2.o\
       $(DIRLAPACK)csptri.o $(DIRLAPACK)spbtrs.o $(DIRLAPACK)zlags2.o\
       $(DIRLAPACK)csptrs.o $(DIRLAPACK)spocon.o $(DIRLAPACK)zlagtm.o\
       $(DIRLAPACK)csrot.o  $(DIRLAPACK)spoequ.o $(DIRLAPACK)zlahef.o\
       $(DIRLAPACK)csrscl.o $(DIRLAPACK)sporfs.o $(DIRLAPACK)zlahqr.o\
       $(DIRLAPACK)cstedc.o $(DIRLAPACK)sposv.o  $(DIRLAPACK)zlahrd.o\
       $(DIRLAPACK)cstegr.o $(DIRLAPACK)sposvx.o $(DIRLAPACK)zlaic1.o\
       $(DIRLAPACK)cstein.o $(DIRLAPACK)spotf2.o $(DIRLAPACK)zlals0.o\
       $(DIRLAPACK)csteqr.o $(DIRLAPACK)spotrf.o $(DIRLAPACK)zlalsa.o\
       $(DIRLAPACK)csycon.o $(DIRLAPACK)spotri.o $(DIRLAPACK)zlalsd.o\
       $(DIRLAPACK)csymv.o  $(DIRLAPACK)spotrs.o $(DIRLAPACK)zlangb.o\
       $(DIRLAPACK)csyr.o   $(DIRLAPACK)sppcon.o $(DIRLAPACK)zlange.o\
       $(DIRLAPACK)csyrfs.o $(DIRLAPACK)sppequ.o $(DIRLAPACK)zlangt.o\
       $(DIRLAPACK)csysv.o  $(DIRLAPACK)spprfs.o $(DIRLAPACK)zlanhb.o\
       $(DIRLAPACK)csysvx.o $(DIRLAPACK)sppsv.o  $(DIRLAPACK)zlanhe.o\
       $(DIRLAPACK)csytf2.o $(DIRLAPACK)sppsvx.o $(DIRLAPACK)zlanhp.o\
       $(DIRLAPACK)csytrf.o $(DIRLAPACK)spptrf.o $(DIRLAPACK)zlanhs.o\
       $(DIRLAPACK)csytri.o $(DIRLAPACK)spptri.o $(DIRLAPACK)zlanht.o\
       $(DIRLAPACK)csytrs.o $(DIRLAPACK)spptrs.o $(DIRLAPACK)zlansb.o\
       $(DIRLAPACK)ctbcon.o $(DIRLAPACK)sptcon.o $(DIRLAPACK)zlansp.o\
       $(DIRLAPACK)ctbrfs.o $(DIRLAPACK)spteqr.o $(DIRLAPACK)zlansy.o\
       $(DIRLAPACK)ctbtrs.o $(DIRLAPACK)sptrfs.o $(DIRLAPACK)zlantb.o\
       $(DIRLAPACK)ctgevc.o $(DIRLAPACK)sptsv.o  $(DIRLAPACK)zlantp.o\
       $(DIRLAPACK)ctgex2.o $(DIRLAPACK)sptsvx.o $(DIRLAPACK)zlantr.o\
       $(DIRLAPACK)ctgexc.o $(DIRLAPACK)spttrf.o $(DIRLAPACK)zlapll.o\
       $(DIRLAPACK)ctgsen.o $(DIRLAPACK)spttrs.o $(DIRLAPACK)zlapmt.o\
       $(DIRLAPACK)ctgsja.o $(DIRLAPACK)sptts2.o $(DIRLAPACK)zlaqgb.o\
       $(DIRLAPACK)ctgsna.o $(DIRLAPACK)srscl.o  $(DIRLAPACK)zlaqge.o\
       $(DIRLAPACK)ctgsy2.o $(DIRLAPACK)ssbevd.o $(DIRLAPACK)zlaqhb.o\
       $(DIRLAPACK)ctgsyl.o $(DIRLAPACK)ssbev.o  $(DIRLAPACK)zlaqhe.o\
       $(DIRLAPACK)ctpcon.o $(DIRLAPACK)ssbevx.o $(DIRLAPACK)zlaqhp.o\
       $(DIRLAPACK)ctprfs.o $(DIRLAPACK)ssbgst.o $(DIRLAPACK)zlaqp2.o\
       $(DIRLAPACK)ctptri.o $(DIRLAPACK)ssbgvd.o $(DIRLAPACK)zlaqps.o\
       $(DIRLAPACK)ctptrs.o $(DIRLAPACK)ssbgv.o  $(DIRLAPACK)zlaqsb.o\
       $(DIRLAPACK)ctrcon.o $(DIRLAPACK)ssbgvx.o $(DIRLAPACK)zlaqsp.o\
       $(DIRLAPACK)ctrevc.o $(DIRLAPACK)ssbtrd.o $(DIRLAPACK)zlaqsy.o\
       $(DIRLAPACK)ctrexc.o $(DIRLAPACK)sspcon.o $(DIRLAPACK)zlar1v.o\
       $(DIRLAPACK)ctrrfs.o $(DIRLAPACK)sspevd.o $(DIRLAPACK)zlar2v.o\
       $(DIRLAPACK)ctrsen.o $(DIRLAPACK)sspev.o  $(DIRLAPACK)zlarcm.o\
       $(DIRLAPACK)ctrsna.o $(DIRLAPACK)sspevx.o $(DIRLAPACK)zlarfb.o\
       $(DIRLAPACK)ctrsyl.o $(DIRLAPACK)sspgst.o $(DIRLAPACK)zlarf.o \
       $(DIRLAPACK)ctrti2.o $(DIRLAPACK)sspgvd.o $(DIRLAPACK)zlarfg.o\
       $(DIRLAPACK)ctrtri.o $(DIRLAPACK)sspgv.o  $(DIRLAPACK)zlarft.o\
       $(DIRLAPACK)ctrtrs.o $(DIRLAPACK)sspgvx.o $(DIRLAPACK)zlarfx.o\
       $(DIRLAPACK)ctzrqf.o $(DIRLAPACK)ssprfs.o $(DIRLAPACK)zlargv.o\
       $(DIRLAPACK)ctzrzf.o $(DIRLAPACK)sspsv.o  $(DIRLAPACK)zlarnv.o\
       $(DIRLAPACK)cung2l.o $(DIRLAPACK)sspsvx.o $(DIRLAPACK)zlarrv.o\
       $(DIRLAPACK)cung2r.o $(DIRLAPACK)ssptrd.o $(DIRLAPACK)zlartg.o\
       $(DIRLAPACK)cungbr.o $(DIRLAPACK)ssptrf.o $(DIRLAPACK)zlartv.o\
       $(DIRLAPACK)cunghr.o $(DIRLAPACK)ssptri.o $(DIRLAPACK)zlarzb.o\
       $(DIRLAPACK)cungl2.o $(DIRLAPACK)ssptrs.o $(DIRLAPACK)zlarz.o \
       $(DIRLAPACK)cunglq.o $(DIRLAPACK)sstebz.o $(DIRLAPACK)zlarzt.o\
       $(DIRLAPACK)cungql.o $(DIRLAPACK)sstedc.o $(DIRLAPACK)zlascl.o\
       $(DIRLAPACK)cungqr.o $(DIRLAPACK)sstegr.o $(DIRLAPACK)zlaset.o\
       $(DIRLAPACK)cungr2.o $(DIRLAPACK)sstein.o $(DIRLAPACK)zlasr.o \
       $(DIRLAPACK)cungrq.o $(DIRLAPACK)ssteqr.o $(DIRLAPACK)zlassq.o\
       $(DIRLAPACK)cungtr.o $(DIRLAPACK)ssterf.o $(DIRLAPACK)zlaswp.o\
       $(DIRLAPACK)cunm2l.o $(DIRLAPACK)sstevd.o $(DIRLAPACK)zlasyf.o\
       $(DIRLAPACK)cunm2r.o $(DIRLAPACK)sstev.o  $(DIRLAPACK)zlatbs.o\
       $(DIRLAPACK)cunmbr.o $(DIRLAPACK)sstevr.o $(DIRLAPACK)zlatdf.o\
       $(DIRLAPACK)cunmhr.o $(DIRLAPACK)sstevx.o $(DIRLAPACK)zlatps.o\
       $(DIRLAPACK)cunml2.o $(DIRLAPACK)ssycon.o $(DIRLAPACK)zlatrd.o\
       $(DIRLAPACK)cunmlq.o $(DIRLAPACK)ssyevd.o $(DIRLAPACK)zlatrs.o\
       $(DIRLAPACK)cunmql.o $(DIRLAPACK)ssyev.o  $(DIRLAPACK)zlatrz.o\
       $(DIRLAPACK)cunmqr.o $(DIRLAPACK)ssyevr.o $(DIRLAPACK)zlatzm.o\
       $(DIRLAPACK)cunmr2.o $(DIRLAPACK)ssyevx.o $(DIRLAPACK)zlauu2.o\
       $(DIRLAPACK)cunmr3.o $(DIRLAPACK)ssygs2.o $(DIRLAPACK)zlauum.o\
       $(DIRLAPACK)cunmrq.o $(DIRLAPACK)ssygst.o $(DIRLAPACK)zpbcon.o\
       $(DIRLAPACK)cunmrz.o $(DIRLAPACK)ssygvd.o $(DIRLAPACK)zpbequ.o\
       $(DIRLAPACK)cunmtr.o $(DIRLAPACK)ssygv.o  $(DIRLAPACK)zpbrfs.o\
       $(DIRLAPACK)cupgtr.o $(DIRLAPACK)ssygvx.o $(DIRLAPACK)zpbstf.o\
       $(DIRLAPACK)cupmtr.o $(DIRLAPACK)ssyrfs.o $(DIRLAPACK)zpbsv.o \
       $(DIRLAPACK)dbdsdc.o $(DIRLAPACK)ssysv.o  $(DIRLAPACK)zpbsvx.o\
       $(DIRLAPACK)dbdsqr.o $(DIRLAPACK)ssysvx.o $(DIRLAPACK)zpbtf2.o\
       $(DIRLAPACK)ddisna.o $(DIRLAPACK)ssytd2.o $(DIRLAPACK)zpbtrf.o\
       $(DIRLAPACK)dgbbrd.o $(DIRLAPACK)ssytf2.o $(DIRLAPACK)zpbtrs.o\
       $(DIRLAPACK)dgbcon.o $(DIRLAPACK)ssytrd.o $(DIRLAPACK)zpocon.o\
       $(DIRLAPACK)dgbequ.o $(DIRLAPACK)ssytrf.o $(DIRLAPACK)zpoequ.o\
       $(DIRLAPACK)dgbrfs.o $(DIRLAPACK)ssytri.o $(DIRLAPACK)zporfs.o\
       $(DIRLAPACK)dgbsv.o  $(DIRLAPACK)ssytrs.o $(DIRLAPACK)zposv.o \
       $(DIRLAPACK)dgbsvx.o $(DIRLAPACK)stbcon.o $(DIRLAPACK)zposvx.o\
       $(DIRLAPACK)dgbtf2.o $(DIRLAPACK)stbrfs.o $(DIRLAPACK)zpotf2.o\
       $(DIRLAPACK)dgbtrf.o $(DIRLAPACK)stbtrs.o $(DIRLAPACK)zpotrf.o\
       $(DIRLAPACK)dgbtrs.o $(DIRLAPACK)stgevc.o $(DIRLAPACK)zpotri.o\
       $(DIRLAPACK)dgebak.o $(DIRLAPACK)stgex2.o $(DIRLAPACK)zpotrs.o\
       $(DIRLAPACK)dgebal.o $(DIRLAPACK)stgexc.o $(DIRLAPACK)zppcon.o\
       $(DIRLAPACK)dgebd2.o $(DIRLAPACK)stgsen.o $(DIRLAPACK)zppequ.o\
       $(DIRLAPACK)dgebrd.o $(DIRLAPACK)stgsja.o $(DIRLAPACK)zpprfs.o\
       $(DIRLAPACK)dgecon.o $(DIRLAPACK)stgsna.o $(DIRLAPACK)zppsv.o \
       $(DIRLAPACK)dgeequ.o $(DIRLAPACK)stgsy2.o $(DIRLAPACK)zppsvx.o\
       $(DIRLAPACK)dgees.o  $(DIRLAPACK)stgsyl.o $(DIRLAPACK)zpptrf.o\
       $(DIRLAPACK)dgeesx.o $(DIRLAPACK)stpcon.o $(DIRLAPACK)zpptri.o\
       $(DIRLAPACK)dgeev.o  $(DIRLAPACK)stprfs.o $(DIRLAPACK)zpptrs.o\
       $(DIRLAPACK)dgeevx.o $(DIRLAPACK)stptri.o $(DIRLAPACK)zptcon.o\
       $(DIRLAPACK)dgegs.o  $(DIRLAPACK)stptrs.o $(DIRLAPACK)zpteqr.o\
       $(DIRLAPACK)dgegv.o  $(DIRLAPACK)strcon.o $(DIRLAPACK)zptrfs.o\
       $(DIRLAPACK)dgehd2.o $(DIRLAPACK)strevc.o $(DIRLAPACK)zptsv.o \
       $(DIRLAPACK)dgehrd.o $(DIRLAPACK)strexc.o $(DIRLAPACK)zptsvx.o\
       $(DIRLAPACK)dgelq2.o $(DIRLAPACK)strrfs.o $(DIRLAPACK)zpttrf.o\
       $(DIRLAPACK)dgelqf.o $(DIRLAPACK)strsen.o $(DIRLAPACK)zpttrs.o\
       $(DIRLAPACK)dgelsd.o $(DIRLAPACK)strsna.o $(DIRLAPACK)zptts2.o\
       $(DIRLAPACK)dgels.o  $(DIRLAPACK)strsyl.o $(DIRLAPACK)zrot.o  \
       $(DIRLAPACK)dgelss.o $(DIRLAPACK)strti2.o $(DIRLAPACK)zspcon.o\
       $(DIRLAPACK)dgelsx.o $(DIRLAPACK)strtri.o $(DIRLAPACK)zspmv.o \
       $(DIRLAPACK)dgelsy.o $(DIRLAPACK)strtrs.o $(DIRLAPACK)zspr.o  \
       $(DIRLAPACK)dgeql2.o $(DIRLAPACK)stzrqf.o $(DIRLAPACK)zsprfs.o\
       $(DIRLAPACK)dgeqlf.o $(DIRLAPACK)stzrzf.o $(DIRLAPACK)zspsv.o \
       $(DIRLAPACK)dgeqp3.o $(DIRLAPACK)xerbla.o $(DIRLAPACK)zspsvx.o\
       $(DIRLAPACK)dgeqpf.o $(DIRLAPACK)zbdsqr.o $(DIRLAPACK)zsptrf.o\
       $(DIRLAPACK)dgeqr2.o $(DIRLAPACK)zdrot.o  $(DIRLAPACK)zsptri.o\
       $(DIRLAPACK)dgeqrf.o $(DIRLAPACK)zdrscl.o $(DIRLAPACK)zsptrs.o\
       $(DIRLAPACK)dgerfs.o $(DIRLAPACK)zgbbrd.o $(DIRLAPACK)zstedc.o\
       $(DIRLAPACK)dgerq2.o $(DIRLAPACK)zgbcon.o $(DIRLAPACK)zstegr.o\
       $(DIRLAPACK)dgerqf.o $(DIRLAPACK)zgbequ.o $(DIRLAPACK)zstein.o\
       $(DIRLAPACK)dgesc2.o $(DIRLAPACK)zgbrfs.o $(DIRLAPACK)zsteqr.o\
       $(DIRLAPACK)dgesdd.o $(DIRLAPACK)zgbsv.o  $(DIRLAPACK)zsycon.o\
       $(DIRLAPACK)dgesvd.o $(DIRLAPACK)zgbsvx.o $(DIRLAPACK)zsymv.o \
       $(DIRLAPACK)dgesv.o  $(DIRLAPACK)zgbtf2.o $(DIRLAPACK)zsyr.o  \
       $(DIRLAPACK)dgesvx.o $(DIRLAPACK)zgbtrf.o $(DIRLAPACK)zsyrfs.o\
       $(DIRLAPACK)dgetc2.o $(DIRLAPACK)zgbtrs.o $(DIRLAPACK)zsysv.o \
       $(DIRLAPACK)dgetf2.o $(DIRLAPACK)zgebak.o $(DIRLAPACK)zsysvx.o\
       $(DIRLAPACK)dgetrf.o $(DIRLAPACK)zgebal.o $(DIRLAPACK)zsytf2.o\
       $(DIRLAPACK)dgetri.o $(DIRLAPACK)zgebd2.o $(DIRLAPACK)zsytrf.o\
       $(DIRLAPACK)dgetrs.o $(DIRLAPACK)zgebrd.o $(DIRLAPACK)zsytri.o\
       $(DIRLAPACK)dggbak.o $(DIRLAPACK)zgecon.o $(DIRLAPACK)zsytrs.o\
       $(DIRLAPACK)dggbal.o $(DIRLAPACK)zgeequ.o $(DIRLAPACK)ztbcon.o\
       $(DIRLAPACK)dgges.o  $(DIRLAPACK)zgees.o  $(DIRLAPACK)ztbrfs.o\
       $(DIRLAPACK)dggesx.o $(DIRLAPACK)zgeesx.o $(DIRLAPACK)ztbtrs.o\
       $(DIRLAPACK)dggev.o  $(DIRLAPACK)zgeev.o  $(DIRLAPACK)ztgevc.o\
       $(DIRLAPACK)dggevx.o $(DIRLAPACK)zgeevx.o $(DIRLAPACK)ztgex2.o\
       $(DIRLAPACK)dggglm.o $(DIRLAPACK)zgegs.o  $(DIRLAPACK)ztgexc.o\
       $(DIRLAPACK)dgghrd.o $(DIRLAPACK)zgegv.o  $(DIRLAPACK)ztgsen.o\
       $(DIRLAPACK)dgglse.o $(DIRLAPACK)zgehd2.o $(DIRLAPACK)ztgsja.o\
       $(DIRLAPACK)dggqrf.o $(DIRLAPACK)zgehrd.o $(DIRLAPACK)ztgsna.o\
       $(DIRLAPACK)dggrqf.o $(DIRLAPACK)zgelq2.o $(DIRLAPACK)ztgsy2.o\
       $(DIRLAPACK)dggsvd.o $(DIRLAPACK)zgelqf.o $(DIRLAPACK)ztgsyl.o\
       $(DIRLAPACK)dggsvp.o $(DIRLAPACK)zgelsd.o $(DIRLAPACK)ztpcon.o\
       $(DIRLAPACK)dgtcon.o $(DIRLAPACK)zgels.o  $(DIRLAPACK)ztprfs.o\
       $(DIRLAPACK)dgtrfs.o $(DIRLAPACK)zgelss.o $(DIRLAPACK)ztptri.o\
       $(DIRLAPACK)dgtsv.o  $(DIRLAPACK)zgelsx.o $(DIRLAPACK)ztptrs.o\
       $(DIRLAPACK)dgtsvx.o $(DIRLAPACK)zgelsy.o $(DIRLAPACK)ztrcon.o\
       $(DIRLAPACK)dgttrf.o $(DIRLAPACK)zgeql2.o $(DIRLAPACK)ztrevc.o\
       $(DIRLAPACK)dgttrs.o $(DIRLAPACK)zgeqlf.o $(DIRLAPACK)ztrexc.o\
       $(DIRLAPACK)dgtts2.o $(DIRLAPACK)zgeqp3.o $(DIRLAPACK)ztrrfs.o\
       $(DIRLAPACK)dhgeqz.o $(DIRLAPACK)zgeqpf.o $(DIRLAPACK)ztrsen.o\
       $(DIRLAPACK)dhsein.o $(DIRLAPACK)zgeqr2.o $(DIRLAPACK)ztrsna.o\
       $(DIRLAPACK)dhseqr.o $(DIRLAPACK)zgeqrf.o $(DIRLAPACK)ztrsyl.o\
       $(DIRLAPACK)dlabad.o $(DIRLAPACK)zgerfs.o $(DIRLAPACK)ztrti2.o\
       $(DIRLAPACK)dlabrd.o $(DIRLAPACK)zgerq2.o $(DIRLAPACK)ztrtri.o\
       $(DIRLAPACK)dlacon.o $(DIRLAPACK)zgerqf.o $(DIRLAPACK)ztrtrs.o\
       $(DIRLAPACK)dlacpy.o $(DIRLAPACK)zgesc2.o $(DIRLAPACK)ztzrqf.o\
       $(DIRLAPACK)dladiv.o $(DIRLAPACK)zgesdd.o $(DIRLAPACK)ztzrzf.o\
       $(DIRLAPACK)dlae2.o  $(DIRLAPACK)zgesvd.o $(DIRLAPACK)zung2l.o\
       $(DIRLAPACK)dlaebz.o $(DIRLAPACK)zgesv.o  $(DIRLAPACK)zung2r.o\
       $(DIRLAPACK)dlaed0.o $(DIRLAPACK)zgesvx.o $(DIRLAPACK)zungbr.o\
       $(DIRLAPACK)dlaed1.o $(DIRLAPACK)zgetc2.o $(DIRLAPACK)zunghr.o\
       $(DIRLAPACK)dlaed2.o $(DIRLAPACK)zgetf2.o $(DIRLAPACK)zungl2.o\
       $(DIRLAPACK)dlaed3.o $(DIRLAPACK)zgetrf.o $(DIRLAPACK)zunglq.o\
       $(DIRLAPACK)dlaed4.o $(DIRLAPACK)zgetri.o $(DIRLAPACK)zungql.o\
       $(DIRLAPACK)dlaed5.o $(DIRLAPACK)zgetrs.o $(DIRLAPACK)zungqr.o\
       $(DIRLAPACK)dlaed6.o $(DIRLAPACK)zggbak.o $(DIRLAPACK)zungr2.o\
       $(DIRLAPACK)dlaed7.o $(DIRLAPACK)zggbal.o $(DIRLAPACK)zungrq.o\
       $(DIRLAPACK)dlaed8.o $(DIRLAPACK)zgges.o  $(DIRLAPACK)zungtr.o\
       $(DIRLAPACK)dlaed9.o $(DIRLAPACK)zggesx.o $(DIRLAPACK)zunm2l.o\
       $(DIRLAPACK)dlaeda.o $(DIRLAPACK)zggev.o  $(DIRLAPACK)zunm2r.o\
       $(DIRLAPACK)dlaein.o $(DIRLAPACK)zggevx.o $(DIRLAPACK)zunmr2.o\
       $(DIRLAPACK)dlaev2.o $(DIRLAPACK)zggglm.o $(DIRLAPACK)zunmr3.o\
       $(DIRLAPACK)dlaexc.o $(DIRLAPACK)zgghrd.o $(DIRLAPACK)zunmrq.o\
       $(DIRLAPACK)dlag2.o  $(DIRLAPACK)zgglse.o $(DIRLAPACK)zunmrz.o\
       $(DIRLAPACK)dlags2.o $(DIRLAPACK)zggqrf.o $(DIRLAPACK)zunmtr.o\
       $(DIRLAPACK)dlagtf.o $(DIRLAPACK)zggrqf.o $(DIRLAPACK)zupgtr.o\
       $(DIRLAPACK)dlagtm.o $(DIRLAPACK)zggsvd.o $(DIRLAPACK)dlapmt.o\
       $(DIRLAPACK)dlagts.o $(DIRLAPACK)zggsvp.o $(DIRLAPACK)dlapy2.o\
       $(DIRLAPACK)dlagv2.o $(DIRLAPACK)zgtcon.o $(DIRLAPACK)dlapy3.o\
       $(DIRLAPACK)dlahqr.o $(DIRLAPACK)zgtrfs.o $(DIRLAPACK)dlaqgb.o\
       $(DIRLAPACK)dlahrd.o $(DIRLAPACK)zgtsv.o  $(DIRLAPACK)dlaqge.o\
       $(DIRLAPACK)dlaic1.o $(DIRLAPACK)zgtsvx.o $(DIRLAPACK)dlaqp2.o\
       $(DIRLAPACK)dlaln2.o $(DIRLAPACK)zgttrf.o $(DIRLAPACK)dlaqps.o\
       $(DIRLAPACK)dlals0.o $(DIRLAPACK)zgttrs.o $(DIRLAPACK)dlaqsb.o\
       $(DIRLAPACK)dlalsa.o $(DIRLAPACK)zgtts2.o $(DIRLAPACK)dlaqsp.o\
       $(DIRLAPACK)dlalsd.o $(DIRLAPACK)zhbevd.o $(DIRLAPACK)dlaqsy.o\
       $(DIRLAPACK)dlamch.o $(DIRLAPACK)zhbev.o  $(DIRLAPACK)zhegv.o \
       $(DIRLAPACK)dlamrg.o $(DIRLAPACK)zhbevx.o $(DIRLAPACK)zhegvx.o\
       $(DIRLAPACK)dlangb.o $(DIRLAPACK)zhbgst.o $(DIRLAPACK)zherfs.o\
       $(DIRLAPACK)dlange.o $(DIRLAPACK)zhbgvd.o $(DIRLAPACK)zhesv.o \
       $(DIRLAPACK)dlangt.o $(DIRLAPACK)zhbgv.o  $(DIRLAPACK)zhesvx.o\
       $(DIRLAPACK)dlanhs.o $(DIRLAPACK)zhbgvx.o $(DIRLAPACK)zhetd2.o\
       $(DIRLAPACK)dlansb.o $(DIRLAPACK)zhbtrd.o $(DIRLAPACK)zhetf2.o\
       $(DIRLAPACK)dlansp.o $(DIRLAPACK)zhecon.o $(DIRLAPACK)zhetrd.o\
       $(DIRLAPACK)dlanst.o $(DIRLAPACK)zheevd.o $(DIRLAPACK)zhetrf.o\
       $(DIRLAPACK)dlansy.o $(DIRLAPACK)zheev.o  $(DIRLAPACK)zhetri.o\
       $(DIRLAPACK)dlantb.o $(DIRLAPACK)zheevr.o $(DIRLAPACK)dlanv2.o\
       $(DIRLAPACK)dlantp.o $(DIRLAPACK)zheevx.o $(DIRLAPACK)dlapll.o\
       $(DIRLAPACK)dlantr.o $(DIRLAPACK)zhegs2.o $(DIRLAPACK)zhegst.o\
       $(DIRLAPACK)zhegvd.o





SAMPLES=$(MAIN).o



STARTDIR=$(PWD)

MYSTARTDIR=$(STARTDIR)

# Linux
#LIBS=-ljadamilu  -llapack -lblas -lm -lc 

# HP alpha
#LIBS=-ljadamilu -llapack -lcxml -lblas -lm -lc -lfor

# IBM AIX
#LIBS=-ljadamilu -llapack -lblas  -lm -lc

# SGI Altix
#LIBS=-ljadamilu  -lmkl_lapack -lmkl  -lm -lc  -lifcore -lguide



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



