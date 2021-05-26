{
   // -------------------------------------------------------------------
   // --------------------     scaling prefix     -----------------------
   // -------------------------------------------------------------------

   /*
     param->ipar[7] gives information about the requested scaling

     param->ipar[7] &  512       indicates initial preprocessing,
                                 initial permutation routine in
                                 the main ILU driver called AMGFACTOR

     param->ipar[7] & 1024       indicates regular reordering,
                                 the second permutation routine in
                                 the main ILU driver called AMGFACTOR

     param->ipar[7] &  512+1024  indicates final pivoting,
                                 the third permutation routine in
                                 the main ILU driver called AMGFACTOR


     depending on the circumstances (initial preprocessing, regular
     reordering, final pivoting)

     param->ipar[7] & ( 1+  2+  4)   initial preprocessing

     param->ipar[7] & ( 8+ 16+ 32)   regular reordering

     param->ipar[7] & (64+128+256)   final pivoting

     give information on whether row scaling (lowest bit 1,8,64), 
     column scaling (medium bit 2,16,128) should be applied.
     The highest bit (4,32,256) defines the order in which the 
     scalings should be perfomed. If the highest bit is cleared, then
     we start with row scaling.

    
     param->ipar[8] defines the norm that should be used

     param->ipar[8] & (   1+   2+   4+   8+   16)  initial preprocessing

     param->ipar[8] & (  32+  64+ 128+ 256+  512)  regular reordering

     param->ipar[8] & (1024+2048+4096+8192+16384)  final pivoting

     The five bits (values [0,...,31] up to shifts) are used for
     0,1,2          infinity norm, 1-norm, 2-norm
     3              spd scaling using the square root of the diagonal
                    entries
     4              symmetric indefinite scaling ensuring that the 
                    entries are less than one in absolute value with 
                    target that the maximum in each row/column is one.

     The scaling routines that are defined with ILUPACK only use
     the nearest powers of 2 for scaling
    */


   // initial preprocessing
   if ((param->ipar[7]&(512+1024))==512) {
      scale=param->ipar[7]&(1+2+4);
      nrm=param->ipar[8]&(1+2+4+8+16);
   }
   // regular reordering
   else if ((param->ipar[7]&(512+1024))==1024) {
      scale=(param->ipar[7]&(8+16+32))>>3;
      nrm=(param->ipar[8]&(32+64+128+256+512))>>5;
   }
   // final pivoting
   else if ((param->ipar[7]&(512+1024))==512+1024) {
      scale=(param->ipar[7]&(64+128+256))>>6;
      nrm=(param->ipar[8]&(1024+2048+4096+8192+16384))>>10;
   }
   else {
      scale=param->ipar[7]&(1+2+4);
      nrm=param->ipar[8]&(1+2+4+8+16);
   }

   // start with scaling from the left
   if (nrm<=2) {
      if ((scale&4)==0) {
	 if (scale&1) {
	    ROWSCALE(&(A.nc),&nrm, A.a, A.ja, A.ia, prowscale, &k);
	    if(k!=0) {
	      ierr=-5;
	      fprintf(STDERR, "ROWSCALE: a zero row...\n" );
	      return (ierr);
	    }
	 }
	 if (scale&2) {
	    COLSCALE(&(A.nc),&nrm, A.a, A.ja, A.ia, pcolscale, &k);
	    if(k!=0) {
	      ierr=-5;
	      fprintf(STDERR, "COLSCALE: a zero col...\n" );
	      return (ierr);
	    }
	 }
      }
      // start with scaling from the right
      else {
	 if (scale&2) {
	    COLSCALE(&(A.nc),&nrm, A.a, A.ja, A.ia, pcolscale, &k);
	    if(k!=0) {
	      ierr=-5;
	      fprintf(STDERR, "COLSCALE: a zero col...\n" );
	      return (ierr);
	    }
	 }
	 if (scale&1) {
	    ROWSCALE(&(A.nc),&nrm, A.a, A.ja, A.ia, prowscale, &k);
	    if(k!=0) {
	      ierr=-5;
	      fprintf(STDERR, "ROWSCALE: a zero row...\n" );
	      return (ierr);
	    }
	 }
      }
   }
   else if (nrm==3) {
      SPDSCALE(&(A.nc), A.a, A.ja, A.ia, pcolscale, &k);
      if(k!=0) {
	ierr=-5;
	fprintf(STDERR, "SPDCALE: a zero row/column...\n" );
	return (ierr);
      }
   }
   else if (nrm==4) {
      // allocate memory with respect to the specific permutation routine
      param->ndbuff=MAX(param->ndbuff,2*(size_t)A.nc);
      param->dbuff=(FLOAT *)REALLOC(param->dbuff,param->ndbuff*sizeof(FLOAT),
				    "perm...:param->dbuff");

      SYMSCALE(&(A.nc), A.a, A.ja, A.ia, pcolscale,param->dbuff, &k);
      if(k!=0) {
	ierr=-5;
	fprintf(STDERR, "SYMSCALE: a zero row/column...\n" );
	return (ierr);
      }
   }      
   // -------------------------------------------------------------------
   // ------------------   END  scaling prefix     ----------------------
   // -------------------------------------------------------------------
}
