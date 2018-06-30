#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>

#include "Functions.h"



SEXP MeanVarAnomaly(SEXP Rx, SEXP Rn, SEXP Rminlength, SEXP Rbetachange, SEXP Rbetaanomaly)
{
  
  /* 
  Rx    : Data
  Rn    : Length of data
  */
	 
 	PROTECT(Rx) ; 
 	PROTECT(Rn) ;
	PROTECT(Rminlength) ;
	PROTECT(Rbetachange) ;
	PROTECT(Rbetaanomaly) ;
	
  	int n = 0, minlength = 0, error = 0;
  	double betachange = 0.0, betaanomaly = 0.0;
  	double* x = NULL;
	
  
 	minlength        = *(INTEGER(Rminlength));
	n                = *(INTEGER(Rn));
  	x          	 =   REAL(Rx);
  	betachange       = *REAL(Rbetachange);
  	betaanomaly      = *REAL(Rbetaanomaly);

	struct orderedobservationlist* mylist;

	populateorderedobservationlist(&mylist, x, n); 

	
	error = solveorderedobservationlist(mylist, n, betachange, betaanomaly, minlength);
	
	if (error){
	  free(mylist);
	  UNPROTECT(5);
	  return R_NilValue ; 
	}

	int numberofchanges = 0, *changes = NULL;

	changepointreturn(mylist, n, &numberofchanges, &changes);
	

	SEXP Rout ; 
  	PROTECT(Rout = allocVector(INTSXP, 2*numberofchanges));

	int *out;
  	out  = INTEGER(Rout);
  
 	int ii = 0;
  	
	for (ii = 0; ii < 2*numberofchanges; ii++)
	{
		out[ii] = changes[ii];
	}
	
	free(changes);

	free(mylist); 

  	UNPROTECT(6);
  	return(Rout) ; 
}










