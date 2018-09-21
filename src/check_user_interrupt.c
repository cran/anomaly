#include <R.h>
#include <Rinternals.h>
#include <Rmath.h> 
#include <math.h> 
#include <stdlib.h>
#include <float.h>
#include <stdbool.h>

#include "Functions.h"

static void check_user_interrupt_handler() {R_CheckUserInterrupt();}

bool check_user_interrupt() {return R_ToplevelExec(check_user_interrupt_handler,NULL) == FALSE;}
