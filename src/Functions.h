#include <stdbool.h>

bool check_user_interrupt();

typedef struct orderedobservationlist 
{
	int    numberofobservation;
	double observation;
  	double observationsquared;

	double cumulativesum;
	double cumulativesumofsquares;
	double optimalcostofprevious;
	double segmentcost;
	
	double optimalcost;
	struct orderedobservationlist* optimalcut;
	int option;

	int    destruction;
  	struct orderedobservationlist* next;
  	struct orderedobservationlist* previous;
} orderedobservationlist;

void populateorderedobservationlist(struct orderedobservationlist **list, double* x , int n);

void updatewithobservation(int ii, struct orderedobservationlist *list, double penaltychange);

void findoptimaloption(int ii, struct orderedobservationlist *list, int minseglength, double penaltyoutlier);

int solveorderedobservationlist(struct orderedobservationlist *list, int n, double penaltychange, double penaltyoutlier, int minseglength);

void changepointreturn(struct orderedobservationlist *list, int n, int* numberofchanges, int** changepoints);

typedef struct orderedobservationlist_mean 
{
	int    numberofobservation;
	double observation;

	double cumulativesum;
	double optimalcostofprevious;
	double segmentcost;
	
	double optimalcost;
	struct orderedobservationlist_mean* optimalcut;
	int option;

	int    destruction;
  	struct orderedobservationlist_mean* next;
  	struct orderedobservationlist_mean* previous;
} orderedobservationlist_mean;

void populateorderedobservationlist_mean(struct orderedobservationlist_mean **list, double* x , int n);

void updatewithobservation_mean(int ii, struct orderedobservationlist_mean *list, double penaltychange);

void findoptimaloption_mean(int ii, struct orderedobservationlist_mean *list, int minseglength, double penaltyoutlier);

int solveorderedobservationlist_mean(struct orderedobservationlist_mean *list, int n, double penaltychange, double penaltyoutlier, int minseglength);

void changepointreturn_mean(struct orderedobservationlist_mean *list, int n, int* numberofchanges, int** changepoints);


