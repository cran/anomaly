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


