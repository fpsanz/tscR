# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>

double minThreeNb (double nombre1, double nombre2, double nombre3);
double maxTwoNb (double nombre1, double nombre2);


void calcMatrixEuclid (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *matDistEuclid);
void calcMatrixEuclidCumul (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *matDistEuclidCumul, int *sumOrMax);
void myCalcMatrixEuclidCumul (double *Py, double *Qy, double *Df, double *Px, double *Qx, int *tailleP, int *tailleQ, double *matDistEuclidCumul, double *matDist, int *lenDf, int *sumOrMax, int *toploop);

int dirThreeNb (double nombreDiag, double nombreHaut, double nombreGauche);
int dirThreeNbBG (double nombreDiag, double nombreHaut, double nombreGauche); 
int dirThreeNbHD (double nombreDiag, double nombreHaut, double nombreGauche);
void calcDirectionPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *matDirectionPath, int *sumOrMax);
//void testDirectionPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *matDirectionPath);

void calcPathFrechet (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *bestPathP, int *bestPathY, int *tailleBestPath, int *sumOrMax);
//void testCalcPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *bestPathP, int *bestPathY, int *tailleBestPath);
void calcMeanFrechet (double *Px, double *Py, int *tailleP, double *weightP, double *Qx, double *Qy, int *tailleQ, double *weightQ, double *meanFrechetX, double *meanFrechetY, int *tailleMeanFrechet, int *sumOrMax);
 
//void testCalcMeanFrechet (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *meanFrechetX, double *meanFrechetY, int *tailleMeanFrechet);
