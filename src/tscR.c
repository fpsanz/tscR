# include "tscR.h"

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
const int diagonale=0,gauche=1,haut=2, directionStart=3 ; //ces valeurs sont fix?es pour tout le programme


void printMatrix(double *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %f",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}


void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne){
    int i=0,j=0;
    for(i=0 ;  i < *nbLigne ; i++){
	for(j=0 ; j < *nbCol ; j++){
	    Rprintf(" %i",mTraj[i * *nbCol + j]);
	}
	Rprintf("\n");
    }
}


/********************************************************
 ****************** quelques fonctions ******************
 ********************************************************/


// fonction renvoyant le nombre minimum entre trois nombres
double minThreeNb (double nombre1, double nombre2, double nombre3){
  double resMin = nombre3;
  if ( (nombre1<nombre2) && (nombre1<nombre3)){
    resMin = nombre1;
  }else{
    if (nombre2 < nombre3){
      resMin = nombre2;
    }else{}
  }
  return resMin;
}


// fonction renvoyant le nombre maximum entre deux nombres
double maxTwoNb (double nombre1, double nombre2){
  double resMax = nombre2;
  if (nombre1 > nombre2){
    resMax = nombre1;
  }else{}
  return resMax;
}


/********************************************************
 ********** Matrice des Distances Euclidiennes **********
 ********************************************************/


// Distance
void calcMatrixEuclid (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *matDistEuclid){
  int i=0, j=0;
  //  Rprintf("--- calcMatrixEuclid ---\n");

  //  printMatrix(matDistEuclid,tailleP,tailleQ);
  for (i=0; i < *tailleQ; i++){
    for (j=0; j < *tailleP; j++){
      matDistEuclid[i * *tailleP + j] = pow((Px[j]-Qx[i])*(Px[j]-Qx[i])+(Py[j]-Qy[i])*(Py[j]-Qy[i]),0.5);
    }
  }
  //  printMatrix(matDistEuclid,tailleP,tailleQ);
  //  Rprintf("--- fin calcMatrixEuclid ---\n");
}

//void testMatrixEuclid (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *matDistEuclid){
//  calcMatrixEuclid(Px, Py, tailleP, Qx, Qy, tailleQ, matDistEuclid);
//}


// Distance cumulee

void calcMatrixEuclidCumul (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *matDistEuclidCumul, int *sumOrMax) //sumOrMax : 1 = Sum sinon Max
{
  double matDistEuclid[*tailleP * *tailleQ];
  calcMatrixEuclid(Px, Py, tailleP, Qx, Qy, tailleQ, matDistEuclid);
  int i, j;
  matDistEuclidCumul[0]=matDistEuclid[0];

  if (*sumOrMax == 1){

    // 1ere ligne et colonne
    for (j = 1; j < *tailleP; j++){ matDistEuclidCumul[j]=matDistEuclid[j] + matDistEuclidCumul[j-1];}
    for (i = 1; i < *tailleQ; i++){ matDistEuclidCumul[i * *tailleP]=matDistEuclid[i * *tailleP] + matDistEuclidCumul[(i-1) * *tailleP];}

    //milieu de matrice
    for (i = 1; i < *tailleQ; i++){ //milieu de matrice
      for (j = 1; j < *tailleP; j++){
	matDistEuclidCumul[i * *tailleP+j] = matDistEuclid[i * *tailleP+j] +
	  minThreeNb(matDistEuclidCumul[i * *tailleP+j-1], matDistEuclidCumul[(i-1) * *tailleP+j],matDistEuclidCumul[(i-1) * *tailleP+j-1]);
      }
    }
  }else{
    // 1ere ligne et colonne
    for (j = 1; j < *tailleP; j++){ matDistEuclidCumul[j]=maxTwoNb(matDistEuclid[j],matDistEuclidCumul[j-1]);}
    for (i = 1; i < *tailleQ; i++){ matDistEuclidCumul[i * *tailleP]=maxTwoNb(matDistEuclid[i * *tailleP],matDistEuclidCumul[(i-1) * *tailleP]);}

    //milieu de matrice
    for (i = 1; i < *tailleQ; i++){
      for (j = 1; j < *tailleP; j++){
	matDistEuclidCumul[i * *tailleP+j] = maxTwoNb(
  	   matDistEuclid[i * *tailleP+j],
           minThreeNb(matDistEuclidCumul[i * *tailleP+j-1], matDistEuclidCumul[(i-1) * *tailleP+j],matDistEuclidCumul[(i-1) * *tailleP+j-1]));
      }
    }
  }
}


void myCalcMatrixEuclidCumul (double *Py, double *Qy, double *Df, double *Px, double *Qx, int *tailleP, int *tailleQ, double *matDistEuclidCumul, double *matDist, int *lenDf, int *sumOrMax, int *toploop)
{
	int a, b, k, l;
	int index = (*tailleP * *tailleQ) -1;
	l = 0;
	int tp = *toploop;
	for(a = 0; a < tp ; a++ ){ // 3 filas
		for(b = 0; b < tp ; b++){ //3 filas
			double matDistEuclid[*tailleP * *tailleQ];
			for(k = 0; k < *tailleP; k++){ // 4 times
				Py[k] = Df[k + (a * *tailleP)];
				Qy[k] = Df[k + (b * *tailleP)];
			}
			calcMatrixEuclid(Px, Py, tailleP, Qx, Qy, tailleQ, matDistEuclid);
			  int i, j;
			  matDistEuclidCumul[0]=matDistEuclid[0];

			  if (*sumOrMax == 1){

			    // 1ere ligne et colonne
			    for (j = 1; j < *tailleP; j++){ matDistEuclidCumul[j]=matDistEuclid[j] + matDistEuclidCumul[j-1];}
			    for (i = 1; i < *tailleQ; i++){ matDistEuclidCumul[i * *tailleP]=matDistEuclid[i * *tailleP] + matDistEuclidCumul[(i-1) * *tailleP];}

			    //milieu de matrice
			    for (i = 1; i < *tailleQ; i++){ //milieu de matrice
			      for (j = 1; j < *tailleP; j++){
				matDistEuclidCumul[i * *tailleP+j] = matDistEuclid[i * *tailleP+j] +
				  minThreeNb(matDistEuclidCumul[i * *tailleP+j-1], matDistEuclidCumul[(i-1) * *tailleP+j],matDistEuclidCumul[(i-1) * *tailleP+j-1]);
			      }
			    }
			  }else{
			    // 1ere ligne et colonne
			    for (j = 1; j < *tailleP; j++){ matDistEuclidCumul[j]=maxTwoNb(matDistEuclid[j],matDistEuclidCumul[j-1]);}
			    for (i = 1; i < *tailleQ; i++){ matDistEuclidCumul[i * *tailleP]=maxTwoNb(matDistEuclid[i * *tailleP],matDistEuclidCumul[(i-1) * *tailleP]);}

			    //milieu de matrice
			    for (i = 1; i < *tailleQ; i++){
			      for (j = 1; j < *tailleP; j++){
				matDistEuclidCumul[i * *tailleP+j] = maxTwoNb(
			  	   matDistEuclid[i * *tailleP+j],
			           minThreeNb(matDistEuclidCumul[i * *tailleP+j-1], matDistEuclidCumul[(i-1) * *tailleP+j],matDistEuclidCumul[(i-1) * *tailleP+j-1]));
			      }
			    }
			  }
			matDist[l]  = matDistEuclidCumul[index];
			l++;
		}
	}
}


/********************************************************
 ****************** Chemin de Frechet *******************
 ********************************************************/

// fonction renvoyant "l'emplacement" du nombre minimum dans le cas triangulaire diagonal
int dirThreeNb (double nombreDiag, double nombreHaut, double nombreGauche)
{
  int direction=gauche;
  if ( (nombreDiag <= nombreGauche) && (nombreDiag <= nombreHaut) ){
    direction = diagonale;
  }else{
    if (nombreHaut <= nombreGauche)	{
      direction = haut;
    }else{}
  }
  return direction;
}


// fonction renvoyant "l'emplacement" du nombre minimum dans le cas triangulaire inferieur gauche
int dirThreeNbBG (double nombreDiag, double nombreHaut, double nombreGauche)
{
  int direction=gauche;
  if ( (nombreHaut <= nombreGauche) && (nombreHaut <= nombreDiag) ){
    direction = haut;
  }else{
    if (nombreDiag <= nombreGauche){
      direction = diagonale;
    }else{}
  }
  return direction;
}

// fonction renvoyant "l'emplacement" du nombre minimum dans le cas triangulaire sup?rieur droit
int dirThreeNbHD (double nombreDiag, double nombreHaut, double nombreGauche)
{
  int direction=haut;
  if ( (nombreGauche <= nombreHaut) && (nombreGauche <= nombreDiag) ){
    direction = gauche;
  }else	{
    if (nombreDiag <= nombreHaut){
	direction = diagonale;
    }else{}
  }
  return direction;
}



// matrice des directions
void calcDirectionPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *matDirectionPath, int *sumOrMax)
{
  double matDistEuclidCumul[*tailleP* *tailleQ];
  calcMatrixEuclidCumul(Px, Py, tailleP, Qx, Qy, tailleQ, matDistEuclidCumul, sumOrMax);
  //  Rprintf("--- calcDirectionPath A ---\n");
  //  printMatrix(matDistEuclidCumul,tailleP,tailleQ);
  int i, j;
  matDirectionPath[0] = directionStart;

  // 1ere ligne & colonne

  for (j = 1; j < *tailleP; j++) 	{matDirectionPath[j]=gauche;}
  for (i = 1; i < *tailleQ; i++)  {matDirectionPath[i * *tailleP]=haut;}

  //  Rprintf("--- calcDirectionPath B ---\n");
  //  printMatrixInt(matDirectionPath,tailleP,tailleQ);
  for (j = 1; j < *tailleP; j++){
    for (i = 1; i < *tailleQ; i++){

      //      Rprintf("j=%i ; i=%i ; (i-1)*(*tailleP-1)=%i ; (j*(tailleQ-1)=%i ; (j-1)*(*tailleQ-1)=%i ; (i*(*tailleP-1))=%i\n",
      //      j,i,((i-1)*(*tailleP-1)),(j*(*tailleQ-1)),(j-1)*(*tailleQ-1),i*(*tailleP-1));

      if (((i-1)*(*tailleP-1)) >= (j*(*tailleQ-1))){
	matDirectionPath[i * *tailleP + j] = dirThreeNbBG(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							  matDistEuclidCumul[(i-1) * *tailleP + j],
							  matDistEuclidCumul[i * *tailleP + (j-1)]);
      }else{
	if (((j-1)*(*tailleQ-1)) >= (i*(*tailleP-1))){
	  matDirectionPath[i * *tailleP + j] = dirThreeNbHD(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							    matDistEuclidCumul[(i-1) * *tailleP + j],
							    matDistEuclidCumul[i * *tailleP + (j-1)]);
	}else{
	  matDirectionPath[i * *tailleP + j] = dirThreeNb(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							  matDistEuclidCumul[(i-1) * *tailleP + j],
							  matDistEuclidCumul[i * *tailleP + (j-1)]);
	}
      }
    }
  }
  /*  VERSION SIMPLIFIEE
  for (j = 1; j < *tailleP; j++){
    for (i = 1; i < *tailleQ; i++){

      if (((i/(*tailleQ-1))- (1/(*tailleQ-1))) > (j/(*tailleP-1))){
	matDirectionPath[i * *tailleP + j] = dirThreeNbBG(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							  matDistEuclidCumul[(i-1) * *tailleP + j],
							  matDistEuclidCumul[i * *tailleP + (j-1)]);
      }	else{
	if (((j/(*tailleP-1))- (1/(*tailleP-1))) > ((i/(*tailleQ-1)))){
	  matDirectionPath[i * *tailleP + j] = dirThreeNbHD(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							    matDistEuclidCumul[(i-1) * *tailleP + j],
							    matDistEuclidCumul[i * *tailleP + (j-1)]);
	}else{
	  matDirectionPath[i * *tailleP + j] = dirThreeNb(matDistEuclidCumul[(i-1) * *tailleP + (j-1)],
							  matDistEuclidCumul[(i-1) * *tailleP + j],
							  matDistEuclidCumul[i * *tailleP + (j-1)]);
	}
      }
    }
  }*/
  //  Rprintf("--- calcDirectionPath E ---\n");
  //printMatrixInt(matDirectionPath,tailleP,tailleQ);
}

//void testDirectionPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *matDirectionPath){
//calcDirectionPath(Px, Py, tailleP, Qx, Qy, tailleQ, matDirectionPath);
//}




/********************************************************
 ****************** Moyenne de Frechet ******************
 ********************************************************/

// Frechet Path
void calcPathFrechet (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ,  int *bestPathP, int *bestPathQ, int *tailleBestPath, int *sumOrMax){
  int matDirectionPath[*tailleP* *tailleQ];
  calcDirectionPath(Px, Py, tailleP, Qx, Qy, tailleQ, matDirectionPath, sumOrMax);
  //  Rprintf("--- calcPath  ---\n");
  // printMatrixInt(matDirectionPath,tailleP,tailleQ);
  int i, j;
  i = *tailleQ-1 ;
  j = *tailleP-1 ;
  bestPathP[0] = j;
  bestPathQ[0] = i;

  *tailleBestPath = 1;
  while(i > 0 || j > 0){
    if(matDirectionPath[i *  *tailleP + j]==1){ //gauche
      j+=-1;
    }else{
      if(matDirectionPath[i *  *tailleP + j]==2){ //haut
	i+=-1;
      }else{ //diagonale
	i+=-1;
	j+=-1;
      }
    }
    bestPathP[*tailleBestPath] = j;
    bestPathQ[*tailleBestPath] = i;
    *tailleBestPath += 1;
  }
}



//void testCalcPath (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, int *bestPathP, int *bestPathQ, int *tailleBestPath){
//	calcPath(Px, Py, tailleP, Qx, Qy, tailleQ, bestPathP, bestPathQ, tailleBestPath);
//}



// Frechet mean
void calcMeanFrechet (double *Px, double *Py, int *tailleP, double *weightP,
                      double *Qx, double *Qy, int *tailleQ, double *weightQ,
                      double *meanFrechetX, double *meanFrechetY, int *tailleMeanFrechet, int *sumOrMax){
  int bestPathP[*tailleP+*tailleQ-2];
  int bestPathQ[*tailleP+*tailleQ-2];
  calcPathFrechet(Px, Py, tailleP, Qx, Qy, tailleQ, bestPathP, bestPathQ, tailleMeanFrechet, sumOrMax);
  int i;
  for (i = *tailleMeanFrechet-1; i>=0  ; i--){
    meanFrechetX[i] = *weightP * Px[bestPathP[i]] + *weightQ * Qx[bestPathQ[i]];
    meanFrechetY[i] = *weightP * Py[bestPathP[i]] + *weightQ * Qy[bestPathQ[i]];
  }
}


//void testCalcMeanFrechet (double *Px, double *Py, int *tailleP, double *Qx, double *Qy, int *tailleQ, double *meanFrechetX, double *meanFrechetY, int *tailleMeanFrechet){
//  calcMeanFrechet(Px, Py, tailleP, Qx, Qy, tailleQ, meanFrechetX, meanFrechetY, tailleMeanFrechet);
//}
