#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "libdesrandom.h"

int S[4][16];
int B[48];
int n=6, h;
double coef=0;


void convertToBinary(int i,int v[],int size){
//prende in input il numero che voglio convertire e array dove voglio scriverlo in bit
	int k, m, j;
	
	for (j = size-1; j >= 0; j--) {
	
		m = 1 << j;
		k = i & m;
		if (k == 0)
			v[j]= 0;
		else
			v[j]=1;
	}
	
	return;
}

//calcoliamo F(a) per ogni a in {0,1}^6
double CalcoloCoef (int a[6], int sbox){
	
	int i,j,m,r,c,s;
	double F=0;
	int x[6];
	
	for(i=0; i<pow(2,n); i++){
		//convertiamo i in bit e scriviamo in x[]
		convertToBinary(i,x,6);
		//moltiplichiamo riga colonna (a)^T x
		m=0;
		
		for(j=0; j<n;j++)
			m=m+a[j]*x[j];
			
		//sommiamo m con S[r][c]
		
		//voglio r e c in decimale, quindi converto da binario
		r = x[0] * 2 + x[5];
		c = 8 * x[1] + 4 * x[2] + 2 * x[3] + x[4];
		
		//le Si sono definite dalle entrate Si(r,c)
		if (sbox == 1)
			s=S1[r][c];
		else if (sbox == 2)
			s=S2[r][c];
		else if (sbox == 3)
			s=S3[r][c];
		else if (sbox == 4)
			s=S4[r][c];
		else if (sbox == 5)
			s=S5[r][c];
		else if (sbox == 6)
			s=S6[r][c];
		else if (sbox == 7)
			s=S7[r][c];
		else if(sbox == 8)
			s=S8[r][c];
		
		h=s;
		s=s+m;
		
		if(s%2==0) //s è pari
			F=F+1;
		else // s dispari
			F=F-1;
	}
	
	F=F*(pow(2,-n));
	
	return(F);
}

int sceltaentrata(int sbox){
	//per ogni a calcolo F(a) e tengo quella più lontana da 1/2
	float F,d=0;
	int i,w;
	int a[6];
	
	for(i=1; i<pow(2,n); i++){ //non consideriamo i=0 così escludiamo a[]=0000
		//convertiamo i in bit e scriviamo in a[]
		convertToBinary(i,a,6);
		F=CalcoloCoef(a, sbox);
		
		if( fabs(F-0.5)>=d){
			d = fabs(F-0.5);
			w=i;
			coef=F;
		}
	
	}
	
	return (w); //restituisce l'entrata ottimale
}

int sceltauscita(int sbox){
	
	int u,w;
	int r,c;
	int x[6];
	
	w=sceltaentrata(sbox);
	
	convertToBinary(w,x,6);
	//voglio r e c in decimale, quindi converto da binario
	r = x[0] * 2 + x[5];
	c = 8 * x[1] + 4 * x[2] + 2 * x[3] + x[4];
	
	//le Si sono definite dalle entrate Si(r,c)
	if (sbox == 1)
		u=S1[r][c];
	else if (sbox == 2)
		u=S2[r][c];
	else if (sbox == 3)
		u=S3[r][c];
	else if (sbox == 4)
		u=S4[r][c];
	else if (sbox == 5)
		u=S5[r][c];
	else if (sbox == 6)
		u=S6[r][c];
	else if (sbox == 7)
		u=S7[r][c];
	else if(sbox == 8)
		u=S8[r][c];
	
	return(u); //restituisce l'uscita ottimale
	
}

int permutazione(int i){

	int j,c;
	
	for (j = 0; j < 32; j++)
		if (P[j] == i + 1)
			c=j;
			
	return(c);
}

void expansion(int pos, int a){
	
	int i;
	
	for(i=0; i<48; i++){
		if (E[i]==pos+1)
			B[i]=a;
	}
	
	return;
}

int main (){
	//scegliamo la a ottimale 
	//otteniamo l'uscita ottimale
	//calcoliamo la permutazione per i 4 bit di uscita 
	//calcoliamo il primo coefficiente di correlazione
	//seguiamo gli 1 calcoliamo per quell'entrata e Sbox il coeffi
	// ogni volta facciamo il prodotto
	int u=15;
	int v[4], C[32], b[6];
	int i,j,k,perm;
	double F;
	 
	 Sbox1();
	 Sbox2();
	 Sbox3();
	 Sbox4();
	 Sbox5();
	 Sbox6();
	 Sbox7();
	 Sbox8();
	 
	 printf("Inserire il numero della Sbox da cui partire \n");
	 scanf("%d", &k);
	 
	 for(i=0; i<48; i++) //inizializzo il vettore
	 	B[i]=0;
	 
	 u=sceltauscita(k); //uscita ottimale al primo passaggio
	 convertToBinary(u,v,4); //scritto in v[] u in bit
	 
	 for(i=0; i<4; i++){
	 
	 	if (v[i]==1) { //vedo dove va nella permutazione
	 		j=permutazione(i);
	 		C[j]=1;		
	 		expansion(j,1);
	 	}
	 	
	 }
	 
	// printf("Il coefficiente di propagazione è %f. \n", coef);
	
	for(k=1; k<16; k++){ //andiamo dal 2 round fino al 16
	 	
		for(i=1; i<=8; i++){
	
			for(j=0; j<6; j++){
				b[j]=B[j+6*(i-1)];
	
			}
			
			if (b[0]!=0 || b[1]!=0 || b[2]!=0 || b[3]!=0 || b[4]!=0 || b[5]!=0){ //se almeno un elemento in b[] è diverso da zero vedo dove va con sbox
			//applichiamo a b[] la sbox i esima
				
				F=CalcoloCoef(b,i);
				coef=coef*F;
				convertToBinary(h,v,4);	
				coef=fabs(coef);
				
				for(j=0;j<4;j++){
					
					perm=permutazione(j+4*(i-1));
					C[perm]=v[j];
					
				}
				
			} else{
		
				for(j=0;j<4;j++){
					
					perm=permutazione(j+4*(i-1));
					C[perm]=0;
					
				}		
			}
			
			
		}
		
		//espansione di C in B
		for (i=0; i<32; i++)
			expansion(i,C[i]);
		
		if(coef==0){
			printf("Il coefficiente di propagazione è 0 al round %d \n",k+1);
			break;
		}
	
	}
	
	return(0);
}