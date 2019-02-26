#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

//permutazione
int P[] = 
{
	  16,  7, 20, 21,
	  29, 12, 28, 17,
	   1, 15, 23, 26,
	   5, 18, 31, 10,
	   2,  8, 24, 14,
	  32, 27,  3,  9,
	  19, 13, 30,  6,
	  22, 11,  4, 25
};


//espansione
int E[] = 
{
	  32,  1,  2,  3,  4,  5,
	   4,  5,  6,  7,  8,  9,
	   8,  9, 10, 11, 12, 13,
	  12, 13, 14, 15, 16, 17,
	  16, 17, 18, 19, 20, 21,
	  20, 21, 22, 23, 24, 25,
	  24, 25, 26, 27, 28, 29,
	  28, 29, 30, 31, 32,  1
};


// SBOX
int S1[4][16];

int S2[4][16];

int S3[4][16];

int S4[4][16];

int S5[4][16];

int S6[4][16];

int S7[4][16];

int S8[4][16];


void Sbox1(){
	int min = 0, max = 15;
	int i,j,h;
	
	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S1[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S1[i][h]==S1[i][j]){
						j--;
						break;
					}
				}
		}

	}	
		
}

void Sbox2(){
	int min = 0, max = 15;
	int i,j,h;

	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S2[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S2[i][h]==S2[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}

void Sbox3(){
	int min = 0, max = 15;
	int i,j,h;
	
	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S3[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S3[i][h]==S3[i][j]){
						j--;
						break;
					}
				}
		}
	}	
	
}

void Sbox4(){
	int min = 0, max = 15;
	int i,j,h;
	
	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S4[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S4[i][h]==S4[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}

void Sbox5(){
	int min = 0, max = 15;
	int i,j,h;

	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S5[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S5[i][h]==S5[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}

void Sbox6(){
	int min = 0, max = 15;
	int i,j,h;

	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S6[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S6[i][h]==S6[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}

void Sbox7(){
	int min = 0, max = 15;
	int i,j,h;

	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S7[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S7[i][h]==S7[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}

void Sbox8(){
	int min = 0, max = 15;
	int i,j,h;

	for (i=0;i<4;i++){
		for(j=0;j<16;j++){
			S8[i][j] = (rand() % (max - min + 1)) + min;
			
				for(h=0;h<j;h++){
					if(S8[i][h]==S8[i][j]){
						j--;
						break;
					}
				}
		}
	}	
}


