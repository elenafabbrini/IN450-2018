#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "libdes.h"

int PC1[] = 
{
	  57, 49, 41, 33, 25, 17,  9,
	   1, 58, 50, 42, 34, 26, 18,
	  10,  2, 59, 51, 43, 35, 27,
	  19, 11,  3, 60, 52, 44, 36,
	  63, 55, 47, 39, 31, 23, 15,
	   7, 62, 54, 46, 38, 30, 22,
	  14,  6, 61, 53, 45, 37, 29,
	  21, 13,  5, 28, 20, 12,  4
};

int PC2[] = 
{
	  14, 17, 11, 24,  1,  5,
	   3, 28, 15,  6, 21, 10,
	  23, 19, 12,  4, 26,  8,
	  16,  7, 27, 20, 13,  2,
	  41, 52, 31, 37, 47, 55,
	  30, 40, 51, 45, 33, 48,
	  44, 49, 39, 56, 34, 53,
	  46, 42, 50, 36, 29, 32
};

int SHIFTS[] = { 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 };

//permutazione iniziale 
int IP[] = 
{
	  58, 50, 42, 34, 26, 18, 10, 2,
	  60, 52, 44, 36, 28, 20, 12, 4,
	  62, 54, 46, 38, 30, 22, 14, 6,
	  64, 56, 48, 40, 32, 24, 16, 8,
	  57, 49, 41, 33, 25, 17,  9, 1,
	  59, 51, 43, 35, 27, 19, 11, 3,
	  61, 53, 45, 37, 29, 21, 13, 5,
	  63, 55, 47, 39, 31, 23, 15, 7
};

//permutazione finale
int FP[] = 
{
	  40, 8, 48, 16, 56, 24, 64, 32,
	  39, 7, 47, 15, 55, 23, 63, 31,
	  38, 6, 46, 14, 54, 22, 62, 30,
	  37, 5, 45, 13, 53, 21, 61, 29,
	  36, 4, 44, 12, 52, 20, 60, 28,
	  35, 3, 43, 11, 51, 19, 59, 27,
	  34, 2, 42, 10, 50, 18, 58, 26,
	  33, 1, 41,  9, 49, 17, 57, 25
};

FILE *pf;
int LEFT[17][32], RIGHT[17][32];// son 17 perché oltre ai 16 round ho anche quello che esce dopo la prima permutazione
int KEYROUND[17][48]; //chiavi da 48 bit dei 16 round 
int key56bit[56];
int PE[64]; //salvo i bit della permutazione iniziale
int EXP[48]; //salvo i bit dopo espansione nella function
int B[48]; // salvo i bit dello xor
int X[8][6];//matrice di appoggio 48 posizioni
int C[32]; //salvo i bit di S(B)
int R[32]; //salvo i bit di P(C)
int CIPHER[64]; //salvo i bit dopo i round 
int ENCRYPTED[64]; //salvo il cifrato in bit

//chiave in key.txt
//testo da cifrare in testo.txt
//plaintext in bit in bits.txt
//ciphertext in bits in cipher_bits.txt
//testo decifrato in bits in decrypted.txt
//testo decifrato in decipher.txt


void key56to48(int round, int pos, int text){

	int i;
	
	for (i = 0; i < 56; i++)
		if (PC2[i] == pos + 1)
			break;
			
	KEYROUND[round][i] = text;
	
	return;
}

void key64to56(int pos, int text){

	int i;
	
	for (i = 0; i < 56; i++)
		if (PC1[i] == pos + 1)
			break;
			
	key56bit[i] = text;
	
	return;
}

void key64to48(unsigned int key[]){

	int k, i, x, j,shift;
	int backup[17][2];
	int CD[17][56];
	int C[17][28], D[17][28];

	for (i = 0; i < 64; i++)
		key64to56(i, key[i]);

	for (i = 0; i < 56; i++)
		if (i < 28)
			C[0][i] = key56bit[i];
		else
			D[0][i - 28] = key56bit[i];

	for (x = 1; x < 17; x++) {
	
		shift = SHIFTS[x - 1];

		for (i = 0; i < shift; i++)
			backup[x - 1][i] = C[x - 1][i];
			
		for (i = 0; i < (28 - shift); i++)
			C[x][i] = C[x - 1][i + shift];
			
		k = 0;
		for (i = 28 - shift; i < 28; i++)
			C[x][i] = backup[x - 1][k++];

		for (i = 0; i < shift; i++)
			backup[x - 1][i] = D[x - 1][i];
			
		for (i = 0; i < (28 - shift); i++)
			D[x][i] = D[x - 1][i + shift];
			
		k = 0;
		for (i = 28 - shift; i < 28; i++)
			D[x][i] = backup[x - 1][k++];
	}

	for (int j = 0; j < 17; j++) {
	
		for (int i = 0; i < 28; i++)
			CD[j][i] = C[j][i];
		for (int i = 28; i < 56; i++)
			CD[j][i] = D[j][i - 28];
	}

	for (int j = 1; j < 17; j++)
		for (int i = 0; i < 56; i++)
			key56to48(j, i, CD[j][i]);
			
	return;
}

void create16Keys(){

	FILE* pt = fopen("key.txt", "rb");
	unsigned int key[64];
	int i = 0, ch;

	while (!feof(pt)) {
	
		ch = getc(pt);
		key[i++] = ch - 48;
	}

	key64to48(key);
	fclose(pt);
	
	return;
}

int sizeFile(){

	int n;
	
	FILE* ft=fopen("testo.txt", "rb");
	
	if(fseek(ft, 0L, SEEK_END))
		perror("fseek() failed");
	else
		n=ftell(ft);  //lunghezza del testo, quante lettere ci sono scritte 
 	fclose(ft);
 	
 	return(n);
 }
 
void convertToBinary(int n){

	int k, m, i;
	
	for (i = 7; i >= 0; i--) {
	
		m = 1 << i;
		k = n & m;
		if (k == 0)
			fprintf(pf, "0");
		else
			fprintf(pf, "1");
	}
	
	return;
}

void convertCharToBit(long int m){
	
	FILE *ft=fopen("testo.txt", "rb");
	pf=fopen("bits.txt","wb+");
	char ch;
	long int n=m*8;
	
	while (n) {
	
		ch = fgetc(ft);
		if (ch == -1) //ultimo carattere del file è -1
			break;
		n--;
		convertToBinary(ch);
	}
	
	fclose(pf);
	fclose(ft);
	
	return;
}

void convertToBit(int ch[]){
	
	int val=0,i;
	
	for(i=7; i>=0; i--)
		val+=(int)pow(2,i) * ch[7-i];
	
	fprintf(pf, "%c", val);
	
	return;
}

void bitToChar(){

	pf=fopen("decipher.txt","ab+");
	int i;
	
	for(i=0;i <64; i=i+8)
		convertToBit(&ENCRYPTED[i]);
	
	fclose(pf);
	
	return;
}
	
void initialPermutation(int pos, int text){

	int i;
	
	for(i=0; i<64; i ++)
		if(IP[i]==pos+1)
			break;
	PE[i]=text;
	
	return;
}

void expansion(int pos, int a){
	
	int i;
	
	for(i=0; i<48; i++){
		if (E[i]==pos+1)
			EXP[i]=a;
	}
	
	return;
}

int XOR(int a, int b){

	return (a ^ b);
}

int F(int i){

	int r, c, j, s; 
	int b[6];
	
	for (j = 0; j < 6; j++) //suddivido B in 8 blocchi da 6 bit ciascuno
		b[j] = X[i][j];
	
	//voglio r e c in decimale, quindi converto da binario
	r = b[0] * 2 + b[5];
	c = 8 * b[1] + 4 * b[2] + 2 * b[3] + b[4];
	
	//le Si sono definite dalle entrate Si(r,c)
	if (i == 0)
		s=S1[r][c];
	else if (i == 1)
		s=S2[r][c];
	else if (i == 2)
		s=S3[r][c];
	else if (i == 3)
		s=S4[r][c];
	else if (i == 4)
		s=S5[r][c];
	else if (i == 5)
		s=S6[r][c];
	else if (i == 6)
		s=S7[r][c];
	else if(i == 7)
		s=S8[r][c];
		
	return(s);
		
}

void ToBits(int val){
//scrivo in C il valore di Si(r,c) in base 4
	int k, j, m;
	static int i;
	
	if (i % 32 == 0)
		i = 0;
		
	for (j = 3; j >= 0; j--) {
	
		m = 1 << j;
		k = val & m;
		if (k == 0)
			C[3 - j + i] = '0' - 48;
		else
			C[3 - j + i] = '1' - 48;
	}
	
	i = i + 4;
	
	return;
}

void SBox(int B[]){
//applico sbox ad ogni 6 bit di B per ottenere C da 32 bit totali
	int k,i,j,val;
	
	k=0;
	
	for (i = 0; i < 8; i++)
		for (j = 0; j < 6; j++)
			X[i][j] = B[k++];
			
	for (i = 0; i < 8; i++) {
		val = F(i);
	//le Si sono definite dalle entrate Si(r,c) in binario come una bitstring di lunghezza 4
	// C=C1,..,C8 in totale 32 bit
		ToBits(val);
	}
	return;
}

void PBox(int pos, int c){

	int i;
	
	for (i = 0; i < 32; i++)
		if (P[i] == pos + 1)
			break;
			
	R[i] = c;
	
	return;
}

void function(int round,int mode){

	int i;
	
	for (i = 0; i < 32; i++)
		expansion(i, RIGHT[round - 1][i]); //ottenuto 48 bits salvati in EXP[]
		
	for (i=0; i <48; i++){
	
		if(mode==0)
			B[i]=XOR(EXP[i],KEYROUND[round][i]); // B=B1,..,B8 ognuno di 6 bit
		else
			B[i]=XOR(EXP[i],KEYROUND[17-round][i]);
	}
	
	SBox(B); //calcolo C_j=S_j(B_j) ognuno di 4 bit
	
	for (i = 0; i < 32; i++) //applico la permutazione su C
		PBox(i, C[i]);
		
	for (i = 0; i < 32; i++)
		RIGHT[round][i] = XOR(LEFT[round - 1][i], R[i]);	
		
	return;

}

void finalPermutation(int pos, int b){

	int i;
	
	for (i = 0; i < 64; i++)
		if (FP[i] == pos + 1)
			break;
			
	ENCRYPTED[i] = b;
	
	return;
}

void Encryption (long int plain[]){
	
	pf=fopen("cipher_bits.txt","ab+");
	int i,j,round;
	
	for(i=0;i< 64;i++)
		initialPermutation(i,plain[i]);
	
	for (i = 0; i < 32; i++)
		LEFT[0][i] = PE[i];
	for (i = 32; i < 64; i++)
		RIGHT[0][i - 32] = PE[i];
		
	//16 iterazioni di tipo Feistel
	for(round=1; round<=16; round ++){ 
		
		function(round,0);
		
		for (j = 0; j < 32; j++)
			LEFT[round][j]=RIGHT[round-1][j];
		
	}
	
	//una volta fatti i round copio tutto su CIPHER
	for (i = 0; i < 64; i++) {
		if (i < 32)
			CIPHER[i] = RIGHT[16][i];
		else
			CIPHER[i] = LEFT[16][i - 32];
			
		finalPermutation(i, CIPHER[i]);
	}
	
	for (i = 0; i < 64; i++)
		fprintf(pf, "%d", ENCRYPTED[i]);
		
	fclose(pf);
	
	return;
}

void Decryption (long int plain[]){
	
	pf=fopen("decrypted.txt","ab+");
	int i,round,j;
	
	for(i=0;i <64;i++)
		initialPermutation(i, plain[i]);
	
	for (i = 0; i < 32; i++)
		LEFT[0][i] = PE[i];
	for (i = 32; i < 64; i++)
		RIGHT[0][i - 32] = PE[i];
		
	//16 iterazioni di tipo Feistel
	for(round=1; round<=16; round ++){ 
		
		function(round,1);
		
		for (j = 0; j < 32; j++)
			LEFT[round][j]=RIGHT[round-1][j];
		
	}
	
	//una volta fatti i round copio tutto su CIPHER
	for (i = 0; i < 64; i++) {
		if (i < 32)
			CIPHER[i] = RIGHT[16][i];
		else
			CIPHER[i] = LEFT[16][i - 32];
			
		finalPermutation(i, CIPHER[i]);
	}
	
	for (i = 0; i < 64; i++)
		fprintf(pf, "%d", ENCRYPTED[i]);
		
	fclose(pf);
	
	return;
}

void encrypt(long int m){

	FILE * bf=fopen("bits.txt", "rb");
	long int plain[m*64];
	int i=-1;
	char ch;
	
	while(!feof(bf)){
		ch=getc(bf);
		plain[++i]=ch-48;
	}
	
	for(i=0; i<m;i++)
		Encryption(plain+ 64 * i);
	
	fclose(bf);
	
	return;
}

void decrypt(long int m){

	FILE *in=fopen("cipher_bits.txt","rb");
	
	long int plain[m*64];
	int i=-1;
	char ch;
	
	while (!feof(in)){
		ch=getc(in);
		plain[++i]=ch-48;
	}
	
	for(i=0;i<m;i++){
		Decryption(plain+i*64);
		bitToChar();
	}
	
	fclose(in);
	
	return;
}

void compare(){

	FILE *pf,*df;
	
	pf=fopen("testo.txt", "r");
	df=fopen("decipher.txt", "r");
	
	char ch1 = getc(pf); 
	char ch2 = getc(df); 
	 
	int error = 0;

	//ciclo fino alla fine dei file
	while (ch1 != EOF && ch2 != EOF) { 
	
		if (ch1 != ch2) 
			error++; 

		ch1 = getc(pf); 
		ch2 = getc(df); 
	} 
	
	if(error==0)
		printf("Il testo inserito e quello cifrato-decifrato coincidono. \n");
	else
		printf("Il testo inserito e quello cifrato-decifrato sono diversi. \n Gli errori totali sono : %d \n", error); 
	
	
	fclose(df);
	fclose(pf);
	
	return;
	
}
	
void stampa(int m){

	char *s=NULL;
	FILE *pf;
	
	s=malloc(sizeof(char)*m);
	pf=fopen("cipher_bits.txt","rb") ;
		if ( pf== NULL )
			printf("Non posso aprire il file cipher_bits.txt"); 
		else if(!s){
			printf("Errore allocazione");
		}
		else{
			while (!feof(pf) ) { 
				fscanf(pf, "%s\n", s);
				printf("%s",s); 
			}
			fclose(pf);
		}
		printf("\n");
	return;
 }
	
int main() {
	
	long int n,m;
	
	int p;
	
	printf("\n");
	printf("Il testo cifrato è quello nel file testo.txt \n");
	printf("\n");
	
	pf=fopen("cipher_bits.txt","wb+");
	fclose(pf);
	
	pf=fopen("decipher.txt","wb+");
	fclose(pf);
	
	pf=fopen("decrypted.txt","wb+");
	fclose(pf);
	
 	
 	create16Keys();
 	//come chiave verrà usata la chiave scritta nel file key.txt 64 bit
 	//nell'algoritmo originale infatti si parte da 64 bit si passa a 56 bit e 
 	//questa dovrà essere usata per creare le 16 chiavi da 48 
 
 	
 	n=sizeFile(); 
 	printf("La lunghezza del testo da cifrare è %ld \n",n);
 	
 
 	if(n%8!=0){
 		printf("Sul testo inserito non può essere applicato l'algoritmo. \n");
 		return(0);
 	}
 	
 	
 	m=n/8; //un char è formato da 8 bit quindi ho n*8 bits
 	//printf("La lunghezza del testo binario da cifrare è %d \n",m);
 	
 	convertCharToBit(m); //converto i caratteri del file in bit e mi da lunghezza del file binario del testo
 	
 	encrypt(m);
	
	printf("Il ciphertext in bit è nel file cipher_bits.txt \n");
	printf("\n");
	
	printf("Se si vuole vedere il ciphertext in bit digitare 1 \n altrimenti digitare 2 \n");
	scanf("%d", &p);

	printf("\n");
	if(p==1){
		printf("Il ciphertext è : ");
		stampa(m);
		printf("\n");
	}
	
	printf("Se si vuole anche decifrare il ciphertext digitare 1 \n altrimenti digitare 2 \n");
	scanf("%d", &p);
	
	if(p==1){
		decrypt(m);
		printf("Il testo decifrato è nel file decipher.txt \n");
		printf("\n");
		p=0;
		printf("Se si vuole comparare il testo nel file decipher.txt e nel file testo.txt digitare 1 \n altrimenti digitare 2 \n");
		scanf("%d",&p);	
			if (p==1)
				compare();
	}
	
	return(0);
}

