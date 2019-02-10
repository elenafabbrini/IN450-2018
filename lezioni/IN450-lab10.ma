(* Funzioni Ausiliarie *)
Pad[l_, lk_] := Join[l, Table[l[[i]], {i, 1, lk - Mod[Length[l], lk]}]];


Pad0[l_, lk_] := Join[l, Table[0, {i, 1, lk - Mod[Length[l], lk]}]];

Sort2[s_]:=Sort[s,(#1[[2]] > #2[[2]])&];

(* AFFINE *)
AFFINE = 2;
Test[AFFINE,key_,message_]:= ((Head[key]==List) && (Length[key] ==2) && (GCD[key[[1]],m]==1)
                             &&(key[[2]]>= 0) && (key[[2]] <=25));
EncodingFunction[AFFINE,k_,p_] := Mod[p k[[1]] + k[[2]], m] ;
DecodingFunction[AFFINE,k_,c_] := Mod[Expand[1/k[[1]], Modulus->m ](c - k[[2]]), m] ;


(* VIGENERE *)

VIGENERE = 3;
m=26;
Test[VIGENERE,k_,p_]:=True ;
EncodingFunction[VIGENERE,k_,p_] := Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];
                  Table[  Mod[p[[i]] + kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[p]}]
)]

DecodingFunction[VIGENERE,k_,c_] :=  Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];	
Print[kk, " ", lk, " ",m];	  
                  Table[  Mod[c[[i]] - kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[c]}]
)]

(* HILL *)

HILL = 4;
m=26;
Test[HILL,k_,p_]:=True ;

EncodingFunction[HILL,k_,p_] := Module[
                 {kk,lk,i,lp},
                 (
                  lk = Length[k];
                  lp=Pad[l, lk];
                  Flatten[
                      Table[
                           Mod[Take[lp, {i, i + lk - 1}].k, m], 
                           {i, 1, Length[l], lk}
                      ]
                  ]

(*                  Flatten[Table[ Mod[Take[p , {(i-1)*lk+1 ,i*lk}].k,m],{i,1,Length[p]/lk}]]*)
)]

DecodingFunction[HILL,k_,c_] :=  Module[
                 {kk,lk,i},
                 (
                  kinv = Inverse[k,Modulus->m];
                  lk = Length[kinv];	
                  Flatten[
                      Table[
                           Mod[Take[c, {i, i + lk - 1}].kinv, m], 
                           {i, 1, Length[l], lk}
                      ]
                  ]
)]


(* SHIFT *)

SHIFT = 1 ;
m = 26;

Test[SHIFT,key_,message_]:= ((key>= 0) && (key <=25));
EncodingFunction[SHIFT,k_,p_] := Mod[p + k, m] ;
DecodingFunction[SHIFT,k_,c_] := Mod[c - k, m] ;

(* LFSR *)

LFSR = 5;

LSHIFT[n_, k_] := n 2^k;
RSHIFT[n_, k_] := Floor[n  / 2^k] ;
LSHIFT[n_, k_, l_] := BitAnd[2^l - 1, LSHIFT[n, k]];
RSHIFT[n_, k_, l_] := BitAnd[2^l - 1, RSHIFT[n, k]];
BitGen[a_, b_] := Mod[Plus @@ IntegerDigits[BitAnd[a, b], 2], 2];
LFSRfunction[a_, b_, k_] := BitOr[LSHIFT[b, 1, k], BitGen[a, b]];
LFSRlength[a_, b_, l_] := Nest[LFSRfunction[a, #, l] &, b, l];
LFSRByte[a_, b_, n_] := LFSRlength[a, b, n 8];
LFSRByteList[a_, b_, n_] := NestList[LFSRByte[a, #, 1] &, b, n];
Test[LFSR,key_,message_]:= True;

thisLFSRcoefficients = 2^7+2; 


EncodingFunction[LFSR,k_,m_] := Module[{l},
	                               (
                                        l = LFSRByteList[thisLFSRcoefficients,k,Length[m]] ;
Print["m= ",m,"l= ",l,"coeffs=",thisLFSRcoefficients,"k=",k];
                                        Print[l=Table[BitXor[m[[i]],l[[i]]] , {i,1,Length[m]}]];
                                        l 
                                )
];


DecodingFunction[LFSR,k_,c_] := EncodingFunction[LFSR,k,c];

(**************************** ciphercode ******************)

Codifica[cipher_,key_,msg_]:=
   Module[{plaintext,ciphercode},
       (
	If[Test[cipher,key,msg],
            (
             plaintext = ToCharacterCode[msg] - 97;
	     ciphercode = EncodingFunction[cipher,key,plaintext];
             FromCharacterCode[ciphercode + 97]
            ),
            (
             Print["exit"];
            )
         ]
       )
];


CodificaByteFile[cipher_,key_,msgfile_]:=
   Module[{plaintext,ciphercode},
       (
	plaintext = ReadList[msgfile,Byte] ; Close[msgfile];
	ciphercode = EncodingFunction[cipher,key,plaintext];
        ciphercode
       )
];


CodificaAsciiFile[cipher_,key_,msgfile_]:=
   Module[{plaintext,ciphercode},
       (
	plaintext = ToCharacterCode[Read[msgfile,String]] - 97; Close[msgfile];
	ciphercode = EncodingFunction[cipher,key,plaintext];
        FromCharacterCode[ciphercode + 97]
       )
];


DeCodifica[cipher_,key_,code_]:=
   Module[{plaintext,ciphercode},
       (
	ciphercode = ToCharacterCode[code] - 97;
	plaintext = DecodingFunction[cipher,key,ciphercode];
        FromCharacterCode[plaintext + 97]
       )
];



(********* CRITTOANALISI ***********)

SetFrequencies[c_] := Module[
    {e},
    (
      e = Union[c];
      l = Length[c];
      Transpose[{e,Table[Length[Select[c, (# == e[[i]]) &]]/N[l], {i, 1, Length[e]}]}]
      )];

AlphaFrequencies[c_] := Module[
    {e},
    (
      e = Table[i,{i,0,25}];
      l = Length[c];
      Transpose[{e,Table[Length[Select[c, (# == e[[i]]) &]]/N[l], {i, 1, Length[e]}]}]
      )];

osigma = {{"A", 0.082}, {"B", 0.015}, {"C", 0.028}, {"D", 0.043}, {"E", 
      0.127}, {"F", 0.022}, {"G", 0.020}, {"H", 0.061}, {"I", 0.070}, {"J", 
      0.002}, {"K", 0.008}, {"L", 0.040}, {"M", 0.024}, {"N", 0.067}, {"O", 
      0.075}, {"P", 0.019}, {"Q", 0.001}, {"R", 0.060}, {"S", 0.063}, {"T", 
      0.091}, {"U", 0.028}, {"V", 0.010}, {"W", 0.023}, {"X", 0.001}, {"Y", 
      0.020}, {"Z", 0.001}};
	  
frenchsigma:=frenchsigma=
    Module[
{text},
  (
      text = Read["letteratura.fr", String]; Close["letteratura.fr"];
      frenchsigma = SetFrequencies[ToCharacterCode[text] - 97]
  )]

computeitaliansigma:=
    Module[
{text},
  (
      text = Read["letteratura.it", String]; Close["letteratura.it"];
      italiansigma = Sort2[SetFrequencies[ToCharacterCode[text] - 97]]
  )]

italiansigma=Sort2[{{0, 0.09830193901307328`}, {1, 0.006423838441160942`}, {2, 
    0.04714846230239487`}, {3, 0.034051925639767326`}, {4, 
    0.10736090017466701`}, {5, 0.011594074811436386`}, {6, 
    0.016673605280478926`}, {7, 0.016550338286495752`}, {8, 
    0.09138735838831895`}, {9, 6.9773770179155785`*^-6}, {10, 
    0.000013954754035831157`}, {11, 0.05374208358432509`}, {12, 
    0.027207118785192146`}, {13, 0.06143580464274667`}, {14, 
    0.08723581906265918`}, {15, 0.025097625133442335`}, {16, 
    0.007049476580434039`}, {17, 0.06036826595900559`}, {18, 
    0.051695386325736524`}, {19, 0.05266524173122679`}, {20, 
    0.0313074906793872`}, {21, 0.018497026474494197`}, {22, 
    2.325792339305193`*^-6}, {23, 0.000011628961696525963`}, {24, 
    2.325792339305193`*^-6}, {25, 0.004298064243035997}}]


italiansigma2=Map[{{#[[1]]},#[[2]]}&,italiansigma];

englishsigma=osigman=Map[{(ToCharacterCode[#[[1]]]-65),#[[2]]}&,osigma];

keymap[ciphertext_,sigma_]:=Transpose[{Sort2[AlphaFrequencies[ciphertext]],Sort2[sigma]}];

testcandidates[ciphertext_,sigma_]:=Module[{sa,sb,x},
	(
		sa=Sort2[AlphaFrequencies[ciphertext]];
		sb=Sort2[sigma];
		If[(x=Abs[sa[[1,2]]-sa[[2,2]]])<Abs[sb[[1,2]]-sb[[2,2]]], Print["Possibile Spurious Key ",x," ",k2=Mod[sa[[2,1]]-sb[[1,1,1]],26]," -> (",FromCharacterCode[k2+65],")"],Print["ok"]]
	)]
		
keycandidates[ciphertext_,sigma_]:=Map[(Mod[#[[1]][[1]]-#[[2]][[1,1]],26])&,keymap[ciphertext,sigma]];

bestkeys[ciphertext_,sigma_]:=Sort2[SetFrequencies[keycandidates[ciphertext,sigma]]];
bestkey[ciphertext_,sigma_]:=bestkeys[ciphertext,sigma][[1]][[1]];

LanguageCoincidenceIndex[freqs_]:=Plus@@Map[(#[[2]]^2)&,freqs];

CoincidenceIndex[ciphercode_]:=Module[
                                 {n,freqs},
                                 (
                                  n=Length[ciphercode];
                                  freqs=AlphaFrequencies[ciphercode];
                                  Plus@@Map[ ((n #[[2]]) ((n #[[2]])-1))&,freqs]/(n(n-1))
)];

ngrams[ciphercode_,kk_]:=Table[Take[ciphercode,{i,i+kk-1}],{i,1,Length[ciphercode]-kk}];


kasiski[ciphercode_,k_]:=Module[
{kk,cc6,X,Y,dd,dd1},
(
kk=k+1;
dd={};
While[Length[dd]<5,
(
kk=kk-1; Print[kk];
cc6=Table[Take[ciphercode,{i,i+kk}],{i,1,Length[ciphercode]-kk}];
X=Union[cc6];
Y=Map[Position[cc6,#]&,X];
dd=Select[Y,Length[#]>1&];
)];
Print[dd];
dd1=Map[Flatten,dd];
GCD@@Map[#[[2]]-#[[1]]&,dd1]
)];

PolyAlphaGroup[ciphercode_,k_]:=Transpose[Partition[Drop[ciphercode,-Mod[Length[ciphercode],k]],k]];

TruncatedPolyAlphaGroup[ciphercode_,k_,len_]:=Map[Take[#,{1,len}]&,Transpose[Partition[Drop[ciphercode,-Mod[Length[ciphercode],k]],k]]];

