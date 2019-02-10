



(* SHIFT *)
m = 26;

Test[key_,message_]:= ((key>= 0) && (key <=25));
EncodingFunction[k_,p_] := Mod[p + k, m] ;
DecodingFunction[k_,c_] := Mod[c - k, m] ;

(* AFFINE *)

Test[key_,message_]:= ((Head[key]==List) && (Length[key] ==2) && (GCD[key[[1]],m]==1)
                             &&(key[[2]]>= 0) && (key[[2]] <=25));
EncodingFunction[k_,p_] := Mod[p k[[1]] + k[[2]], m] ;
DecodingFunction[k_,c_] := Mod[Expand[1/k[[1]], Modulus->m ](c - k[[2]]), m] ;


(* VIGENERE *)

m=26;
Test[k_,p_]:=True ;
EncodingFunction[k_,p_] := Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];
                  Table[  Mod[p[[i]] + kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[p]}]
)]

DecodingFunction[k_,c_] :=  Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];	
Print[kk, " ", lk, " ",m];	  
                  Table[  Mod[c[[i]] - kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[c]}]
)]

(* HILL *)

m=26;
Test[k_,p_]:=True ;
EncodingFunction[k_,p_] := Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];
                  Table[  Mod[p[[i]] + kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[p]}]
)]

DecodingFunction[k_,c_] :=  Module[
                 {kk,lk,i},
                 (
                  kk = ToCharacterCode[k] - 97;
                  lk = Length[kk];	
Print[kk, " ", lk, " ",m];	  
                  Table[  Mod[c[[i]] - kk[[Mod[i-1,lk]+1]] , m]   ,{i,1,Length[c]}]
)]



Codifica[key_,msg_]:=
   Module[{plaintext,ciphercode},
       (
	If[Test[key,msg],
            (
             plaintext = ToCharacterCode[msg] - 97;
	     ciphercode = EncodingFunction[key,plaintext];
             FromCharacterCode[ciphercode + 97]
            ),
            (
             Print["exit"];
            )
         ]
       )
];


CodificaFile[key_,msgfile_]:=
   Module[{plaintext,ciphercode},
       (
	plaintext = ToCharacterCode[Read[msgfile,String]] - 97;
	ciphercode = EncodingFunction[key,plaintext];
        FromCharacterCode[ciphercode + 97]
       )
];


DeCodifica[key_,code_]:=
   Module[{plaintext,ciphercode},
       (
	ciphercode = ToCharacterCode[code] - 97;
	plaintext = DecodingFunction[key,ciphercode];
        FromCharacterCode[plaintext + 97]
       )
];



