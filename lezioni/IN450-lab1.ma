
(* SHIFT *)
m = 26;
EncodingFunction[k_,p_] := Mod[p + k, m] ;
DecodingFunction[k_,c_] := Mod[c - k, m] ;

(* AFFINE *)

EncodingFunction[k_,p_] := Mod[p k[[1]] + k[[2]], m] ;
DecodingFunction[k_,c_] := Mod[Inverse[k[[1]] ](c - k[[2]]), m] ;

Codifica[key_,msg_]:=
   Module[{plaintext,ciphercode},
       (
	plaintext = ToCharacterCode[Read[msg,String]] - 97;
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



