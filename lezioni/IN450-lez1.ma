ShiftEncrypt[plaintext_, key_] := Module[ {x, y},
  (
   x = ToCharacterCode[plaintext] - 97;
   y = Mod[x + key , 26];
   FromCharacterCode[y + 65]
   )]

ShiftDecrypt[ciphertext_, key_] := Module[ {x, y},
  (
   y = ToCharacterCode[ciphertext] - 65;
   x = Mod[y - key , 26];
   FromCharacterCode[x + 97]
   )]

AffineEncrypt[plaintext_, key_] := Module[ {x, y, a, b},
  (
   x = ToCharacterCode[plaintext] - 97;
   a = key[[1]]; 
   If[GCD[a, 26] == 1,
    (
     b = key[[2]];
     y = Mod[a x + b , 26];
     FromCharacterCode[y + 65]
     ), Print["INVALID KEY"]; plaintext]
   )]

AffineDecrypt[ciphertext_, key_] := Module[ {x, y},
  (
   y = ToCharacterCode[ciphertext] - 65;
   a = key[[1]]; 
   If[GCD[a, 26] == 1,
    (
     b = key[[2]];
     x = Mod[(y - b)/a , 26];
     FromCharacterCode[x + 97]
     ), Print["INVALID KEY"]; plaintext]
   )]

