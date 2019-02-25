(*KEY and IV SETUP*)
keylen = 80;
adim = 93;
ivlen = 80;
bdim = 84;
cdim = 111;
K = Table[ToExpression["K" <> ToString[i]], {i, 1, 80}];
IV = Table[ToExpression["IV" <> ToString[i]], {i, 1, 80}];
And @@ Map[# <= 1 &, K] && And @@ Map[# >= 0 &, K] && 
  And @@ Map[# <= 1 &, IV] && And @@ Map[# >= 0 &, IV];

SetUpState[] := Module[{s},
   (
    s = Table[0, {i, 1, adim + bdim + cdim}];
    Table[s[[i]] = K[[i]], {i, 1, keylen}];
    Table[s[[i]] = 0, {i, keylen + 1, adim}];
    Table[s[[i]] = IV[[i - adim]], {i, adim + 1, adim + ivlen}];
    Table[s[[i]] = 0, {i, adim + ivlen + 1, adim + bdim}];
    Table[s[[i]] = 0, {i, adim + bdim + 1, adim + bdim + cdim - 3}];
    Table[s[[i]] = 1, {i, adim + bdim + cdim - 2, adim + bdim + cdim}];
    s
    )];

s[0] := SetUpState[]

TriviumCore[list_, i1_, i2_, i3_, i4_, i5_] := 
  Module[{Mvars0, Mcon0, vars, pos},
   (
    vars = list;
    z = Table[ToExpression["z" <> ToString[j]], {j, 1, 4}];
    indici = {i1, i2, i3, i4};
    pos = {1, 2, 3, 4};
    cond0 = 
     And @@ Map[# <= 1 &, vars] && And @@ Map[# >= 0 &, vars] && 
      And @@ Map[# <= 1 &, z] && And @@ Map[# >= 0 &, z] && a >= 0 && 
      a <= 1;
    Mcon0 = 
     Flatten[List[
       vars[[indici]] = PolynomialMod[list[[indici]] - z[[pos]], 2],
       vars[[i5]] = 
        PolynomialMod[list[[i5]] + a + z[[1]] + z[[2]], 2], 
       a >= z[[3]], a >= z[[4]]]];
    Mvars0 = Flatten[List[vars[[indici]], vars[[i5]], z[[pos]], a ]];
    {Mvars0, Mcon0, vars}
    )];

UpdateState[state_] := Module[{s, MV1, MC1},
   (
    s = state;
    {Mvars1, Mcon1, x} = TriviumCore[s, 66, 171, 91, 92, 93];
    {Mvars2, Mcon2, y} = TriviumCore[x, 162, 264, 175, 176, 177];
    {Mvars3, Mcon3, s} = TriviumCore[y, 243, 69, 286, 287, 288];
    MV1 = Flatten[List[Mvars1, Mvars2, Mvars3]];
    MC1 = Flatten[List[Mcon1, Mcon2, Mcon3]];
    MV = Flatten[List[MV, MV1]];
    MC = Flatten[List[MC, MC1]];
    s = RotateRight[s, 1]
    )];

FunzioneRound[state_] := Module[{g, r},
   (
    g = state;
    For[r = 1, r <= 150, r++, g = UpdateState[g]];
    g
    )];

TriviumEval[] :=
  (
   M1 = List[];
   MV = s[0];
   MC = List[];
   state1 = FunzioneRound[s[0]];
   pos = {66, 93, 162, 177, 243, 288};
   state = Table[0, {i, 1, 288}];
   state[[pos]] = state1[[pos]];
   Mc = List[Plus @@ state[[pos]] == 1];
   MC = Flatten[List[MC, Mc]];
   Mc1 = Delete[state, {{66}, {93}, {162}, {177}, {243}, {288}}];
   MC = Flatten[List[MC, Mc1]];
   M = List[MV, MC]
   );

TriviumEval[]
