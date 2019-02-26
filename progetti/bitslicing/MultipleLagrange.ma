

f=y^6+y+1;
len=Exponent[f,y];

FieldInversion[-1]=1;
FieldInversion[1]=1;
FieldInversion[k_] := FieldInversion[k] = Module[{u,v,g1,g2,i,ux,vx},
(
	u=PolynomialMod[k,2]; v=f; g1=1; g2=0;
	i=0;
	While[
		While[Coefficient[u,y,0]==0,
			(
				u=Simplify[u/y];
				If[Coefficient[g1,y,0]==0,
					g1=g1/y,
					g1=Simplify[PolynomialMod[(g1+f),{2}]/y]
				]
			)
		];
		(u=!=1)&&(i<10),
		(
			If[Exponent[u,y]<Exponent[v,y],
				(
					ux=v;v=u;u=ux;
					ux=g2;g2=g1;g1=ux
				)
			];
			u=PolynomialMod[(u+v),{2}];
			g1=PolynomialMod[(g1+g2),{2}];
			i++;
		)
	];
	g1
)];

Poly[n_]:=Reverse[IntegerDigits[n,2,6]].Table[y^i,{i,0,6-1}];

Int2Vec[x_]:=Reverse@IntegerDigits[x,2,len];
Int2Poly[x_]:=Int2Vec[x].Table[y^i,{i,0,len-1}];

Pol2Vec[x_]:=Reverse@CoefficientList[x,y,len];
Pol2Int[x_]:=FromDigits[Pol2Vec[x],2];

FP[a_,b_]:=FP[a,b]=Pol2Int[PolynomialMod[Int2Poly[a]Int2Poly[b],{f,2}]];
FA[a_,b_]:=FA[a,b]=Pol2Int[PolynomialMod[Int2Poly[a]+Int2Poly[b],{f,2}]];
FI[a_]:=FI[a]=Pol2Int[FieldInversion[Int2Poly[a]]];

rule1=GF[a_] GF[b_]:>GF[FP[a,b]];
rule2=GF[a_]+ GF[b_]:>GF[FA[a,b]];
rule3=Power[GF[a_],b_/;b>0]:>GF[Nest[FP[#,a]&,a,b-1]];
rule4=Power[GF[a_],-1]:>GF[FI[a]];
rule5=-GF[a_]:>GF[a];
(*rule6=(a_;IntegerQ[a]) GF[b_]:>GF[Nest[FA[#,b]&,a,a-1]];*)
rule7=GF[0]->0;
(*   
rule8 = 1\[Rule] GF[1];
rule9=b_ GF[a_]\[RuleDelayed] Mod[b,2] GF[a];
*)
rules={rule1,rule2,rule3,rule4,rule5,rule7};

(*Produttoria[k_]:=Partition[Map[(Collect[Expand[#],x]//.rules)&,x(Xp[[k]]-Drop[Xp,{k}])^(-1)-Drop[Xp,{k}] (Xp[[k]]-Drop[Xp,{k}])^(-1) ],2,2,{1,1},1];*)
Produttoria[k_]:=Partition[((x (Xp[[k]]-Drop[Xp,{k}])^(-1))//.rules)+(-Drop[Xp,{k}] (Xp[[k]]-Drop[Xp,{k}])^(-1)//.rules),2,2,{1,1},GF[1]];

f2[c_]:=PolynomialMod[Collect[Expand[c[[1]] c[[2]]],x]//.rules,2];
ApplyStep[p2_]:=If[(Length[p2]>=2),Partition[Map[f2,p2],2],f2[p2[[1]]],p2];

Lagrange[Yp_] :=
Module[{sum, XX, pp, tt, stt},
(sum = 0;
XX = Table[
{Produttoria[kk],
Yp[[kk]],
pp = Nest[ApplyStep, Produttoria[kk], Log[2, points]],
tt = Yp[[kk]] pp,
stt = f2[tt],
sum = f2[{sum + stt, 1}]}, {kk, 1, points}];
TableForm[XX, TableDepth -> 2];
PolynomialMod[XX[[-1, -1]], {2}]
)]

(* DES FUNCTIONS *)

PERM = {16, 7, 20, 21, 29, 12, 28, 17, 1, 15, 23, 26, 5, 18, 31, 10,
2, 8, 24, 14, 32, 27, 3, 9, 19, 13, 30, 6, 22, 11, 4, 25}

EXPANSION = {32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9, 8, 9, 10, 11, 12,
13, 12, 13, 14, 15, 16, 17, 16, 17, 18, 19, 20, 21, 20, 21, 22, 23,
24, 25, 24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1}

DES1 = {{14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7}, {0,
15, 7, 4, 14, 2, 13, 10, 3, 6, 12, 11, 9, 5, 3, 8}, {4, 1, 14, 8,
13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0}, {15, 12, 8, 2, 4, 9, 1,
7, 5, 11, 3, 14, 10, 0, 6, 13}};
DES2 = {{15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10}, {3,
13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5}, {0, 14, 7, 11,
10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15}, {13, 8, 10, 1, 3, 15, 4,
2, 11, 6, 7, 12, 0, 5, 14, 9}};
DES3 = {{10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8}, {13,
7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1}, {13, 6, 4, 9,
8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7}, {1, 10, 13, 0, 6, 9, 8,
7, 4, 15, 14, 3, 11, 5, 2, 12}};
DES4 = {{7, 3, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15}, {13, 8,
11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9}, {10, 6, 9, 0, 12,
11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4}, {3, 15, 0, 6, 10, 1, 13, 8,
9, 4, 5, 11, 12, 7, 2, 14}};
DES5 = {{2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9}, {14,
11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6}, {4, 2, 1, 11,
10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14}, {11, 8, 12, 7, 1, 14, 2,
13, 6, 15, 0, 9, 10, 4, 5, 3}};
DES6 = {{12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11}, {10,
15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8}, {9, 14, 15, 5,
2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6}, {4, 3, 2, 12, 9, 5, 15,
10, 11, 14, 1, 7, 6, 0, 8, 13}};
DES7 = {{4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1}, {13,
0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6}, {1, 4, 11, 13,
12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2}, {6, 11, 13, 8, 1, 4, 10,
7, 9, 5, 0, 15, 14, 2, 3, 12}};
DES8 = {{13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7}, {1,
15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2}, {7, 11, 4, 1,
9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8}, {2, 1, 14, 7, 4, 10, 8,
13, 15, 12, 9, 0, 3, 5, 6, 11}};

(* SLICING *)

Slice[sbox_, x_, bit_] :=
Mod[Floor[
sbox[[ (Mod[x, 2] + Mod[Floor[x/16], 2] 2) + 1,
If[Floor[x/2] >=16, Floor[x/2] - 16+1, Floor[x/2] + 1  ]]]/(2^
bit)], 2];

LagrangeSlice[i_] := Lagrange[Map[GF,Table[Slice[DES[[Floor[i/4] + 1]], j, Mod[i, 4]], {j, 0,points - 1}]]]

DesBlock[i_] := Module[{pol1,pol2},
		(
			pol1 = Range[32];
			pol2 = Partition[(pol1[[PERM]])[[EXPANSION]], 6];
			Map[LagrangeSlice, pol2[[i]]]
		)
];

var[i_] :=Map[ToExpression["x" <> ToString[#]] &, Floor[pol2[[i]]/4] + 1]
sub[k_] := Map[Rule[x, #] &, var[k]]
Composed[s_] := ExpandAll[ Sum[GF[2^(Length[var[s]] - 1 - j)] LagrangeSlice[s][[j + 1]] /.sub[s][[j + 1]], {j, 0, Length[var[s]] - 1}]] //. rules

FinalPoly[P_, composed_] :=	f2[{P /. Join @@ Table[{x^(2 i) -> Map[#^(2 i) &, composed], x^(2 i + 1) -> (composed Map[#^(2 i) &, composed])}, {i, 1, Floor[Exponent[P, x]/2]}], 1}]

(* EXAMPLE RUN of BIT-SLICING*)

bit = 2 ; 										(* we select the slice *)
degree = 2 ; 									(* approximation degree of the second round s-box *)
P2 = Select[LagrangeSlice[1], Exponent[#, x] <= degree &]; (* we truncate the Lagrange polynomial to degree *)
composed=Composed[bit];
FinalPoly[P2, composed];

(* RUN PRELIMINARI - TEST INIZIALI - da cancellare*)

Print[points=2^len];
Print[Xp=Map[GF,Range[points]-1];Length[Xp]];
Print[Yp=Map[GF,Table[Slice[DES1,i,1],{i,1,points}]];]
Print[Length[Yp]];

(* MAIN COMPUTATION of THE TABLE WITH PARTIAL RESULTS*)
sum=0;
XX=Table[
	{
	Produttoria[kk],
	Yp[[kk]],
	pp=Nest[ApplyStep,Produttoria[kk],Log[2,points]],
	tt=Yp[[kk]] pp,
	stt=f2[tt],
	sum=f2[{sum+ stt,1}]},{kk,1,points}
];

Print[TableForm[XX,TableDepth->2]];

lagrange=PolynomialMod[XX[[-1,-1]],{2}];

Print["Lagrange polynomial ",lagrange/.GF[a_]:>a];

(* VERIFICATION *)
legenda={"x_i","y_i","L(x_i)"};
Print[TableForm@Join[{legenda},Table[{Xp[[i]],PolynomialMod[PolynomialMod[(lagrange/.x->Xp[[i]])//.rules,{2}]//.rules,{2}],Yp[[i]]},{i,1,Length[Xp]}]]];





