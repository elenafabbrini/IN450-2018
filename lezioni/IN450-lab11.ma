(* Definisco un polinomio *)

f[v_, x_] :=
Module[{v1, v2, v3, v4, v5, v6, v7, v8, x1, x2, x3, x4, x5, x6, x7,
x8},
(
{v1, v2, v3, v4, v5, v6, v7, v8} = v;
{x1, x2, x3, x4, x5, x6, x7, x8} = x;
Mod[(x1 + v2 x1 + v6 x1 + v2 v4 v6 x1 + v7 x1 + v5 v7 x1 + v1 x2 +
v2 x2 + v3 v5 x2 + v5 v7 x2 + v8 x2 + v7 x1 x2 + v5 v7 x1 x2 +
v1 x3 + v2 x3 + v4 x3 + v2 v6 x3 + v2 v4 v6 x3 + v7 x3 +
v5 v7 x3 + v7 x1 x3 + x2 x3 + v3 v5 x2 x3 + x4 + v1 x4 + v2 x4 +
v2 v6 x4 + v5 v7 x4 + v8 x4 + v6 v8 x4 + v5 v7 x1 x4 + x2 x4 +
v3 v5 x2 x4 + v7 x2 x4 + v3 v5 x3 x4 + v7 x3 x4 + v2 x5 +
v3 x5 + v2 v4 v6 x5 + v7 x5 + x2 x5 + v3 x2 x5 + v3 v5 x2 x5 +
v7 x2 x5 + v5 v7 x2 x5 + v3 v5 x3 x5 + v5 v7 x3 x5 + v3 x4 x5 +
v7 x4 x5 + x6 + v1 x6 + v2 x6 + v4 x6 + v3 v5 x6 + v2 v6 x6 +
v2 v4 v6 x6 + v6 v8 x6 + v7 x1 x6 + x2 x6 + v7 x2 x6 +
v3 v5 x3 x6 + v7 x3 x6 + v3 v5 x4 x6 + v7 x4 x6 + v5 v7 x4 x6 +
v3 v5 x5 x6 + v7 x5 x6 + v2 x7 + v3 x7 + v2 v6 x7 +
v2 v4 v6 x7 + v8 x7 + v5 v7 x1 x7 + v3 x2 x7 + v3 v5 x2 x7 +
x3 x7 + v3 v5 x3 x7 + v7 x3 x7 + v5 v7 x3 x7 + x4 x7 +
v3 x4 x7 + v7 x4 x7 + x5 x7 + v7 x5 x7 + x6 x7 + v3 v5 x6 x7 +
v2 x8 + v6 x8 + v2 v4 v6 x8 + v7 x8 + v6 v8 x8 + v7 x1 x8 +
v5 v7 x1 x8 + v3 v5 x2 x8 + x3 x8 + v3 v5 x3 x8 + x4 x8 +
v7 x4 x8 + x5 x8 + v5 v7 x5 x8 + x6 x8 + v3 v5 x6 x8 +
v7 x6 x8 + v5 v7 x6 x8 + v7 x7 x8 + v5 v7 x7 x8), 2]
)]


(* divido le variabili in pubblice e private *)

pubvars = {v1, v2, v3, v4, v5, v6, v7, v8} ;
privars = {x1, x2, x3, x4, x5, x6, x7, x8} ;


chosenpubvars = Table[Min[RandomInteger[3], 1], {Length[pubvars]}]

ClearAll[Cube,alpha0,alpha]

zerov=Table[0,{Length[privars]}];

Cube[chosenpubs_,imask_]:=Module[{lmask,vpub},
	(
		vpub=chosenpubs;
		lmask=Length[imask];Table[vpub[[imask]]=IntegerDigits[nv,2,lmask];vpub,{nv,0,2^lmask-1}]
	)
];

iversor[j_]:=If[j==0,zerov,Module[{vars},vars=zerov;vars[[j]]=1;vars]];

alpha0[chosenpubs_,imask_]:=alpha0[chosenpubs,imask]=Mod[Plus@@Map[f[#,iversor[0]]&,Cube[chosenpubs,imask]],2];
alpha[chosenpubs_,imask_,j_]:=alpha[chosenpubs,imask,j]=Mod[Plus@@Map[f[#,iversor[j]]&,Cube[chosenpubs,imask]]-alpha0[chosenpubs,imask],2];

SuperPolyCoefficients[chosenpubs_,imask_]:={imask,chosenpubs,Table[alpha[chosenpubs,imask,j],{j,1,Length[privars]}],alpha0[chosenpubs,imask]}

SuperPoly[chosenpubs_,imask_]:=Module[{spc}, (
		spc=SuperPolyCoefficients[chosenpubs,imask];
		spc[[3]].privars+spc[[4]]
)];

alpha[chosenpubs_,imask_,j1_,j2_]:= alpha[chosenpubs,imask,j1,j2]=
	Mod[Plus@@Map[f[#,iversor[j1]+iversor[j2]]&,Cube[chosenpubs,imask]]-alpha[chosenpubs,imask,j1]-alpha[chosenpubs,imask,j2]-alpha0[chosenpubs,imask],2];

LinearityTest[chosenpubs_, imask_] :=
	Plus @@ Flatten@
		Table[alpha[chosenpubs, imask, j1, j2], {j1, 1, Length[privars] - 1}, {j2, j1 + 1, Length[privars]}] === 0 ;

PickRandom[state_] := (
		If[state[[2]] === {}, lstate = Reverse[PickRandom[Reverse[state]]],
		lstate = state];
		index = RandomInteger[Length[lstate[[2]]] - 1] + 1;
		{Append[lstate[[1]], lstate[[2]][[index]]], Drop[lstate[[2]], {index}]});

MaximalRank[st_] := Module[{M},
		If[st === {},
			0,
			(
				M = Map[#[[3]] &, st];
				MatrixRank[M, Modulus -> 2]
			)
		]
];

(* RANDOM WALK IN THE MONOMIAL LATTICE *)

savedterms = {}
kk = 0
While[(MaximalRank[savedterms] < Length[privars]) && (kk < 50),
(
kk = kk + 1;
chosenpubvars = Table[Min[RandomInteger[3], 1], {Length[pubvars]}];

state = {{1}, {2, 3, 4, 5, 6, 7, 8}};
state = PickRandom[state];
sp = {0, zerov, iversor[0], 0};
While[(Plus @@ sp[[3]] === 0),
state = Reverse[PickRandom[Reverse[state]]];
If[state[[1]] === {}, state = PickRandom[state]];
Print["before  LinTest:", state];
If[state[[1]] =!= {},
While[! LinearityTest[chosenpubvars, imask = state[[1]]],
Print[state];
state = PickRandom[state]];
Print[sp];
sp = SuperPolyCoefficients[chosenpubvars, imask];
]
];
savedterms = Append[savedterms, sp]
(*SuperPoly[chosenpubvars,imask]*)
)]

