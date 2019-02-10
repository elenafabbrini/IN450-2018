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
rule6=(a_;IntegerQ[a]) GF[b_]:>GF[Nest[FA[#,b]&,a,a-1]];
rule7=GF[0]->0;
(*
rule8 = 1\[Rule] GF[1];
rule9=b_ GF[a_]\[RuleDelayed] Mod[b,2] GF[a];
*)
rules={rule1,rule2,rule3,rule4,rule5,rule6,rule7};

(*Produttoria[k_]:=Partition[Map[(Collect[Expand[#],x]//.rules)&,x(Xp[[k]]-Drop[Xp,{k}])^(-1)-Drop[Xp,{k}] (Xp[[k]]-Drop[Xp,{k}])^(-1) ],2,2,{1,1},1];*)
Produttoria[k_]:=Partition[((x (Xp[[k]]-Drop[Xp,{k}])^(-1))//.rules)+(-Drop[Xp,{k}] (Xp[[k]]-Drop[Xp,{k}])^(-1)//.rules),2,2,{1,1},GF[1]];

f2[c_]:=PolynomialMod[Collect[Expand[c[[1]] c[[2]]],x]//.rules,2];
ApplyStep[p2_]:=If[(Length[p2]>=2),Partition[Map[f2,p2],2],f2[p2[[1]]],p2];

(* EXAMPLE RUN *)
Print[points=64];
Print[Xp=Map[GF,Range[points]-1];Length[Xp]];
Print[Yp=Map[GF,Table[RandomInteger[1],{points}]];]
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





