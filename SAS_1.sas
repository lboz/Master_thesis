options linesize=75;
libname memoire 'C:\Documents and Settings\caliaboz\Mes documents\Mémoire';


/* socio-démo */

proc means data=memoire.nov n nmiss mean std median min max;  
var Sexe Age Travail Revenu;
run;

/* donne des tableaux de fréquences */
proc freq data=memoire.nov;
tables Sexe Age Travail Revenu;
run;


/* Evaluation produit hédonique */

proc corr data=memoire.scurt alpha;
var h_1 h_2 h_3;
run; 

proc factor data=memoire.scurt scree method=prinit;
var h_1 h_2 h_3;
run; 

proc factor data=memoire.nov  method=prinit msa scree rotate=varimax  maxiter=500 nfact=2 res;
var h_1 h_2 h_3;
run; 

proc princomp data=memoire.nov out=temp cov ;
var h_1 h_2 h_3;
run;


/* Evaluation produit utilitariste */

proc corr data=memoire.nov alpha;
var u_1 u_2 u_3;
run; 

proc factor data=memoire.nov  method=prinit msa scree;
var u_1 u_2 u_3;
run; 

proc factor data=memoire.data  method=prinit msa scree rotate=varimax  maxiter=500 nfact=2 res;
var eval_u_1 eval_u_2 eval_u_3;
run; 

proc princomp data=memoire.data out=temp cov;
var eval_u_1 eval_u_2 eval_u_3;
run;


/* Familiarité produit */

proc corr data=memoire.nov alpha;
var fam_1 fam_2 fam_3;
run; 

proc factor data=memoire.nov  msa scree method=prinit;
var fam_1 fam_2 fam_3;
run; 

proc factor data=memoire.data2  method=prinit msa scree rotate=varimax maxiter=500 nfact=2 res;
var famil_1 famil_2 famil_3;
run; 

proc princomp data=memoire.data2 out=temp cov;
var famil_1 famil_2 famil_3;
run;


/* Familiarité Internet */

proc corr data=memoire.nov alpha;
var Internet_3 - Internet_6;
run; 

proc factor data=memoire.nov msa scree  method=prinit;
var Internet_3 - Internet_6;
run; 

proc corr data=memoire.scurt alpha;
var control plaisir;
run; 

proc factor data=memoire.scurt msa scree  method=prinit;
var control plaisir;
run; 



proc factor data=memoire.nov  method=prinit msa scree rotate=varimax  maxiter=500 nfact=2 res;
var Internet_1 - Internet_6;
run; 

proc princomp data=memoire.data1 out=temp cov;
var Internet_1 - Internet_6;
run;

data memoire.iscurt; set memoire.test2;
Iscurt = mean(Internet_3 - Internet_6);
run;

proc corr data=memoire.data2 alpha;
var control plaisir;
run; 


/* Confiance */

proc corr data=memoire.nov alpha;
var Confiance_1 - Confiance_5;
run; 

proc factor data=memoire.nov  msa scree method=prinit;
var Confiance_1 - Confiance_5;
run; 

proc factor data=memoire.nov  method=prinit msa scree rotate=varimax  maxiter=500  res;
var Confiance_1 - Confiance_5;
run; 

proc princomp data=memoire.data out=temp cov;
var Confiance_1 - Confiance_5;
run;

data memoire.test2; set memoire.test2;
Iscurt = mean(Internet_3, Internet_4, Internet_5, Internet_6);
run;

/* Sentiments */

proc corr data=memoire.nov1 alpha;
var Sentiment_pos Sentiment_joie;
run; 

proc factor data=memoire.data  method=prinit;
var Sentiment_1 Sentiment_2;
run; 

proc factor data=memoire.data  method=prinit msa scree rotate=varimax  maxiter=500 nfact=2 res;
var Sentiment_pos Sentiment_joie;
run; 

proc princomp data=memoire.data1 out=temp cov;
var Sentiment_pos Sentiment_joie;
run;

proc corr data=memoire.test2 alpha;
var fam confiance iscurt utilisation_Internet multitaches /*sexe age travail etudes revenu*/;
run; 

/* logit */
data memoire.test2; set memoire.test2;
if Preference_H_U = 1 then pref=1; else
if Preference_H_U = 2 then pref=0;
run;

proc sql;
create table inter as
select *
from memoire.test2
where interruption in (1,2);
quit;

proc logistic data=memoire.test2;
/*freq count;*/
class produit interruption / param = ref;
model pref  = /*produit interruption fam confiance iscurt utilisation_Internet multitaches sexe age travail revenu*/ etudes  / link=logit;
run;

/* ttest produit */

proc ttest data=memoire.nov;
class HEC;
var Preference_H_U;
run;


/* ttest interruption */

proc ttest data=memoire.data;
class groupe;
var u h;
run;

proc ttest data=inter;
class interruption;
var h u;
run;



title 't-test avec Y1 sur les groupes 1 et 2'; 
data temp2;
set memoire.test2;
if produit=2 then delete; 
run;
proc ttest data=memoire.test2;
class interruption produit;
var  H U ;
run;
Preference_H_U
proc means;


title 'test T2 sur les 2 variables simultanément avec les groupes 1 et 2'; 
proc discrim data=temp pool=test;
class groupe;
var y1 y2;
run;

proc glm data=temp1;
class interruption produit;
model U H =  interruption produit;
manova h = interruption produit;
run;

proc glm data=memoire.data1;
class produit inter;
model eval_u eval_h preference = produit inter produit * inter;
run;
by famil_pr internet confiance multitaches;


proc glm data=memoire.data;
class etud;
model preference = etud;
run;
repeated eval_h, eval_u;

manova h = groupe;


title 'ANOVA sur Y1 avec les 3 groupes 1, 2 et 3'; 
proc glm data=memoire.test2;
class interruption;
model H = groupe;
run;

title 'MANOVA sur les 2 variables simultanément avec les 6 groupes 1, 2, 3, 4, 5, 6'; 
proc glm data=memoire.test2;
class multitaches /*fam internet confiance Multitaches Utilisation_Internet*/;
manova h = groupe;
model  u h  = multitaches;
run;
title 'Comparaisons multiples (tous les groupes 2X2) séparément pour Y1 et Y2'; 
proc glm data=memoire.test2;
class interruption /*fam internet confiance Multitaches Utilisation_Internet*/;
model u h  = interruption;
/* manova h = multitaches; */
means interruption / bon tukey lsd cldiff clm lines;
run;


title 't-tests sur chaque variable séparément'; 
 
proc ttest data=multi.manova2;
class groupe;
var y1 y2;
run;

title 'tests T2 sur les 2 variables';
proc discrim data=multi.manova2 pool=test;
class groupe;
var y1 y2;
run;
proc glm data=multi.manova2;
class groupe;
model y1 y2 = groupe;
manova h = groupe;
run; 


title 'tests T2 sur les 2 variables';
proc discrim data=memoire.test2 pool=test3;
class groupe;
var u h;
run;
proc glm data=memoire.test2;
class interruption;
model h = interruption;
/*manova h = groupe;*/
run; 



proc sort data=memoire.datapref out=data1;
by preference;
run;


proc logistic data=inter descending;
model pref = produit interruption  produit*interruption /*multitaches famil internet confiance_test temps_internet /*age sex emploi etudes revenu*/ / clparm=both clodds=both rsquare expb link=logit;
ods output out=predicted1 p=pred_prob;
run; 


data memoire.test1;
set memoire.nov;
if Interruption = 1 and produit = 1 then Groupe = 1; else
if Interruption = 2 and produit = 1 then Groupe = 2; else
if Interruption = 0 and produit = 1 then Groupe = 3; else
if Interruption = 1 and produit = 2 then Groupe = 4; else
if Interruption = 2 and produit = 2 then Groupe = 5; else
if Interruption = 0 and produit = 2 then Groupe = 6; 
run;

data memoire.test2;
set memoire.test1;
H=(h_1+h_2+h_3)/3;
u=(u_1+u_2+u_3)/3; 
fam=(fam_1+fam_2+fam_3)/3;
internet=(internet_1+internet_2+internet_3+internet_4+internet_5+internet_6)/6;
confiance=(confiance_1+confiance_2+confiance_3+confiance_4+confiance_5)/5;
run;

proc means data=memoire.test2;
var fam internet confiance multitaches utilisation_internet;
class  sexe;
run;

proc means data=temp1;
var Preference_H_U u h;
run;

proc freq data=temp1;
table  Preference_H_U;
run;


proc freq data=memoire.test2;
table sexe*internet sexe*confiance sexe*multitaches sexe*utilisation_internet;
run;

/*
proc corr data=temp;
var factor1-factor4;
with service produit paiement prix;
run;

proc factor data=temp  method=prinit msa scree rotate=varimax  maxiter=500 res;
var x1-x5;
by y;
run; 
*/
