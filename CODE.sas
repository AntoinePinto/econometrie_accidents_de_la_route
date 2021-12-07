LIBNAME projet 'C:\Users\Antoi\OneDrive\Université\Master 1\Semestre 1\SAS\Projet\output'; RUN;

/* Importation des données */

PROC IMPORT DATAFILE="C:\Users\Antoi\OneDrive\Université\Master 1\Semestre 1\SAS\Accidents de la route\accidents.csv"
OUT=data
DBMS=csv  REPLACE ;
getnames=yes;
delimiter = ";";
RUN ;

PROC CONTENTS DATA = data; RUN;

PROC PRINT DATA = data(OBS=10); 
RUN; 

/* Création du coefficient de gravité */

data data1;
set data;
coef_grav = (8*nb_tue + 1*nb_blesse);

run;

/* Création d'une macro permettant de faire un résumé concernant les données manquantes sur les variables quantitatives */ 

%macro resume_stat(variab);  

 	proc sort data=data1 out=data1;
	   	by &variab;
	run;

	proc means data=data1  mean N NMISS;
		by &variab;
		var Coef_grav;
	run;

%mend;

%resume_stat(nb_dispo_enfant_non_mis)

/* Création de plusieurs variables indicatrices potentiellement utile pour notre étude */ 

data data1; set data1;

hors_agglo = (localisation = "Hors agglomération");

route_departementale_nationale = (Categorie_route = "Route Départementale" OR Categorie_route = "Route Nationale");
route_communale = (Categorie_route = "Voie Communale");
route_autoroute = (Categorie_route = "Autoroute");

circulation_A_chaussees_separees = (Circulation = "A chaussées séparées");
circulation_Bidirectionnelle = (Circulation = "Bidirectionnelle");
circulation_A_sens_unique = (Circulation = "A sens unique");

conditions_tres_mauvaises = (Conditions_atmospheriques = "Vent fort - tempête" OR Conditions_atmospheriques = "Brouillard - fumée");
conditions_mauvaises = (Conditions_atmospheriques = "Temps éblouissant" OR
						Conditions_atmospheriques = "Temps couvert" OR
						Conditions_atmospheriques = "Pluie légère" OR
						Conditions_atmospheriques = "Pluie forte" OR
						Conditions_atmospheriques = "Neige - grêle" );
conditions_normales = (Conditions_atmospheriques = "Normale");

intersection_hors_intersection = (Intersection = "Hors intersection" OR	Intersection = "Place");
intersection_passage_a_niveau = (Intersection = "Passage à niveau");
intersection_intersection = (Intersection = "Intersection en T" OR
								Intersection = "Intersection en X" OR
								Intersection = "Intersection en Y" OR
								Intersection = "Intersection à plu");

lumiere_nuit_avec_eclairage =(Lumiere = "Nuit avec éclairage public allumé");
nuit_sans_eclairage =(Lumiere = "Nuit avec éclairage public non allumé" OR Lumiere = "Nuit sans éclairage public" );
lumiere_jour =(Lumiere = "Plein jour");

nb_de_voies_aucune = (Nombre_de_voies = 0);
nb_de_voies_1 = (Nombre_de_voies = 1);
nb_de_voies_2 = (Nombre_de_voies = 2);
nb_de_voies_3_et_plus = (Nombre_de_voies > 2.5);

profil_sommet = (Profil = "Sommet de côte");
profil_plat = (Profil = "Plat");
profil_pente = (Profil = "Pente" OR Profil = "Bas de côte");

situation_accotement = (Situation = "Sur accotement" OR Situation = "Sur trottoir" OR Situation = "Sur bande d’arrêt d’urgence");
situation_chaussee = (Situation = "Sur chaussée" OR	Situation = "Sur piste cyclable");

surface_inondee_enneigee = (Surface = "verglacée" OR Surface = "inondée" OR Surface = "flaques" OR Surface = "enneigée");
surface_normale = (Surface = "normale");
surface_mouillee = (Surface = "mouillée");

run;
  
/* Suppression de variable superflus ou mal codées */ 

data data1;
set data1 ;
DROP 	categorie_route
		Circulation
		Conditions
		Intersection
		Localisation
		Lumiere
		Nb_de_voies
		Profil
		Situation
		Surface
		Collision
		Code_commune
		Code_Insee
		Adresse
		Latitude
		Longitude
		Code_Postal
		Numero
		Coordonnees
		PR
		V1
		Env1
		Voie
		V2
		PR1
		Identifiant_vehicule
		Place
		Sens
		Commune
		Code_Officiel_Departement
		date
		Code_Officiel_EPCI
		Nom_Officiel_EPCI
		Code_Officiel_Region
		Code_Officiel_Commune
		;
run;

PROC CONTENTS DATA = data1; RUN;

PROC PRINT DATA = data1(OBS=10); RUN; 


/* Création et renommage des variables utiles à notre modélisation */ 

data data1;
set data1 ;
age_moyen_2 = age_moyen_usagers*age_moyen_usagers ;
rename nb_individu = nb_indiv ;
rename age_moyen_usagers = age_moyen ;
rename point_de_choc_arriere = nb_choc_arriere ;
nb_secu_non_mis = (nb_ceintures_non_attachees + nb_casque_non_mis +  nb_dispo_enfant_non_mis +  nb_autre_secu_non_mis + nb_equip_reflech_non_mis ) ;
juillet_aout = (mois = 7) OR (mois = 8);
heure_nuit = (heure > 22) OR (heure < 9) ;
run;

PROC FREQ data= data1 ;
  TABLES hors_agglo nuit_sans_eclairage nb_indiv nb_secu_non_mis age_moyen nb_choc_arriere  ;
run ;

/* Nous supprimons les accidents de plus de 10 individus, ils ne représentent que 0.1 % de notre échantillon */

data data1;
set data1 ;
where nb_indiv < 11 ;
if age_moyen = . then delete;
run;

PROC CONTENTS DATA = data1; RUN;

/* Vérifions qu'il n'y a pas de trop forte corrélation entre les variables explicatives */


PROC CORR data = data1 ;
VAR coef_grav hors_agglo nuit_sans_eclairage nb_indiv nb_secu_non_mis age_moyen nb_choc_arriere;
run ;

/* Première regression par la méthode des moindres carrés ordinaires */ 
Proc REG data=data1;
title 'Régression';
model coef_grav = 	hors_agglo 
					nuit_sans_eclairage 
					nb_indiv
					nb_secu_non_mis
					age_moyen
					age_moyen_2
					nb_choc_arriere;
run; 


proc freq data = data1;
tables lumiere ;
run;



/* Étude de la multicolinéarité */ 

/* VIF */ 

Proc REG data=data1;
title 'Régression';
model coef_grav = 	hors_agglo 
					nuit_sans_eclairage 
					nb_indiv
					nb_secu_non_mis
					age_moyen
					age_moyen_2
					nb_choc_arriere  / vif;
run; 


/* Test de White */ 

Proc REG data=data1;
title 'Régression';
model coef_grav = 	hors_agglo 
					nuit_sans_eclairage 
					nb_indiv
					nb_secu_non_mis
					age_moyen
					age_moyen_2
					nb_choc_arriere  /noint  spec;
run; 

/* BREUSH PAGAN TEST */ 

Proc MODEL data=data1;
coef_grav = b_constante+ b_hors_agglo*hors_agglo + b_nuit_sans_eclairage*nuit_sans_eclairage + 
			b_nb_indiv*nb_indiv + b_nb_secu_non_mis*nb_secu_non_mis + b_age_moyen*age_moyen + 
			b_age_moyen_2*age_moyen_2 + b_nb_choc_arriere*nb_choc_arriere ;
FIT coef_grav / breush=(nb_indiv) ;
run; 
	

/* Estimation avec écarts-type robustes */ 

Proc REG data=data1;
title 'Régression';
model coef_grav = 	hors_agglo 
					nuit_sans_eclairage 
					nb_indiv
					nb_secu_non_mis
					age_moyen
					age_moyen_2
					nb_choc_arriere  / white;
run; 

/* ENDOGENEITE */ 

/* GMM */ 

/* Méthode IV - avec hétéroscédasticité donc nous devons utiliser les écarts-types robustes */ 

proc model data = data1 plots = none; 
coef_grav = b_constante+ 
			b_hors_agglo*hors_agglo + 
			b_nuit_sans_eclairage*nuit_sans_eclairage + 
			b_nb_indiv*nb_indiv +
			b_nb_secu_non_mis*nb_secu_non_mis + 
			b_age_moyen*age_moyen + 
			b_age_moyen_2*age_moyen_2 +
			b_nb_choc_arriere*nb_choc_arriere ;
exogenous    nb_indiv nb_secu_non_mis age_moyen age_moyen_2 nb_choc_arriere   ;
endogenous coef_grav hors_agglo nuit_sans_eclairage;
instruments _exog_ juillet_aout tracteur heure_nuit ;   
fit coef_grav / 2SLS hccme = 1 out=residuals outresid ;  
run; 

/* Méthode GMM + Test d'exogénéité des variables instrumentales */  

proc model data = data1 plots = none; 
coef_grav = b_constante+ 
			b_hors_agglo*hors_agglo + 
			b_nuit_sans_eclairage*nuit_sans_eclairage + 
			b_nb_indiv*nb_indiv +
			b_nb_secu_non_mis*nb_secu_non_mis + 
			b_age_moyen*age_moyen + 
			b_age_moyen_2*age_moyen_2 + 
			b_nb_choc_arriere*nb_choc_arriere ;
exogenous    nb_indiv nb_secu_non_mis age_moyen age_moyen_2 nb_choc_arriere   ;
endogenous coef_grav hors_agglo nuit_sans_eclairage ;
instruments _exog_ juillet_aout tracteur heure_nuit ;   
fit coef_grav / GMM KERNEL = (PARZEN,0,) ;  
run; 

/* Approche de la régression augmentée - Utilisation des FGLS */

data residuals(keep = iv_residuals); set residuals;
rename coef_grav = iv_residuals;
run;

data data1; merge data1 residuals;
run;

data data1; set data1;
iv_resid2 = iv_residuals*iv_residuals;
log_u2 = log(iv_resid2);
run;

proc reg data = data1 outest = stats  rsquare plots = none;
model log_u2 = hors_agglo nuit_sans_eclairage age_moyen ;
output out = pred_u2 p = pred_log_u2; 
run;

data data1; set pred_u2; run;

data data1; set data1;
sigma2 = exp(pred_log_u2); 
one_over_sigma = 1/(sigma2**0.5);  
run;

/* Si les résidus sont significatifs alors on rejette l'hypothèse d'exogénéité */ 

proc reg data = data1 plots = none;
model hors_agglo =  nuit_sans_eclairage nb_indiv nb_secu_non_mis age_moyen age_moyen_2 nb_choc_arriere juillet_aout tracteur heure_nuit;
output out = hors_agglo_resid r = hors_agglo_residuals; 
test juillet_aout=0, tracteur=0, heure_nuit=0; 
run;

proc reg data = data1 plots = none;
model nuit_sans_eclairage  =  hors_agglo nb_indiv nb_secu_non_mis age_moyen age_moyen_2 nb_choc_arriere juillet_aout tracteur heure_nuit;
output out = nuit_sans_eclairage_resid r = nuit_sans_eclairage_residuals; 
test juillet_aout=0, tracteur=0, heure_nuit=0; 
run;

data data2; merge data1 hors_agglo_resid(keep = hors_agglo_residuals) nuit_sans_eclairage_resid(keep = nuit_sans_eclairage_residuals);
run;

proc reg data = data2 plots = none;
model coef_grav = hors_agglo nuit_sans_eclairage nb_indiv nb_secu_non_mis age_moyen age_moyen_2 nb_choc_arriere hors_agglo_residuals nuit_sans_eclairage_residuals;
weight one_over_sigma; 
test hors_agglo_residuals=0, nuit_sans_eclairage_residuals=0;
run;


/**********************************/
/***PARTIE 2 : MODELE LOGISTIQUE***/
/**********************************/ 

/* Importation de la base de données */

PROC IMPORT DATAFILE="C:\Users\Antoi\OneDrive\Université\Master 1\Semestre 1\SAS\Accidents de la route\individus_accidentes.csv"
OUT=data2
DBMS=csv  REPLACE ;
getnames=yes;
delimiter = ";";
RUN ;

PROC CONTENTS DATA = data2; RUN;

PROC PRINT DATA = data2(OBS=10); 
RUN; 

data data2;
set data2 ;
blesse_ou_tue = (gravite = "Blessé") OR (gravite = "Tué") ;
run;

data data2;
set data2 ;
where nb_indiv < 11 ;
run;

data data2;
set data2 ;
secu_oublie = (secu_mise = "Non");
passager = (categ_usager = "Passager");
pieton_ = (categ_usager = "Piéton" OR categ_usager = "Piéton en" );
homme = (sexe = "Masculin");
hors_agglo = (Localisation = "Hors agglomération");
heure_nuit = (input(heure, best12.) < 7 ) OR (input(heure, best12.)  > 21);
usag_petit_vehi_presence_pieton = usager_petit_vehicule*presence_pieton;
usag_voiture_presence_petit_vehi = usager_voiture*presence_petit_vehicule;
usager_voiture_presence_pieton = usager_voiture*presence_pieton;
sup_16_age_au_carre = (age > 15)*age*age;
inf_16 = (age < 16);
juillet_aout = (mois = 7) OR (mois = 8);
run;


/* Vérifions qu'il n'y a pas de trop forte corrélation entre les variables explicatives */


PROC CORR data = data2 ;
VAR blesse_ou_tue 
	secu_oublie
	usager_petit_vehicule
	nb_indiv
	passager
	pieton_ 
	homme 
	hors_agglo 	
	heure_nuit
	presence_pieton 
	usag_voiture_presence_petit_vehi
	age;
run ;


/* Résultats */

/* Modèle logistique de base */ 

PROC QLIM DATA = data2 plots = none ;
MODEL blesse_ou_tue = 	secu_oublie
						usager_petit_vehicule
						nb_indiv
						passager
						pieton_ 
						homme 
						hors_agglo 	
						heure_nuit
						presence_pieton 
						usag_voiture_presence_petit_vehi
						age  
						inf_16
						sup_16_age_au_carre /  discrete (d=logit);
RUN;

/* Modèle logistique avec hétéroscédasticité */ 

PROC QLIM DATA = data2 plots = none outest = estimation;
MODEL blesse_ou_tue = 	secu_oublie
						usager_petit_vehicule
						nb_indiv
						passager
						pieton_ 
						homme 
						hors_agglo 	
						heure_nuit
						presence_pieton 
						usag_voiture_presence_petit_vehi
						age  
						inf_16
						sup_16_age_au_carre  /  discrete (d=logit) ;
hetero blesse_ou_tue ~ age sup_16_age_au_carre  /  LINK= EXP NOCONST;
output out=prediction  EXPECTED PREDICTED PROBALL MARGINAL;
RUN;



/* Étude de l'endogénéité */ 

/* Tester la validité des instruments */

/* ATTENTION CETTE COMMANDE PREND BEAUCOUP DE TEMPS A S'EXECUTER, NOUS N'AVONS PAS PU L'EXECUTER */

PROC QLIM DATA = data2 plots = none ;
MODEL blesse_ou_tue = 	secu_oublie
						usager_petit_vehicule
						nb_indiv
						passager
						pieton_ 
						homme 
						hors_agglo 	
						heure_nuit
						presence_pieton 
						usag_voiture_presence_petit_vehi
						age  
						inf_16
						sup_16_age_au_carre / discrete overid(hors_agglo.juillet_aout hors_agglo.nb_tracteur) ;

MODEL hors_agglo = 		secu_oublie
						usager_petit_vehicule
						nb_indiv
						passager
						pieton_ 
						homme  	
						heure_nuit
						presence_pieton 
						usag_voiture_presence_petit_vehi
						age  
						inf_16
						sup_16_age_au_carre
						juillet_aout
						nb_tracteur / discrete ;

test _rho = 0 / LR 
RUN;

/* Effets marginaux */

proc means data = prediction ;
run ;

/* Matrice de confusion */ 

 proc freq data=prediction;
   tables P_blesse_ou_tue*blesse_ou_tue / out=qual nocol norow;
   title "Matrice de confusion";
 run;

/* Matrice de confusion 2 */ 

data prediction;
  set prediction;
  if Prob2_blesse_ou_tue > 0.8 then predit="1";
    else if Prob2_blesse_ou_tue < 0.2 then predit="0";
		else predit = "";
 run;

 proc freq data=prediction;
   tables predit*blesse_ou_tue / out=qual nocol norow;
   title "Matrice de confusion";
 run;
