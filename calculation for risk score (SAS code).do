****************************************************************

*  MACRO NAME:   AUC
*
*  VERSION:      rocprg20c3
*
*  FUNCTION:  << CALCULATE VARIOUS GOODNESS OF FIT TYPE MEASURES FOR A
*                PROPORTIONAL HAZARDS REGRESSION MODEL TO PREDICT RISK.
*                OUTPUT INCLUDES EXPECTED VALUE, RATE VALUE, AUC (AREA 
*                UNDER ROC CURVE VALUE), ROC CURVE GRAPH, PLOT OF AUC BY 
*                YEAR, AND PLOT OF PROBABILITY OF EVENT BY DECILE OF RISK.
*                Follow-up time is assumed to be in days units.THE PROGRAM
*                BREAKS TIES IN EVENT TIMES.  A numeric ID variable IDN is
*                assumed. >>
*
*
*  PARAMETERS:   INFILE   = NAME OF SAS DATA SET TO BE USED
*                DEP_TIME = NAME OF THE FOLLOW-UP TIME VARIABLE
*                DEP_CENS = NAME OF THE EVENT INDICATOR VARIABLE
*                             (1 = EVENT OCCURRED, 0 = CENSORED)
*                XVARS    = VARIABLES FOR MODEL
*                DAYS     = ASSUMING FUTIME IN DAYS, # DAYS AT WHICH AUC CALCULATED
*                EXCLUDE  = LIST OF VARIABLES TO EXCLUDE ON IF MISSING.  THE MACRO
*                           DOES NOT ALLOW MISSING VALUES.
*                ATVARS   = LIST OF VARIABLES FOR WHICH VALUE WILL BE HELD CONSTANT
*                           IN CALCULATION OF SCORE FUNCTION
*                ATVALUES = LIST OF CONSTANT VALUES FOR &ATVARS (IN SAME ORDER AS &ATVARS)
*   NOTE: ATVARS must appear in same order as in XVARS, and with same capitalization.
*                FIXEDDAT = IF FIXEDDAT=YES AND ATVARS/ATVALUES ARE SPECIFIED, THEN OUTPUTS
*                           DATASET FROM PHREG CONTAINING XBETA VALUE AND SURVIVAL PROBABILITY
*                           FOR EACH OBSERVATION WITH SPECIFIED VARIABLES FIXED. SINCE THIS 
*                           DATASET IS NOT USED IN THIS MACRO BUT CAN BE USED FOR ADDITIONAL
*                           ANALYSIS, THE DEFAULT IS FIXEDDAT=NO
*                FIRST    = IF THIS MACRO IS RUN MORE THAN ONCE IN THE SAME JOB USING  
*                           THE SAME PHREG MODEL(PERHAPS WITH DIFFERENT ATVARS/ATVALUES), 
*                           THEN SAVE COMPUTING TIME BY SETTING FIRST=YES FOR THE FIRST  
*                           MACRO RUN AND FIRST=NO FOR CONSEQUENT MACRO RUNS WITH THE SAME 
*                           PHREG MODEL (DEFAULT IS FIRST=YES)
*                ROCcurve = IF ROCcurve=YES THEN PLOTS ROC CURVE  (DEFAULT IS RCOcurve=NO)
*                POP_RISK = THE VALUES OF L FOR WHICH YOU WOULD LIKE TO CALCULATE THE
*                           PERCENT OF THE POPULATION HAVING &DAYS RISK >= L
*                           (CAN SPECIFY UP TO 3 VALUES FOR L [SEE EXAMPLE BELOW]) 
*                           (ROCcurve=YES MUST BE SPECIFIED FOR POP_RISK TO BE COMPUTED)
*                AUC_year = IF AUC_year=YES THEN PLOTS AUC*YEAR  (DEFAULT IS AUC_year=NO)
*                decilePL = IF decilePL=YES THEN PLOTS PROB(EVENT) BY DECILE OF RISK  
*                           (DEFAULT IS decilePL=NO)
*		         GRAPHDIR = DIRECTORY IN WHICH TO SAVE AN HTML FILE OF REQUESTED GRAPHS
*                           (LEAVE BLANK IF NO GRAPHS REQUESTED OR DO NOT WANT TO USE ODS)
*		         GRAPHFIL = NAME OF HTML FILE IN WHICH TO SAVE GRAPHS (FILE WILL BE CREATED
*                           IN &GRAPHDIR) (LEAVE BLANK IF NO GRAPHS REQUESTED OR DO NOT 
*                           WANT TO USE ODS)
*                PRINT    = IF PRINT=YES THEN OUTPUTS EVTDAT AND OBSEXP MATRICES (SEE CODE)
*                           (DEFAULT IS PRINT=NO)
*                TITLE1   = TO OVERRIDE THE DEFAULT, WHICH IS:
*                           title1 " Model:&DEP_TIME*&DEP_CENS = &XVARS",
*							SPECIFY TITLE1 = "DESIRED FIRST TITLE"
*                method2  = 1 to include estimates of auc, sens, spec from method 2, 1 to use
*                             method 1 only.  Default is method 1 only  
*
*  EXAMPLE 1: this example uses all macro capabilities 
*	%AUC(INFILE   = Females, 
*        DEP_TIME = ft99isc,
*        DEP_CENS = in99isc,
*        XVARS    = race v1age01 SBPA21 HYPTMD01 DIABTS03 CURSMK01 PRVCHD05 clvh,
*        DAYS     = 3650,
*        EXCLUDE  = bmi01 lipa08 lipa06 lipa07 chma13 hema09 hema11 hmta03 
*                   sprt_i02 fevo_e ecgma31 keys packyr hema07 hema17 
*                   chma09 wsthpr01 ethanl03 p_tfat chma06 lipa01 hdl01,
*        ATVARS   = race v1age01,
*        ATVALUES = .74 55,
*        FIRST    = YES,
*        ROCcurve = YES,
*        POP_RISK = 0.05 0.1,
*        AUC_year = YES,
*        decilePL = YES,
*		 GRAPHDIR ='c:\maryjo',
*		 GRAPHFIL ='Test Graphs.html',
*        PRINT    = YES,
*        TITLE1   = 'Females, Model:ft99isc*in99isc = basic risk factors');
*
*  EXAMPLE 2: this example accepts all defaults, does not fix values, and has already
*             performed data step exclusions for &XVARS 
*	%AUC(INFILE   = Females, 
*        DEP_TIME = ft99isc,
*        DEP_CENS = in99isc,
*        XVARS    = race v1age01 SBPA21 HYPTMD01 DIABTS03 CURSMK01 PRVCHD05 clvh,
*        DAYS     = 3650);
*

*
*************************************************************************;

%MACRO AUC(INFILE=,
           DEP_TIME=,
           DEP_CENS=,
		   XVARS=,
           DAYS=,
           EXCLUDE=,
           ATVARS=,
           ATVALUES=,
		   FIXEDDAT=NO,
           FIRST=YES,
           ROCcurve=NO,
		   POP_RISK=,
           AUC_year=NO,
           decilePL=NO,
		   GRAPHDIR=,
		   GRAPHFIL=,
           PRINT=NO,
           TITLE1=,
method2=);

*options nomprint;

title1 &TITLE1;
%if %length(&TITLE1)=0 %then %do;
title1 " Model:&DEP_TIME*&DEP_CENS = &XVARS";
%end;

/**--------------------------------------------------------------------------**/

%if %upcase(&FIRST)=YES %then %do;

    data anal;
      set &infile;
    %if %length(&exclude)>0 %then %do;
          if nmiss(of &exclude)>=1 then delete;
    %end;
    run;
    
    *break ties in futime(dep_time) for events(dep_cens=1);
    data anal0 anal1;
      set anal;
      if &dep_cens=1 then output anal1;
      if &dep_cens=0 then output anal0;
    run;
    
    *when a futime in anal1 has more than one event, change futime for all but first;
    proc sort data=anal1;
      by &dep_time;
    run;
    
    data anal1;
      set anal1;
      by &dep_time;
      retain m 1 fut 0;
      if first.&dep_time then do;
         m=1;
         fut=&dep_time;
      end;
      if first.&dep_time=0 then do;
         &dep_time=fut+(0.5**m);
         fut=&dep_time;
         m=m+1;
      end;
    run;
    
    data anal;
      set anal1 anal0;
    run;
    
    proc sort data=anal;
      by id;
    run;
    
     /*run phreg and output score (xbeta) and survival for observed futime*/
    /* KEEP */
    
    PROC PHREG DATA=anal outest=betas
         %if %upcase(&PRINT)=NO %then %do;
             /*noprint*/
         %end;;
      MODEL &DEP_TIME*&DEP_CENS(0)= &xvarS/RL COVB;
      ID IDN;
      output OUT=observed SURVIVAL=SURVobs xbeta=xbetaobs;
    run;
    

    ***Go into IML to compute AUC and SENS and SPEC at various t;
    data foriml;
      set observed ;
      run;
 
      proc means data=foriml n noprint;
      var xbetaobs;
      output out=ncount n=n;
      run;
    
      data foriml;
      set foriml;
      if _n_=1 then set ncount(keep=n);
      run;
    
    *Now sort by futime*descendingevent;
    proc sort data=foriml;
      by &DEP_TIME descending &DEP_CENS;
    run;

    * Checking to see if dataset has enough observations for decile calculations (need 100). *;
    data _null_;
      set foriml end=eof;
      if eof and _n_ < 100 then do;
         put 'WARNING: After exclusions, you have fewer than 100 observations.  Your dataset is not';
         put 'of adequate size.  This will cause errors to occur later in the run of this macro.';
      end;
    run;

    * Checking to see if desired follow-up time falls within the proper range. *;
	proc means data=foriml noprint;
      where &DEP_CENS=1;
      var &DEP_TIME;
      output out=checkfut max=maxobsfut;
    run;

    data _null_;
      set checkfut;
      if maxobsfut < &DAYS then do;
         put 'WARNING: You have chosen a follow up time greater than the maximum observed';
         put 'follow-up time to event.  Please set &DAYS <= ' maxobsfut 6.0 '.  This will cause';
         put 'errors to occur later in the run of this macro.';
      end;
    run;
    
    * Checking to if there are missing values for any of the &XVARS (PHREG predictors). *;
    proc freq data=foriml noprint;
      where xbetaobs = .;
      table xbetaobs / out=freq; 
    run;

    data _null_;
      set freq;
      if count > 0 then do;
         put 'WARNING: Missing values exist for one or more of your predictor variables';
         put '(&XVARS).  Add these variables to the &EXCLUDE list.  This will cause';
         put 'errors to occur later in the run of this macro.';
      end;
    run;

%end; * %upcase(&FIRST)=YES ;

/**--------------------------------------------------------------------------**/

title2 "All variables included in risk score";
%if %length(&ATVARS)>0 %then %do;
title2 "Variables fixed in risk score: &ATVARS (values: &ATVALUES)";
%end;

%if %length(&ATVARS)>0 %then %do;
    * Count the number of variables listed in &XVARS, then set number = &X_NUM. *;
    * Count the number of variables listed in &ATVARS, then set number = &AT_NUM. *;
    %MACRO COUNT(COUNTN,COUNTVAR);
               %GLOBAL &COUNTN._NUM;
               %IF %LENGTH(&COUNTVAR)>0 %THEN %DO;
                 %LET COUNT1=1;
                 %DO %UNTIL(%SCAN(&COUNTVAR,&COUNT1)=);
                     %LET COUNT1=%EVAL(&COUNT1 + 1);
                 %END;
                 %LET &COUNTN._NUM = %EVAL(&COUNT1-1);
                 %PUT &&&COUNTN._NUM;
               %END;
               %ELSE %DO;
                 %LET &COUNTN._NUM = 0;
               %END;
    %MEND;

    %COUNT(X,&XVARS);
    %COUNT(AT,&ATVARS);

    * Create a list of order of atvalues in the x variables list for order matrix. *;
    %macro orderval;
    %do i = 1 %to &X_NUM;
       %do j = 1 %to &AT_NUM;
           %let atvar&j = %scan(&atvars, &j, ' ');
           %let holder="%scan(&xvars, &i, ' ')";
           %if &holder="&&atvar&j" %then &i;
       %end;
    %end;
    %mend orderval;
    %let order = %orderval;
%end; * %length(&ATVARS)>0;

/**--------------------------------------------------------------------------**/

proc iml;

use foriml;
read all var {idn  xbetaobs &DEP_TIME &DEP_CENS survobs} into dat;
close foriml;

* create baseline survival function (not K-M survival functions
  to be created later) for each follow-up time;
s0=dat[,5]##exp(-dat[,2]);

/**--------------------------------------------------------------------------**/

%if %length(&ATVARS)>0 %then %do;
    *calculate auc with certain variables in the model held constant in the risk score;
    *first calculate the risk score holding certain variables constant;
    xbetaobs=dat[,2];  
    xbetanew=xbetaobs;
    use betas;
    read all var{&xvars} into beta;
    close betas;

    use foriml;
    read all var{&xvars} into xvar;
    close foriml;

    *row vector of values desired to hold constant;
    ones=xbetaobs - xbetaobs + 1; * column of ones *;
    xvarcons = ones*{&atvalues}; * so that xvarcons has as many rows as xbetaobs *;

    *order=order of variable in the xvar list to be held constant, in same order as xvarcons;
    order={&order};

    xbetanew=xbetaobs + (xvarcons - xvar[,order])*beta[,order]`;*this replaces the contribution 
    to xbetaobs by the variables to be held constant by suitable constants;

	** now substitute xbetanew for dat[,2] ;
    datnew = dat[,1] || xbetanew || dat[,3] || dat[,4] || dat[,5];
    dat = datnew;

    %if &FIXEDDAT=YES %then %do;
      * Create dataset called datnew containing: numeric ID, xbeta with variables
        fixed as requested, time and censoring variables, and survival estimates.;
        name={"idn"  "xbetanew" "&DEP_TIME" "&DEP_CENS" "survobs"};
        create datnew from datnew[colname=name];
        append from datnew;
        close datnew;
    %end;
%end;

/**--------------------------------------------------------------------------**/

* Calculate deciles of risk (of xbetaobs, or xbetanew if that is being used);

* We want to sort dat by dat[,2]. ;
* First concatenate a counter to keep track of the original sort. ; 
n = nrow(dat);  *number of persons in dataset;
cnter = J(n,1,0);

do cntJ = 1 to n;
   cnter[cntJ,] = cntJ;
end;

dat = dat || cnter;

dat2 = dat[,2];
dattmp = dat;
dat[rank(dat2),] = dattmp;

* Now concatenate a Decile variable ;
decile = J(n,1,0);
do cnti = 1 to n;
   do dec = 1 to 10;
      if round((dec - 1)*(n/10) + 1) <= cnti 
         & cnti <= round(dec*(n/10)) then decile[cnti,] = dec;
   end;
end;

dat = dat || decile;

* Now re-sort dat as it was;
dat6 = dat[,6];
dattmp = dat;
dat[rank(dat6),] = dattmp;

* Now drop the counter column;
dat = dat[,1] || dat[,2] || dat[,3] || dat[,4] || dat[,5] || dat[,7];

/**--------------------------------------------------------------------------**/

*set up some placeholder matrices;
r=j(n,1,n);
s=j(n,1,1);
lam=j(n,1,0);
tau=lam;
gam=lam;
auc=lam;

lam[1,]=dat[1,4]/r[1,];*hazard rate;
s[1,]=(1-lam[1,]);* Kaplan-Meier survival function;
A1: do i=1 to n;
 H1:if i>1 then do;
  r[i,]=r[i-1,]-1;/* number at risk. Note that at tied futime the event, if one, is last, so
  the persons censored at the same time as an event don't get removed until after the event */
  lam[i,]=dat[i,4]/r[i,];
  s[i,]=s[i-1,]*(1-lam[i,]);
 H2:end;
 B1: if dat[i,4]=1 then do;
  tautmp=0;
  i_1=0;
  I1:if i>1 then do;
   C1: do j=1 to i-1;
    tautmp=tautmp+(dat[j,2]>dat[i,2])*dat[j,4];
    i_1=i_1+dat[j,4];
   C2: end;
   if i_1>0 then tau[i,]=tautmp/i_1;*Note that tau remains at 0 until 2nd event;
  I2:end;
  gamtmp=0;
  D1:do jj=i+1 to n;
   gamtmp=gamtmp+(dat[i,2]>dat[jj,2]);
  D2:end;
  gam[i,]=gamtmp/(r[i,]-1);
  auctmp=0;
  if dat[1,4]=1 then auctmp=auctmp+gam[1,]*lam[1,]*(1-lam[1,])*(1**2)
      -tau[1,]*lam[1,]*1*(1-(1));
  E1:do kk=2 to i;
   if dat[kk,4]=1 then auctmp=auctmp+gam[kk,]*lam[kk,]*(1-lam[kk,])*(s[kk-1,]**2)
      -tau[kk,]*lam[kk,]*s[kk-1,]*(1-(s[kk-1,]));
  E2:end;
  auc[i,]=auctmp/(s[i,]*(1-s[i,]));*area under roc curve;
 B2:end;
A2:end;
*select auc at one time point, &days;
xx=0;yy=0;zz=0;
F1:do ii=1 to n;
 FF1:if dat[ii,4]=1 & dat[ii,3] <= &days then do;
  yy=auc[ii,];
  zz=dat[ii,3];
 FF2:end;
 G1:if dat[ii,3]>&days & dat[ii,4]=1 & xx=0 then do;
  auc_t=yy;
  maxfu=zz;*max futime <= &days;
  xx=1;
 G2:end;
F2: end;

/**--------------------------------------------------------------------------**/

 dat=dat||s0;
 alldat=dat[,2:3]|| r||lam || gam||tau||s||auc||s0; 
               * alldat[,1] = xbetaobs unless replaced by xbetanew, alldat[,2] = &FUTIME;
 tmp=loc(dat[,4]=1); *location of events;
 evtdat=alldat[tmp,];*auc at all time points of events;
 nevt=nrow(evtdat);*number of events;

 **Get s0 at maxfu;
 tmpxx=loc(evtdat[,2]=maxfu);
 s0maxfu=evtdat[tmpxx,9];
%if %upcase(&PRINT)=YES %then %do;
  print "survival function at max futime <= &days and all covariates zero" s0maxfu;
%end;

*Add 1 - lam0 to evtdat, where lam0=baseline hazard, NOT the K-M hazard lam;
one_lam0=j(nevt,1,0);
s0_1=1;
SS1:do bi=1 to nevt;
  one_lam0[bi,] = evtdat[bi,9]/s0_1; * S(t+1) = S(t)*(1 - lam(t+1));
  s0_1=evtdat[bi,9];
SS1:end;
evtdat=evtdat||one_lam0; tmpdat=evtdat;
locltday=loc(evtdat[,2]<=1.2*&days);
evtdatp=tmpdat[locltday,];*restricting for printing to not too far out in time;
name={"x" "fut" "risk" "lam" "gam" "tau" "s" "auc" "s0" "one_lam0"};

%if %upcase(&PRINT)=YES %then %do;
  print evtdatp[colname=name];
%end;

 tmpa=evtdat[,2];
obsmaxfu=loc(tmpa=maxfu);*event with max fu <=&days;

%if %upcase(&PRINT)=YES %then %do;
  print auc_t maxfu obsmaxfu;*auc at the chosen time;
%end;

 create evtdat from evtdat[colname=name];
 append from evtdat;
 close evtdat;

/**--------------------------------------------------------------------------**/

%if %upcase(&ROCcurve)=YES %then %do;

    /* For some runs we'll want to calculate sensitivity and specificity, to plot the ROC curve.
       Again, there is a curve for each t, so a specific t=&days will need to be specified */
    sens=j(99,1,0);*placeholders for sens and spec at each K,at time=&days;
    spec=sens;
    Kvec=sens;*placeholder for K values;
    PPV=sens;
	NPV=sens;
	pvec=sens;
    SKtm=j(99,1,0);

    * Sort dat[,2], get percentiles, and put into Kvec. ;
    dat2 = dat[,2];
    b = dat2;
    dat2[rank(dat2),] = b;

    * Append an order vector ;
	ordv = J(n,1,0);
	do cutk = 1 to n;
	   ordv[cutk,] = (cutk/n)*100;
	end;
	dat2_ord = dat2 || ordv;

	* Calculate the % of population with &days risk >=L ;
    %if %length(&POP_RISK)>0 %then %do;
      %do Lcount = 1 %to 3;
        %let L = %scan(&POP_RISK,&Lcount,' ');
        %if %length(&L)>0 %then %do;
            xbetaL = log((log(1-&L)) / (log(s0maxfu)));
            locxbeta=loc(dat2_ord[,1]<=xbetaL);
            JJJJ = ncol(locxbeta);
            prop = 1 - ((dat2_ord[JJJJ,2])/100);
            prop&Lcount = &L || prop;
        %end;
      %end;
      prop = prop1 // prop2 // prop3;
      print prop[colname={"L"  "        % risk >= L"}];
    %end;

	* Keep percentiles of dat2;
	KVEC = J(99,1,0);
	pctk = 1;
	do cutkk = 1 to n-1;
	   if dat2_ord[cutkk,2] <= pctk & dat2_ord[cutkk+1,2] > pctk then do;
	      Kvec[pctk,] = dat2_ord[cutkk,1];
		  pctk = pctk + 1;
		  if pctk = 100 then stop;
	   end;
	end;

     J1: do uu=1 to 99 ;
     sensK=j(nevt,1,0);*placeholder for  sens and spec at each evttime, given K;
     specK=sensK;
     K=Kvec[uu,];
    
     p_xltk=0;*Define P(Xi<=K);
     K1: do vv=1 to n;
      p_xltk=p_xltk+(dat[vv,2]<=K); *dat[,2] is xbetaobs unless replaced by xbetanew;
     K2: end;
     p_xltk=p_xltk/n;;
    
     L1: do iii=1 to nevt;
      senstmp=0;
      spectmp=0;
       M1: do jjj=1 to iii;
        rhotK=(evtdat[jjj,1]>K);
        N1: if jjj>1 then do;
        senstmp=senstmp+rhotk*evtdat[jjj,4]*evtdat[jjj-1,7]; * rho*lam*S(lagged) ;
        spectmp=spectmp+(1-rhotk)*evtdat[jjj,4]*evtdat[jjj-1,7];
        N2: end;
         O1: if jjj=1 then do;
          senstmp=senstmp+rhotk*evtdat[jjj,4]*1;
          spectmp=spectmp+(1-rhotk)*evtdat[jjj,4]*1;
         O2:end;
       M2: end;
      sensK[iii,]=senstmp/(1-evtdat[iii,7]);
      specK[iii,]=(p_xltk - spectmp)/evtdat[iii,7];
     L2: end;
     *select sens, spec at one time point, 3650;
     sens[uu,]=sensK[obsmaxfu,];
     spec[uu,]=specK[obsmaxfu,];
	 www=evtdat[obsmaxfu,7]; *S(tm);
     if p_xltk = 0 | p_xltk = 1 then do;
	    PPV[uu,]=-99.99;
	    NPV[uu,]=-99.99;
	 end;
     if p_xltk > 0 & p_xltk < 1 then do;
        PPV[uu,] = (sens[uu,]*(1-www)) / (1-p_xltk);  /* positive predicted value */
        NPV[uu,] = (spec[uu,]*(www)) / (p_xltk);      /* negative predicted value */
     end;

	 pvec[uu,]=p_xltk;
	 SKtm[uu,]=evtdat[obsmaxfu,9]**exp(K); *s0**exp(K) is baseline survival at tm
	 raised to exp(K) for score=K, so is survival at tm for score=K;

    J2: end;

    *create data for plotting roc curve at &days;
    one_spec=j(99,1,1)-spec;
    pevt=j(99,1,1)-SKtm;

	*create precentile name vector;
	pctnam=J(99,1,0);
	do namj = 1 to 99;
	  pctnam[namj,]=namj;
	end;

    datsum1=pctnam||Kvec||pvec||sens||spec||one_spec||PPV||NPV||SKtm||pevt;
    coln1 ={"PCTNAM" "K" "P(Zi<=K)" "SENS" "SPEC" "ONE_SPEC" "PPV" "NPV" "S(K,tm)" "Prob(Evt)"};
    coln1a={"PCTNAM" "K" "P_Zi_leK" "SENS" "SPEC" "ONE_SPEC" "PPV" "NPV" "SKtm" "pevt"};
    
    %if %upcase(&PRINT)=YES %then %do;
      print datsum1[colname=coln1];
    %end;

    create plot1 from datsum1[colname=coln1a];
    append from datsum1;
    close plot1;

%end;
        
/**--------------------------------------------------------------------------**/
    
%if %upcase(&AUC_year)=YES %then %do;

    *create data for plotting auc curve vs &days or years;
    datsum2=evtdat[,{2 8}];
    coln2={"fudays" "auc" };
    create plot2 from datsum2[colname=coln2];
    append from datsum2;
    close plot2;

%end;
    
/**--------------------------------------------------------------------------**/

*compute expected and observed number of events and 
   prob of event within &days (usually 10*365) by decile of risk;
expected=j(10,1,0);
observed=expected;
probevt=expected;
ndec=expected;
dec=expected;
h_l=expected;
SS3:do ci=1 to 10;
dec[ci,]=ci;
expectci= 0;observci=0;probevci=0;ndecci=0;
 ss5:do ei=1 to nevt;
  SS6:do fi=1 to n;
   SS7:if dat[fi,3]>=evtdat[ei,2] &  dat[fi,6]=ci then do;
    if ei>1 then
    expectci=expectci + 1 - (evtdat[ei,9]/evtdat[ei - 1,9])**(exp(dat[fi,2]));
    ss8:if ei=1 then do;
     expectci=expectci + 1 - (evtdat[ei,9]/1)**(exp(dat[fi,2]));
     observci=observci + dat[fi,4];
	SS8:end;
   ss7:end;
   ss9:if ei=1 then do;
    ss10:if dat[fi,6]=ci then do;
     probevci=probevci + (1 - evtdat[obsmaxfu,9]**exp(dat[fi,2]));
	 ndecci=ndecci+1;
	ss10:end;
   ss9:end;
  ss6:end;
 ss5:end;
expected[ci,]=expectci;
observed[ci,]=observci;
ndec[ci,]=ndecci;
probevt[ci,]=probevci/ndecci;
H_L[ci,]=ndecci*(observci-expectci)*(observci-expectci)/(expectci*(ndecci-expectci));
SS3:end;
*onehl=j(10,1,1);
*h_l=onehl`*H_L_I;
obsexp=dec||ndec||observed||expected||probevt||h_l;

obsexnam={"decile" "n" "observed" "expected" "probevt" "h_l"};

create obsexp from obsexp[colname=obsexnam];
append from obsexp;
close obsexp;

quit;


%if %upcase(&PRINT)=YES %then %do;
  /* Calculating PAR to add to expected/observed printout */
  
  data new;
    set obsexp(keep=decile n probevt rename=(n=np));
    sump = np * probevt;
    drop probevt;
  
    retain sp nn sumpltk nltk 0;
    if decile > 1 then do;
    sumpltk = sumpltk + sp;
    nltk = nltk + nn;
    end;
    nn = np;
    sp = sump;
  run;

  proc sort data=new;
    by descending decile;
  run;

  data new;
    set new;
    retain sumpgek ngek 0;
    sumpgek = sumpgek + sump;
    ngek = ngek + np;
  run;

  proc sort data=new;
    by decile;
  run;

  data new(drop=np sp nn);
    set new;
    if decile>1 then do;
       rr = (sumpgek/ngek)/(sumpltk/nltk);
       p = (11-decile)/10;
       par = 100*p*(rr-1)/(p*(rr-1)+1);
    end;
  run;

  data obsexp;
    merge obsexp new;
    by decile;
  run;

  proc print data=obsexp noobs;
  sum h_l observed expected;
  run;
%end;

PROC APPEND BASE=deciles DATA=obsexp force;
RUN;

/***************** Optional Plots *****************/

%if (%upcase(&ROCcurve)=YES|%upcase(&AUC_year)=YES|%upcase(&decilePL)=YES) %then %do;
    goptions reset=global gunit=pct border cback=white
             colors=(black blue green red)
             ftitle=swissb ftext=swiss htitle=6 htext=4; 
    		 symbol1 color=green width=2 value=triangle height=3;
    *to connect points: symbol1 color=green interpol=spline width=2 value=triangle height=3;
    		 symbol2 color=blue width=2 value=circle height=3;
		 symbol3 color=red width=2 value=square height=3;
    axis1 label=('Prob(Event)' justify=right) width=3;
    legend1 label=NONE value=(tick=1);
%end;

%if (%length(&graphdir)>0) and (%length(&graphfil)>0) %then %do;
    filename odsout &graphdir;
    goptions device=gif;
    ods html body=&graphfil path=odsout;
%end;


%if %upcase(&ROCcurve)=YES %then %do;
 /* Creating cumulative datasets for the ROC curve, in case you want to overlay
    plots from different runs of the macro. */
    data plot1;
       set plot1;
       length fixedvar $25;
       dataname = "&infile";
       fixedvar = 'none';
    %if %length(&ATVARS)>0 %then %do;
       fixedvar = "&ATVARS=&ATVALUES";
    %end;
    run;

    PROC APPEND BASE=ROCcurveset DATA=plot1 force;
    RUN;

    title1 &TITLE1;
    %if %length(&TITLE1)=0 %then %do;
      title1 "&INFILE, Model:&DEP_TIME*&DEP_CENS = &XVARS";
    %end;
    title2 "All variables included in risk score";
    %if %length(&ATVARS)>0 %then %do;
      title2 "Variables fixed in risk score: &ATVARS (values: &ATVALUES)";
    %end;
    
    title3 'ROC Curve';
    title4 "for &INFILE";

    symbol1 interpol=join value=none color=black;
    proc gplot data=plot1;
    plot sens*one_spec;
	label sens='Sensitivity' one_spec='1 - Specificity';
    run;
    quit;

%end;

    
%if %upcase(&AUC_year)=YES %then %do;
 /* Creating cumulative datasets for the AUC*year plot, in case you want to overlay
    plots from different runs of the macro. */
    data plot2;
       set plot2(keep=auc fudays);
       length fixedvar $25;
       year=fudays/365;
	   drop fudays;
       dataname = "&infile";
       fixedvar = 'none';
    %if %length(&ATVARS)>0 %then %do;
       fixedvar = "&ATVARS=&ATVALUES";
    %end;
    run;

    PROC APPEND BASE=AUC_yearset DATA=plot2 force;
    RUN;

    title1 &TITLE1;
    %if %length(&TITLE1)=0 %then %do;
      title1 " Model:&DEP_TIME*&DEP_CENS = &XVARS";
    %end;
    title2 "All variables included in risk score";
    %if %length(&ATVARS)>0 %then %do;
      title2 "Variables fixed in risk score: &ATVARS (values: &ATVALUES)";
    %end;
    
    title3 'Area Under the ROC Curve, by Year';
    title4 "for &INFILE";

    symbol1 interpol=join value=none color=black;
    proc gplot data=plot2;
    plot auc*year/
     haxis= 1 to 12 by 1
     vaxis=0.5 to 1 by 0.05 ;
	 label auc='AUC' year='Year';
    run;
    quit;

%end;


%if %upcase(&decilePL)=YES %then %do;
 /* Creating cumulative datasets for the observed/expected plot, in case you want to overlay
    plots from different runs of the macro. */
    data obsexp;
       set obsexp;
       length xvar dataname $12 fixedvar $25;
       xvar = "%scan(&xvars,9,' ')";  ** may need to be changed **;
       dataname = "&infile";
       fixedvar = 'none';
    %if %length(&ATVARS)>0 %then %do;
       fixedvar = "&ATVARS=&ATVALUES";
    %end;
    run;

    PROC APPEND BASE=decilePLset DATA=obsexp force;
    RUN;

    title1 &TITLE1;
    %if %length(&TITLE1)=0 %then %do;
      title1 " Model:&DEP_TIME*&DEP_CENS = &XVARS";
    %end;
    title2 "All variables included in risk score";
    %if %length(&ATVARS)>0 %then %do;
      title2 "Variables fixed in risk score: &ATVARS (values: &ATVALUES)";
    %end;
    
    title3 'Predicted Probability of Event in 10 Years by Decile of Risk';
    title4 "for &INFILE";
    proc gplot data=obsexp;
      plot probevt*decile/legend=legend1 vaxis=axis1;
      /* FOOTNOTE IS MESSED UP */
      *footnote1 h=3 j=l "Model: ft99isc*in99isc = race v1age01 &basic";
      label decile='Decile of Risk';
    run;
    quit;
    
    title3;
%end;

%if (%length(&graphdir)>0) and (%length(&graphfil)>0) %then %do;
    ods html close;
%end;
    
/***************** End Optional Plots *****************/


data temp;
   set evtdat;
   if fut < &DAYS;
run;

data temp;
   set temp end=eof;
   length xvar $30 dataname $8 fixedvar $25;
   xvar = "%scan(&xvars,9,' ')";  ** may need to be changed **;
   dataname = "&infile";
   fixedvar = 'none';
%if %length(&ATVARS)>0 %then %do;
   fixedvar = "&ATVARS=&ATVALUES";
%end;
   if eof then output;
run;

proc print data=temp noobs label;
  var dataname fut auc
      %if %length(&ATVARS)>0 %then %do;
         fixedvar;
      %end;;
  label dataname='Dataset' fut='Selected Follow-Up Time' auc='AUC (Area Under ROC Curve)'
        %if %length(&ATVARS)>0 %then %do;
           fixedvar='Variables Held Fixed';
        %end;;
  *title3 '(see work.BASE dataset for full details)';
run;

PROC APPEND BASE=BASE DATA=temp force;
RUN;

%if &method2=1 %then %do;
  %auc2
 %end;

%MEND AUC;

%MACRO AUC2;		 


title1 " Model:&DEP_TIME*&DEP_CENS = &VARS";
title2 "method2";
	
PROC PHREG DATA=&INFILE outest=myauc_betas noprint;
	MODEL &DEP_TIME*&DEP_CENS(0)= &VARS /RL COVB;
	baseline out=myauc_base covariates=&INFILE survival=surv;
	output OUT=observed SURVIVAL=SURVobs xbeta=xbetaobs;
RUN;

proc iml;
	use myauc_base;
	read all into base;
	close myauc_base;

	use observed;
	read all into observed;
	close observed;
	n=nrow(observed); ncolobs = ncol(observed);

	ncolbase = ncol(base);
	T_LEQ10=loc(base[,ncolbase-1]<=&DAYS); base01=base[T_LEQ10,];
	T_10=loc(base01[,ncolbase-1]=max(base01[,ncolbase-1])); base02=base01[T_10,];
	base02=base02[1:n,];
	X=observed[,ncolobs-1]; surv=base02[,ncolbase];
	E_S=sum(surv)/n;

	numerator=0;
	do i=1 to n-1;
		do j=i+1 to n;
			if (X[i]>X[j]) then numerator = numerator+surv[j]*(1-surv[i]);
			if (X[i]<X[j]) then numerator = numerator+surv[i]*(1-surv[j]);
		end;
	end;

	myauc=(numerator/(n*(n-1)))/(E_S*(1-E_S));

	sens = J(n,1,0);
	spec = J(n,1,0);
	thresh = J(n,1,0);
	thresh[rank(X)] = X;

	do i=1 to n;
		do j=1 to n;
			sens[i] = sens[i] + (1-surv[j])*(X[j]>thresh[i]);
			spec[i] = spec[i] + surv[j]*(X[j]<=thresh[i]);
		end;
		sens[i] = sens[i]/(n*(1-E_S));
		spec[i] = spec[i]/(n*E_S);
	end;
	
	print "AUC at &DAYS";
	print myauc;
	
	create OUTROC  var{sens spec};
	append;
	close OUTROC;

	create OUTAUC var{myauc};
	append;
	close OUTAUC;
quit;

data OUTROC;
	set OUTROC;
	ONE_SPEC = 1 - SPEC;
run;


goptions reset=global gunit=pct border cback=white device=win
             colors=(black blue green red)
             ftitle=swissb ftext=swiss htitle=6 htext=4; 
    		 symbol1 color=green width=2 value=triangle height=3;
    *to connect points: symbol1 color=green interpol=spline width=2 value=triangle height=3;
    		 symbol2 color=blue width=2 value=circle height=3;
proc gplot data=OUTROC;
	plot SENS*ONE_SPEC;
	label sens='Sensitivity' one_spec='1 - Specificity';
run;
quit;
%MEND AUC2;
