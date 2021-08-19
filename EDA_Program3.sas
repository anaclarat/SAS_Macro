PROC OPTIONS OPTION = MACRO;
RUN;

/*Auxiliar Information*/

*T-test - Road map:;
DATA TESTT;
INPUT @1 TEXT $CHAR125.;
INFILE DATALINES TRUNCOVER;
DATALINES;
Null hypothesis: There’s no difference in means
Assumptions:
1.Sample distribution must be normal:
e.g:Shapiro (null hypothesis: sample has a normal distribution)
CLT :
    a.If it looks normal and each group have more than 30 observations 
    b.If moderately skewed, each group must have more than 100 observations 
2.Groups are independent of one another. 
3.There are no major outliers.
4.A check for unequal variances will help determine which version of an independent samples t-test is most appropriate: 
(e.g:Levene’s test, null hypothesis: equal variances) 
    a.If variances are equal, then a pooled t-test is appropriate
    b.If variances are unequal, then a Satterthwaite (also known as Welch’s) t-test is appropriate
;
RUN;

DATA TEST_ANOVA;
INPUT @1 TEXT $CHAR125.;
INFILE DATALINES TRUNCOVER;
DATALINES;
One-way ANOVA Assumptions 
In order to run a one-way ANOVA the following assumptions must be met:
                                                                                                                         
1.The response of interest is continuous and normally distributed for each treatment group:
	Normality test: PROC UNIVARIATE NORMAL and QQPlot for each group.
2.Treatment groups are independent of one another. Experimental units only receive one treatment,and they do not overlap.
3.There are no major outliers.
4.A check for unequal variances will help determine which version of a one-way ANOVA is most appropriate 
(Levene’s test, Null hypothesis: variances are equal between groups):
     A .If variances are equal, then the assumptions of a standard one-way ANOVA are met.
     B. If variances are unequal, then a Welch’s one-way ANOVA is appropriate.
;

*Levene’s test:;
DATA LEVENE;
INPUT @1 TEXT $CHAR125. ;
INFILE DATALINES TRUNCOVER;
DATALINES;
Null hypothesis: equal variances 
   a.If variances are equal, then a pooled t-test is appropriate
   b.If variances are unequal, then a Satterthwaite (also known as Welch’s) t-test is appropriate
;
RUN;

DATA SHAPIRO;
INPUT @1 TEXT $CHAR140.;
INFILE DATALINES TRUNCOVER;
DATALINES;
Null hypothesis: sample has a normal distribution
CLT :
    a.If it looks normal and each group have more than 30 observations 
    b.If moderately skewed, each group must have more than 100 observations
	*rule of thumb: If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
	*if the sample size is over 2000, the Kolmgorov test should be used. If the sample size is less than 2000, the Shapiro test is better.
;
RUN;

DATA TESTT1;
INPUT TEXT $160.;
DATALINES;
 "Null hypothesis: There’s no difference in means"
;
RUN;

DATA CHI;
INPUT TEXT $140.;
DATALINES;
Chi – square (Road Map and Assumptions):
If condition of chi-square are satisfied and p-value is less than significant level (5%) reject null hypothesis: 
There is a relationship between variables at the defined significant level.

Null hypothesis: Variables are independents.

1. N, the total frequency, should be reasonably large (greater than 50)
2. The sample observations should be independent. No individual item should be included twice or more in the sample
3. No expected frequencies should be small. Preferably each expected frequency should be larger than 10 but in any case not less than 5.
;
RUN;


DATA CORR;
INPUT TEXT $160.;
DATALINES;
Null hypothesis: there’s no association between variables.
1.Normal distribution for both variables for pearson
2.homoscedasticity assumes that data is equally distributed about the regression line.
3.Linear: 
Linear: pearson
Monotonically related (not normal): spearman kendall hoeffding 
;
RUN;

/*UNIVARIATE*/

/*NUMERICAL*/
%MACRO NUMERICAL(DATA,VAR,CLASS,ALPHA=);

%IF %SUPERQ(ALPHA) NE %STR() %THEN %LET ALPHA= &ALPHA; %ELSE %LET ALPHA = 0.05;

/*This presents a grouped univariate analysis*/
%IF %SUPERQ(CLASS) NE %STR() %THEN %DO;
TITLE "THIS IS UNIVARIATE ANALYSIS FOR &VAR. IN &DATA. BY &CLASS";

%LET N = %SYSFUNC(COUNTW(&VAR));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&VAR,&I);
PROC MEANS  DATA=&DATA N NMISS MEAN MEDIAN MODE MIN MAX STDDEV VAR RANGE Q1 Q3 QRANGE CLM MAXDEC=2 ALPHA = &ALPHA;
CLASS &CLASS;
VAR &VAR;
OUTPUT OUT= OUT_&CLASS._&X. MIN =   MEAN=  STD = MAX = /AUTONAME ;
%END;

PROC SORT DATA=&DATA;
BY &CLASS ;

PROC SGPLOT DATA=&DATA;
VBOX &VAR/ GROUP = &CLASS DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA ;
RUN;

PROC SGPANEL DATA=&DATA;
PANELBY &CLASS / UNISCALE=ROW skipemptycells  ;
HISTOGRAM &VAR /DATASKIN=PRESSED 
					FILLATTRS=(COLOR=PLUM);
RUN;
%END;
/*This presents a sole univariate analysis*/
%ELSE %DO;
TITLE "THIS IS UNIVARIATE ANALYSIS FOR &VAR. IN &DATA";
PROC MEANS DATA=&DATA N NMISS MEAN MEDIAN MODE MIN MAX STDDEV VAR RANGE QRANGE CV CLM MAXDEC=2 ALPHA = &ALPHA;
VAR &VAR;RUN;

 TITLE "THIS IS HISTOGRAM OF &VAR. FOR &DATA";
 PROC SGPLOT DATA=&DATA;
  HISTOGRAM &VAR / FILLATTRS =(COLOR = Plum)dataskin=pressed;
  DENSITY &VAR/ LINEATTRS = (COLOR = black);
  DENSITY &VAR/TYPE = KERNEL LINEATTRS = (COLOR = darkBlue) ;
    STYLEATTRS 
    BACKCOLOR = snow
    WALLCOLOR = WhiteSmoke;
  KEYLEGEND / LOCATION = inside POSITION = topright;
 RUN;
 QUIT;
 TITLE "THIS IS HORIZONTAL BOXPLOT OF &VAR. FOR &DATA";
 PROC SGPLOT DATA=&DATA;
  HBOX &VAR / FILLATTRS=(COLOR = Plum) DATASKIN=Matte MEANATTRS=(SYMBOL = DiamondFilled COLOR = Turquoise);
    STYLEATTRS 
    BACKCOLOR= Snow
    WALLCOLOR= WhiteSmoke
	AXISEXTENT=DATA;
 RUN;
TITLE;
RUN;
%END;
%MEND NUMERICAL ;


/*CATEGORICAL*/

%MACRO CATEGORICAL(DATA,VAR,ALPHA=);
%IF %SUPERQ(ALPHA) NE %STR() %THEN %LET ALPHA= &ALPHA; %ELSE %LET ALPHA = 0.05;

 TITLE "UNIVARIATE ANALYSIS OF &VAR. FOR &DATA";
  PROC FREQ DATA=&DATA NLEVELS ;
  TABLE &VAR/MISSING ALPHA=&ALPHA;
 RUN;

TITLE "BARCHART OF &VAR. FOR &DATA";
PROC SGPLOT DATA = &DATA;
 VBAR &VAR / FILLATTRS=(COLOR = Plum)DATASKIN=pressed;

 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA 
     ;
 RUN;

/*TITLE "PIECHART OF &VAR FOR &DATA";*/
/*PROC GCHART DATA=&DATA;*/
/*  PIE3D &VAR/discrete */
/*             value=INSIDE /*ARROW, OUTSIDE*/*/
/*             percent=OUTSIDE*/
/*             EXPLODE=ALL*/
/*			 SLICE=OUTSIDE*/
/*			 RADIUS=22		*/
;

RUN;
%MEND CATEGORICAL ;

/*BIVARIATE*/


/*CATEGORICAL VS CATEGORICAL*/

%MACRO BIVAR_CAT_CAT(DATA,VAR1,VAR2,ALPHA=);
%IF %SUPERQ(ALPHA) NE %STR() %THEN %LET ALPHA= &ALPHA; %ELSE %LET ALPHA = 0.05;

TITLE J=L "BIVARIATE ANALYSIS OF &VAR1 AND &VAR2 FOR &DATA"; *J=L JUSTIFY LEFT;

proc report data= CHI nowindows noheader nocenter
  style(column) = { background = NONE };
RUN;


PROC SORT DATA=&DATA;
BY &VAR1 &VAR2;
PROC FREQ DATA = &DATA /*ORDER = FREQ*/;
TABLES &VAR1*&VAR2 / PLOTS = (FREQPLOT MOSAICPLOT) CHISQ ALPHA= &ALPHA;
/*TABLES &VAR1*&VAR2/ MISSING  plots=freqplot(type=dotplot);*/
RUN;
TITLE;
TITLE2;
FOOTNOTE1;
%MEND BIVAR_CAT_CAT;

/*CONTINUOUS VS CONTINUOUS*/
/* Correlation*/

%MACRO BIVAR_CONT_CONT(DATA,VAR1,VAR2);

TITLE "BIVARIATE ANALYSIS OF &VAR1 AND &VAR2 FOR &DATA";

proc report data= CORR nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;TITLE2;

TITLE "CORRELATION OF &VAR1 AND &VAR2 FOR &DATA";
PROC CORR DATA = &DATA  PEARSON SPEARMAN  PLOTS= (SCATTER MATRIX(HISTOGRAM));
 VAR &VAR1 &VAR2;
RUN;


%MEND BIVAR_CONT_CONT;



/*CATEGORICAL VS CONTINUOUS*/

%MACRO BIVAR_CAT_CONT(DATA ,CLASS , VAR, ALPHA= );

%IF %SUPERQ(ALPHA) NE %STR() %THEN %LET ALPHA= &ALPHA; %ELSE %LET ALPHA = 0.05;

TITLE "BIVARIATE ANALYSIS OF &CLASS. AND &VAR. FOR &DATA";

PROC SORT DATA = &DATA;
BY &CLASS; RUN;

/*This presents summary for bivariate analysis*/
%LET N = %SYSFUNC(COUNTW(&VAR));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&VAR,&I);
	PROC MEANS DATA = &DATA. N NMISS MIN Q1 MEDIAN MEAN Q3 MAX QRANGE CV CLM MAXDEC=2 ALPHA = &ALPHA ;
	TITLE2 " RELATION BETWEEN &X. AND &CLASS.";
	CLASS &CLASS. ;
	VAR &X.;
	OUTPUT OUT= OUT_&CLASS._&X. MIN =   MEAN=  STD = MAX = /AUTONAME ;
	RUN;
%END;

TITLE;TITLE2; RUN;

/*This presents visual for bivariate analysis*/
PROC SGPLOT DATA=&DATA;
VBOX &VAR/ GROUP = &CLASS DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA ;
RUN;

PROC SGPLOT DATA=&DATA;
HISTOGRAM &VAR /DATASKIN=PRESSED 
				FILLATTRS=(COLOR=Plum);
RUN;

/*This presents test of independency for bivariate analysis*/

/*How many levels has CLASS variable?*/
PROC SQL NOPRINT;
SELECT COUNT(DISTINCT &CLASS.) INTO: LEVELS
FROM &DATA.
QUIT;

*CONDUCT T-TEST IF CAT HAS 2 LEVELS; 
%IF &LEVELS. EQ 2 %THEN %DO;
TITLE "T-test - Road map:";
PROC REPORT DATA = TESTT NOWINDOWS NOHEADER NOCENTER
  STYLE(COLUMN) = { BACKGROUND= NONE }; RUN;
TITLE1;

/*Normality check*/
TITLE "THIS IS FOR NORMALITY CHECK OF &VAR. AND &CLASS";

proc report data= SHAPIRO nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;TITLE2;

PROC UNIVARIATE DATA=&DATA NORMAL ALPHA=&ALPHA;
VAR &VAR;
QQPLOT / NORMAL(MU=EST SIGMA=EST) SQUARE ;
/*HISTOGRAM / NORMAL(COLOR=(RED BLUE) mu= est sigma= est);*/
BY &CLASS;
RUN;

/*Variance check*/
TITLE "THIS IS FOR VARIANCE CHECK OF &VAR. AND &CLASS";
TITLE2 "Levene’s test";

proc report data= LEVENE nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;TITLE2;

PROC GLM data=&DATA ALPHA=&ALPHA;
CLASS &CLASS;
MODEL &VAR = &CLASS;
MEANS &CLASS / hovtest=levene(type=abs) WELCH ALPHA=&ALPHA;
/*LSMEANS &CLASS /PDIFF ADJUST=TUKEY PLOT = MEANPLOT(CONNECT CL) LINES ALPHA=&ALPHA;*/
RUN;
QUIT;

/*TTEST*/

TITLE "THIS IS FOR TTEST OF &VAR. AND &CLASS";
TITLE2 "Levene’s test";
proc report data= TESTT1 nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC TTEST DATA = &DATA /*ALPHA=&ALPHA*/;
VAR &VAR;
CLASS &CLASS;
RUN;
%END;

%ELSE %DO;*CONDUCT ANOVA IF CAT HAS MORE THAN 2 LEVELS;
TITLE "Anova - Roadmap";
PROC REPORT DATA = TEST_ANOVA NOWINDOWS NOHEADER NOCENTER
  STYLE(COLUMN) = { BACKGROUND= NONE }; RUN;
TITLE1;

/*Normality check*/
TITLE "THIS IS FOR NORMALITY CHECK OF &VAR. AND &CLASS";

proc report data= SHAPIRO nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;TITLE2;

PROC UNIVARIATE DATA=&DATA NORMAL ALPHA=&ALPHA;
VAR &VAR;
QQPLOT / NORMAL(MU=EST SIGMA=EST) SQUARE ;
/*HISTOGRAM / NORMAL(COLOR=(RED BLUE) mu= est sigma= est);*/
BY &CLASS;
RUN;

/*Variance check*/
TITLE "THIS IS FOR ANOVA AND VARIANCE CHECK OF &VAR. AND &CLASS";
TITLE2 "Levene’s test";

proc report data= LEVENE nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;TITLE2;

PROC GLM data=&DATA ALPHA=&ALPHA;
CLASS &CLASS;
MODEL &VAR = &CLASS;
MEANS &CLASS / hovtest=levene(type=abs) WELCH ALPHA=&ALPHA;
LSMEANS &CLASS /PDIFF ADJUST=TUKEY PLOT = MEANPLOT(CONNECT CL) LINES ALPHA=&ALPHA;
RUN;
QUIT;
%END;

%MEND BIVAR_CAT_CONT;




****************************************TESTING************************************************************;

DATA TEST;
SET SASHELP.CARS;RUN;

PROC PRINT DATA = TEST (OBS=5);RUN;

DATA TEST1;
SET SASHELP.CLASS;RUN;

PROC PRINT DATA = TEST1 (OBS=5);RUN;

%NUMERICAL(TEST,INVOICE,ALPHA=0.01);
%NUMERICAL(TEST,INVOICE,TYPE);
%NUMERICAL(TEST,INVOICE,TYPE,ALPHA=0.01);

%CATEGORICAL(TEST,TYPE,ALPHA=0.01);

%BIVAR_CAT_CAT(TEST,TYPE,ORIGIN)
%BIVAR_CAT_CAT(TEST,TYPE,ORIGIN,ALPHA=0.01)

%BIVAR_CAT_CONT(TEST,TYPE,INVOICE) *ANOVA;
%BIVAR_CAT_CONT(TEST,TYPE,INVOICE,ALPHA=0.01)*ANOVA;

%BIVAR_CAT_CONT(TEST1,SEX,HEIGHT) *TTEST;
%BIVAR_CAT_CONT(TEST1,SEX,HEIGHT,ALPHA=0.01)*TTEST;

%BIVAR_CONT_CONT(TEST,MPG_Highway,Weight)



