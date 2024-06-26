********************************************************************************
********************************************************************************

* Research note: Family structure and attitudes toward filial obligations 
* among younger and middle-aged adults

* Author: Hans Hämäläinen (email: hans.hamalainen@utu.fi)
* Date: 10 June 2024

********************************************************************************
********************************************************************************

/* 

Note: If any part of this code is used, please cite the following: 

Hämäläinen, H., Tanskanen, A. O., Kääriäinen, J., & Danielsbacka, M. (2024).
Research note: Family structure and attitudes toward filial obligations 
among younger and middle-aged adults. Journal of Family Research, 36, 178–191. 
doi: https://doi.org/10.20377/jfr-972


This study uses data from the German Family Panel 
(Pairfam, v13.0; doi: https://doi.org/10.4232/pairfam.5678.13.0.0). 
The data can be accessed through GESIS: 
https://search.gesis.org/research_data/ZA5678.
*/

********************************************************************************
********************************** setup ***************************************
********************************************************************************

clear all
set more off		
set maxvar 10000	

* Pairfam data files (v13.0)
global inpath `"" /file path ""'

* Use anchor data from wave 2 of Pairfam:
cd $inpath
use anchor2, clear	

* labels to english
lab language en

********************************************************************************
**************************** data manipulation *********************************
********************************************************************************

// attitude variables

	recode val2i1 val2i5 (-2 -1 = .), gen(at1 at2)
	label define at_scale 1 "Disagree completely" 5 "Agree completely"
	label values at1 at2 at_scale 
	label variable at1 "Children should accommodate their parents if they cannot take care for themselves"
	label variable at2 "Children should arrange their work so as to be able to care for their sick parents"

// Family structure (i.e. parents' composition) -variable

	* original parent-child relation variables:
		* hm1		"mother living"
		* hv1		"father living"
		* he1		"Biological parents are couple"
		* he2		"Biological parents cohabitating"
		* hsv1		"Stepfather exists" 
		* hsm1		"Stepmother exists" 
		* contact with parent mentioned 
			* hm2 (mother)
			* hsv2 (step-father)
			* hv2(father)
			* hsm2 (step-mother)

	* no contact with any (step)parent
		recode hm2 hsv2 hv2 hsm2 (.=0)
		egen par_nocont = rowmax(hm2 hsv2 hv2 hsm2)
		recode par_nocont (0 = 1) (1 = 0) 

	* biological parents couple / not couple
		recode he1 (. = 2), gen(he1_new) 
		gen bio_couple = he1_new == 1
		gen bio_not_couple = he1_new == 2
		recode bio_not_couple (1 = 10)
		
	* a step-parent exists
		recode hsv1 hsm1 (2=0) (.=0), gen(stepf stepm)
		egen step_exist	= rowmax(stepf stepm)
		recode step_exist (1 = 100), gen(step_yes)

	* biological parents are dead
		gen no_parents = hm1 == 2 & hv1 == 2 
		recode no_parents (1 = 1000)
		
	* create the family structure variable
		egen parcomp = rowtotal(bio_couple bio_not_couple step_yes no_parents)
			/*
			1 Biological parents are couple 1 (1 0 0 0)
			2 Biological parents are not couple, no step-parents 10 (0 10 0 0)
			3 Biological parents are not couple, step-parents exist 110 (0 10 100 0)
			4 No biological parents 0 (0 10 0 1000)
			* 101 = parents are couple and step-parent exist = . (n = 13)
			*/
			
		recode parcomp (10 = 2) (110 = 3) (1010 = 4) (101 = .)
		
		label define parcomp_labs ///
			1 "Parents are couple" ///
			2 "Parents not couple, no steps" ///
			3 "Parents not couple, steps exist" ///
			4 "No parents" ///
			, replace

		label values parcomp parcomp_labs
		label variable parcomp "Parents' composition"

// gender
	recode sex_gen (-4 = .) (1 = 1 "Male") (2 = 2 "Female"), gen(sex)

// family networks

	* number of biological children
	recode nkidsbioalv (-7 = .)
	* have biological children 
	recode nkidsbioalv (0 = 0 "No") (1/10 = 1 "Yes"), gen(biochildren)
	* sisters
	recode net17i1 (-2/-1 = .), gen(sisters)
	recode sisters (0 = 0 "None") (1/99 = 1 "Has sisters"), gen(sisters_yes)
	* brothers
	recode net17i2 (-2/-1 = .), gen(brothers)
	recode brothers (0 = 0 "None") (1/99 = 1 "Has brothers"), gen(brothers_yes)
	* siblings
	egen siblings = rowtotal(sisters brothers)
	recode siblings (0 = 0 "None") (1/99 = 1 "Has siblings"), gen(siblings_yes)

// marital status
	recode marstat (-7=.) (1=0 "Never married") (2=1 "Married/civil union") ///
	(3 4=3 "Divorced or widowed"), gen(marit)

// background: ethnicity
	recode ethni (-7 = .)
	recode ethni (1 = 1 "German native") (3 = 2 "Half-German") ///
	 (2 = 3 "Ethnic-German immigrant") (4 5 = 4 "Non-German background") ///
	 , gen(ethni4)
	
// labour force status
	recode lfs (-7=.) (9 = 1 "Full-time Employed") (10 11 12 = 2 "Other employed") ///
	(4 = 3 "Unemployed") (1 8 = 4 "In education") (2 3 = 5 "Homemaker") ///
	(5 6 7 13 = 6 "Other"), gen(lfs6)

	recode lfs6 (1 2 = 1 "Employed") (4 = 2 "In education") ///
	 (3 5 6 = 3 "Not employed or in education"), gen(lfs3)
	 
// level of education 
	recode isced (-7 = .)
	recode isced (0=0 "Currently enrolled") (1/3 = 1 "Low") (4/6 = 2 "Middle") ///
	 (7/8 = 3 "High"), gen(edu_level)

********************************************************************************
******************************** Analyses **************************************
********************************************************************************

************
// globals *
************

* covariates: Models 1 to 4
global cov_model1 i.parcomp
global cov_model2 i.(parcomp sex cohort ethni4)
global cov_model3 i.(parcomp sex cohort ethni4 marit biochildren siblings_yes)
global cov_model4 i.(parcomp sex cohort ethni4 marit biochildren siblings_yes edu_level lfs3)

* only complete cases
global cov_if if !missing(parcomp, sex, cohort, edu_level, marit, lfs3, biochildren, siblings_yes, ethni4)
 
**********************
// Attitude 1        *
**********************

* model 1
reg at1 $cov_model1 $cov_if 
qui eststo tab_at1_m1
* model 2
reg at1 $cov_model2 $cov_if 
qui eststo tab_at1_m2
* model 3
reg at1 $cov_model3 $cov_if 
qui eststo tab_at1_m3
* model 4
reg at1 $cov_model4 $cov_if 
qui eststo tab_at1_m4

**********************
// Attitude 2        *
**********************

* model 1
reg at2 $cov_model1 $cov_if 
qui eststo tab_at2_m1
* model 2
reg at2 $cov_model2 $cov_if 
qui eststo tab_at2_m2
* model 3
reg at2 $cov_model3 $cov_if 
qui eststo tab_at2_m3
* model 4
reg at2 $cov_model4 $cov_if 
qui eststo tab_at2_m4

******************
// Tables        *
******************

* Table 1. Descriptive statistics: dependent variables
	tab at1 $cov_if & at1 !=.
	tab at2 $cov_if & at2 !=.
	
* Table 2. Descriptive statistics (n = 8709)
	tab1 sex cohort ethni4 marit biochildren siblings_yes edu_level lfs3 parcomp $cov_if & at1 !=.

* Table 3. Results from regression models: Statement 1
	esttab tab_at1_m1 tab_at1_m2 tab_at1_m3 tab_at1_m4 ///
	, nocons cells("b(star fmt(%5.2f)) p(fmt(%5.3f)) ci_l ci_u") 
	
* Table 4. Results from regression models: Statement 2
	esttab tab_at2_m1 tab_at2_m2 tab_at2_m3 tab_at2_m4 ///
	, nocons cells("b(star fmt(%5.2f)) p(fmt(%5.3f)) ci_l ci_u") 

***********************
// Predictive margins *
***********************

* Appendix Table 1. Predictive margins with 95% confidence intervals (Regression model 4) 

global pmeans parcomp sex cohort ethni4 marit biochildren siblings_yes edu_level lfs3
 
* margins all: attitude 1
	qui reg at1 $cov_model4 $cov_if
	margins $pmeans , nopv cformat(%6.2f)
		pwcompare parcomp, cformat(%6.2f) pv

* margins all: attitude 2
	qui reg at2 $cov_model4 $cov_if
	margins $pmeans , nopv cformat(%6.2f)
		pwcompare parcomp, cformat(%6.2f) pv

**************************************
// Figures 1 & 2: Predictive margins *
**************************************

* Figure 1
	
	qui reg at1 $cov_model4 $cov_if
	qui margins parcomp, post
	marginsplot, ///
		horiz ///
		recast(scatter) ///
		plot1(mcol(black))  ///
		xline(`=e(b)[1,1]', lcolor(gs10) lpattern(dash)) /// 
		xline(`=e(b)[1,2]', lcolor(gs10) lpattern(dash)) /// 
		xline(`=e(b)[1,3]', lcolor(gs10) lpattern(dash)) /// 
		xline(`=e(b)[1,4]', lcolor(gs10) lpattern(dash)) ///
		title("Statement 1") ///
		xtitle("Predictive margin") ///
		ytitle("") ///
		ylabel( ///
			4 "No parents" ///
			3 `" "Parents are not couple," "stepparents exist" "' ///
			2 `" "Parents are not couple," "no stepparents" "' ///
			1 "Parents are couple") ///
		graphregion(color(white)) ///
		ci(col(black))

* Figure 2
		
		qui reg at2 $cov_model4 $cov_if
		qui margins parcomp, post
		marginsplot, ///
			horiz ///
			recast(scatter) ///
			ci(col(black)) ///
			plot1(mcol(black))  ///
			xline(`=e(b)[1,1]', lcolor(gs10) lpattern(dash)) /// 
			xline(`=e(b)[1,2]', lcolor(gs10) lpattern(dash)) /// 
			xline(`=e(b)[1,3]', lcolor(gs10) lpattern(dash)) /// 
			xline(`=e(b)[1,4]', lcolor(gs10) lpattern(dash)) ///
			title("Statement 2") ///
			xtitle("Predictive margin") ///
			ytitle("") ///
			ylabel( ///
				4 "No parents" ///
				3 `" "Parents are not couple," "stepparents exist" "' ///
				2 `" "Parents are not couple," "no stepparents" "' ///
				1 "Parents are couple") ///
			graphregion(color(white)) 

******************************************************
// Appendix Table 2: interactions & Figures 3 – 6	 *
******************************************************

// Attitude 1: interactions
***************************

* main model
	reg at1 $cov_model4 $cov_if ,  cformat(%5.2f) pformat(%5.3f)

* Figure 3. Interaction between parents' composition and respondents' gender (Statement 1)
	reg at1 i.parcomp##i.sex $cov_model4 $cov_if, cformat(%5.2f) pformat(%5.3f)
	margins i.parcomp#sex
	mplotoffset, offset(0.1) ///
	plot1opts(lcolor(black) lpattern(dash) msymbol(circle) mcolor(black) msize(small)) ///
		ci1opt(color(black)) ///
		plot2opts(lcolor(gs5) msymbol(triangle) mcolor(gs5) msize(small)) ///
		ci2opt(color(gs5)) ///
		title("") ///
		subtitle("") ///
		ytitle("Predictive margin") ///
		xtitle("") ///
		xlabel(`=1' "Parents are couple" `=2' `" "Parents are not couple," "no stepparents" "'  `=3' `" "Parents are not couple," "stepparents exist" "' `=4' "No parents", labsize(small)) ///
		graphregion(color(white)) ///
		legend(position(top) ring(0) size(small))
	
* Figure 4. Interaction between parents' composition and birth cohort (Statement 1)
	reg at1 i.parcomp##cohort $cov_model4 $cov_if, cformat(%5.2f) pformat(%5.3f)
	margins i.parcomp#cohort
	mplotoffset, offset(0.1) ///
		plot1opts(lcolor(black) lpattern(dash) msymbol(circle) mcolor(black) msize(small)) ///
		ci1opt(color(black)) ///
		plot2opts(lcolor(black) msymbol(triangle) mcolor(gs5) msize(small)) ///
		ci2opt(color(gs5)) ///
		plot3opts(lcolor(black) lpattern(dot) msymbol(square) mcolor(gs1) msize(small)) ///
		ci3opt(color(gs1)) ///
		title("") ///
		subtitle("") ///
		ytitle("Predictive margin") ///
		xtitle("") ///
		xlabel(`=1' "Parents are couple" `=2' `" "Parents are not couple," "no stepparents" "'  `=3' `" "Parents are not couple," "stepparents exist" "' `=4' "No parents", labsize(small)) ///
		graphregion(color(white)) ///
		legend(position(top) ring(0) size(small))
	

// Attitude 2: interactions
****************************

* main model
reg at2 i.parcomp $cov_model4 $cov_if, cformat(%5.2f) pformat(%5.3f)

*Figure 5. Interaction between parents' composition and respondents' gender (Statement 2)
	reg at2 i.parcomp##i.sex $cov_model4 $cov_if, cformat(%5.2f) pformat(%5.3f)
	margins i.parcomp#sex
		mplotoffset, offset(0.1) ///
		plot1opts(lcolor(black) lpattern(dash) msymbol(circle) mcolor(black) msize(small)) ///
		ci1opt(color(black)) ///
		plot2opts(lcolor(gs5) msymbol(triangle) mcolor(gs5) msize(small)) ///
		ci2opt(color(gs5)) ///
		title("") ///
		subtitle("") ///
		ytitle("Predictive margin") ///
		xtitle("") ///
		xlabel(`=1' "Parents are couple" `=2' `" "Parents are not couple," "no stepparents" "'  `=3' `" "Parents are not couple," "stepparents exist" "' `=4' "No parents", labsize(small)) ///
		graphregion(color(white)) ///
		legend(position(top) ring(0) size(small))

* Figure 6. Interaction between parents' composition and birth cohort (Statement 2)
	reg at2 i.parcomp##cohort $cov_model4 $cov_if, cformat(%5.2f) pformat(%5.3f)
	margins i.parcomp#cohort
		mplotoffset, offset(0.1) ///
		plot1opts(lcolor(black) lpattern(dash) msymbol(circle) mcolor(black) msize(small)) ///
		ci1opt(color(black)) ///
		plot2opts(lcolor(black) msymbol(triangle) mcolor(gs5) msize(small)) ///
		ci2opt(color(gs5)) ///
		plot3opts(lcolor(black) lpattern(dot) msymbol(square) mcolor(gs1) msize(small)) ///
		ci3opt(color(gs1)) ///
		title("") ///
		subtitle("") ///
		ytitle("Predictive margin") ///
		xtitle("") ///
		xlabel(`=1' "Parents are couple" `=2' `" "Parents are not couple," "no stepparents" "'  `=3' `" "Parents are not couple," "stepparents exist" "' `=4' "No parents", labsize(small)) ///
		graphregion(color(white)) ///
		legend(position(top) ring(0) size(small))
	
********************************************************************************	
********************************************************************************
********************************************************************************	
************************** THAT'S ALL DOCS *************************************
********************************************************************************
********************************************************************************
