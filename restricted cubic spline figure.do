
* Figure 1 - splines with histogram - in persons with diabetes only

* stset the data so stata knows it time-to-event
* since i used imputed values, all the relevant commands start with "mi"
mi stset $dem1
  
* model for figure  (simplified for blog)
global model_fig v2age22_mean black_mean fem1_mean 
   
foreach k in v2ag { // exposure variable 
  foreach j in model_fig { //models
preserve
***** Cubic spline 
   keep if diabstrat==1 //we're only looking in persons with diabetes
    gen `k'5 = -`k'/5 //per 5-unit change in the exposure
    quietly sum `k', d //dropping the bottom and top 1% since its less stable
    keep if `k' < r(p99)
    keep if `k' > r(p1)
 
 **** Run the cox model
 gen `k'_cubic = `k'
 mkspline rcal_a1c_cubic = rcal_a1c, cubic nknots(4) displayknots 
 quietly mi estimate, saving(est1, replace): stcox `k'5 $`j' rcal_a1c_cubic*, efron
 mi estimate, hr
 mi estimate, saving(est1, replace): stcox `k'_cubic $`j' rcal_a1c_cubic*, efron
  
 quietly sum `k'_cubic, d
 replace `k'_cubic = `k'_cubic - 10 //r(p50)
   
 * Set values to 0
 for var v2age22_mean fem1_mean educ1_mean educ2_mean black_mean ///
    htn_mean prvstr21_mean prvchd23_mean bmi21_mean smoke1_mean smoke2_mean ///
    apoe1_mean apoe2_mean drink2_mean drink3_mean rcal_a1c_cubic* insulin_mean : replace X=0

  * Predict 
 mi predict ln_fitval2 using est1 //predict the log(HR)
 mi predict secubic using est1, stdp //predict the log(HR)
 gen fitval2 = exp(ln_fitval2)
 gen lcicubic = exp(ln_fitval2 - 1.96*secubic) //95% lower confidence limit
 gen ucicubic= exp(ln_fitval2 + 1.96*secubic)  //95% upper confidence limit

        * Keep the relevant variables and save the file - these are the cubic spline estimates
 keep id fitval2 lcicubic ucicubic


 save "cubic.dta", replace
restore
preserve
***** Linear spline - everything is the same except we're using a linear spline
   keep if diabstrat==1
    quietly sum `k', d
    keep if `k' < r(p99)
    keep if `k' > r(p1)
  
 mkspline rcal_a1c_cubic = rcal_a1c, cubic nknots(4) displayknots 
 *linear
 quietly centile `k', centile(5 35 65 95)
 local new1 = round(r(c_1),.1)
 local new2 = round(r(c_2),.1)
 local new3 = round(r(c_3),.1)
 local new4 = round(r(c_4),.1)
 local var1 `"`new1' `new2' `new3' `new4'"'
 display "**`var1'**"
  
 mkspline `k'_cubic1 `new1' `k'_cubic2 `new2' `k'_cubic3 `new3' `k'_cubic4 `new4' `k'_cubic5 = `k', displayknots
 local nknots = r(N_knots) - 1 
 
 **** Run the cox model
        mi estimate, saving(est1, replace): stcox `k'_cubic* $`j' rcal_a1c_cubic*, efron
 sum `k', d
    
 * Center values at the median
 forvalues i = 1(1)`nknots' {
    quietly sum `k'_cubic`i', d
    quietly centile `k'_cubic`i', centile(58.5)
    replace `k'_cubic`i' = `k'_cubic`i' - r(c_1) //r(p50) 
 }    

  * Set values to 0
 for var v2age22_mean fem1_mean educ1_mean educ2_mean black_mean ///
   htn_mean prvstr21_mean prvchd23_mean bmi21_mean smoke1_mean smoke2_mean ///
   apoe1_mean apoe2_mean drink2_mean drink3_mean rcal_a1c_mean rcal_a1c_cubic* insulin_mean: replace X=0
    
 * Predict 
 mi predict ln_fitval2 using est1 //predict the log(HR)
 gen fitval2_lin = exp(ln_fitval2)    

        * Keep the relevant values (I didn't plot the CIs for this so didn't keep those)
 keep id fitval2_lin
 save "linear.dta", replace
restore
preserve
  ***** Merge the two files and make the plot
  keep if diabstrat==1
  quietly sum `k', d
 keep if `k' < r(p99)
 keep if `k' > r(p1)

        merge 1:1 id using cubic.dta
 drop _merge
  
 merge 1:1 id using linear.dta
 drop _merge
  
 * Make graph
        gen yref = 1 //ref line at y=1
 gen lb = .22 //lower bound
 gen demmarks1 = .24 //place to put dementia tick marks
 gen demmarks2 = .265
 gen v2ag_jitter = v2ag + 0.1*runiform()
 gen symbol = "|"
    
        twoway ///
      (hist `k', lw(vthin) yaxis(2) fcolor(gs12) freq lcolor(gs11)) ///
      (line lcicubic `k', sort lcolor(black) lp(dash) lwidth(thin)) /// 
      (line ucicubic `k', sort lcolor(black) lp(dash) lwidth(thin)) /// 
        (line fitval2 `k' , yaxis(1) sort lcolor(black) lwidth(med) lp(solid)) ///
        (line fitval2_lin `k' if !(fitval2_lin < 1.2 & v2ag<5) , yaxis(1) sort(`k') lcolor(black) lwidth(med) lp(dot)) ///truncate values that are high/low
      (line yref `k', yaxis(1) lpattern(solid) lwidth(medthin) lc(gs4)) ///
      (pcspike demmarks1 v2ag_jitter demmarks2 v2ag_jitter if all_dementia==1, lw(med) lc(black)) ///
      (scatter lb `k', msize(vtiny) mcolor(none)) ///this sets the lower bound for the y axis
    , ///everything after this is to make the graph nice
       graphregion(color(white) lcolor(gray) margin(small)) ///
       plotregion(fcolor(white) margin(small)) ///
              name(diabetes, replace) ///
       ytitle("Hazard Ratio (95% CI)", size(vvtiny)) ///
       ytitle("Frequency Distribution of 1,5-Anhydroglucitol", axis(2) orientation(rvertical)) ///
       xtitle("1,5-Anhydroglucitol, ug/mL", size(med)) ///
       yline(1, lpattern(solid) lw(medthin) lcolor(gs4)) ///
       ylabel(0.25 "0.25" 0.5 "0.5" 1 2 4, labsize(med) angle(horizontal) nogrid) ///
              xlabel(0(5)25 , labsize(med) angle(horizontal) nogrid) ///
       ylabel(0(100)350, axis(2) angle(horizontal)) ///
       xsize(7) ysize(5) ///
       yscale(alt axis(2)) ///
       yscale(log alt axis(1)) ///
       xmtick(none) ///
       legend(order(4 "Estimated Hazard Ratio"  ///
        5 "Estimated Hazard Ratio (Spline)" 2 "Confidence Interval for Hazard Ratio" ///
        1 "1,5-Anhydroglucitol Distribution" 7 "Dementia Case") ///
        col(1) position(1) ring(0) size(small) symxsize(8)) 
restore
  } //end of model
} //end of exposure


* Figure 2 is a little more complicated because we need the estimates from the model first (using mi estimate, which is similar to lincom but with imputed data). But once you have the estimates and CIs the figure itself is easy to generate

*** NOTE: this will write all estimates/SEs to a file so they're easier to capture
* define location of where to store coefficients from models
capture file close myfile
global filename file_betas_MP2606
file open myfile using "../4 - results/$filename.txt", write   

* create the first line of the file, variable names
capture log close
log using "../4 - results/$filename.log", replace
display "****model = $modelfull_cent ********"
file write myfile "Model_Type" _tab "Outcome" _tab "Exposure" _tab "Model" _tab "Imputation_of" _tab "Exposure_Status" ///
  _tab "Time" _tab "Estimate" _tab "SE" _n 

* I used this chunk of code to estimate the 20-year declines (and also declines for years 1-6 and 6-20), which wrote the estimates out to a separate file

foreach outcome in globalz_ {   //Outcome variables
  foreach exp in diab6lev {  //Exposure variable - should be 0/1
    foreach model in modelfull_cent {  //Model specification
   foreach condition in imp_2004 imp_alive { //imp_covars imp_exp_covars imp_alive imp_all imp_2004 { 
   global labels "mixed" _tab "`outcome'" _tab "`exp'" _tab "`model'" _tab "`condition'" 
      display "******** Outcome = `outcome', Exposure = `exp', Model = `model', Condition = `condition'  ********"
      mi estimate, saving(est1, replace): ///
     xtmixed `outcome' $`model' ///
     i.`exp' i.`exp'#c.timesp1 i.`exp'#c.timesp2 if ${`condition'} ///
     || idnum: timesp1 timesp2 , var cov(independent) //vce(robust)
        display"****************************** YEARS 1-6 *******************************"
   foreach loop in `loop_new' { //1 2 3 4 5 6 { //groups 1 to 6 - absolute decline
   mi estimate (group`loop': _b[c.timesp1] + _b[`loop'.`exp'#c.timesp1]) using est1
     matrix beta = e(b_Q_mi)
     matrix var = e(V_Q_mi)
     file write myfile "${labels}" _tab "Group`loop'" _tab "Year1-6" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
   }
   foreach loop in `loop_new1' { //1 3 5 { //differences in group decline years 1-6
     local loop1 = `loop' + 1
     mi estimate (group`loop': _b[`loop1'.`exp'#c.timesp1] - _b[`loop'.`exp'#c.timesp1]) using est1
     matrix beta = e(b_Q_mi)
     matrix var = e(V_Q_mi)
     file write myfile "${labels}" _tab "Diff`loop'-`loop1'" _tab "Year1-6" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
   }   
 display"****************************** YEARS 6-20 *******************************"
   foreach loop in `loop_new' { //1 2 3 4 5 6 { //groups 1 to 6 - absolute decline
     mi estimate (group`loop': 14/6*_b[c.timesp2] + 14/6*_b[`loop'.`exp'#c.timesp2]) using est
     matrix beta = e(b_Q_mi)
            matrix var = e(V_Q_mi)
            file write myfile "${labels}" _tab "Group`loop'" _tab "Year6-20" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
  }
  foreach loop in `loop_new1' { //3 5 { //differences in group decline years 6-20
             local loop1 = `loop' + 1
      mi estimate (group`loop': 14/6*_b[`loop1'.`exp'#c.timesp2] - 14/6*_b[`loop'.`exp'#c.timesp2]) using est1
      matrix beta = e(b_Q_mi)
      matrix var = e(V_Q_mi)
      file write myfile "${labels}" _tab "Diff`loop'-`loop1'" _tab "Year6-20" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
   }    
 display"****************************** YEARS 1-20 *******************************"
  foreach loop in `loop_new' { //1 2 3 4 5 6 { //groups 1 to 6 - absolute decline
     mi estimate (group`loop': _b[c.timesp1] + _b[`loop'.`exp'#c.timesp1] + 14/6*_b[c.timesp2] + 14/6*_b[`loop'.`exp'#c.timesp2]) using est1
     matrix beta = e(b_Q_mi)
     matrix var = e(V_Q_mi)
     file write myfile "${labels}" _tab "Group`loop'" _tab "Year1-20" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
  }
  foreach loop in `loop_new1' { //3 5 { //differences in group decline years 1-20
      local loop1 = `loop' + 1
      mi estimate (group`loop': _b[`loop1'.`exp'#c.timesp1] + 14/6*_b[`loop1'.`exp'#c.timesp2] - _b[`loop'.`exp'#c.timesp1] - 14/6*_b[`loop'.`exp'#c.timesp2]) using est1
     matrix beta = e(b_Q_mi)
     matrix var = e(V_Q_mi)
     file write myfile "${labels}" _tab "Diff`loop'-`loop1'" _tab "Year1-20" _tab (beta[1,1]) _tab (sqrt(var[1,1])) _n 
  }  
      }
    }
  }
}     
file close myfile  
log close

The file it creates looks like this (small section of the file):
Model_Type Outcome Exposure Model Imputation_of Exposure_Status Time Estimate SE
mixed globalz_ diab6lev modelfull_cent none Group1 Year1-6 -.09859903 .00924783
mixed globalz_ diab6lev modelfull_cent none Group2 Year1-6 -.10714462 .02909718
mixed globalz_ diab6lev modelfull_cent none Group3 Year1-6 -.11793243 .03246177
mixed globalz_ diab6lev modelfull_cent none Group4 Year1-6 -.26979403 .06551715
mixed globalz_ diab6lev modelfull_cent none Group5 Year1-6 -.18650354 .05846179
mixed globalz_ diab6lev modelfull_cent none Group6 Year1-6 -.23305821 .02741727
mixed globalz_ diab6lev modelfull_cent none Diff1-2 Year1-6 -.00854559 .02891338
mixed globalz_ diab6lev modelfull_cent none Diff3-4 Year1-6 -.15186161 .07225254
mixed globalz_ diab6lev modelfull_cent none Diff5-6 Year1-6 -.04655468 .06319007

* With Imputation
clear
input str8 imp str7 group str8 years est se lcl ucl 
imp_2004 Group1 Year1-20 -0.962265 0.025805 -1.012842 -0.911688
imp_2004 Group2 Year1-20 -0.908259 0.063732 -1.033175 -0.783344
imp_2004 Group3 Year1-20 -1.012728 0.061065 -1.132415 -0.893041
imp_2004 Group4 Year1-20 -1.156766 0.125817 -1.403367 -0.910165
imp_2004 Group5 Year1-20 -1.016785 0.150761 -1.312276 -0.721293
imp_2004 Group6 Year1-20 -1.424160 0.089096 -1.598788 -1.249531
end 

* This is the x-axis position for the graph and subjective based on how far apart you want the points
gen position1 = . 
  replace position1 = 1 if group=="Group1"
  replace position1 = 1.08 if group=="Group2"
  replace position1 = 1.3 if group=="Group3"
  replace position1 = 1.38 if group=="Group4"
  replace position1 = 1.6 if group=="Group5"
  replace position1 = 1.68 if group=="Group6"

global ag_ge10 group=="Group1"|group=="Group3"|group=="Group5" 
global ag_lt10 group=="Group2"|group=="Group4"|group=="Group6"

gen temp = -1.5
gen min = .9
gen max = 1.8
twoway ///
  (rcap lcl ucl position1 if $ag_ge10 | $ag_lt10 , lcolor(black) lwidth(medthick)) ///
  (scatter est position1 if group=="Group1" , msymbol(O) msize(vlarge) mcolor(white) mlcolor(black) lwidth(thick)) ///
  (scatter est position1 if group=="Group3", msymbol(O) msize(vlarge) mcolor(white) mlcolor(black) lwidth(thick)) ///
  (scatter est position1 if group=="Group5", msymbol(O) msize(vlarge) mcolor(white) mlcolor(black) lwidth(thick)) ///
  (scatter est position1 if group=="Group2" , msymbol(S) msize(vlarge) mcolor(eltblue) mlcolor(navy) lwidth(thick)) ///
  (scatter est position1 if group=="Group4" , msymbol(S) msize(vlarge) mcolor(eltblue) mlcolor(navy) lwidth(thick)) ///
  (scatter est position1 if group=="Group6" , msymbol(S) msize(vlarge) mcolor(eltblue) mlcolor(navy) lwidth(thick)) ///
  (lfit est position1 if $ag_ge10, lp(dash) lc(gs8)) ///
  (lfit est position1 if $ag_lt10, lp(dash) lc(gs8)) ///
  (scatter temp min, msymbol(none)) ///
  (scatter temp max, msymbol(none)) ///
  , ///
  ylabel(-1.1(.1)-1.7 -.6 "-0.6" -.7 "-0.7" -.8 "-0.8" -.9 "-0.9" -1 "-1.0", angle(0) nogrid) ///
  legend(order(2 "1,5-Anhydroglucitol {&ge}10 {&mu}g/mL" 5 "1,5-Anhydroglucitol <10 {&mu}g/mL") /// 
     position(7) ring(0) col(1) size(medsmall) region(lcolor(none) fcolor(none))) ///
  xlabel(1.04 "No Diabetes" 1.34 "A1c <7%" 1.64 "A1c {&ge}7%", labsize(medlarge) notick) ///  
  name(figure2_1, replace) xtitle("") ///
  yline(0, lp(dash) lcolor(black)) graphregion(color(white)) ///
  ytitle("Estimated 20-year Decline, Z Score", size(medsmall)) ///
  text(-.6 1.04 "0.05 (-0.05, 0.16)", size(small))  ///
  text(-.6 1.34 "-0.15 (-0.44, 0.16)", size(small))  ///
  text(-.6 1.64 "-0.41 (-0.73, -0.08)*", size(small)) ///
  text(-.65 1.04 "(p-value=0.300)", size(small))  ///
  text(-.65 1.34 "(p-value=0.353)", size(small))  ///
  text(-.65 1.64 "(p-value=0.025)", size(small))  
