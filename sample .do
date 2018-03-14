***** Table 1, Overall and Stratified ****

    cd "C:\Users\Dropbox"  /* change direction here */
   
    do "C:\Users\Dropbox\Programs for Table1.do"  /* run your ado file here */


/* Summary of all programs:
> print_mean 
> print_meansd
> print_median
> print_2cat
> print_n
> 
> print_mean_by
> print_meansd_by
> print_median_by
> print_2cat_by
> print_multicat_by   //only output p-value
> print_subcat_by     //don't output p-value
> */
*------------------------- Overall --------------------------*
foreach j in 1/1 {
  qui capture log close table1
  qui capture log using "table1.txt", replace name(table1)
      di ""
	  print_n_by, title("N (%)") byvar(v2drinkscat2_4cat)
      print_meansd_by v2age, title("Age (years)") byvar(v2drinkscat2_4cat)
	  print_2cat_by black, title("Black, %") byvar(v2drinkscat2_4cat)
      print_2cat_by  female, title("Female, %") byvar(v2drinkscat2_4cat)
	  print_2cat_by  lowincome, title("v1 Family income <35000, %") byvar(v2drinkscat2_4cat) 
	  print_2cat_by  lowedu, title("Education less than high school, %") byvar(v2drinkscat2_4cat)
	  print_meansd_by bmi21,  title("BMI (kg/m2)") byvar(v2drinkscat2_4cat)
	  print_2cat_by  obesity, title("Obesity, %") byvar(v2drinkscat2_4cat) 
	  
	  print_meansd_by v2sbp, title("SBP (mmHg)") byvar(v2drinkscat2_4cat) 
	  print_meansd_by v2dbp, title("DBP (mmHg)") byvar(v2drinkscat2_4cat) 
	  print_2cat_by  hypert25, title("Hypertension, %") byvar(v2drinkscat2_4cat) 
	  print_2cat_by  hyptmdcode21, title("Antihypertensive Med Use, %") byvar(v2drinkscat2_4cat) 
	  print_2cat_by  v2bpopt, title("Optimal BP, %") byvar(v2drinkscat2_4cat)
	  
	  print_meansd_by v2hdl, title("HDL-cholesterol (mg/dL)") byvar(v2drinkscat2_4cat)
	  print_2cat_by  lowhdl, title("Low HDL-c, %") byvar(v2drinkscat2_4cat)
	  print_meansd_by v2ldl, title("LDL-cholesterol (mg/dL)") byvar(v2drinkscat2_4cat)
	  print_median_by v2tg, title("Triglycerides (mg/dL), median") byvar(v2drinkscat2_4cat)
	  print_2cat_by  cholmdcode21, title("Lipid lowering Med Use, %") byvar(v2drinkscat2_4cat) 
	  print_meansd_by v2crp, title("C-reactive protein (mg/L), median") byvar(v2drinkscat2_4cat)
	  
	  print_multicat_by  dia_fg, title("Fasting glucose status, %")  byvar(v2drinkscat2_4cat)
	  print_subcat_by  dia_fg1, title("Normal (<100 mg/dL)") byvar(v2drinkscat2_4cat) 
	  print_subcat_by  dia_fg2, title("Impaired (100-125 mg/dL)") byvar(v2drinkscat2_4cat) 
	  print_subcat_by  dia_fg3, title("Undiagnosed diabetes (>=126 mg/dL)") byvar(v2drinkscat2_4cat) 
	  print_subcat_by  v2diabetes, title("Diabetes (self-report, meds, FG, A1c), %") byvar(v2drinkscat2_4cat) 

	  print_multicat_by  cigt21, title("Smoking status, %") byvar(v2drinkscat2_4cat)
	  print_subcat_by  v2smok1, title("Current") byvar(v2drinkscat2_4cat) 
	  print_subcat_by  v2smok2, title("Former") byvar(v2drinkscat2_4cat)
	  
	  print_2cat_by  v2lvh, title("LVH, %") byvar(v2drinkscat2_4cat)
	  print_meansd_by egfrcr_v2, title("eGFR (ml/min/1.73 m2)") byvar(v2drinkscat2_4cat)
	  print_2cat_by v3binge, title("Ever Binge Drinking") byvar(v2drinkscat2_4cat2) 
	  
  qui capture log close table1
}
