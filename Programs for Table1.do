******* Programs for Table 1 *********
** Sample Output sytex
* conti_var; [title]; Non-missing n; overall mean (sd)   
* conti_var; [title]; Non-missing n; overall mean (sd); cat1 mean(sd); cat2...
**************************************

*----------------------- Continuous Var, Single: mean----------------------*
* Program for conti_var; [title]; n; mean (sd) 
capture program drop print_mean
program print_mean
syntax varlist, [title(string)]  [if]
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   qui sum `varlist', d
   local mean = trim(string(`r(mean)',"%8.1f"))   
di "`varlist';`title';`n';`mean'"
end 
* example 


*----------------------- Continuous Var, Single: mean(sd)----------------------*
* Program for conti_var; [title]; n; mean (sd) 
capture program drop print_meansd
program print_meansd
syntax varlist, [title(string)] [if]
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   qui sum `varlist', d
   local mean = trim(string(`r(mean)',"%8.1f"))   
   local sd = trim(string(`r(sd)',"%8.1f"))      
di "`varlist';`title';`n';`mean' (`sd')"
end 
* example 

*----------------------- Continuous Var, Single: median ----------------------*
* Program for conti_var; [title]; n; mean (sd) 
capture program drop print_median
program print_median
syntax varlist, [title(string)] [if]
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   qui sum `varlist', d
   local median = trim(string(`r(p50)',"%8.1f"))   
di "`varlist';`title';`n';`median';"
end 
* example 


*----------------------- Continuous Var, Single: median (IQR)----------------------*
* Program for conti_var; [title]; n; mean (sd) 
capture program drop print_medianiqr
program print_medianiqr
syntax varlist, [title(string)] [if]
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   qui sum `varlist', d
   local median = trim(string(`r(p50)',"%8.1f")+" ("+string(`r(p75)'-`r(p25)',"%8.1f")+")")
di "`varlist';`title';`n';`median';"
end 


*----------------------- 2-Category Var, Single: % ----------------------*
* Program for Cat_var; [title];N; %  //need to tab, gen() for each cat beforehand 
capture program drop print_2cat
program print_2cat
syntax varlist, [title(string)]
   qui count if !missing(`varlist')
   local N = `r(N)'
   qui count if `varlist'==1
   local n = `r(N)'
   local per = trim(string(`n'/`N'*100,"%8.1f"))
di "`varlist';`title';`N';`per'"
end 


*----------------------- Categorical Var: Display only name  ----------------------*
capture program drop print_cat_name
program print_cat_name
syntax varlist, [title(string)] 
   * Non-missing #
   qui count if !missing(`varlist')
   local N = `r(N)'  
   di "`varlist';`title';`N';"
end 


*--------------------------- total N -----------------------*
* Program for overall n 
capture program drop print_n
program print_n
syntax , [title(string)] 
   * Overall N 
   qui count                      //Overall N
   local N = `r(N)'
   di "N;`title';;`N';" 
end 



********************************** BY ***************************************
*----------------------- Continuous Var, BY: mean----------------------*
capture program drop print_mean_by
program print_mean_by
syntax varlist, [title(string)] byvar(varname)   
   
   * Unmissing Number
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   * Overall mean
   qui sum `varlist', d
   local mean = trim(string(`r(mean)',"%8.1f"))   
   di "`varlist';`title';`n';`mean';" _c
   
   * by var mean(sd)
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui sum `varlist' if `byvar'==`i', d 
	   local mean = trim(string(`r(mean)',"%8.1f"))   
	   di "`mean';" _c
	   }
	   
	* p-value by anova test
	qui anova `varlist' `byvar'
       local p=Ftail(e(df_1),e(N)-e(df_1)-1,e(F))
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"

end 
* example


*----------------------- Continuous Var, BY: mean(sd) ----------------------*
capture program drop print_meansd_by
program print_meansd_by
syntax varlist, [title(string)] byvar(varname)   
   
   * Unmissing Number
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   * Overall mean(sd)
   qui sum `varlist', d
   local mean = trim(string(`r(mean)',"%8.1f"))   
   local sd = trim(string(`r(sd)',"%8.1f")) 
   di "`varlist';`title';`n';`mean' (`sd');" _c
   
   * by var mean(sd)
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui sum `varlist' if `byvar'==`i', d 
	   local mean = trim(string(`r(mean)',"%8.1f"))   
       local sd = trim(string(`r(sd)',"%8.1f")) 
	   di "`mean' (`sd');" _c
	   }
	   
	* p-value by anova test
	qui anova `varlist' `byvar'
       local p=Ftail(e(df_1),e(N)-e(df_1)-1,e(F))
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"

end 
* example


*----------------------- Continuous Var, BY: median ----------------------*
capture program drop print_median_by
program print_median_by
syntax varlist, [title(string)] byvar(varname)   
   
   * Unmissing Number
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   * Overall median
   qui sum `varlist', d
   local median = trim(string(`r(p50)',"%8.1f"))   
   di "`varlist';`title';`n';`median';" _c
   
   * by var median
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui sum `varlist' if `byvar'==`i', d 
       local median = trim(string(`r(p50)',"%8.1f")) 
	   di "`median';" _c
	   }
	* median test    
	qui median `varlist', by(`byvar')
       local p=`r(p)'
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"
end 
* example
*print_median_by v2tg, byvar(v2drinkscat2)


*---------------------- Continuous Var, BY: median (IQR)----------------------*
capture program drop print_medianiqr_by
program print_medianiqr_by
syntax varlist, [title(string)] byvar(varname)   
   
   * Unmissing Number
   qui count if !missing(`varlist')
   local n = `r(N)'
   
   * Overall median
   qui sum `varlist', d
   local median = trim(string(`r(p50)',"%8.1f")+" ("+string(`r(p75)'-`r(p25)',"%8.1f")+")")  
   di "`varlist';`title';`n';`median';" _c
   
   * by var median
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui sum `varlist' if `byvar'==`i', d 
       local median = trim(string(`r(p50)',"%8.1f")+" ("+string(`r(p75)'-`r(p25)',"%8.1f")+")")
	   di "`median';" _c
	   }
	* median test    
	qui median `varlist', by(`byvar')
       local p=`r(p)'
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"

end


*----------------------- 2-Category Var, BY: %, p-value  ----------------------*
* Program for Cat_var; [title];non-missing N; overall %; by cat %...; p-value from Chi-square test
* need to tab, gen() for each cat beforehand 
capture program drop print_2cat_by
program print_2cat_by
syntax varlist, [title(string)] byvar(varname)
   *di "varname;title;overall %; 
   
   * Non-missing #
   qui count if !missing(`varlist')
   local N = `r(N)'
   
   * Overall %
   qui count if `varlist'==1
   local n = `r(N)'
   local per = trim(string(`n'/`N'*100,"%8.1f"))
   di "`varlist';`title';`N';`per';" _c
   
   * By category %
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui count if `byvar'==`i'   //number in each category
	   local N = `r(N)'
       qui count if `byvar'==`i' & `varlist'==1  //number in each category & exposure==1
       local n = `r(N)'
       local per = trim(string(`n'/`N'*100,"%8.1f"))
       di "`per';" _c
	   }
	   
   * Chi-square test 
    qui tab `varlist' `byvar', chi
       local p=`r(p)'
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"
end 
* example 
*print_cat_by female, byvar(v2drinker)



*----------------------- Multi-Cat var (seperate for two programs)----------------------*
*------------ Main Cat: Only p-value 
capture program drop print_multicat_by
program print_multicat_by
syntax varlist, [title(string)] byvar(varname)
   
   * Non-missing #
   qui count if !missing(`varlist')
   local N = `r(N)'
   di "`varlist';`title';`N';;" _c  //one "" for overall per
   
   * di ";" for each cat in order to be the same format
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       di ";" _c
	   }
	   
   * Chi-square test for p-value 
    qui tab `varlist' `byvar', chi
       local p=`r(p)'
	   local p_prefix=`p'<=.001
       local p_print=cond(`p_prefix',"<0.001",trim(string(`p',cond(`p'<0.1,"%8.3f","%8.3f")))) 
	   di "`p_print';"

end 
/* example 
	  print_multicat_by  cigt21, title("Smoking status, %") byvar(v2drinkscat2) 
	  print_subcat_by  v2smok1, title("Current") byvar(v2drinkscat2) 
	  print_subcat_by  v2smok2, title("Former") byvar(v2drinkscat2)
*/

*------------- Sub-Category, BY: % for each cat
* Cat_var; [title];non-missing N; overall %; by cat %
* need to tab, gen() for each sub-cat beforehand 
capture program drop print_subcat_by
program print_subcat_by
syntax varlist, [title(string)] byvar(varname)
   *di "varname;title;overall %; 
   
   * Non-missing #
   qui count if !missing(`varlist')
   local N = `r(N)'
   
   * Overall %
   qui count if `varlist'==1
   local n = `r(N)'
   local per = trim(string(`n'/`N'*100,"%8.1f"))
   di "`varlist';`title';`N';`per';" _c
      
   * By category %
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui count if `byvar'==`i'   //number in each category
	   local N = `r(N)'
       qui count if `byvar'==`i' & `varlist'==1  //number in each category & exposure==1
       local n = `r(N)'
       local per = trim(string(`n'/`N'*100,"%8.1f"))
       di "`per';" _c
	   }
	   di ""

end 
* example 
*print_cat_by female, byvar(v2drinker)


*--------------------------- N (%), BY -----------------------*
* Program for Unmissing n 
capture program drop print_n_by
program print_n_by
syntax , [title(string)] [byvar(varname)]
   
   * Overall N 
   qui count if !missing(`byvar') //unmissing byvar N0, just to check 
   local N0 = `r(N)'
   qui count                      //Overall N
   local N = `r(N)'
   di "N;`title';`N0';`N';" _c
	 
   * By category N (%)
   qui sum `byvar'
   local bymin = `r(min)'
   local bymax = `r(max)' 
   forvalue i = `bymin'/`bymax' {
       qui count if `byvar'==`i'   //number in each category
	   local n = `r(N)'
       local per = trim(string(`n'/`N'*100,"%8.1f"))
       di "`n' (`per'%);" _c
	   }
	   di ""
end 
* example
*print_n, byvar(v2drinker)
















