foreach out in pad1 cli1 /*pad2 cli2*/ {
      stset fu_`out'_v2, failure(inc_`out'_v2)
         foreach exp in log2hsctnt log2ntprobnp {
          
              local demo age2 gender2 race2
                local full `demo' ib1.edulv2 bmi2 tchol2 hdlc2 sbp2 antihtn2 cursmk2 diabts2 prvhf2 prvchd2 prvstrk2 egfrepi2 loghsctnt logntprobnp

              if "`exp'"=="log2hsctnt" local adj:subinstr local full "loghsctnt" ""
                else if "`exp'"=="log2ntprobnp" local adj:subinstr local full "logntprobnp" ""
                
               local i=1
                foreach v in beta stderr p_intx p_intx_lr num {
                         gen `v'=.
                        }
                 gen varname="" 
                 
              stcox `exp' `adj', nolog
                est store A
                
              local h=1
                set more off 
                foreach sub in agegrp race2 gender2 cursmk2 diabts2 hypert2 hx_cvd ckd {
                        if "`sub'"=="agegrp" local adj2:subinstr local adj "age2" ""
                        else if "`sub'"=="cursmk2" local adj2:subinstr local adj "cursmk2" ""
                        else if "`sub'"=="hypert2" local adj2:subinstr local adj "sbp2 antihtn2" ""
                        else if "`sub'"=="hx_cvd" local adj2:subinstr local adj "prvhf2 prvchd2 prvstrk2" ""
                         else if "`sub'"=="ckd" local adj2:subinstr local adj "egfrepi2" ""
                         else local adj2:subinstr local adj "`sub'" ""
                        
                      forval k=0/1 {  
                                 stcox `exp' `adj2' if `sub'==`k', nolog 
                                 mata:st_store(`i',("beta"),st_matrix("e(b)")[|1,1|])
                                mata:st_store(`i',("stderr"),sqrt(diagonal(st_matrix("e(V)"))')[|1,1|])
                                count if `sub'==`k'
                                 replace num=r(N) in `i++'
                                }
                                 
                       stcox c.`exp'##i.`sub' `adj2', nolog
                         test c.`exp'#1.`sub'
                         replace p_intx=r(p) in `h'
                         
                      *stcox c.`exp'##i.`sub' `adj2', nolog
                       est store B
                        lrtest A B, stats force
                        if "`sub'"!="hypert2" replace p_intx_lr=r(p) in `h'
                        
                     local h=`h'+2
                        }
                
              local demo age2 gender2 race2
                 local full `demo' ib1.edulv2 bmi2 tchol2 hdlc2 hypert2 cursmk2 diabts2 prvhf2 prvchd2 prvstrk2 egfrepi2 loghsctnt logntprobnp
                
               if "`exp'"=="log2hsctnt" local adj:subinstr local full "loghsctnt" ""
                else if "`exp'"=="log2ntprobnp" local adj:subinstr local full "logntprobnp" ""
                
              stcox `exp' `adj', nolog
                 est store A
                 
              foreach sub in hypert2 {
                        if "`sub'"=="hypert2" local adj2:subinstr local adj "sbp2 antihtn2" ""
                         stcox c.`exp'##i.`sub' `adj2', nolog
                        est store B
                        lrtest A B, stats force
                         replace p_intx_lr=r(p) in 11
                         }
                         
               gen str5 pval=string(p_intx_lr, "%5.3f") if p_intx_lr>=0.001 & p_intx_lr<.
                 replace pval="<0.001" if p_intx_lr<0.001 & p_intx_lr<.
                 replace pval="NA" if p_intx_lr==0
                 
              replace beta=0 if beta>10 | stderr==0
                 replace stderr=0.0000000001 if beta==0  
                
              //gen cond=model+" "+"("+string(num, "%8.0fc")+")" if num<.
                 
              label var pval "P for interaction"      
                 label var exposure "Subgroup"
                 label var cond "Condition(N)"
				 
				 capt drop exposure
				 gen exposure= ""
				 replace exposure= "age" in 1
				 replace exposure= "gender" in 3
 
                metan beta stderr in 1/16, eform nobox effect(Hazard Ratio) lcols(exposure cond) rcols(pval) ///
                        graphr(color(white)) xlabel("0.7, 1, 1.3, 1.6, 2, 2.5") astext(70) force xsize(6.5) ysize(5)
                graph save "forest subgroup `out' `exp' using lrtest 20160705.gph", replace 
                
               drop beta stderr varname p_intx pval p_intx_lr num cond
                }
        }

