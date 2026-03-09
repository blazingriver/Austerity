****************************************************
**												  **
**  Econ 706 Master's Thesis Research Proposal    **
**												  **
****************************************************


/* Handy actions
	
	***Re-run code/reset tempfiles
	
		clear
		capture postclose _all
		macro drop _all
		clear matrix
		
*/


*---------------------------------------------------*
*-	 	Change Directory and Load Data		       -*
*---------------------------------------------------*

global root "."

cd "$root/." 

use JSTdatasetR6, clear

save "macrohistory", replace

/*********************************************************************************************
**			Data Processing/Cleaning and Estimation	 (Dec. 10 5:33pm) 				    **
**********************************************************************************************/
		* Define preferred window
		keep if year >= 1970 & year <= 2020

		* Create Country IDs *
		encode iso, gen(iso_id)
		tab iso iso_id
		xtset iso_id year

		count
		local N0 = r(N)
		di "Original observations = `N0'"


		di "Observations after restricting to 1970–2020 = `N1'"
		di "Dropped due to year restriction = `= `N0' - `N1''"

		drop if missing(rgdpmad) | missing(debtgdp)
		count
		local N2 = r(N)

		di "Observations after dropping missing GDP or debtgdp = `N2'"
		di "Dropped due to missing GDP or debtgdp = `= `N1' - `N2''"

		gen miss_gdp = missing(rgdpmad)
		gen miss_debt = missing(debtgdp)

		bys iso: egen sum_miss_gdp = total(miss_gdp)
		bys iso: egen sum_miss_debt = total(miss_debt)

		list iso sum_miss_gdp sum_miss_debt if sum_miss_gdp > 0 | sum_miss_debt > 0

		tab year if missing(rgdpmad) | missing(debtgdp)

		keep if year>=1970 & year<=2020
		gen miss = missing(rgdpmad) | missing(debtgdp)
		list iso year if miss==1

		* Missing Belgium 1980-1981 and Denmark 1997
		drop if missing(rgdpmad) | missing(debtgdp)


	/*--------------------------------------------------------------------
	---			Construct Outcome Variables	(Dec. 10 11:27am)		--
	--------------------------------------------------------------------*/
	xtset iso_id year
	
		***** Real GDP Growth (Main Outcome) Log Difference *****
		gen d_rgdp = 100*(log(rgdpmad) - log(L.rgdpmad))

		
		***** Nominal GDP Growth (Alternative Robustness) *****
		gen d_gdp = 100*(log(gdp) - log(L.gdp))
		
		***** Real GDP Per Capita (Alternative Robustness) *****
		gen d_rgdppc = 100*(log(rgdpmad/pop) - log(L.rgdpmad/L.pop))
		
		***** Consumption Growth (Alternative Robustness) *****
		gen d_consumption = 100*(log(rconsbarro) - log(L.rconsbarro))
		
		***** Investment Growth (Alternative Robustness) *****
		gen d_investment = 100*(log(expenditure) - log(L.expenditure))

		***** CPI/Inflation (Alternative Robustness) *****
		gen inf = 100*(log(cpi) - log(L.cpi))
		
		**** Real business Credit Growth (Alternative Robustness) *****
		gen d_credit = 100*(log(tloans) - log(L.tloans))
		
		**** Unemployment (Alternative Robustness) *****
		gen d_unemp = unemp - L.unemp
		



	/*--------------------------------------------------------------------
	---			Construct Threshold Variables	(Dec. 10 1:00pm)		--
	--------------------------------------------------------------------*/


		**** Public Debt Thresholds (Main Treatment Variable) *****
			gen high60 = debtgdp > 0.60
			gen high70 = debtgdp > 0.70
			gen high80 = debtgdp > 0.80
			gen high90 = debtgdp > 0.90
			
				** Check countries **
					foreach tau in 60 70 80 90 {

					di "======================"
					di " Threshold = `tau'% "
					di "======================"

					* treated status ever
					bys iso_id: egen ever_high`tau' = max(high`tau')
					tab ever_high`tau'

					* identify first crossing year (when treatment switches from 0 → 1)
					bys iso_id (year): gen cross`tau' = high`tau'==1 & L.high`tau'==0

					di "First crossing years for tau=`tau'%"
					tab year if cross`tau'==1

					* drop intermediate flags (clean controls)
					drop ever_high`tau' cross`tau'
					}

			
		***** Private Debt Thresholds (Alternative Robustness) *****
			gen privdebt = tloans / gdp
			list iso year tloans gdp privdebt if missing(privdebt)
			

			summ privdebt
			
					** Exploration **		
						twoway (kdensity privdebt, lcolor(maroon) lwidth(medthick)), ///
						title("Kernel Density of Private Debt-to-GDP") ///
						xtitle("Debt/GDP") ytitle("Density") ///
						graphregion(color(white))
						graph export "PrivateDebt_KernelDensity.png", replace
	
						twoway ///
						(kdensity debtgdp,  lcolor(navy)    lwidth(medthick)) ///
						(kdensity privdebt, lcolor(maroon)  lpattern(dash) lwidth(medthick)), ///
						legend(order(1 "Public Debt/GDP" 2 "Private Debt/GDP")) ///
						title("Public vs Private Debt Distributions") ///
						xtitle("Debt-to-GDP Ratio") ytitle("Kernel Density") ///
						graphregion(color(white))
						graph export "CombineDebt_KernelDensity.png", replace

						** These share a very similar distribution, with private debt having more ///
						pronounced increases/decreases and a larger magnitude. This makes private debt ///
						an interesting proxy. **
	
						preserve
						collapse (mean) mean_pub=debtgdp mean_priv=privdebt ///
						(sd) sd_pub=debtgdp sd_priv=privdebt, by(year)

						twoway (line mean_pub year, lcolor(navy)) ///
						(line mean_priv year, lcolor(maroon)), ///
						title("Average Debt Ratios Over Time") ytitle("Debt/GDP")
						graph export "AvgDebtRatios_OverPeriod.png", replace
						restore
							*---------------------------------   Quick Note on Twoway.  AvgDebt -----------------------------------------*
						
								* Interesting divergence between public and private debt around the mid-1990s and again in the 	mid-2000s, 		///
									-> private debt surged while public debt remained stable or declined ///
																												///
										Consistent with the financial lib./credit-boom episodes in Jordà, Schularick, and Taylor (2016,2020)			///
											-> Building up of household and corporate leverage 														///
											-> After the 2008 crisis, the relationship reversed														///
																												///
										Evidence that high public debt often emerges endogenously after private-sector balance sheet recessions rather than causing them


		* Missing Japan Private Debt Data 2018-2020*
		drop if missing(privdebt)

			
			gen high_priv100 = privdebt > 1.0
			gen high_priv120 = privdebt > 1.2
			gen high_priv135 = privdebt > 1.35
			gen high_priv140 = privdebt >1.4
			gen high_priv150 = privdebt > 1.5

			
			foreach tau in 100 120 135 140 150 {
			
			** Check countries **
				di "======================"
				di " Private Debt Threshold = `tau'% "
				di "======================"

				gen highp`tau' = privdebt > (`tau'/100)

				bys iso_id: egen ever_highp`tau' = max(highp`tau')
				tab ever_highp`tau'

				bys iso_id (year): gen crossp`tau' = highp`tau'==1 & L.highp`tau'==0
				tab year if crossp`tau'==1

				drop highp`tau' ever_highp`tau' crossp`tau'
				}
				
		/*---------------------------------------------------------------*
			Diagnostics: Which Checking Threshold Obs and Variation (Dec 10 12:25am)
		*---------------------------------------------------------------*/

		di " "
		di "==============================="
		di "   DIAGNOSTIC: Treatment Timing"
		di "==============================="
		di " "


		local allT high60 high70 high80 high90 high_priv100 high_priv120 high_priv135 high_priv140 high_priv150

		foreach T of local allT {

			di "----------------------------------------"
			di "   Checking Treatment Variable: `T'"
			di "----------------------------------------"

			* 1. How many countries ever exceed this threshold?
			bys iso_id: egen ever_`T' = max(`T')
			tab ever_`T'

			* 2. When do countries FIRST cross the threshold?
			bysort iso_id (year): gen first_`T' = (`T'==1 & L.`T'==0)
			di "First crossing years:"
			tab year if first_`T'==1

			* 3. How many treated obs per horizon (data availability proxy)
			di "Observation count under this treatment:"
			count if `T'==1

			* Clean up
			drop ever_`T' first_`T'
		}


		** Originally, I used 2 SD for private debt with the highest threshold at 150% ///
		This was only 37 observations. With trial and error I moved it down to 135% to get 85 observations ///
		
		

		/*======================================================================
			Baseline and Robustness Controls
		======================================================================*/

		*-------------------------------
		* 1. Baseline
		*-------------------------------
		
		* Trade Openness
		capture drop trade_open
		gen trade_open = (imports + exports) / gdp if !missing(imports, exports, gdp)
		gen L_trade_open   = L.trade_open if !missing(L.trade_open)


		* Credit Growth (change in private debt-GDP)
		capture drop credit_growth
		gen credit_growth = 100 * (privdebt - L.privdebt) if !missing(privdebt, L.privdebt)
		gen L_credit_growth = L.credit_growth if !missing(L.credit_growth)

		* Crisis dummy (JST) (1 in crisis years, 0 otherwise)
		capture drop crisis_lag
		gen crisis_lag = L.crisisJST if !missing(L.crisisJST)
		
		
		*Unemployment
		gen L_unemp        = L.unemp if !missing(L.unemp)
		
		* Current Account Balance in % of GDP
			* I did this to keep range/units of measurement more in tact. L_ca range was between -16B and +5B
		gen ca_gdp = (ca / gdp) * 100
		gen ln_ca_gdp = log(ca_gdp)
		gen L_ln_ca_gdp = L.ln_ca_gdp

		

		* Contemporaneous Controls for Baseline - Log Population
			* I did this to keep units of measurement more similar across variables
		gen ln_pop = log(pop)

		
		*-------------------------------
		* 2. Robustness Controls
		*-------------------------------
		* Real Housing Return
		capture drop  L_housing_tr
			gen L_housing_tr  = L.housing_tr if !missing(L.housing_tr)
		
		*bill_rate bond_tr (conetemporaneous for robustness)
			*/ (Jorda 2005; Ramey-Zubairy; Gertler-Karadi; Schularick–Taylor), financial conditions that ///
			measure the price of credit today are typically included contemporaneously */
	

	save "austerity_clean", replace


	/*---------------------------------------------------------------*
			Data Descriptions:  (Dec 10 5:11pm)
	*---------------------------------------------------------------*/

		clear
		use "austerity_clean.dta", clear     

		/*****************************************************************
		*  A. Summary Stats: Outcomes + Debt Variables
		*****************************************************************/

		* Define variable list
		local outcomes d_rgdp d_gdp d_rgdppc d_consumption d_investment ///
				inf d_credit d_unemp debtgdp privdebt

		* Post stats including median
		estpost tabstat `outcomes', ///
			statistics(count mean median sd min max) ///
			columns(statistics)

		esttab using "Table1_SummaryStats.tex", ///
			cells("count mean median sd min max") ///
			label nomtitle nonumber replace ///
			title("Summary Statistics: Outcomes and Debt Variables") ///
			varlabels( ///
				d_rgdp        "Real GDP growth" ///
				d_gdp         "Nominal GDP growth" ///
				d_rgdppc      "Real GDP per capita growth" ///
				d_consumption "Consumption growth" ///
				d_investment  "Investment growth" ///
				inf           "Inflation" ///
				d_credit      "Credit growth" ///
				d_unemp       "Change in unemployment" ///
				debtgdp       "Public debt (% of GDP)" ///
				privdebt      "Private debt (% of GDP)" ///
			)



		/*****************************************************************
		*  B. Summary Stats: Treatment Indicators
		*****************************************************************/

		local treats high60 high70 high80 high90 high_priv100 high_priv120 high_priv135

		estpost summarize `treats'

		esttab using "Table2_TreatmentDummyStats.tex", ///
			cells("count mean sd min max") ///
			label nomtitle nonumber replace ///
			title("Summary Statistics: Treatment Indicators (Debt Thresholds)") ///
			varlabels( ///
				high60        "Public debt > 60%" ///
				high70        "Public debt > 70%" ///
				high80        "Public debt > 80%" ///
				high90        "Public debt > 90%" ///
				high_priv100  "Private debt > 100%" ///
				high_priv120  "Private debt > 120%" ///
				high_priv135  "Private debt > 135%" ///
			)


		/*****************************************************************
		*  C. Country-Level Average Debt + Growth
		*****************************************************************/

		preserve

		collapse (mean)  avg_rgdp=d_rgdp avg_pub=debtgdp avg_priv=privdebt ///
				(median) med_pub=debtgdp, by(iso_id)

		estpost summarize avg_rgdp avg_pub avg_priv med_pub

		esttab using "Table3_CountryLevel.tex", ///
			cells("mean sd min max") ///
			label nonumber replace ///
			title("Country-Level Descriptive Statistics")

		restore


		/*****************************************************************
		*  D. Correlation Matrix
		*****************************************************************/

		* Correlate chosen macro variables
		correlate debtgdp privdebt d_rgdp d_credit d_consumption d_investment

		matrix C = r(C)

		esttab matrix(C) using "Table4_CorrMatrix.tex", replace ///
			title("Correlation Matrix: Debt and Macroeconomic Variables")


		/*****************************************************************
		*  E. Summary Stats: Baseline & Robustness Controls (4:39pm)
		*****************************************************************/

			*---------------------------------------------------------------*
			* Print descriptive statistics for controls directly in Stata
			*---------------------------------------------------------------*

			local controls L_trade_open L_unemp L_ln_ca_gdp L_credit_growth L_housing_tr ///
               bill_rate bond_tr crisis_lag ln_pop

			quietly count
			local total = r(N)

			display as text "-------------------------------------------------------------------------"
			display as text "Variable               Count     Mean     Median     SD      Min      Max   %Missing"
			display as text "-------------------------------------------------------------------------"

			foreach v of local controls {

				quietly summarize `v', detail

				local count  = r(N)
				local mean   = r(mean)
				local median = r(p50)
				local sd     = r(sd)
				local min    = r(min)
				local max    = r(max)
				local miss   = 100 * (`total' - `count') / `total'

				display as result ///
				"`v' " ///
					%12.0f `count' "  " ///
					%9.4f `mean'  "  " ///
					%9.4f `median' "  " ///
					%9.4f `sd' "  " ///
					%9.4f `min' "  " ///
					%9.4f `max' "  " ///
					%9.2f `miss'
			}
	
			display as text "-------------------------------------------------------------------------"
	
		/*---------------------------------------------------------------*
			Restricted Sample based on prelim summary review
		----------------------------------------------------------------*/
		
		keep if !missing(d_rgdp, debtgdp, privdebt, L_trade_open, L_unemp, ///
                 L_credit_growth, L_housing_tr, crisis_lag, ln_pop)
		count
		*L_ln_ca_gdp, bill_rate, bond_tr are not considered since they will be used for robustness anyway
		* This gives us a final number 769 / 912, that keeps 84% of our data while allowing for consistent review across methods
		* Data set now shrinks to 1972-2020, This works well because it avoids Bretton-Woods Years.
		save "austerity_rest", replace
		
		
	
	
	
	
/*********************************************************************************************
**				Final Summary Stats with Restricted Sample	 (Dec. 10 6:53pm )			    **
**********************************************************************************************/
	use "austerity_rest.dta", clear
	count /// sanity check


	/*-------------------------------------------------------------------------
		(sanity check)	Kernel Density of restricted Samples and Twoway Plots
	-------------------------------------------------------------------------*/
	
	twoway (kdensity privdebt, lcolor(maroon) lwidth(medthick)), ///
						title("Kernel Density of Private Debt-to-GDP") ///
						xtitle("Debt/GDP") ytitle("Density") ///
						graphregion(color(white))
						graph export "RESTRICTED_PrivateDebt_KernelDensity.png", replace
	
						twoway ///
						(kdensity debtgdp,  lcolor(navy)    lwidth(medthick)) ///
						(kdensity privdebt, lcolor(maroon)  lpattern(dash) lwidth(medthick)), ///
						legend(order(1 "Public Debt/GDP" 2 "Private Debt/GDP")) ///
						title("Public vs Private Debt Distributions") ///
						xtitle("Debt-to-GDP Ratio") ytitle("Kernel Density") ///
						graphregion(color(white))
						graph export "RESTRICTED_CombineDebt_KernelDensity.png", replace

						** These share a very similar distribution, with private debt having more ///
						pronounced increases/decreases and a larger magnitude. This makes private debt ///
						an interesting proxy. **
	
						preserve
						collapse (mean) mean_pub=debtgdp mean_priv=privdebt ///
						(sd) sd_pub=debtgdp sd_priv=privdebt, by(year)

						twoway (line mean_pub year, lcolor(navy)) ///
						(line mean_priv year, lcolor(maroon)), ///
						title("Average Debt Ratios Over Time") ytitle("Debt/GDP")
						graph export "RESTRICTED_AvgDebtRatios_OverPeriod.png", replace
						restore
	
	/*-------------------------------------------------------------------------
		Restricted Sample Threshold Diagnostics
	-------------------------------------------------------------------------*/
	
		di " "
		di "==============================="
		di "   DIAGNOSTIC: Treatment Timing"
		di "==============================="
		di " "


		local allT high60 high70 high80 high90 high_priv100 high_priv120 high_priv135 high_priv140 high_priv150

		foreach T of local allT {

			di "----------------------------------------"
			di "   Checking Treatment Variable: `T'"
			di "----------------------------------------"

			* 1. How many countries ever exceed this threshold?
			bys iso_id: egen ever_`T' = max(`T')
			tab ever_`T'

			* 2. When do countries FIRST cross the threshold?
			bysort iso_id (year): gen first_`T' = (`T'==1 & L.`T'==0)
			di "First crossing years:"
			tab year if first_`T'==1

			* 3. How many treated obs per horizon (data availability proxy)
			di "Observation count under this treatment:"
			count if `T'==1

			* Clean up
			drop ever_`T' first_`T'
		}

		
	/*-------------------------------------------------------------------------
		Restricted Sample - Control Variable Summary Stats
	-------------------------------------------------------------------------*/
		local controls L_trade_open L_unemp L_ln_ca_gdp ///
               L_credit_growth L_housing_tr ///
               bill_rate bond_tr crisis_lag ln_pop

		estpost summarize `controls', detail

		esttab using "Table_ControlStats.tex", ///
			cells("count mean p50 sd min max") ///
			label replace nonumber ///
			title("Summary Statistics: Baseline and Robustness Controls (Restricted Sample)") ///
			varlabels( ///
				L_trade_open     "Lagged trade openness" ///
				L_unemp          "Lagged unemployment rate (\%)" ///
				L_ln_ca_gdp      "Lagged log(CA/GDP)" ///
				L_credit_growth  "Lagged credit growth (\%)" ///
				L_housing_tr     "Lagged real housing return" ///
				bill_rate        "Short-term interest rate" ///
				bond_tr          "Bond trading volume" ///
				crisis_lag       "Crisis indicator (t-1)" ///
				ln_pop           "Log population" ///
			)


	/*-------------------------------------------------------------------------
		Restricted Sample - Threatment Summary Statistics
	-------------------------------------------------------------------------*/
		local treats high60 high70 high80 high90 ///
             high_priv100 high_priv120 high_priv135

		estpost summarize `treats'

		esttab using "Table_TreatmentStats.tex", ///
			cells("count mean sd min max") ///
			label replace nonumber ///
			title("Summary Statistics: Treatment Indicators (Restricted Sample)") ///
			varlabels( ///
				high60        "Public debt > 60\%" ///
				high70        "Public debt > 70\%" ///
				high80        "Public debt > 80\%" ///
				high90        "Public debt > 90\%" ///
				high_priv100  "Private debt > 100\%" ///
				high_priv120  "Private debt > 120\%" ///
				high_priv135  "Private debt > 135\%" ///
			)

	/*-------------------------------------------------------------------------
		Restricted Sample - Outcome Summary Statistics
	-------------------------------------------------------------------------*/
		local outcomes d_rgdp d_gdp d_rgdppc d_consumption ///
               d_investment inf d_credit d_unemp ///
               debtgdp privdebt

		estpost summarize `outcomes', detail

		esttab using "Table_OutcomeStats.tex", ///
			cells("count mean p50 sd min max") ///
			label replace nonumber ///
			title("Summary Statistics: Outcomes and Debt Variables (Restricted Sample)") ///
			varlabels( ///
				d_rgdp        "Real GDP growth" ///
				d_gdp         "Nominal GDP growth" ///
				d_rgdppc      "Real GDP per capita growth" ///
				d_consumption "Consumption growth" ///
				d_investment  "Investment growth" ///
				inf           "Inflation" ///
				d_credit      "Credit growth" ///
				d_unemp       "Change in unemployment" ///
				debtgdp       "Public debt (% of GDP)" ///
				privdebt      "Private debt (% of GDP)" ///
			)


	/*-------------------------------------------------------------------------
		Preliminary Review Real GDP Growth V. Public Debt Scatter (Dec 10 7:32pm)
	-------------------------------------------------------------------------*/
		twoway ///
			(scatter d_rgdp L.debtgdp, mcolor(gs12) msymbol(o) msize(small)) ///
			(lfit d_rgdp L.debtgdp, lcolor(navy)), ///
			xtitle("Lagged Public Debt (% of GDP)") ///
			ytitle("Real GDP Growth (t)") ///
			title("Debt and Growth: Unconditional Relationship", size(med)) ///
			legend(off) graphregion(color(white))
			graph export "RGDP_PubDebt_Scatter.png", replace


	/*-------------------------------------------------------------------------
		Preliminary Review Real GDP Growth V. Public Debt Scatter (Binned) ( Dec 10 7:32pm)
	-------------------------------------------------------------------------*/
		preserve
			* 20 equally-sized bins by public debt
			xtile debt_bin = L.debtgdp, n(20)

			collapse (mean) d_rgdp Ldebt=L.debtgdp, by(debt_bin)

			twoway ///
				(scatter d_rgdp Ldebt, msymbol(O) msize(med) mcolor(navy)) ///
				(lfit d_rgdp Ldebt, lcolor(maroon)), ///
				xtitle("Lagged Public Debt (% of GDP)") ///
				ytitle("Mean Real GDP Growth") ///
				title("Binned Scatter: Debt and Growth") ///
				legend(off) graphregion(color(white))
				graph export "RGDP_PubDebt_BinnedScatter.png", replace
		restore
		
	/*-------------------------------------------------------------------------
		Preliminary Review   Public V.  Debt Joint Distribution ( Dec 10 7:32pm)
	-------------------------------------------------------------------------*/
		twoway scatter privdebt debtgdp, ///
			msymbol(o) mcolor(gs12) ///
			xtitle("Public Debt (% of GDP)") ///
			ytitle("Private Debt (% of GDP)") ///
			title("Joint Distribution of Public and Private Debt") ///
			legend(off) graphregion(color(white))
			graph export "Pub_Priv_JointDist.png", replace
			
		
		
		
		* Other Option
		preserve
		collapse (mean) mean_pub = debtgdp (mean) mean_priv = privdebt ///
         (sd) sd_pub = debtgdp (sd) sd_priv = privdebt, by(year)

		gen ub_pub = mean_pub + sd_pub
		gen lb_pub = mean_pub - sd_pub

		gen ub_priv = mean_priv + sd_priv
		gen lb_priv = mean_priv - sd_priv

	
	twoway ///
    (rarea ub_pub lb_pub year, color(blue%20) legend(off)) ///
    (line mean_pub year, lcolor(blue) lwidth(med) legend(on)) ///
    (rarea ub_priv lb_priv year, color(red%20) legend(off)) ///
    (line mean_priv year, lcolor(red) lwidth(med) legend(on)) ///
    , ///
    legend(order(2 "Public Debt" 4 "Private Debt") pos(11) ring(0)) ///
    ytitle("Debt (% of GDP)") ///
    xtitle("Year") ///
    title("Average Public and Private Debt Over Time")
	graph export "Pub_Priv_Time.png", replace
	restore
	

	/*-------------------------------------------------------------------------
		Preliminary Review   Pre-trends using Reinhart Rogoff and 80% ( Dec 10 8:25pm)
	-------------------------------------------------------------------------*/
		* Define group: countries ever crossing 90% debt
		preserve
			bys iso_id: gen ever90 = (sum(high90) > 0)

			collapse (mean) d_rgdp, by(year ever90)

			twoway ///
				(line d_rgdp year if ever90==0, lcolor(navy) lpattern(solid)) ///
				(line d_rgdp year if ever90==1, lcolor(maroon) lpattern(dash)), ///
				legend(order(1 "Never > 90%" 2 "Ever > 90%")) ///
				xtitle("Year") ytitle("Average Real GDP Growth") ///
				title("Group Pre-Trends in GDP Growth: High vs. Low Debt Countries") ///
				graphregion(color(white))
				graph export "Puretrends_90.png", replace
		restore
				
		* Define group: countries ever crossing 80% debt
		preserve
			bys iso_id: gen ever80 = (sum(high80) > 0)

			collapse (mean) d_rgdp, by(year ever80)

			twoway ///
				(line d_rgdp year if ever80==0, lcolor(navy) lpattern(solid)) ///
				(line d_rgdp year if ever80==1, lcolor(maroon) lpattern(dash)), ///
				legend(order(1 "Never > 80%" 2 "Ever > 80%")) ///
				xtitle("Year") ytitle("Average Real GDP Growth") ///
				title("Group Pre-Trends in GDP Growth: High vs. Low Debt Countries") ///
				graphregion(color(white))
				graph export "Puretrends_80.png", replace
		restore
		
		/* We do not see a significant difference between patterns in treated vs. untreated
			-> satisfies parallel trends assumption needed for LP-DiD						*/
				

	/*----------------------------------------------------------
		Preliminary Review 	Clean and Staggered Treatment  Dec 10 9:23pm
	----------------------------------------------------------*/
		preserve
		capture drop ever90
		bys iso_id: egen ever90 = max(high90)

		di " "
		di "==============================================="
		di " Ever-treated vs Never-treated (Public Debt > 90%)"
		di "==============================================="
		tab ever90
		* Interpretation: number of countries with at least one year > 90%

		restore


		preserve
		capture drop first_high90
		bys iso_id (year): gen first_high90 = (high90 == 1 & L.high90 == 0)

		di " "
		di "==============================================="
		di " First Year Countries Cross Public Debt > 90%"
		di "==============================================="
		tab year if first_high90 == 1, sort

		restore
		
		*  About half of countries (49%) ever exceed the 90% public-debt threshold, indicating substantial variation in treatment status for identification. ///
																																							 ///
			The first-crossing years are staggered (between 1990 and 2020)																					///
				-> supports the use of staggered-adoption LP–DiD methods n(o single historical event drives treatment timing)								*/


	/*----------------------------------------------------------
		Preliminary Review 	Corr/Cov Matrix of Control Dec 10 9:54pm
	----------------------------------------------------------*/
		local controls L_trade_open L_unemp L_ln_ca_gdp ///
					   L_credit_growth L_housing_tr ///
					   bill_rate bond_tr crisis_lag ln_pop

		* Correlation matrix
		corr `controls'
		matrix list r(C)

		* Covariance matrix
		correlate `controls', covariance
		matrix list r(C)
		
		* Some of our robust controls (CA balance and Short-term rate) have high correlation with eachother CA is also highly correlated with ln_pop  	///
			-> Short term interest (Obstfeld & Rogoff (1996, 2003) support persistent CA surpluses occur in economies with low interest rates, 			///
				high savings, and strong net foreign asset positions																					///
																																						///
			-> Aizenman & Pinto (2005), Shows that volatility of CA/GDP declines with country and size. 												///
																																						///
		  Our baseline controls do not suffer as much from this problem, but ln_pop is negatively correlated at .39. 									///
			->  Large countries are an indicator of restricted trade Helpman, Melitz & Rubinstein (2008)
			
		* VIF
		reg d_rgdp L_trade_open L_unemp L_ln_ca_gdp L_credit_growth L_housing_tr bill_rate bond_tr crisis_lag ln_pop
		vif

			/* Our VIF table showes no control VIF > 1.70
					-> below conventional thresholds (e.g., 5 = moderate concern, 10 = severe concern).
					->  mean VIF is 1.33, indicating that the variance of each coefficient is inflated by only 33% relative to the ideal case of no correlation.

				Observed correlations among controls (e.g., between population, CA/GDP, and short-term rates) are well understood in macroeconomic theory
					-> we proceed with LP-DiD estimation using the full set of controls 																		*/



					
		/*--------------------------------------------------------------*
			  Preliminary Review Crisis Windows    
		*--------------------------------------------------------------*/
		* Reconstruct crisis_t from crisis_lag (crisis last year)
			bys iso_id (year): gen crisis = F.crisis_lag   // crisis in t if lag=1 in t-1
			replace crisis = 0 if missing(crisis)
			
			
					xtset iso_id year

			*--------------------------------------------------------------*
			*  Define crisis episodes and 3-year post-crisis window     *
			*--------------------------------------------------------------*
			
			* Identify the start year of each crisis episode (per country)
			bys iso_id (year): gen crisis_start = (crisis == 1 & L.crisis == 0)

			* Allow for multiple crises per country: episode index
			bys iso_id (year): gen crisis_ep = sum(crisis_start)

			* Keep only obs that ever belong to a crisis episode
			replace crisis_ep = . if crisis_ep == 0

			* For each (country, episode) pair, find the crisis start year
			egen crisis_start_year = min(cond(crisis_start==1, year, .)), ///
				by(iso_id crisis_ep)

			* Relative year to the start of the episode
			gen rel_year = year - crisis_start_year if crisis_ep < .

			* 3-year post-crisis window: crisis year t, t+1, t+2, t+3
			gen in_crisis_window_3y = inrange(rel_year, 0, 3)

			label var in_crisis_window_3y ///
				"1 if within 0–3 years after crisis start"

					/*-----------------------------------------------------------------------
						1. Crisis Freq. per Country
					------------------------------------------------------------------------*/

					bys iso_id: egen crisis_count = total(crisis_start)
					tab crisis_count, missing

					* Export table: country × # of crises
					preserve
					collapse (sum) crisis_count, by(iso_id iso)
					export delimited using "CrisisFreq_ByCountry.csv", replace
					restore
					
					* 55.79% of observations will see one crisis (year x country), and as many as 3 within my sample *
					
					preserve
					collapse (max) crisis_count, by(iso_id iso)

					tab crisis_count
					restore
					* 9 out of 16, or 56% experience exactl 1 crisis, 6 or 37.50% will experience exactly 2. Only one country will experience 3.
					
						
						preserve
						collapse (sum) crisis_start, by(iso)
						rename crisis_start crisis_count

						levelsof iso if crisis_count == 3, local(countries)
						restore

						foreach c of local countries {
							display "========================="
							display "Crisis years for `c'"
							display "========================="
							list iso year crisis_start if iso == "`c'" & crisis_start == 1
						}
						* That country is great britian in 1974, 1991, and 2007 ///
								-> 2007 global financial crisis					///
								-> 1991 UK Banking Crisis						///
								-> 1974 Secondary Banking Crisis (Safeboat Operation) ///



					/*-----------------------------------------------------------------------
						2. Crisis Timining (First per Country)
					------------------------------------------------------------------------*/

					preserve
					keep if crisis_start == 1
					bys iso_id: egen first_crisis_year = min(year)
					keep iso iso_id first_crisis_year
					export delimited using "CrisisTiming_FirstCrisisYear.csv", replace
					restore
					
					/*-----------------------------------------------------------------------
						3. # Obs in 3-year Crisis Window
					------------------------------------------------------------------------*/

					bys iso_id: egen crisis_window_obs = total(in_crisis_window_3y)
					preserve
					collapse (sum) crisis_window_obs, by(iso_id iso)
					export delimited using "CrisisWindowObs_ByCountry.csv", replace
					restore

					/*-----------------------------------------------------------------------
						4. Probability of Crossing Public Debt Thresh During Crisis Window
					------------------------------------------------------------------------*/
					local pubT high60 high80 high90

					foreach T of local pubT {

						di "===================================================="
						di "   PUBLIC DEBT THRESHOLD: `T'"
						di "===================================================="

						* First crossing of threshold
						bys iso_id (year): gen cross_`T' = (`T'==1 & L.`T'==0)

						* Crossing during crisis window?
						gen cross_crisis_`T' = cross_`T' == 1 & in_crisis_window_3y == 1

						* Count events
						count if cross_`T' == 1
						local total_cross = r(N)

						count if cross_crisis_`T' == 1
						local crisis_cross = r(N)

						di "Total crossings of `T': `total_cross'"
						di "Crossings during crisis window: `crisis_cross'"
						di "Probability = " 100 * (`crisis_cross' / `total_cross') "%"

						drop cross_`T' cross_crisis_`T'
					}

					/*-----------------------------------------------------------------------
						5. Probability of Crossing Private Thresh During Crisis Window
					------------------------------------------------------------------------*/
					local privT high_priv100 high_priv120 high_priv135

					foreach T of local privT {

						di "===================================================="
						di "   PRIVATE DEBT THRESHOLD: `T'"
						di "===================================================="

						bys iso_id (year): gen cross_`T' = (`T'==1 & L.`T'==0)

						gen cross_crisis_`T' = cross_`T' == 1 & in_crisis_window_3y == 1

						count if cross_`T' == 1
						local total_cross = r(N)

						count if cross_crisis_`T' == 1
						local crisis_cross = r(N)

						di "Total crossings of `T': `total_cross'"
						di "Crossings during crisis window: `crisis_cross'"
						di "Probability = " 100 * (`crisis_cross' / `total_cross') "%"

						drop cross_`T' cross_crisis_`T'
					}


/*********************************************************************************************
**  LP–DiD Master Pipeline: Baseline, Crisis-Window, Robustness (Dec 11 1:27am)
*********************************************************************************************/

	clear all
	set more off
	clear
		capture postclose _all
		macro drop _all
		clear matrix
		

	*-------------------------------------------------------------------------------
	* 0. Paths & folders
	*-------------------------------------------------------------------------------
	global root "/Users/maciwoyat/Desktop/Econ706"
	cd "$root/Data"

	capture mkdir LP_DiD_Output
	capture mkdir LP_DiD_Output/IRFs
	capture mkdir LP_DiD_Output/Panels
	capture mkdir LP_DiD_Output/Diagnostics
	capture mkdir LP_DiD_Output/Tables

	use "austerity_rest.dta", clear

	* Panel structure
	xtset iso_id year


	*-------------------------------------------------------------------------------
	* 1. Crisis reconstruction & 3-year window (for diagnostics + robustness spec)
	*-------------------------------------------------------------------------------

	* crisis_t from lagged crisis indicator
	capture drop crisis
	bys iso_id (year): gen crisis = F.crisis_lag   // crisis in t if lag=1 in t-1
	replace crisis = 0 if missing(crisis)

	* Identify crisis episodes
	capture drop crisis_start crisis_ep crisis_start_year rel_year in_crisis_window_3y

	bys iso_id (year): gen crisis_start = (crisis == 1 & L.crisis == 0)
	bys iso_id (year): gen crisis_ep    = sum(crisis_start)
	replace crisis_ep = . if crisis_ep == 0

	egen crisis_start_year = min(cond(crisis_start==1, year, .)), ///
		by(iso_id crisis_ep)

	gen rel_year = year - crisis_start_year if crisis_ep < .

	* 3-year post-crisis window: t, t+1, t+2, t+3
	gen in_crisis_window_3y = inrange(rel_year, 0, 3)
	label var in_crisis_window_3y "1 if within 0–3 years after crisis start"


	*-------------------------------------------------------------------------------
	* 2. Construct outcome variables (log diffs & levels)
	*-------------------------------------------------------------------------------

	local outcomes d_rgdp d_gdp d_rgdppc d_consumption d_investment inf d_credit d_unemp
	local lhs      rgdpmad gdp rgdpmad rconsbarro expenditure cpi tloans unemp
	local type     log     log log     log        log       log level  level

	* Remove any existing versions
	foreach v of local outcomes {
		capture drop `v'
	}

	local i = 1
	foreach var of local lhs {
		local t   : word `i' of `type'
		local out : word `i' of `outcomes'

		if "`t'" == "log" {
			gen `out' = 100*( log(`var') - log(L.`var') )
		}
		else {
			gen `out' = `var' - L.`var'
		}

		local ++i
	}

	* Sanity check
	summ d_rgdp d_gdp d_rgdppc d_consumption d_investment inf d_credit d_unemp


	*-------------------------------------------------------------------------------
	* 3. LP–DiD master loop: 3 specs × outcomes × thresholds × horizons
	*-------------------------------------------------------------------------------

	cd "$root/Data/LP_DiD_Output"

	* Thresholds to use
	local thresholds_all  high60 high70 high80 high90 ///
						  high_priv100 high_priv120 high_priv135

	* Outcomes (already defined above)
	local outcomes d_rgdp d_gdp d_rgdppc d_consumption d_investment inf d_credit d_unemp

	* Horizons
	local horizons 0 1 2 3 4 5 6 7 8 9 10

	* Controls
	local baseline_ctrls  L_trade_open L_unemp L_credit_growth ///
						  L_housing_tr crisis_lag ln_pop

	local robust_ctrls    `baseline_ctrls' L_ln_ca_gdp bill_rate bond_tr

	* Master results file (all specs together)
	local masterfile "LP_results_master_all.dta"
	capture postclose masterpf
	postfile masterpf str15 spec str20 outcome str20 treat ///
			int horizon double b se using "`masterfile'", replace


	*=====================================================================
	* Spec 1: baseline_full  (no crisis-window restriction)
	*=====================================================================

	local spec "baseline_full"

	foreach Y of local outcomes {

		di "=============================================="
		di "   SPEC: `spec' | OUTCOME: `Y'"
		di "=============================================="

		foreach T of local thresholds_all {

			di "------- Treatment: `T' (baseline_full) --------"

			capture drop shock
			bys iso_id (year): gen shock = (`T'==1 & L.`T'==0)

			quietly count if shock == 1
			if r(N) == 0 {
				di "No threshold crossings for `T' in spec `spec' — skipping."
				drop shock
				continue
			}

			foreach h of local horizons {

				capture drop Y_h

				if "`Y'" == "d_rgdp" {
					gen Y_h = 100*( log(F`h'.rgdpmad) - log(L.rgdpmad) )
				}
				else {
					gen Y_h = F`h'.`Y'
				}

				quietly xtreg Y_h shock `baseline_ctrls' i.year, fe vce(cluster iso_id)

				matrix b = e(b)
				matrix V = e(V)

				scalar beta = b[1,"shock"]
				scalar se   = sqrt(V[1,1])

				post masterpf ("`spec'") ("`Y'") ("`T'") (`h') (beta) (se)
			}

			drop shock
		}
	}


	*=====================================================================
	* Spec 2: baseline_cw3y  (shock only if crossing within 0–3y post-crisis)
	*=====================================================================

	local spec "baseline_cw3y"

	foreach Y of local outcomes {

		di "=============================================="
		di "   SPEC: `spec' | OUTCOME: `Y'"
		di "=============================================="

		foreach T of local thresholds_all {

			di "------- Treatment: `T' (baseline_cw3y) --------"

			capture drop shock_raw shock
			bys iso_id (year): gen shock_raw = (`T'==1 & L.`T'==0)
			gen shock = shock_raw * in_crisis_window_3y   // keep only crisis-window crossings

			quietly count if shock == 1
			if r(N) == 0 {
				di "No crisis-window crossings for `T' in spec `spec' — skipping."
				drop shock_raw shock
				continue
			}

			foreach h of local horizons {

				capture drop Y_h

				if "`Y'" == "d_rgdp" {
					gen Y_h = 100*( log(F`h'.rgdpmad) - log(L.rgdpmad) )
				}
				else {
					gen Y_h = F`h'.`Y'
				}

				quietly xtreg Y_h shock `baseline_ctrls' i.year, fe vce(cluster iso_id)

				matrix b = e(b)
				matrix V = e(V)

				scalar beta = b[1,"shock"]
				scalar se   = sqrt(V[1,1])

				post masterpf ("`spec'") ("`Y'") ("`T'") (`h') (beta) (se)
			}

			drop shock_raw shock
		}
	}


	*=====================================================================
	* Spec 3: robust_full  (no crisis restriction, baseline + robustness controls)
	*=====================================================================

	local spec "robust_full"

	foreach Y of local outcomes {

		di "=============================================="
		di "   SPEC: `spec' | OUTCOME: `Y'"
		di "=============================================="

		foreach T of local thresholds_all {

			di "------- Treatment: `T' (robust_full) --------"

			capture drop shock
			bys iso_id (year): gen shock = (`T'==1 & L.`T'==0)

			quietly count if shock == 1
			if r(N) == 0 {
				di "No threshold crossings for `T' in spec `spec' — skipping."
				drop shock
				continue
			}

			foreach h of local horizons {

				capture drop Y_h

				if "`Y'" == "d_rgdp" {
					gen Y_h = 100*( log(F`h'.rgdpmad) - log(L.rgdpmad) )
				}
				else {
					gen Y_h = F`h'.`Y'
				}

				quietly xtreg Y_h shock `robust_ctrls' i.year, fe vce(cluster iso_id)

				matrix b = e(b)
				matrix V = e(V)

				scalar beta = b[1,"shock"]
				scalar se   = sqrt(V[1,1])

				post masterpf ("`spec'") ("`Y'") ("`T'") (`h') (beta) (se)
			}

			drop shock
		}
	}


	* Close postfile and save master results
	postclose masterpf

	use "`masterfile'", clear
	save "`masterfile'", replace
	di "Master LP–DiD results saved: `masterfile'"


		/*********************************************************************************************
		**  4. IRF Construction & Panel Plots by Spec
		*********************************************************************************************/

		cd "$root/Data/LP_DiD_Output"

		use "`masterfile'", clear

		* Confidence bands
		gen ub = b + 1.96*se
		gen lb = b - 1.96*se

		replace spec    = trim(spec)
		replace outcome = trim(outcome)
		replace treat   = trim(treat)

		* Outcome labels
		local outlab_d_rgdp        "Real GDP growth"
		local outlab_d_gdp         "Nominal GDP growth"
		local outlab_d_rgdppc      "Real GDP per capita growth"
		local outlab_d_consumption "Consumption growth"
		local outlab_d_investment  "Investment growth"
		local outlab_inf           "Inflation"
		local outlab_d_credit      "Credit growth"
		local outlab_d_unemp       "Change in unemployment"

		* Treatment labels
		local lab_high60        "Public debt > 60% of GDP"
		local lab_high70        "Public debt > 70% of GDP"
		local lab_high80        "Public debt > 80% of GDP"
		local lab_high90        "Public debt > 90% of GDP"
		local lab_high_priv100  "Private debt > 100% of GDP"
		local lab_high_priv120  "Private debt > 120% of GDP"
		local lab_high_priv135  "Private debt > 135% of GDP"

		levelsof spec, local(specs)

		foreach S of local specs {

			* Work only with this spec
			levelsof outcome if spec == "`S'", local(outcomes_list)
			levelsof treat   if spec == "`S'", local(treat_list)

			*---------------------------------------------------------------
			* A. Individual IRFs (per outcome × threshold) for spec `S'
			*---------------------------------------------------------------
			foreach Y of local outcomes_list {
				foreach T of local treat_list {

					preserve
					keep if spec == "`S'" & outcome == "`Y'" & treat == "`T'"
					sort horizon

					* Look up labels
					capture local Ytitle : local outlab_`Y'
					if _rc local Ytitle "`Y'"

					capture local Ttitle : local lab_`T'
					if _rc local Ttitle "`T'"

					twoway ///
						(rarea ub lb horizon, sort fcolor(gs12%50) lcolor(gs12)) ///
						(line  b   horizon, sort lwidth(medthick) lcolor(navy)), ///
						yline(0, lcolor(gs6) lpattern(dash)) ///
						xtitle("Horizon (years after threshold crossing)") ///
						ytitle("Effect on `Ytitle'") ///
						title("LP-DiD IRF (`S'): `Ytitle'") ///
						subtitle("Treatment: `Ttitle'") ///
						graphregion(color(white))

					graph export "IRFs/IRF_`S'_`Y'_`T'.png", replace
					graph save   "IRFs/IRF_`S'_`Y'_`T'.gph", replace
					restore
				}
			}

			*---------------------------------------------------------------
			* B. Panels: Thresholds Across Outcomes  (fix T, vary Y)
			*---------------------------------------------------------------
			foreach T of local treat_list {

				local panelgraphs ""
				foreach Y of local outcomes_list {
					local panelgraphs "`panelgraphs' IRFs/IRF_`S'_`Y'_`T'.gph"
				}

				graph combine `panelgraphs', ///
					col(4) ///
					title("IRFs for Threshold `T' Across All Outcomes (`S')") ///
					iscale(.8) ///
					graphregion(color(white))

				graph export "Panels/PANEL_`S'_BY_TREATMENT_`T'.png", replace
			}

			*---------------------------------------------------------------
			* C. Panels: Outcomes Across Thresholds (fix Y, vary T)
			*---------------------------------------------------------------
			foreach Y of local outcomes_list {

				local panelgraphs ""
				foreach T of local treat_list {
					local panelgraphs "`panelgraphs' IRFs/IRF_`S'_`Y'_`T'.gph"
				}

				graph combine `panelgraphs', ///
					col(4) ///
					title("IRFs for Outcome `Y' Across All Thresholds (`S')") ///
					iscale(.8) ///
					graphregion(color(white))

				graph export "Panels/PANEL_`S'_BY_OUTCOME_`Y'.png", replace
			}
		}

		di "All IRFs and panel graphs saved in LP_DiD_Output/IRFs and LP_DiD_Output/Panels"
		
		
	/********************************************************************
							Additional Insight:	Dec 11 (9:09am)
		Probability (%) that first crossing occurs in crisis window
	*********************************************************************/
		* Optional, but keeps things clean
		clear
		capture postclose _all
		macro drop _all
		clear matrix
		
		global root "/Users/maciwoyat/Desktop/Econ706"
		cd "$root/Data" 

			use "austerity_rest.dta", clear
			xtset iso_id year

			/*--------------------------------------------------------------
			   Recreate crisis window 
			--------------------------------------------------------------*/

			* crisis_t = crisis in year t if crisis last year in t-1
			bys iso_id (year): gen crisis = F.crisis_lag
			replace crisis = 0 if missing(crisis)

			* identify new crisis start
			bys iso_id (year): gen crisis_start = (crisis==1 & L.crisis==0)

			* episode index
			bys iso_id (year): gen crisis_ep = sum(crisis_start)
			replace crisis_ep = . if crisis_ep == 0

			* obtain crisis-start year per episode
			egen crisis_start_year = min(cond(crisis_start==1, year, .)), ///
				 by(iso_id crisis_ep)

			* rel-year from crisis start
			gen rel_year = year - crisis_start_year if crisis_ep < .

			* crisis window: year 0 to +3
			gen in_crisis_window_3y = inrange(rel_year, 0, 3)


			/*----------------------------------------------------------------
			   Define thresholds to evaluate
			----------------------------------------------------------------*/

			local pubT  high60 high80 high90
			local privT high_priv100 high_priv120 high_priv135

			tempfile out
			postfile pf str20 threshold double total_cross crisis_cross prob using `out', replace


			/*----------------------------------------------------------------
			   Public Debt Thresholds
			----------------------------------------------------------------*/

			foreach T of local pubT {

				* first crossing indicator
				bys iso_id (year): gen cross_`T' = (`T'==1 & L.`T'==0)

				* which occur during crisis window?
				gen crisis_cross_`T' = (cross_`T'==1 & in_crisis_window_3y==1)

				* counts
				count if cross_`T'==1
				local tot = r(N)

				count if crisis_cross_`T'==1
				local cris = r(N)

				* probability (%)
				local p = 100 * (`cris' / `tot')

				post pf ("`T'") (`tot') (`cris') (`p')

				drop cross_`T' crisis_cross_`T'
			}


			/*----------------------------------------------------------------
			   Private Debt Thresholds
			----------------------------------------------------------------*/

			foreach T of local privT {

				bys iso_id (year): gen cross_`T' = (`T'==1 & L.`T'==0)
				gen crisis_cross_`T' = (cross_`T'==1 & in_crisis_window_3y==1)

				count if cross_`T'==1
				local tot = r(N)

				count if crisis_cross_`T'==1
				local cris = r(N)

				local p = 100 * (`cris' / `tot')

				post pf ("`T'") (`tot') (`cris') (`p')

				drop cross_`T' crisis_cross_`T'
			}

			postclose pf


			/*----------------------------------------------------------------
			   Convert postfile → dataset and label for easy plotting
			----------------------------------------------------------------*/

			use `out', clear

			rename threshold th_raw
			rename total_cross total
			rename crisis_cross crisis
			rename prob probability

			* Generate readable labels
			gen threshold = ""
			replace threshold = "Public Debt > 60%"  if th_raw=="high60"
			replace threshold = "Public Debt > 80%"  if th_raw=="high80"
			replace threshold = "Public Debt > 90%"  if th_raw=="high90"
			replace threshold = "Private Debt > 100%" if th_raw=="high_priv100"
			replace threshold = "Private Debt > 120%" if th_raw=="high_priv120"
			replace threshold = "Private Debt > 135%" if th_raw=="high_priv135"

			order threshold probability total crisis th_raw
			drop th_raw

			save "crisis_cross_probs.dta", replace

			di "File saved: crisis_cross_probs.dta"

		
		
********************************************************************************
** 		Empirical Results Section Tables/Figure     (Dec 11 9:01am)            **
********************************************************************************
*			ATTENTION: Each Block will have to be ran separately here		   *
*					they will -break- on their own. 						   *
********************************************************************************

	/*------------------------------------------------------------
		A. LP-DiD IRFs for Real GDP Growth
	------------------------------------------------------------*/
	cd "$root/Data/LP_DiD_Output/IRFs"

	* Pick out the IRF plot files
	local plots IRF_baseline_full_d_rgdp_high80.gph ///
            IRF_baseline_full_d_rgdp_high90.gph ///
            IRF_baseline_full_d_rgdp_high_priv100.gph ///
            IRF_baseline_full_d_rgdp_high_priv120.gph


	graph combine `plots', ///
		col(2) ///
		title("Impact of Debt Threshold Crossings on Real GDP Growth", size(medlarge)) ///
		subtitle("Full-Sample LP-DiD Estimates") ///
		note("Each panel shows the cumulative dynamic response of real GDP growth after a debt threshold crossing." ///
			 "Thresholds: Public Debt > 80%, > 90%, Private Debt > 100%, Private Debt > 120%.") ///
		graphregion(color(white)) iscale(.9)

	graph export "Panels/PANEL_fullsample_rgdp.png", replace
	
	
	
	/*------------------------------------------------------------
		B. LP-DiD IRFs for Real GDP Growth and Crisis Window
	------------------------------------------------------------*/

	local plots ///
		IRF_baseline_cw3y_d_rgdp_high80.gph ///
		IRF_baseline_cw3y_d_rgdp_high90.gph ///
		IRF_baseline_cw3y_d_rgdp_high_priv100.gph ///
		IRF_baseline_cw3y_d_rgdp_high_priv120.gph

	graph combine `plots', ///
		col(2) ///
		title("Impact of Debt Threshold Crossings on Real GDP Growth", size(medlarge)) ///
		subtitle("Restricted to 0–3 Years After Financial Crises") ///
		note("High-debt episodes occurring within crisis windows show no systematic negative effect on growth." ///
			 "This contrasts with the full-sample estimates and supports a crisis-driven confounding mechanism.") ///
		graphregion(color(white)) ///
		iscale(.9)

	graph export "Panels/PANEL_crisiswindow_rgdp.png", replace

	
	/*------------------------------------------------------------
		C. Probability of Debt Threshold Crossings During Crisis Windows 
	------------------------------------------------------------*/
	cd "$root/Data/"
	use "crisis_cross_probs.dta", clear

	label define thr 1 "Public Debt > 60%" ///
					 2 "Public Debt > 80%" ///
					 3 "Public Debt > 90%" ///
					 4 "Private Debt > 100%" ///
					 5 "Private Debt > 120%" ///
					 6 "Private Debt > 135%"

		encode threshold, gen(Threshold) label(thr)
				
		* Format Probability label
		gen pct_label = string(probability, "%4.1f") + "%"
		
		
		twoway ///
			(bar probability Threshold, barwidth(0.6) color(navy)) ///
			(scatter probability Threshold, ///
				mlabel(pct_label) mlabpos(12) mlabsize(medsmall) mlabcolor(black) ///
				msymbol(none)) ///
			, ///
			xlabel(1 `"Public Debt > 60%"' ///
				   2 `"Public Debt > 80%"' ///
				   3 `"Public Debt > 90%"' ///
				   4 `"Private Debt > 100%"' ///
				   5 `"Private Debt > 120%"' ///
				   6 `"Private Debt > 135%"' , angle(45) labsize(medsmall)) ///
			ylabel(0(10)60, angle(0)) ///
			ytitle("Probability (%)", size(medlarge)) ///
			title("How Often Do Debt Threshold Crossings Occur During Crises?", size(medlarge)) ///
			subtitle("Share of first crossings occurring 0–3 years after a financial crisis") ///
			graphregion(color(white))

			
		graph export "LP_DiD_Output/Panels/PANEL_crisis_crossing_probs.png", replace
		graph save "LP_DiD_Output/IRFs/PANEL_crisis_crossing_probs.gph", replace
	
	/*------------------------------------------------------------
		D. Full-Sample vs Crisis-Window LP-DiD Comparison (Selected Outcomes) 
	------------------------------------------------------------*/
	cd "$root/Data/LP_DiD_Output/IRFs"
	
	local outcomes d_rgdp d_consumption d_investment d_credit
	local thr high90  // most policy-relevant threshold

	foreach Y of local outcomes {
		local full  IRF_baseline_full_`Y'_`thr'.gph
		local cw    IRF_baseline_cw3y_`Y'_`thr'.gph

		graph combine `full' `cw', ///
			col(2) ///
			title("Debt Threshold Effects on `Y' — Full Sample vs Crisis Window") ///
			subtitle("Threshold: Public Debt > 90%") ///
			note("Left: Full sample. Right: Restricted to 0–3 years after crises.") ///
			graphregion(color(white)) ///
			iscale(.9)

		graph export "Panels/PANEL_compare_`Y'_`thr'.png", replace
	}

	
**************************************************************************************************	
	/*------------------------------------------------------------
		E. LP-DiD Pre-Trend Review
	------------------------------------------------------------*/
	clear
	global root "/Users/maciwoyat/Desktop/Econ706"
	cd "$root/Data/"
	
	use "austerity_rest.dta", clear
	
			* Reconstruct crisis_t from crisis_lag (crisis last year) --Pasted from earlier
			bys iso_id (year): gen crisis = F.crisis_lag   // crisis in t if lag=1 in t-1
			replace crisis = 0 if missing(crisis)
			
			
					xtset iso_id year

			*--------------------------------------------------------------*
			*  Define crisis episodes and 3-year post-crisis window     *
			*--------------------------------------------------------------*
			
			* Identify the start year of each crisis episode (per country)
			bys iso_id (year): gen crisis_start = (crisis == 1 & L.crisis == 0)

			* Allow for multiple crises per country: episode index
			bys iso_id (year): gen crisis_ep = sum(crisis_start)

			* Keep only obs that ever belong to a crisis episode
			replace crisis_ep = . if crisis_ep == 0

			* For each (country, episode) pair, find the crisis start year
			egen crisis_start_year = min(cond(crisis_start==1, year, .)), ///
				by(iso_id crisis_ep)

			* Relative year to the start of the episode
			gen rel_year = year - crisis_start_year if crisis_ep < .

			* 3-year post-crisis window: crisis year t, t+1, t+2, t+3
			gen in_crisis_window_3y = inrange(rel_year, 0, 3)

			label var in_crisis_window_3y ///
				"1 if within 0–3 years after crisis start"


	
	* Required locals
	local controls L_trade_open L_unemp L_credit_growth L_housing_tr crisis_lag ln_pop
	local outcomes d_rgdp d_gdp d_rgdppc d_consumption d_investment inf d_credit d_unemp
	local thresholds high80 high90 high_priv100 high_priv120
	local horizons -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10

	* Create master postfile
	tempname prepf
	tempfile pretrend_results
	postfile `prepf' str20 sample str20 outcome str20 threshold ///
			horizon beta se using `pretrend_results', replace

	local horizons -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10

	foreach Y of local outcomes {

		di "================ OUTCOME: `Y' ================"

		foreach T of local thresholds {

			di "---------- Threshold: `T' ----------"

			* 1. Basic threshold crossing
			capture drop shock_raw
			bys iso_id (year): gen shock_raw = (`T'==1 & L.`T'==0)

			* 2. Crisis-window shock
			capture drop shock_cw
			gen shock_cw = shock_raw * in_crisis_window_3y

			* Check existence
			quietly count if shock_raw==1
			local fullN = r(N)

			quietly count if shock_cw==1
			local cwN = r(N)

			if `fullN'==0 & `cwN'==0 {
				di "No crossings for `T'. Skipping."
				continue
			}

			foreach h of local horizons {

				capture drop Y_h

				* Define outcome depending on horizon sign
				if `h'>=0 {
					if "`Y'"=="d_rgdp" {
						gen Y_h = 100*( log(F`h'.rgdpmad) - log(L.rgdpmad) )
					}
					else gen Y_h = F`h'.`Y'
				}
				else {
					local hh = abs(`h')
					if "`Y'"=="d_rgdp" {
						gen Y_h = 100*( log(L`hh'.rgdpmad) - log(L1.rgdpmad) )
					}
					else gen Y_h = L`hh'.`Y'
				}

				quietly count if Y_h!=.
				if r(N)==0 continue

				*---------------------*
				* FULL SAMPLE VERSION *
				*---------------------*
				if `fullN'>0 {
					quietly xtreg Y_h shock_raw `controls' i.year, fe vce(cluster iso_id)
					matrix b = e(b)
					matrix V = e(V)
					post `prepf' ("pretrend_full") ("`Y'") ("`T'") ///
						 (`h') (b[1,1]) (sqrt(V[1,1]))
				}

				*----------------------------*
				* CRISIS-WINDOW VERSION     *
				*----------------------------*
				if `cwN'>0 {
					quietly xtreg Y_h shock_cw `controls' i.year, fe vce(cluster iso_id)
					matrix b = e(b)
					matrix V = e(V)
					post `prepf' ("pretrend_cw3y") ("`Y'") ("`T'") ///
						 (`h') (b[1,1]) (sqrt(V[1,1]))
				}

			}  // horizon loop
		}      // threshold loop
	}          // outcome loop

	postclose `prepf'
	use `pretrend_results', clear
	save "LP_pretrend_results.dta", replace
	di "Pretrend results saved."

	/*-----------------------------------------------------------
				Pre-Trend Plots by Threshold (4 outcomes)
				d_rgdp, d_consumption, d_investment, d_unemp
	-----------------------------------------------------------*/

	global root "/Users/maciwoyat/Desktop/Econ706"
	cd "$root/Data"

	* Thresholds you care about
	local thresholds high80 high90 high_priv100 high_priv120

	foreach T of local thresholds {

		di "=============================================="
		di "   THRESHOLD: `T'"
		di "=============================================="

		*---------------------------------------------------*
		* 1. REAL GDP GROWTH (d_rgdp)
		*---------------------------------------------------*
		use "LP_pretrend_results.dta", clear
		keep if threshold=="`T'" & outcome=="d_rgdp"
		if _N {

			gen ub = beta + 1.96*se
			gen lb = beta - 1.96*se

			* Full sample
			preserve
				keep if sample=="pretrend_full"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Real GDP Growth") ///
						subtitle("Full Sample — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Real GDP Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_full_`T'_d_rgdp.png", replace width(2000)
					graph save   "PANEL_pretrend_full_`T'_d_rgdp.gph", replace
				}
			restore

			* Crisis window 0–3y
			preserve
				keep if sample=="pretrend_cw3y"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Real GDP Growth") ///
						subtitle("Crisis Window 0–3y — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Real GDP Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_cw3y_`T'_d_rgdp.png", replace width(2000)
					graph save   "PANEL_pretrend_cw3y_`T'_d_rgdp.gph", replace
				}
			restore

			* Combined two-panel (if both exist)
			capture graph combine ///
				"PANEL_pretrend_full_`T'_d_rgdp.gph" ///
				"PANEL_pretrend_cw3y_`T'_d_rgdp.gph" ///
				, col(2) ///
				  title("Pre-Trend Diagnostics: Real GDP Growth") ///
				  subtitle("Threshold `T': Full Sample vs Crisis Window") ///
				  note("Left: Full-sample pre-trend. Right: Crisis-window 0–3y.") ///
				  graphregion(color(white))
			if !_rc {
				graph export "PANEL_pretrend_compare_`T'_d_rgdp.png", replace width(2600)
			}
		}

		*---------------------------------------------------*
		* 2. CONSUMPTION (d_consumption)
		*---------------------------------------------------*
		use "LP_pretrend_results.dta", clear
		keep if threshold=="`T'" & outcome=="d_consumption"
		if _N {

			gen ub = beta + 1.96*se
			gen lb = beta - 1.96*se

			preserve
				keep if sample=="pretrend_full"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Consumption") ///
						subtitle("Full Sample — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Consumption Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_full_`T'_d_consumption.png", replace width(2000)
					graph save   "PANEL_pretrend_full_`T'_d_consumption.gph", replace
				}
			restore

			preserve
				keep if sample=="pretrend_cw3y"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Consumption") ///
						subtitle("Crisis Window 0–3y — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Consumption Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_cw3y_`T'_d_consumption.png", replace width(2000)
					graph save   "PANEL_pretrend_cw3y_`T'_d_consumption.gph", replace
				}
			restore

			capture graph combine ///
				"PANEL_pretrend_full_`T'_d_consumption.gph" ///
				"PANEL_pretrend_cw3y_`T'_d_consumption.gph" ///
				, col(2) ///
				  title("Pre-Trend Diagnostics: Consumption") ///
				  subtitle("Threshold `T': Full Sample vs Crisis Window") ///
				  note("Left: Full-sample pre-trend. Right: Crisis-window 0–3y.") ///
				  graphregion(color(white))
			if !_rc {
				graph export "PANEL_pretrend_compare_`T'_d_consumption.png", replace width(2600)
			}
		}

		*---------------------------------------------------*
		* 3. INVESTMENT (d_investment)
		*---------------------------------------------------*
		use "LP_pretrend_results.dta", clear
		keep if threshold=="`T'" & outcome=="d_investment"
		if _N {

			gen ub = beta + 1.96*se
			gen lb = beta - 1.96*se

			preserve
				keep if sample=="pretrend_full"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Investment") ///
						subtitle("Full Sample — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Investment Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_full_`T'_d_investment.png", replace width(2000)
					graph save   "PANEL_pretrend_full_`T'_d_investment.gph", replace
				}
			restore

			preserve
				keep if sample=="pretrend_cw3y"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Investment") ///
						subtitle("Crisis Window 0–3y — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Investment Growth (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_cw3y_`T'_d_investment.png", replace width(2000)
					graph save   "PANEL_pretrend_cw3y_`T'_d_investment.gph", replace
				}
			restore

			capture graph combine ///
				"PANEL_pretrend_full_`T'_d_investment.gph" ///
				"PANEL_pretrend_cw3y_`T'_d_investment.gph" ///
				, col(2) ///
				  title("Pre-Trend Diagnostics: Investment") ///
				  subtitle("Threshold `T': Full Sample vs Crisis Window") ///
				  note("Left: Full-sample pre-trend. Right: Crisis-window 0–3y.") ///
				  graphregion(color(white))
			if !_rc {
				graph export "PANEL_pretrend_compare_`T'_d_investment.png", replace width(2600)
			}
		}

		*---------------------------------------------------*
		* 4. UNEMPLOYMENT (d_unemp)
		*---------------------------------------------------*
		use "LP_pretrend_results.dta", clear
		keep if threshold=="`T'" & outcome=="d_unemp"
		if _N {

			gen ub = beta + 1.96*se
			gen lb = beta - 1.96*se

			preserve
				keep if sample=="pretrend_full"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Unemployment") ///
						subtitle("Full Sample — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Unemployment (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_full_`T'_d_unemp.png", replace width(2000)
					graph save   "PANEL_pretrend_full_`T'_d_unemp.gph", replace
				}
			restore

			preserve
				keep if sample=="pretrend_cw3y"
				if _N {
					twoway ///
						(rarea ub lb horizon, color(gs12%40)) ///
						(line  beta horizon, lcolor(navy) lwidth(medthick)) ///
						, ///
						title("LP-DiD Pre-Trends: Unemployment") ///
						subtitle("Crisis Window 0–3y — Threshold `T'") ///
						xtitle("Horizon (years)") ///
						ytitle("Effect on Unemployment (pp)") ///
						yline(0, lpattern(dash) lcolor(gs8)) ///
						graphregion(color(white)) legend(off)

					graph export "PANEL_pretrend_cw3y_`T'_d_unemp.png", replace width(2000)
					graph save   "PANEL_pretrend_cw3y_`T'_d_unemp.gph", replace
				}
			restore

			capture graph combine ///
				"PANEL_pretrend_full_`T'_d_unemp.gph" ///
				"PANEL_pretrend_cw3y_`T'_d_unemp.gph" ///
				, col(2) ///
				  title("Pre-Trend Diagnostics: Unemployment") ///
				  subtitle("Threshold `T': Full Sample vs Crisis Window") ///
				  note("Left: Full-sample pre-trend. Right: Crisis-window 0–3y.") ///
				  graphregion(color(white))
			if !_rc {
				graph export "PANEL_pretrend_compare_`T'_d_unemp.png", replace width(2600)
			}
		}

	}  // end threshold loop


			
				
	
	
	
	
	
	
	
	
	
	
	