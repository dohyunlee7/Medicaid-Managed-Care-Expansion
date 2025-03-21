import delimited "C:\Users\ruochens\Dropbox (Penn)\Research project\MCO\Medicaid_privatization_exp\Temp\data_ready_BJS.csv", clear 

gen treatment_year_num = real(treatment_year)  // Convert to numeric
replace treatment_year_num = . if treatment_year == ""  // Handle empty strings
drop treatment_year  // Remove the old string variable
rename treatment_year_num treatment_year  // Rename the new numeric variable

replace treatment_year = 9999 if missing(treatment_year)
replace center_time = . if treatment_year == 9999  // Exclude never-treated from event study

*drop if center_time < -10 | center_time > 10

did_imputation log_all_medicaid_spending_per_ca state_id year treatment_year, horizons(0/20) pretrend(20) minn(0)
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("BJS - Log Medicaid Spending Per Capita") xlabel(-20(1)20)) stub_lag(tau#) stub_lead(pre#) together	

	
did_imputation log_all_medicaid_spending_per_ca state_id year treatment_year, horizons(0/10) pretrend(10) minn(0)
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("BJS - Log Medicaid Spending Per Capita") xlabel(-10(1)10)) stub_lag(tau#) stub_lead(pre#) together	

	
did_imputation pct_in_comp_mco state_id year treatment_year, horizons(0/20) pretrend(20) minn(0)
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("BJS - Comp MMC Enroll") xlabel(-20(1)20)) stub_lag(tau#) stub_lead(pre#) together	