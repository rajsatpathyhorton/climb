*! CLIMB package v0.12 (05/14/2025)
*! Rajesh Satpathy-Horton (rsatpat1@jh.edu)
*! Written using StataNow 19.5

*v0.12 (05/14/2025): minor edits

*-------------------------------------------------------*
* Program initialization
*-------------------------------------------------------*
cap program drop climb

program define climb 
	args task

*--------------------------*
* Input validation
*--------------------------*	
	// Handle empty arg
    if "`task'" == "" {
        di as error "You must specify a task. Example: climb phq9"
        exit 198
    }

	// Handle unsupported tasks
	if !inlist("`task'","phq","gad","ptsd") {
		di as error "Not a valid argument. Valid args include: phq, gad, ptsd."
		exit 198
	}

	*-------------------------------------------------------*
	* PHQ-9 variable generation
	*-------------------------------------------------------*
	if "`task'" == "phq" {
		*--------------------------*
		* Wave-by-wave missingness
		*--------------------------*
		forval i=1/4 {
			di as red `"Missing value report for the Wave `i' PHQ-9 survey."'
			misstable summarize D6A_T`i' D6B_T`i' D6C_T`i' D6D_T`i' D6E_T`i' D6F_T`i' D6G_T`i' D6H_T`i' D6I_T`i'
		}

		di as red "Missing value report for the Wave 5 PHQ-9 survey."
		misstable summarize D6_T5A_T5 D6_T5B_T5 D6_T5C_T5 D6_T5D_T5 D6_T5E_T5 D6_T5F_T5 D6_T5G_T5 D6_T5H_T5 D6_T5I_T5

		*--------------------------*
		* Continuous Score
		*--------------------------*
		forval i=1/4 {
			qui g PHQ9_score_T`i' = 0
			egen rowmiss_PHQ9_T`i' = rowmiss(D6A_T`i' D6B_T`i' D6C_T`i' D6D_T`i' D6E_T`i' D6F_T`i' D6G_T`i' D6H_T`i' D6I_T`i')
			foreach x in D6A_T`i' D6B_T`i' D6C_T`i' D6D_T`i' D6E_T`i' D6F_T`i' D6G_T`i' D6H_T`i' D6I_T`i' {
				qui replace PHQ9_score_T`i' = PHQ9_score_T`i' + (`x'-1) if (`x'!=77 & `x'!=98 & `x'!=.) // & rowmiss_PHQ9_T`i'==0)
				qui replace PHQ9_score_T`i' = . if rowmiss_PHQ9_T`i'==9
			}
			di as red `"Wave `i' PHQ-9 continuous measure generated as PHQ9_score_T`i'."'
		}

		qui g PHQ9_score_T5 = 0
		egen rowmiss_PHQ9_T5 = rowmiss(D6_T5A_T5 D6_T5B_T5 D6_T5C_T5 D6_T5D_T5 D6_T5E_T5 D6_T5F_T5 D6_T5G_T5 D6_T5H_T5 D6_T5I_T5)
		foreach x in D6_T5A_T5 D6_T5B_T5 D6_T5C_T5 D6_T5D_T5 D6_T5E_T5 D6_T5F_T5 D6_T5G_T5 D6_T5H_T5 D6_T5I_T5 {
			qui replace PHQ9_score_T5 = PHQ9_score_T5 + (`x'-1) if (`x'!=77 & `x'!=98 & `x'!=.) //& rowmiss_PHQ9_T5==0)
			qui replace PHQ9_score_T5 = . if rowmiss_PHQ9_T5==9
			}
		di as red "Wave 5 PHQ-9 continuous measure generated as PHQ9_score_T5."

		*--------------------------*
		* Full and partial incompleteness
		*--------------------------*
		forval i=1/5 {
			di as red `"Partial and incomplete survey responses for Wave `i'"'
			tab PHQ9_score_T`i' rowmiss_PHQ9_T`i', m
		}

		*--------------------------*
		* Binary Measure
		*--------------------------*
		forval i=1/5 {
			qui g PHQ9_binary_T`i' = . 
			qui {
				replace PHQ9_binary_T`i' = 0 if (PHQ9_score_T`i'<10) & (PHQ9_score_T`i'>=0)
				replace PHQ9_binary_T`i' = 1 if (PHQ9_score_T`i'>=10) & (PHQ9_score_T`i'<28)
				replace PHQ9_binary_T`i' = . if rowmiss_PHQ9_T`i'==9 // This sets binary outcome to missing for only fully incomplete surveys
			}
			di as red `"Binary PHQ-9 variable for Wave `i' created: PHQ9_binary_T`i'."'
		}	

		* Check harmonization with PHQ9 scores 
		forval i=1/5 {
			tab PHQ9_binary_T`i' PHQ9_score_T`i', m
		}

		* Label PHQ9_binary_T(i) for ease of understanding
		label define PHQ9_binary_labels 0 "No Depression (PHQ9 < 10)" 1 "Depression (PHQ9 >= 10)"
		forval i=1/5 {
			label values PHQ9_binary_T`i' PHQ9_binary_labels
			la var PHQ9_binary_T`i' `"Wave `i' PHQ9 binary variable"'
		}
	}

	*-------------------------------------------------------*
	* GAD-7 variable generation
	*-------------------------------------------------------*
	if "`task'"=="gad" {
		* Note: Partially complete responses are not set to missing. 
		forval i=1/4 {
			di as red `"Missing values in Wave `i' GAD-7 survey."'
			misstable summarize D8A_T`i' D8B_T`i' D8C_T`i' D8D_T`i' D8E_T`i' D8F_T`i' D8G_T`i'
		}
		di as red "Missing values in Wave 5 GAD-7 survey."
		misstable summarize D8_T5A_T5 D8_T5B_T5 D8_T5C_T5 D8_T5D_T5 D8_T5E_T5 D8_T5F_T5 D8_T5G_T5 

		*--------------------------*
		* GAD-7 Continuous Measure
		*--------------------------*
		forval i = 1/4 {
			qui g GAD7_score_T`i' = 0
			egen rowmiss_GAD7_T`i' = rowmiss(D8A_T`i' D8B_T`i' D8C_T`i' D8D_T`i' D8E_T`i' D8F_T`i' D8G_T`i')
			foreach x in D8A_T`i' D8B_T`i' D8C_T`i' D8D_T`i' D8E_T`i' D8F_T`i' D8G_T`i' {
				qui replace GAD7_score_T`i' = GAD7_score_T`i' + (`x'-1) if (`x'!=77 & `x'!=98 & `x'!=.) // & rowmiss_GAD7_T`i'==0)
				qui replace GAD7_score_T`i' = . if rowmiss_GAD7_T`i'==7
			}
		}	

		qui g GAD7_score_T5 = 0
		egen rowmiss_GAD7_T5 = rowmiss(D8_T5A_T5 D8_T5B_T5 D8_T5C_T5 D8_T5D_T5 D8_T5E_T5 D8_T5F_T5 D8_T5G_T5)
		foreach x in D8_T5A_T5 D8_T5B_T5 D8_T5C_T5 D8_T5D_T5 D8_T5E_T5 D8_T5F_T5 D8_T5G_T5 {
			qui replace GAD7_score_T5 = GAD7_score_T5 + (`x'-1) if (`x'!=98 & `x'!=77 & `x'!=.) // & rowmiss_GAD7_T`i'==0
			qui replace GAD7_score_T5 = . if rowmiss_GAD7_T5==7
			}

		* Evaluate complete and partial missingness in GAD7 scores across all waves
		* Wave 1 - 
		forval i=1/5 {
			di as red `"Wave `i' full and partial missingness report."'
			tab GAD7_score_T`i' rowmiss_GAD7_T`i', m
		}

		*--------------------------*
		* GAD-7 Binary Measure
		*--------------------------*
		forval i=1/5 {
			qui g GAD7_binary_T`i' = .
			qui replace GAD7_binary_T`i' = 0 if (GAD7_score_T`i'<10 & GAD7_score_T`i'>=0)
			qui replace GAD7_binary_T`i' = 1 if (GAD7_score_T`i'>=10 & GAD7_score_T`i'<=21)
			qui replace GAD7_binary_T`i' = . if rowmiss_GAD7_T`i'==9 // This sets GAD7 binary to missing if all GAD7 responses are missing
			di as red `"Wave `i' GAD-7 binary measure generated as GAD7_binary_T`i'."'
		}

		// Check if harmonized with GAD7 score variables at each wave.
		forval i=1/5 {
			di as red `"Checking Wave `i' harmonization between continuous and binary GAD-7 variables."'
			tab GAD7_binary_T`i' GAD7_score_T`i', m
		}

		// Label for GAD7 binary vars for comprehension
		label define GAD7_binary_labels 0 "No Anxiety (GAD7 < 10)" 1 "Anxiety (GAD7>=10)"
		forval i=1/5 {
			label values GAD7_binary_T`i' GAD7_binary_labels
			la var `"Wave `i' GAD-7 binary measure"'
		}
	}

	*-------------------------------------------------------*
	* PC-PTSD variable generation
	*-------------------------------------------------------*
	* Note: Partially missing responses are not set to missing. 
	if "`task'"=="ptsd" {

		*--------------------------*
		* Continuous PC-PTSD-4
		*--------------------------*
		* Note: Wave 1-5 
		forval i = 1/4 {
			qui g PTSD4_score_T`i' = 0
			egen rowmiss_PTSD4_T`i' = rowmiss(D9A_T`i' D9B_T`i' D9C_T`i' D9D_T`i')
			foreach x in D9A_T`i' D9B_T`i' D9C_T`i' D9D_T`i' {
				qui replace PTSD4_score_T`i' = PTSD4_score_T`i' + 1 if `x'==1
			}
			qui replace PTSD4_score_T`i' = . if rowmiss_PTSD4_T`i'==4
			di as red `"Wave `i' PC-PTSD-4 variable generated as PTSD4_score_T`i'."'
		}

		qui g PTSD4_score_T5 = 0
		egen rowmiss_PTSD4_T5 = rowmiss(D9_T5A_T5 D9_T5B_T5 D9_T5C_T5 D9_T5D_T5)
		foreach x in D9_T5A_T5 D9_T5B_T5 D9_T5C_T5 D9_T5D_T5 {
			qui replace PTSD4_score_T5 = PTSD4_score_T5 + 1 if `x'==1
			qui replace PTSD4_score_T5 = . if rowmiss_PTSD4_T5==4
			}
		di as red "Wave 5 PC-PTSD-4 variable generated as PTSD4_score_T5."

		*--------------------------*
		* Continuous PC-PTSD-5
		*--------------------------*
		* Note: Not present in Waves 1-3
		qui g PTSD5_score_T4 = 0
		egen rowmiss_PTSD5_T4 = rowmiss(D9A_T4 D9B_T4 D9C_T4 D9D_T4 D9_T4E)
		foreach x in D9A_T4 D9B_T4 D9C_T4 D9D_T4 D9_T4E {
			qui replace PTSD5_score_T4 = PTSD5_score_T4 + 1 if `x'==1
			qui replace PTSD5_score_T4 = . if rowmiss_PTSD5_T4==5
		}
		di as red "Wave 4 PC-PTSD-5 variable generated as PTSD5_score_T4."

		qui g PTSD5_score_T5 = 0
		egen rowmiss_PTSD5_T5 = rowmiss(D9_T5A_T5 D9_T5B_T5 D9_T5C_T5 D9_T5D_T5 D9_T5E_T5)
		foreach x in D9_T5A_T5 D9_T5B_T5 D9_T5C_T5 D9_T5D_T5 D9_T5E_T5 {
			qui replace PTSD5_score_T5 = PTSD5_score_T5 + 1 if `x'==1
			qui replace PTSD5_score_T5 = . if rowmiss_PTSD5_T5==5
		}
		di as red "Wave 5 PC-PTSD-5 variable generated as PTSD5_score_T5."

		*--------------------------*
		* Binary PC-PTSD-4
		*--------------------------*
		forval i=1/5 {
			qui {
				g PTSD4_binary_T`i' = .
				replace PTSD4_binary_T`i' = 0 if inrange(PTSD4_score_T`i',0,3)
				replace PTSD4_binary_T`i' = 1 if PTSD4_score_T`i'==4
				replace PTSD4_binary_T`i' = . if rowmiss_PTSD4_T`i'==4
			}
			di as red `"Binary Wave `i' PC-PTSD-4 variable generated as PTSD4_binary_T`i'."'
		}

		* Check harmonization
		forval i=1/5 {
			di as red `"Checking harmonization between Wave `i' binary and continuous variables."'
			tab PTSD4_binary_T`i' PTSD4_score_T`i', m
		}

		*--------------------------*
		* Binary PC-PTSD-5
		*--------------------------*
		* Note: Only in Waves 4-5
		forval i=4/5 {
			qui {
				g PTSD5_binary_T`i' = .
				replace PTSD5_binary_T`i' = 0 if inrange(PTSD5_score_T`i',0,3)
				replace PTSD5_binary_T`i' = 1 if inlist(PTSD5_score_T`i',4,5)
				replace PTSD5_binary_T`i' = . if rowmiss_PTSD5_T`i'==5
			}
			di as red `"Binary Wave `i' PC-PTSD-5 variable generated as PTSD5_binary_T`i'."'
		}

		* Check harmonization
		forval i=4/5 {
			di as red `"Checking harmonization between Wave `i' binary and continuous variables."'
			tab PTSD5_binary_T`i' PTSD5_score_T`i', m
		}

		*--------------------------*
		* Labeling PTSD binary vars
		*--------------------------*
		la def PTSD4_lab 0 "No PTSD (PC-PTSD-4 <= 3)" 1 "PTSD (PC-PTSD-4 == 4)"
		la def PTSD5_lab 0 "No PTSD (PC-PTSD-5 <= 3)" 1 "PTSD (PC-PTSD-5 >= 4)"

		forval i=1/5 {
			label values PTSD4_binary_T`i' PTSD4_lab
			la var PTSD4_binary_T`i' `"Wave `i' Binary PC-PTSD-4"'
		}

		forval i=4/5 {
			la val PTSD5_binary_T`i' PTSD5_lab
			la var PTSD5_binary_T`i' `"Wave `i' Binary PC-PTSD-5"'
		}
	}

end
