### Datasets
All data is stored within `Input_Data`

- `CMS_64_report`
  - Stores Excel sheets of CMS-64 reports from 1997 to 2023
  - Variables for spending and utilization extracted from these reports

- `Inflation_BLS`
  - `SeriesReport-20240326212745_31976f.xlsx`
    - Monthly Consumer Price Index from 2010 to 2024 for inflation adjustment

- `Medicaid_CHIP_enroll`
  - `data.csv`
    - Enrollment numbers for Medicaid and CHIP including figures related to
    applications
    
- `Medicaid_managedcare_enrollment_report`
  - `by_program_pop_from_report`
    - Contains annual CSV files for Managed Care enrollment by different entity
    types by state (2006-2022, 2007 & 2012 missing)
    
  - `by_state_plan_from_report`
    - Contains CSV files for different Managed Care program types and their names
    (2013-2021)
  
  - `census`
    - County-level population Census data with estimates from 2000 to 2010
  
  - `claims`
    - Estimates of total Medicaid and Managed Care enrollment from Medicaid 
    claims. We have these numbers for years 2005-2012. Mainly used to fill in
    missing data we don't have from the enrollment reports, particularly 2007
    and 2012
  
  - `external`
    - Stata `.dta` files and code used in the original paper 
    sent from Dr. Tamara Hayford. 
    - Contains total Medicaid and Managed Care enrollment and spending by 
    different categories for years 1991-2005 + county-level mandates for years
      - `fymcdben.dta`: Fiscal year Medicaid enrollment as of June 30th for state, year
      - `mcdben.dta`: "Ever enrolled" in Medicaid for state, year
      - `mc91_05.dta`: Medicaid Managed Care enrollment by different entity types
      by state and year for years 1991-2005
      - `uimmc.dta`: County-level mandates for managed care entity type 
      (e.g., mandatory/voluntary HMO, PCCM, etc.). Denoted as binary indicators.
  
  - `program_summary_from_report`
  
  - `raw_comp_mco_data`
    - Public raw data (2003-2021) downloaded from KFF with enrollment for 
    Comprehensive MCO by state
  
  - `reports`
    - PDFs of Medicaid Managed Care Enrollment reports from 1995 to 2019. 
    Enrollment reports from 2007 and 2012 are nowhere to be found online.