### Code

### Main Scripts for the Expansion
  
  - `data_prepper.R`
  
    - **Read in data**
      - Row bind tables containing the spending variables for  stored in `/Temp` 
  
    - **Merge enrollment data onto spending data**
    
      - Combine enrollment tables from `/Input_Data/Medicaid_managedcare_enrollment_report/by_program_pop_from_report` and join onto spending data extracted from enrollment reports for years 1997-2023
      
    - **Cleaning panel column names for merging with Tamara's data**
    
      - Add "m-", "a-", or "c-" tags in front of the spending variables denoting if they're Medicaid (m and a) or CHIP (c and s) to match original data
      
      - Make naming for balance, collections, and total net expenditures consistent with the original dataset
      
    - **Merging panel and Tamara's data**
    
      - `merged_data` is the row-binded dataframe from `data_91_05` (enrollment + spending data 1991-2005) and `new_data` (spending data from 1997-2023)
      
    - **Integrate Comp. MCO data from KFF tracker**
    
      - Read in the raw data from the KFF tracker, store the datasets in a list of lists, collapse into one dataframe, and merge this dataframe onto `new_merged_data` (things will keep getting added to this dataframe)

    - **Integrating 2006-07 Managed Care enrollment from claims**
    
      - Integrate managed care enrollment numbers from 2007 claims into `new_merged_data`
      - Integrate enrollment numbers from 2006 Managed Care enrollment report into `new_merged_data`
    
    - **Read in claims data for 2012**
    
      - Integrate managed care enrollment numbers from 2012 claims into `new_merged_data`
      
    - **Get Total Medicaid and SCHIP Spending**
    
      - Add variables `mt-total net expenditures` + `at-total net expenditures` to get total medicaid spending if the value for that state and year is missing
      - Do the same thing for SCHIP
      
    - **Append enrollment numbers for 1991 - 1995**
    
      - `data_1991_2005.rds`/`fmr90-05_2.dta` doesn't include total Medicaid and Managed Care enrollment
      
        - It's repeatedly stated in the paper that "June 30 Medicaid enrollment was not reported for the years 1991 to 1995; it was instead constructed using fiscal year enrollment from the Medicaid Statistical Information Statistics reports for those years and the ratio of fiscal year enrollment to June 30 enrollment for 1996."
        
        - The ratio for `mcdben` and `fymcdben` (June 30/FY enrollment) for 1996 is calculated and is used to estimate the population for June 30th, XXXX for years 1991-1995
      
      - `mc91_05` contains unduplicated totals (`undup_tot`) for managed care for each state, year and enrollment by different entity types. `undup_tot` is used to fill in the missing values for managed care enrollment for 1991-1994
      
    - **Integrate mandate data and county-level Census data**
      - `mandate` (`uimmc.dta`) and `census_2000` are merged to get the proportion of the population residing in a county with an MMC mandate for each state, year
      
        - Mandates are only available from 1991 to 2001.
          - **Note: I still can't find how they extended the mandate to 2002 and 2003. I just copied the data from 2001 onto these years for now, but I'm still looking**
          
        - Multiply the 2000 population estimate by 1 or 0 in the columns for the different types of mandates (e.g., mandatory/voluntary HMO, PCCM, no MMC, mixed mandate, etc.)
        
        - Divide these weighted estimates by the sum of the county-level populations after grouping by year and/or state.
        
      - **Adjust spending for inflation**
      
        - Using the library `fredr` and an API key from the Federal Reserve, get annual CPI and calculate year-over-year percentage change
        
        - Merge CPI for 2023 to `new_merged_data` and calculate the adjustment factor to adjust spending for inflation
    
    - `clean_cms_64_97_01.R`, `clean_cms_64_02_11.R`, `clean_cms_64_2012.R`, `clean_cms_64_13_21.R`
    
      - Takes Excel sheets and structures them into wide-pivoted dataframes with MAP and ADP spending variables across multiple years consolidated into one dataframe

  - `data_prepper.R`





