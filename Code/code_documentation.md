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
      - Read in the raw data from the KFF tracker, store the datasets in a list of lists, collapse into one dataframe, and merge this dataframe onto `new_merged_data`

    - **Integrating 2006-07 Managed Care enrollment from claims**
      -







