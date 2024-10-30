#'
#' clean_cms_64_functions.R -- Functions used to clean parts of the CMS-64 data
#'

#' Extracts sheets stored within the tabs of the excel file
#' @param fname file name
multiple_sheets <- function(fname) {
  
  # Getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, 
                   function(x) readxl::read_excel(fname, skip = 3, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # Assigning names to data frames
  names(data_frame) <- sheets
  
  # Print data frame
  print(data_frame)
}

#' Cleans financial management report, splitting the page at the state/territory
#' level and splitting by MAP and ADP data (1997-2001)
#' @param d FMR page
#' @return Cleaned list of lists
fmr_cleaner_97_01 <- function(d) {
  
  # Remove rows that filled with NAs
  dd <- d[rowSums(is.na(d)) != ncol(d), ]
  
  # Identify the indices to chunk -- splits should occur at state names where they
  # are stored in the first column followed by NAs in the following 7
  split_indices <- which(apply(dd, 1, function(row) {
    any(!is.na(row)) & (sum(is.na(row)) >= 7)
  }))
  
  # Chunk by state adding state name as name of sublist
  split_list <- list()
  for (i in seq_along(split_indices)) {
    if (i < length(split_indices)) {
      state_name <- as.character(dd[split_indices[i], 1])
      state_data <- dd[split_indices[i]:(split_indices[i + 1] - 1), ]
    } else {
      state_name <- as.character(dd[split_indices[i], 1])
      state_data <- dd[split_indices[i]:nrow(dd), ]
    }
    # Split vertically to have MAP and ADP separate
    map_data <- state_data[, 1:4]
    adp_data <- state_data[, 5:8]
    
    #Omit the first two rows and set the new first row as column names for "map"
    map_data <- map_data[-c(1, 2), ] #Remove 1st, 2nd rows. For 1997-2001 data
    colnames(map_data) <- map_data[1, ]  # Set the new first row as column names
    map_data <- map_data[-1, ]  # Remove the new first row (now redundant)


    # Omit the first two rows and set the new first row as column names for "adp"
    adp_data <- adp_data[-c(1, 2), ] #Remove 1st, 2nd rows. For 1997-2001 data
    colnames(adp_data) <- adp_data[1, ]  # Set the new first row as column names
    adp_data <- adp_data[-1, ]  # Remove the new first row (now redundant)
    
    # Store the processed "map" and "adp" in the split list
    split_list[[state_name]] <- list(map = map_data,
                                     adp = adp_data)
  }
  return(split_list)
}

#' Cleans financial management report, splitting the page at the state/territory
#' level and splitting by MAP and ADP data (2002-2011)
#' @param d FMR page
#' @return Cleaned list of lists
fmr_cleaner_02_11 <- function(d, year) {
  
  # Remove rows that filled with NAs
  dd <- d[rowSums(is.na(d)) != ncol(d), ]
  
  # Omit the "Created on" tag
  dd <- dd[!grepl("Created On",
                  dd[, 1],
                  fixed = FALSE,
                  ignore.case = TRUE), ]
  
  # Identify the indices to chunk -- splits should occur at state names where they
  # are stored in the first column followed by NAs in the following columns
  split_indices <- which(apply(dd, 1, function(row) {
    any(!is.na(row)) & (sum(is.na(row)) >= 7)
  }))
  
  # Chunk by state adding state name as name of sublist
  split_list <- list()
  for (i in seq_along(split_indices)) {
    if (i < length(split_indices)) {
      # state_name <- as.character(dd[split_indices[i], 1])
      state_data <- dd[split_indices[i]:(split_indices[i + 1] - 1), ]
    } else {
      # state_name <- as.character(dd[split_indices[i], 1])
      state_data <- dd[split_indices[i]:nrow(dd), ]
    }
    
    # Identify the row where the state name is located within the current chunk
    state_name_row <- which(rowSums(is.na(state_data[, 1:ncol(state_data)])) >= 5)[1]
    
    # Find the column that contains the state name in the identified row
    state_name_col <- which(!is.na(state_data[state_name_row, ]))[1]
    
    # Extract the state name from the identified row and column
    state_name <- as.character(state_data[state_name_row, state_name_col])
    
    # Determine how to split the data based on the year, rename column names
    if (year >= "2002" && year <= "2008") {
      map_data <- state_data[, 1:4]
      adp_data <- state_data[, 5:8]
      
      names(map_data) <- c("Service Category", 
                           "Total Computable", 
                           "Federal Share",
                           "State Share")
      
      # Omit unneeded rows
      map_data <- map_data[complete.cases(map_data), ]
      adp_data <- adp_data[complete.cases(adp_data), ]
      
      # Placeholders since not in data b/t 2002 - 2008
      map_data$`Federal Share Medicaid` <- NA
      map_data$`Federal Share ARRA` <- NA
      
      # Rename column names
      map_data <- map_data[, c("Service Category", 
                               "Total Computable", 
                               "Federal Share",
                               "Federal Share Medicaid",
                               "Federal Share ARRA",
                               "State Share")]
      
      names(adp_data) <- c("Service Category", 
                           "Total Computable", 
                           "Federal Share",
                           "State Share")
      
    } else if (year >= "2009" && year <= "2011") {
      map_data <- state_data[, 1:6]
      adp_data <- state_data[, 7:10]
      
      # Rename column names
      names(map_data) <- c("Service Category", 
                           "Total Computable", 
                           "Federal Share",
                           "Federal Share Medicaid",
                           "Federal Share ARRA",
                           "State Share")
      names(adp_data) <- c("Service Category", 
                           "Total Computable", 
                           "Federal Share",
                           "State Share")
      
      # Omit unneeded rows
      map_data <- map_data[complete.cases(map_data), ]
      adp_data <- adp_data[complete.cases(adp_data), ]
    }
    
    
    # Omit rows that are the column names
    map_data <- map_data[!grepl("Service Category",
                                map_data[, 1],
                                fixed = FALSE,
                                ignore.case = TRUE), ]
    
    adp_data <- adp_data[!grepl("Service Category",
                                adp_data[, 1],
                                fixed = FALSE,
                                ignore.case = TRUE), ]
    
    
    # Store the processed "map" and "adp" in the split list
    split_list[[state_name]] <- list(map = map_data,
                                     adp = adp_data)
  }
  return(split_list)
}

#' Cleans financial management report, splitting the page at the state/territory
#' level and splitting by MAP and ADP data (2012)
#' @param d FMR page
#' @param state name of state/territory
#' @return Cleaned list of lists
fmr_cleaner_2012 <- function(d, state) {
  
  # Remove filler rows 
  dd <- d[rowSums(is.na(d)) < ncol(d) - 2, ]
  
  map_data <- dd[, 1:7]
  adp_data <- dd[, 8:11]
  
  # Set the new first row as column names
  colnames(map_data) <- str_squish(map_data[1, ]) 
  
  # Remove the new first row (now redundant)
  map_data <- map_data[-1, ]  
  
  # Set the new first row as column names
  colnames(adp_data) <- str_squish(adp_data[1, ])
  
  # Remove the new first row (now redundant)
  adp_data <- adp_data[-1, ]  
  
  # Removes filler NA rows
  map_data <- map_data[complete.cases(map_data), ]
  adp_data <- adp_data[complete.cases(adp_data), ]
  
  map_data$`Federal Share BIPP` <- NA
  
  split_list <- list()
  
  # Store the processed "map" and "adp" in the split list
  split_list[[state]] <- list(map = map_data,
                              adp = adp_data)
}


#' Converts pdf text to list
#' @param text page of text as a string
#' @return list with each line as its own sublist
pdf_to_list <- function(text) {
  
  # Splits string text
  lines <- strsplit(text, "\n")[[1]]
  
  # To the filtered lines format the spacing for the columns
  list_data <- lapply(lines, function(line) {
    columns <- strsplit(line, "\\s{2,}|\t")[[1]]
    return(columns)
  })
  return(list_data)
}











