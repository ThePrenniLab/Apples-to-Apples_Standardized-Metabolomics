# Load necessary libraries
library(dplyr)
library(data.table)

# Read in the dataset
a_dat <-read.csv("apples2apples_LabA_xcms_positive_andBLs.csv", row.names = 1) #same data as Table S9
b_dat <-read.csv("apples2apples_LabB_xcms_positive_andBLs.csv", row.names = 1) #same data as Table S10
c_dat <-read.csv("apples2apples_LabC_xcms_positive_andBLs.csv", row.names = 1) #same data as Table S11

#set RI threshold 
ri_min <- 100
ri_max <- 600

#set m/z threshold 
mz_min <- 75
mz_max <- 1200

#clean Lab A data to specified mass and RI windows
a_dat1 <- a_dat %>%
  filter(ri >= ri_min & ri <= ri_max, mz >= mz_min & mz<= mz_max)

#clean Lab B data to specified mass and RI windows
b_dat1 <- b_dat %>%
  filter(ri >= ri_min & ri <= ri_max, mz >= mz_min & mz<= mz_max)

#clean Lab C data to specified mass and RI windows
c_dat1 <- c_dat %>%
  filter(ri >= ri_min & ri <= ri_max, mz >= mz_min & mz<= mz_max)


#order datasets by uuid, ri, and mz
a_dat1 <- a_dat1[order(a_dat1$uuid, a_dat1$ri, a_dat1$mz), ]
b_dat1 <- b_dat1[order(b_dat1$uuid, b_dat1$ri, b_dat1$mz), ]
c_dat1 <- c_dat1[order(c_dat1$uuid, c_dat1$ri, c_dat1$mz), ]


#Function to combine features that are within +/- 10 ri units and 0.01 mass units within each lab
combine_similar <- function(df) {
  dt <- as.data.table(df)
  
  # Sort by uuid, RI, and mass
  setorder(dt, uuid, ri, mz)
  
  # Initialize new group column
  dt[, new_group := NA_integer_]
  group_counter <- 1
  
  for (uuid_value in unique(dt$uuid)) {
    subset_dt <- dt[uuid == uuid_value]
    n <- nrow(subset_dt)
    
    if (n == 0) next
    
    group_ids <- integer(n)
    group_ids[1] <- group_counter
    
    for (i in 2:n) {
      if (abs(subset_dt$ri[i] - subset_dt$ri[i-1]) <= 10 &&
          abs(subset_dt$mz[i] - subset_dt$mz[i-1]) <= 0.01) {
        group_ids[i] <- group_ids[i-1]
      } else {
        group_counter <- group_counter + 1
        group_ids[i] <- group_counter
      }
    }
    
    dt[uuid == uuid_value, new_group := group_ids]
    group_counter <- max(group_ids) + 1
  }
  
  # Aggregate by new group, keeping max value for 'maxo' and mean for other columns
  combined_dt <- dt[, lapply(.SD, function(x) {
    if (is.numeric(x)) {
      if ("maxo" %in% colnames(dt) && identical(names(x), "maxo")) {
        return(max(x, na.rm = TRUE))  # Keep max value for maxo column
      } else {
        return(mean(x, na.rm = TRUE))  # Mean for all other columns
      }
    } else {
      return(x[1])  # First element for non-numeric columns
    }
  }), by = new_group, .SDcols = !c("new_group")]
  
  combined_dt[, new_group := NULL]
  return(as.data.table(combined_dt))
}

#Merge features within a lab with RI +/-2 and mz +/- 0.005
a_dat2 <- combine_similar(a_dat1)
b_dat2 <- combine_similar(b_dat1)
c_dat2 <- combine_similar(c_dat1)


# Merge isotopes (mass difference of approximately 1.00) within +/- 10 RI units, keeping the lowest mz
merge_isotopes <- function(df) {
  dt <- as.data.table(df)
  
  # Sort by uuid, RI, and mz
  setorder(dt, uuid, ri, mz)
  
  # Initialize new group column for isotopes
  dt[, new_group := NA_integer_]
  
  # Loop through each uuid group
  dt[, {
    # Subset data for current uuid
    subset_dt <- .SD
    
    # Initialize group counter
    group_counter <- 1
    
    # Identify pairs of rows with a mass difference of approximately 1.00
    isotope_groups <- list()
    n <- nrow(subset_dt)
    
    # Loop through rows within the uuid
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        if (abs(subset_dt$mz[i] - subset_dt$mz[j]) > 1.0 - 0.01 && abs(subset_dt$mz[i] - subset_dt$mz[j]) < 1.0 + 0.01 &&
            abs(subset_dt$ri[i] - subset_dt$ri[j]) <= 10) {
          # If within mass difference and RI range, group them together
          isotope_groups <- c(isotope_groups, list(c(i, j)))
        }
      }
    }
    
    # For each isotope group, keep the one with the lowest mz value
    if (length(isotope_groups) > 0) {
      rows_to_keep <- sapply(isotope_groups, function(group_indices) {
        min_mz_index <- which.min(subset_dt$mz[group_indices])
        group_indices[min_mz_index]
      })
      
      subset_dt <- subset_dt[rows_to_keep]
    }
    
    # Return the subset for the current uuid
    subset_dt
  }, by = uuid]
  
  return(as.data.frame(dt))
}

#Merge possible isotope features within a lab with RI +/-2
a_dat3 <- merge_isotopes(a_dat2)
b_dat3 <- merge_isotopes(b_dat2)
c_dat3 <- merge_isotopes(c_dat2)


#Extract IRTS across all samples
extract_irts <- function(data, targets, mz_thresh = 0.01, ri_thresh = 10) {
  # Convert to data.table safely
  data <- as.data.table(data)
  targets <- as.data.table(targets)
  
  # Check column names
  if (!all(c("mz", "ri") %in% names(data))) {
    stop("The dataset must contain 'mz' and 'ri' columns.")
  }
  if (!all(c("mz", "ri") %in% names(targets))) {
    stop("The targets list must contain 'mz' and 'ri' columns.")
  }
  
  matches <- data[0]  # initialize empty data.table
  
  for (i in seq_len(nrow(targets))) {
    mz_target <- targets[i][["mz"]]
    ri_target <- targets[i][["ri"]]
    
    temp_matches <- data[
      abs(mz - mz_target) <= mz_thresh &
        abs(ri - ri_target) <= ri_thresh
    ]
    
    matches <- rbind(matches, temp_matches)
  }
  
  return(matches)
}

# Load list of IRTS species
targets <- fread("irts_list_for_analysis.csv")  # must have 'mz' and 'ri' columns

# Apply to each dataset
a_irts_matches <- extract_irts(a_dat3, targets)
b_irts_matches <- extract_irts(b_dat3, targets)
c_irts_matches <- extract_irts(c_dat3, targets)

#Write csv of irts annotations across samples for normalization
write.csv(a_irts_matches, "LabA_irts_hits.csv")
write.csv(b_irts_matches, "LabB_irts_hits.csv")
write.csv(c_irts_matches, "LabC_irts_hits.csv")

#Function to clean food features also found in the blank

clean_within_dataset <- function(dt, reference_uuid = "VersoIRTS_1.0", ri_thresh = 10, mz_thresh = 0.01) {
  dt <- copy(dt)  # avoid modifying in place
  
  # Split into reference and non-reference rows
  ref_rows <- dt[uuid == reference_uuid]
  other_rows <- dt[uuid != reference_uuid]
  
  # Start with all rows retained
  keep_flags <- rep(TRUE, nrow(other_rows))
  
  # Loop over each reference row and find matching other_rows
  for (i in seq_len(nrow(ref_rows))) {
    ref_ri <- ref_rows[i, ri]
    ref_mz <- ref_rows[i, mz]
    
    match_idx <- abs(other_rows[, ri] - ref_ri) <= ri_thresh &
      abs(other_rows[, mz] - ref_mz) <= mz_thresh
    
    keep_flags <- keep_flags & !match_idx  # set FALSE for matching rows
  }
  
  # Combine the kept rows
  cleaned_dt <- rbind(ref_rows, other_rows[keep_flags])
  return(cleaned_dt)
}

#Convert dataset back to data table
a_dat3 <- as.data.table(a_dat3)
b_dat3 <- as.data.table(b_dat3)
c_dat3 <- as.data.table(c_dat3)

#Remove food features found in blank within each lab
a_dat4 <- clean_within_dataset(a_dat3)
b_dat4 <- clean_within_dataset(b_dat3)
c_dat4 <- clean_within_dataset(c_dat3)

#Write csv output of datasets
write.csv(a_dat4, "LabA_processed_data.csv")
write.csv(b_dat4, "LabB_processed_data.csv")
write.csv(c_dat4, "LabC_processed_data.csv")
