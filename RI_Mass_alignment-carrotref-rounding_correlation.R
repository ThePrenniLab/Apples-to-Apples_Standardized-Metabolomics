# Load necessary libraries
library(dplyr)

# Load the datasets
dataset1 <- read.csv("LabA-pos-cleaned-carrotref_correlation.csv")
dataset2 <- read.csv("LabB-pos-cleaned-carrotref_correlation.csv")
dataset3 <- read.csv("LabC-pos-cleaned-carrotref_correlation.csv")


# Ensure each dataset has distinct entries
dataset1 <- dataset1 %>% distinct()
dataset2 <- dataset2 %>% distinct()
dataset3 <- dataset3 %>% distinct()

# Define thresholds
RI_threshold <- 3  # Adjust as needed
mass_threshold <- 0.1  # Adjust as needed

# Adjusted merge_within_lab function to merge based on RI and MZ, and combine IDs
merge_within_lab <- function(data) {
  unique_values <- data %>%
    select(ri, mz) %>%
    unique()
  
  merged_data <- data.frame()
  
  for (i in 1:nrow(unique_values)) {
    ri_value <- unique_values$ri[i]
    mz_value <- unique_values$mz[i]
    
    # Find rows that match within the threshold for RI and MZ
    matches <- data %>%
      filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
      summarise(
        ri = mean(ri),      # Take the mean RI value
        mz = mean(mz),      # Take the mean MZ value
        lab = first(lab),   # Retain the lab ID (assuming all are the same)
        ID = paste(unique(unlist(strsplit(ID, ", "))), collapse = ", ")  # Combine unique IDs
      )
    
    merged_data <- bind_rows(merged_data, matches)
  }
  
  return(merged_data)
}

# Function to check majority overlap and keep unique rows
merge_ids_majority_overlap <- function(df, id_column) {
  # Split the ID column by commas and remove leading/trailing spaces
  df$id_list <- strsplit(as.character(df[[id_column]]), ",\\s*")
  
  # Initialize a vector to keep track of whether a row has been merged
  merged <- rep(FALSE, nrow(df))
  
  # Initialize an empty list to store unique rows after merging
  unique_rows <- list()
  
  # Loop through each row in the dataset
  for (i in 1:nrow(df)) {
    # Skip this row if it has already been merged
    if (merged[i]) next
    
    # Get the IDs for this row
    ids_i <- df$id_list[[i]]
    
    # Initialize a variable to store indices of rows to merge with
    merge_with <- c(i)
    
    # Compare with every other row
    for (j in (i+1):nrow(df)) {
      # Check if j is within bounds and if row j has not been merged yet
      if (j > nrow(df) || merged[j]) next
      
      # Get the IDs for row j
      ids_j <- df$id_list[[j]]
      
      # Find the overlap between the two ID sets
      overlap <- intersect(ids_i, ids_j)
      
      # If more than half of the IDs overlap, consider them similar and mark for merging
      if (length(overlap) >= (min(length(ids_i), length(ids_j)) / 2)) {
        merge_with <- c(merge_with, j)
        merged[j] <- TRUE  # Mark row j as merged
      }
    }
    
    # Combine IDs from all rows marked for merging
    merged_ids <- unique(unlist(df$id_list[merge_with]))
    merged_ids_combined <- paste(merged_ids, collapse = ", ")
    
    # Store the result in unique_rows
    unique_rows[[length(unique_rows) + 1]] <- df[merge_with[1], ]  # Take the first row for other columns
    unique_rows[[length(unique_rows)]]$ID <- merged_ids_combined   # Replace the ID with combined unique IDs
  }
  
  # Return the dataframe with unique rows
  unique_df <- do.call(rbind, unique_rows)
  return(unique_df)
}

# Apply the merging function to your dataset

dataset1_merged <- merge_within_lab(dataset1)
dataset2_merged <- merge_within_lab(dataset2)
dataset3_merged <- merge_within_lab(dataset3)

dataset1_merged_cleaned <- merge_ids_majority_overlap(dataset1_merged, "ID")
dataset2_merged_cleaned <- merge_ids_majority_overlap(dataset2_merged, "ID")
dataset3_merged_cleaned <- merge_ids_majority_overlap(dataset3_merged, "ID")

cat("Rows in matched results:", nrow(dataset1_merged_cleaned), "\n")
df1 <- apply(dataset1_merged_cleaned,2,as.character)

cat("Rows in matched results:", nrow(dataset2_merged_cleaned), "\n")
df2 <- apply(dataset2_merged_cleaned,2,as.character)

cat("Rows in matched results:", nrow(dataset3_merged_cleaned), "\n")
df3 <- apply(dataset3_merged_cleaned,2,as.character)


write.csv(df1, "LabA-carrot_correlation.csv")
write.csv(df2, "LabB-carrot_correlation.csv")
write.csv(df3, "LabC-carrot_correlation.csv")
