# Load necessary libraries
library(dplyr)

# Load the datasets
df_a <- read.csv("LabA_processed_data.csv")
df_b <- read.csv("LabB_processed_data.csv")
df_c <- read.csv("LabC_processed_data.csv")


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



#################### Beef Ribeye #######################

# Example UUID you want to keep
beef_ribeye <- "GGB100032"

# Filter the dataframe (assuming it's called df and has a column named UUID)
a_beef <- df_a[df_a$uuid == beef_ribeye, ]
b_beef <- df_b[df_b$uuid == beef_ribeye, ]
c_beef <- df_c[df_c$uuid == beef_ribeye, ]

# Ensure each dataset has distinct entries
a_beef <- a_beef %>% distinct()
b_beef <- b_beef %>% distinct()
c_beef <- c_beef %>% distinct()

# Define thresholds
RI_threshold <- 10  # Adjust as needed
mass_threshold <- 0.1  # Adjust as needed


# Combine the merged datasets across labssa
combined_data <- bind_rows(a_beef, b_beef, c_beef)

# Create a list of unique RI and MZ values
unique_values <- combined_data %>%
  select(ID, lab, ri, mz) %>%
  unique()


matched_results <- data.frame()

for (i in 1:nrow(unique_values)) {
  ri_value <- unique_values$ri[i]
  mz_value <- unique_values$mz[i]
  index <- as.character(i)
  
  # Find matches across the combined dataset
  matches <- combined_data %>%
    filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
    mutate(index = index) %>%
    unique()
  
  if (nrow(matches) > 0) {
    # Keep only the match with the highest maxo per lab
    top_matches <- matches %>%
      group_by(lab) %>%
      slice_max(maxo, with_ties = FALSE) %>%
      ungroup()
    
    matched_row <- data.frame(
      ri = ri_value,
      mz = mz_value,
      labs = paste(unique(top_matches$lab), collapse = ", "),
      IDs = paste(unique(top_matches$ID), collapse = ", ")
    )
    
    matched_results <- bind_rows(matched_results, matched_row)
  }
}


# View the first few rows of the matched results

matched_results_cleaned <- merge_ids_majority_overlap(matched_results, "IDs")

# Check the number of rows in matched_results compared to original datasets
cat("Rows in matched results:", nrow(matched_results_cleaned), "\n")


df <- apply(matched_results_cleaned,2,as.character)

# Write the results to a CSV file
write.csv(df, "beef-ribeye_cleaned-RI10.csv", row.names = FALSE)

print(matched_results)






#################### Carrot #######################

# Example UUID you want to keep
crf <- "GGB100648"

# Filter the dataframe (assuming it's called df and has a column named UUID)
a_crf <- df_a[df_a$uuid == crf, ]
b_crf <- df_b[df_b$uuid == crf, ]
c_crf <- df_c[df_c$uuid == crf, ]

# Ensure each dataset has distinct entries
a_crf <- a_crf %>% distinct()
b_crf <- b_crf %>% distinct()
c_crf <- c_crf %>% distinct()


# Combine the merged datasets across labs
combined_data <- bind_rows(a_crf, b_crf, c_crf)

# Create a list of unique RI and MZ values
unique_values <- combined_data %>%
  select(ID, lab, ri, mz) %>%
  unique()

# Initialize an empty data frame to store matched results
matched_results <- data.frame()

for (i in 1:nrow(unique_values)) {
  ri_value <- unique_values$ri[i]
  mz_value <- unique_values$mz[i]
  index <- as.character(i)
  
  # Find matches across the combined dataset
  matches <- combined_data %>%
    filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
    mutate(index = index) %>%
    unique()
  
  if (nrow(matches) > 0) {
    # Keep only the match with the highest maxo per lab
    top_matches <- matches %>%
      group_by(lab) %>%
      slice_max(maxo, with_ties = FALSE) %>%
      ungroup()
    
    matched_row <- data.frame(
      ri = ri_value,
      mz = mz_value,
      labs = paste(unique(top_matches$lab), collapse = ", "),
      IDs = paste(unique(top_matches$ID), collapse = ", ")
    )
    
    matched_results <- bind_rows(matched_results, matched_row)
  }
}



# View the first few rows of the matched results
head(matched_results)

matched_results_cleaned <- merge_ids_majority_overlap(matched_results, "IDs")

# Check the number of rows in matched_results compared to original datasets
cat("Rows in matched results:", nrow(matched_results), "\n")


df <- apply(matched_results_cleaned,2,as.character)

# Write the results to a CSV file
write.csv(df, "carrot_fin.csv", row.names = FALSE)






#################### Strawberry #######################

# Example UUID you want to keep
strawberry <- "GGB100215"

# Filter the dataframe (assuming it's called df and has a column named UUID)
a_strawberry <- df_a[df_a$uuid == strawberry, ]
b_strawberry <- df_b[df_b$uuid == strawberry, ]
c_strawberry <- df_c[df_c$uuid == strawberry, ]

# Ensure each dataset has distinct entries
a_strawberry <- a_strawberry %>% distinct()
b_strawberry <- b_strawberry %>% distinct()
c_strawberry <- c_strawberry %>% distinct()


# Combine the merged datasets across labs
combined_data <- bind_rows(a_strawberry, b_strawberry, c_strawberry)

# Create a list of unique RI and MZ values
unique_values <- combined_data %>%
  select(ID, lab, ri, mz) %>%
  unique()

# Initialize an empty data frame to store matched results

# Initialize an empty data frame to store matched results
matched_results <- data.frame()

for (i in 1:nrow(unique_values)) {
  ri_value <- unique_values$ri[i]
  mz_value <- unique_values$mz[i]
  index <- as.character(i)
  
  # Find matches across the combined dataset
  matches <- combined_data %>%
    filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
    mutate(index = index) %>%
    unique()
  
  if (nrow(matches) > 0) {
    # Keep only the match with the highest maxo per lab
    top_matches <- matches %>%
      group_by(lab) %>%
      slice_max(maxo, with_ties = FALSE) %>%
      ungroup()
    
    matched_row <- data.frame(
      ri = ri_value,
      mz = mz_value,
      labs = paste(unique(top_matches$lab), collapse = ", "),
      IDs = paste(unique(top_matches$ID), collapse = ", ")
    )
    
    matched_results <- bind_rows(matched_results, matched_row)
  }
}



# View the first few rows of the matched results
head(matched_results)

matched_results_cleaned <- merge_ids_majority_overlap(matched_results, "IDs")

# Check the number of rows in matched_results compared to original datasets
cat("Rows in matched results:", nrow(matched_results), "\n")


df <- apply(matched_results_cleaned,2,as.character)

# Write the results to a CSV file
write.csv(df, "strawberry_fin.csv", row.names = FALSE)




#################### Wheat Flour #######################

# Example UUID you want to keep
wheat_flour <- "GGB100016"

# Filter the dataframe (assuming it's called df and has a column named UUID)
a_wheat_flour <- df_a[df_a$uuid == wheat_flour, ]
b_wheat_flour <- df_b[df_b$uuid == wheat_flour, ]
c_wheat_flour <- df_c[df_c$uuid == wheat_flour, ]

# Ensure each dataset has distinct entries
a_wheat_flour <- a_wheat_flour %>% distinct()
b_wheat_flour <- b_wheat_flour %>% distinct()
c_wheat_flour <- c_wheat_flour %>% distinct()

# Define thresholds
RI_threshold <- 10  # Adjust as needed
mass_threshold <- 0.1  # Adjust as needed


# Combine the merged datasets across labs
combined_data <- bind_rows(a_wheat_flour, b_wheat_flour, c_wheat_flour)

# Create a list of unique RI and MZ values
unique_values <- combined_data %>%
  select(ID, lab, ri, mz) %>%
  unique()


# Initialize an empty data frame to store matched results
matched_results <- data.frame()

for (i in 1:nrow(unique_values)) {
  ri_value <- unique_values$ri[i]
  mz_value <- unique_values$mz[i]
  index <- as.character(i)
  
  # Find matches across the combined dataset
  matches <- combined_data %>%
    filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
    mutate(index = index) %>%
    unique()
  
  if (nrow(matches) > 0) {
    # Keep only the match with the highest maxo per lab
    top_matches <- matches %>%
      group_by(lab) %>%
      slice_max(maxo, with_ties = FALSE) %>%
      ungroup()
    
    matched_row <- data.frame(
      ri = ri_value,
      mz = mz_value,
      labs = paste(unique(top_matches$lab), collapse = ", "),
      IDs = paste(unique(top_matches$ID), collapse = ", ")
    )
    
    matched_results <- bind_rows(matched_results, matched_row)
  }
}


# View the first few rows of the matched results
head(matched_results)

matched_results_cleaned <- merge_ids_majority_overlap(matched_results, "IDs")

# Check the number of rows in matched_results compared to original datasets
cat("Rows in matched results:", nrow(matched_results), "\n")


df <- apply(matched_results_cleaned,2,as.character)

# Write the results to a CSV file
write.csv(df, "wheat_flour.csv", row.names = FALSE)







#################### Green Pepper #######################

# Example UUID you want to keep
green_pepper <- "GGB100010"

# Filter the dataframe (assuming it's called df and has a column named UUID)
a_green_pepper <- df_a[df_a$uuid == green_pepper, ]
b_green_pepper <- df_b[df_b$uuid == green_pepper, ]
c_green_pepper <- df_c[df_c$uuid == green_pepper, ]

# Ensure each dataset has distinct entries
a_green_pepper <- a_green_pepper %>% distinct()
b_green_pepper <- b_green_pepper %>% distinct()
c_green_pepper <- c_green_pepper %>% distinct()

# Define thresholds
RI_threshold <- 10  # Adjust as needed
mass_threshold <- 0.1  # Adjust as needed


# Combine the merged datasets across labs
combined_data <- bind_rows(a_green_pepper, b_green_pepper, c_green_pepper)

# Create a list of unique RI and MZ values
unique_values <- combined_data %>%
  select(ID, lab, ri, mz) %>%
  unique()

# Initialize an empty data frame to store matched results
matched_results <- data.frame()

for (i in 1:nrow(unique_values)) {
  ri_value <- unique_values$ri[i]
  mz_value <- unique_values$mz[i]
  index <- as.character(i)
  
  # Find matches across the combined dataset
  matches <- combined_data %>%
    filter(abs(ri - ri_value) <= RI_threshold & abs(mz - mz_value) <= mass_threshold) %>%
    mutate(index = index) %>%
    unique()
  
  if (nrow(matches) > 0) {
    # Keep only the match with the highest maxo per lab
    top_matches <- matches %>%
      group_by(lab) %>%
      slice_max(maxo, with_ties = FALSE) %>%
      ungroup()
    
    matched_row <- data.frame(
      ri = ri_value,
      mz = mz_value,
      labs = paste(unique(top_matches$lab), collapse = ", "),
      IDs = paste(unique(top_matches$ID), collapse = ", ")
    )
    
    matched_results <- bind_rows(matched_results, matched_row)
  }
}


# View the first few rows of the matched results
head(matched_results)

matched_results_cleaned <- merge_ids_majority_overlap(matched_results, "IDs")

# Check the number of rows in matched_results compared to original datasets
cat("Rows in matched results:", nrow(matched_results), "\n")


df <- apply(matched_results_cleaned,2,as.character)

# Write the results to a CSV file
write.csv(df, "green_pepper.csv", row.names = FALSE)

