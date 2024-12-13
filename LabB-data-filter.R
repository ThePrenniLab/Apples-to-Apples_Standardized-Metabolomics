# Load necessary libraries
library(dplyr)


# Read in the dataset
df <- read.csv("LabB_xcms_ri_pos.csv")

# Define RI and mass thresholds
ri_min <- 100
ri_max <- 600
mz_min <- 75
mz_max <- 1200


filtered_data <- df %>%
  filter(ri >= ri_min & ri <= ri_max, mz >= mz_min & mz<= mz_max)

carrot_ref <- filtered_data %>%
  filter(uuid == 'GGB100648')

wheat_flour <- filtered_data %>%
  filter(uuid == 'GGB100016')

beef_ribeye <- filtered_data %>%
  filter(uuid == 'GGB100032')

strawberry <- filtered_data %>%
  filter(uuid == 'GGB100215')

# Save the filtered data to a new CSV file
write.csv(wheat_flour, "LabB-pos-cleaned-noisomer-1-wheat-flour.csv")

# Save the filtered data to a new CSV file
write.csv(beef_ribeye, "LabB-pos-cleaned-noisomer-1-beef_ribeye.csv")

# Save the filtered data to a new CSV file
write.csv(strawberry, "LabB-pos-cleaned-noisomer-1-strawberry.csv")

# Save the filtered data to a new CSV file
write.csv(carrot_ref, "LabB-pos-cleaned-noisomer-1-carrotref.csv")
