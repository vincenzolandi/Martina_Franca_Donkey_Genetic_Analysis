# Load necessary libraries
library(lubridate)  # For handling date columns
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(pedigree)   # For pedigree analysis
library(kinship2)   # For calculating kinship
library(officer)    # For creating Word documents
library(flextable)  # For formatting tables in Word documents

# Step 1: Data Correction
# Function to correct missing birth dates using 'anno' column and handle format issues
#in some cases i just have year of birth without day and month
correct_missing_birth_dates <- function(data) {
  data <- data %>%
    mutate(DATA_NASCI = ifelse(is.na(DATA_NASCI), 
                               paste(anno, "01", "01", sep = "-"), 
                               DATA_NASCI))
  
  return(data)
}

# Apply the function to clean the date column
merged_data <- correct_missing_birth_dates(merged_data)

# Step 2: Data Preparation
# Convert Sire and Dam columns to character and replace "0" with NA
merged_data$Sire <- as.character(merged_data$Sire)
merged_data$Dam <- as.character(merged_data$Dam)
merged_data$Sire[merged_data$Sire == "0"] <- NA
merged_data$Dam[merged_data$Dam == "0"] <- NA

# Ensure consistency: Set both parents to NA if one is missing
incomplete_parentage <- which(is.na(merged_data$Sire) & !is.na(merged_data$Dam) | 
                                !is.na(merged_data$Sire) & is.na(merged_data$Dam))

merged_data$Sire[incomplete_parentage] <- NA
merged_data$Dam[incomplete_parentage] <- NA

# Step 3: Check for Missing Pedigree Entries
missing_sires <- setdiff(unique(na.omit(merged_data$Sire)), merged_data$Progeny)
missing_dams <- setdiff(unique(na.omit(merged_data$Dam)), merged_data$Progeny)

# Add missing sires and dams if necessary
if (length(missing_sires) > 0) {
  new_sires <- data.frame(Progeny = missing_sires, stringsAsFactors = FALSE)
  merged_data <- rbind(merged_data, new_sires)
}
if (length(missing_dams) > 0) {
  new_dams <- data.frame(Progeny = missing_dams, stringsAsFactors = FALSE)
  merged_data <- rbind(merged_data, new_dams)
}

# Step 4: Calculate Inbreeding Coefficients... just another methods (not run)
#sex <- ifelse(merged_data$SESSO_COD == 1, "M", "F")
#ped <- pedigree(id = merged_data$Progeny, dadid = merged_data$Sire, momid = merged_data$Dam, sex = sex)
#A <- kinship(ped)
#inbreeding_coefficients <- diag(A) - 1
#merged_data$Inbreeding <- inbreeding_coefficients

# Step 5: Plot Inbreeding Coefficients Over Time
#ggplot(merged_data, aes(x = DATA_NASCI, y = Inbreeding)) +
#  geom_point(alpha = 0.5) +
#  labs(title = "Inbreeding Coefficient Over Time", x = "Year of Birth", y = "Inbreeding Coefficient (F)") +
#  theme_minimal()

# ############################results#############################

# Step 6: Family Statistics (Table 2 in the paper)

calculate_family_stats <- function(data) {
  full_siblings <- data %>%
    filter(!is.na(Sire) & !is.na(Dam)) %>%
    group_by(Sire, Dam) %>%
    summarise(FamilySize = n(), .groups = 'drop')
  
  num_full_sibling_families <- nrow(full_siblings)
  avg_family_size <- ifelse(num_full_sibling_families > 0, mean(full_siblings$FamilySize), NA)
  max_family_size <- ifelse(num_full_sibling_families > 0, max(full_siblings$FamilySize), NA)
  min_family_size <- ifelse(num_full_sibling_families > 0, min(full_siblings$FamilySize), NA)
  
  founders <- data %>%
    filter(is.na(Sire) & is.na(Dam)) %>%
    dplyr::select(Progeny)
  
  num_founders <- nrow(founders)
  
  founder_offspring <- data %>%
    filter(Sire %in% founders$Progeny | Dam %in% founders$Progeny) %>%
    group_by(Founder = if_else(Sire %in% founders$Progeny, Sire, Dam)) %>%
    summarise(OffspringCount = n(), .groups = 'drop')
  
  founders_with_offspring <- founder_offspring %>%
    filter(OffspringCount > 2) %>%
    nrow()
  
  return(list(
    num_full_sibling_families = num_full_sibling_families,
    avg_family_size = avg_family_size,
    max_family_size = max_family_size,
    min_family_size = min_family_size,
    num_founders = num_founders,
    founders_with_offspring = founders_with_offspring
  ))
}

# Step 7: Population Statistics for Total Population (TP), Ancestor (ANC), and Reference Population (RP)
total_population <- merged_data
reference_population <- merged_data %>% filter(cod_alive == 1)
ancestors <- merged_data %>% filter(is.na(Sire) & is.na(Dam))

# Calculate family statistics for each population
tp_stats <- calculate_family_stats(total_population)
anc_stats <- calculate_family_stats(ancestors)
rp_stats <- calculate_family_stats(reference_population)

# Create Summary Table (Table 2)
summary_table <- data.frame(
  Parameter = c("Number of full-sibling families", "Average size of full-sibling family",
                "Maximum size of full-sibling family", "Minimum size of full-sibling family",
                "Number of founders", "Number of founders with > 2 offspring"),
  TP = c(tp_stats$num_full_sibling_families, tp_stats$avg_family_size, tp_stats$max_family_size, 
         tp_stats$min_family_size, tp_stats$num_founders, tp_stats$founders_with_offspring),
  ANC = c(anc_stats$num_full_sibling_families, anc_stats$avg_family_size, anc_stats$max_family_size, 
          anc_stats$min_family_size, anc_stats$num_founders, anc_stats$founders_with_offspring),
  RP = c(rp_stats$num_full_sibling_families, rp_stats$avg_family_size, rp_stats$max_family_size, 
         rp_stats$min_family_size, rp_stats$num_founders, rp_stats$founders_with_offspring)
)

# Step 8: Save the Summary Table to a Word Document

# Create the directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Create a Word document and add the summary table
doc <- read_docx() %>%
  body_add_par("Summary of Family Statistics", style = "heading 1") %>%
  body_add_table(summary_table, style = "table_template")

# Save the Word document
print(doc, target = "results/summary_table_corrected.docx")

cat("The summary table has been saved to 'results/summary_table_corrected.docx'\n")

