######################## Preparatory Part ########################

# Load necessary libraries
library(purgeR)
library(optiSel)
library(officer)
library(flextable)

# Load the data (assuming merged_data is loaded in the current environment from script 1)
# Prepare pedigree data by converting 0 to NA
prepare_pedigree_data <- function(data) {
  data$Sire[data$Sire == 0] <- NA
  data$Dam[data$Dam == 0] <- NA
  return(data)
}

# Apply the function to prepare the dataset
prepared_data <- prepare_pedigree_data(merged_data)

# Verify the structure of the prepared data
str(prepared_data)

######################## End of Preparatory Part ########################

######################## Parameters using purgeR ########################

# Prepare the pedigree data for purgeR
pedigree_data <- data.frame(
  id = as.integer(as.character(prepared_data$Progeny)),  # Convert Progeny to integer IDs
  sire = ifelse(is.na(prepared_data$Sire), 0, as.integer(prepared_data$Sire)),  # Convert Sire, replace NA with 0
  dam = ifelse(is.na(prepared_data$Dam), 0, as.integer(prepared_data$Dam)),  # Convert Dam, replace NA with 0
  reference = prepared_data$anno > 2005  # Logical vector for reference population
)

# Ensure correct data types
pedigree_data$id <- as.integer(pedigree_data$id)
pedigree_data$sire <- as.integer(pedigree_data$sire)
pedigree_data$dam <- as.integer(pedigree_data$dam)

# Verify the structure of the data prepared for purgeR
str(pedigree_data)

# Compute inbreeding coefficients and generation numbers using purgeR
pedigree_data <- ip_F(pedigree_data, name_to = "inbreeding")
pedigree_data <- pop_t(pedigree_data, name_to = "generation")

######################## Parameters using optiSel ########################

# Prepare the pedigree using optiSel
pedigree_optisel <- prePed(pedigree_data)

# Calculate generation numbers with optiSel
pedigree_summary <- summary(pedigree_optisel)

# Merge generation metrics into the pedigree data
pedigree_data$equiGen <- pedigree_summary$equiGen
pedigree_data$fullGen <- pedigree_summary$fullGen
pedigree_data$maxGen <- pedigree_summary$maxGen

# Verify the updated structure
str(pedigree_data)

######################## Analysis ########################

# Analyze the reference population
reference_population_results <- pop_Nancestors(pedigree_data, reference = "reference")

# Calculate Ne for the reference population using generation columns
Ne_max_ref <- pop_Ne(pedigree_data[pedigree_data$reference, ], Fcol = "inbreeding", tcol = "maxGen")
Ne_full_ref <- pop_Ne(pedigree_data[pedigree_data$reference, ], Fcol = "inbreeding", tcol = "fullGen")
Ne_equi_ref <- pop_Ne(pedigree_data[pedigree_data$reference, ], Fcol = "inbreeding", tcol = "equiGen")

# Analyze the total population
pedigree_data$reference_total <- TRUE
total_population_results <- pop_Nancestors(pedigree_data, reference = "reference_total")

# Calculate Ne for the total population
Ne_max_total <- pop_Ne(pedigree_data, Fcol = "inbreeding", tcol = "maxGen")
Ne_full_total <- pop_Ne(pedigree_data, Fcol = "inbreeding", tcol = "fullGen")
Ne_equi_total <- pop_Ne(pedigree_data, Fcol = "inbreeding", tcol = "equiGen")

# Remove the temporary column
pedigree_data$reference_total <- NULL

######################## Summary Table ########################

# Create a combined summary table for both populations
summary_table_combined <- data.frame(
  Parameter = c(
    "Total number of founders", "Total number of ancestors", 
    "Effective number of founders", "Effective number of ancestors", 
    "Effective number of founder genome equivalents", "Genetic drift", 
    "Bottleneck effect", "Genetic diversity lost due to bottleneck effect", 
    "Losses of founder alleles among generations", 
    "Effective population size: by increase in inbreeding by maximum generation", 
    "Effective population size: by increase in inbreeding by complete generation", 
    "Effective population size: by increase in inbreeding by equivalent generation"
  ),
  Reference_Population = c(
    reference_population_results$Nf, reference_population_results$Na, 
    reference_population_results$Nfe, reference_population_results$Nae, 
    reference_population_results$Ng, 
    reference_population_results$Nfe - reference_population_results$Ng, 
    reference_population_results$Nfe / reference_population_results$Nae, 
    1 - (1 / (2 * reference_population_results$Nae)), 
    1 - (reference_population_results$Nfe / reference_population_results$Nf), 
    Ne_max_ref$Ne, Ne_full_ref$Ne, Ne_equi_ref$Ne
  ),
  Total_Population = c(
    total_population_results$Nf, total_population_results$Na, 
    total_population_results$Nfe, total_population_results$Nae, 
    total_population_results$Ng, 
    total_population_results$Nfe - total_population_results$Ng, 
    total_population_results$Nfe / total_population_results$Nae, 
    1 - (1 / (2 * total_population_results$Nae)), 
    1 - (total_population_results$Nfe / total_population_results$Nf), 
    Ne_max_total$Ne, Ne_full_total$Ne, Ne_equi_total$Ne
  )
)

# Print the combined summary table
print(summary_table_combined)

# Write the results to a Word document
output_path <- "results/genetic_diversity_summary.docx"

# Create the directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Write the summary table to a Word document
doc <- read_docx() %>%
  body_add_par("Genetic Diversity Summary", style = "heading 1") %>%
  body_add_flextable(flextable(summary_table_combined))

# Save the Word document
print(doc, target = output_path)

cat("The summary table has been saved as a Word document in the 'results' folder.\n")
