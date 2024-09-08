# Load necessary libraries
library(dplyr)
library(ggplot2)

# Function to prepare pedigree data by converting 0 to NA for missing parents
prepare_pedigree_data <- function(data) {
  data$Sire[data$Sire == 0] <- NA
  data$Dam[data$Dam == 0] <- NA
  return(data)
}

# Apply the preparation function to the dataset
merged_data <- prepare_pedigree_data(merged_data) #"merged_data from script 1

# Function to calculate Maximum Generations (MG) for each individual
calculate_mg <- function(data) {
  data <- data %>%
    mutate(MG = 0)  # Initialize MG column
  
  # Loop through each individual to calculate MG
  for (i in 1:nrow(data)) {
    progeny <- data$Progeny[i]
    generations <- 0
    current <- progeny
    
    while (!is.na(current)) {
      # Get the parents of the current individual
      parents <- data %>% filter(Progeny == current)
      parent_sire <- parents$Sire
      parent_dam <- parents$Dam
      
      # Break if both parents are missing
      if (is.na(parent_sire) & is.na(parent_dam)) {
        break
      }
      
      # Proceed with the next generation (prefer Sire if available)
      current <- ifelse(!is.na(parent_sire), parent_sire, parent_dam)
      generations <- generations + 1
    }
    
    # Assign the calculated MG value to the individual
    data$MG[i] <- generations
  }
  
  return(data)
}

# Calculate Maximum Generations for merged_data
merged_data <- calculate_mg(merged_data)

# Separate ancestor population (those with cod_alive == 0)
ancestor_population <- merged_data %>% filter(cod_alive == 0)

# Separate the living population (cod_alive == 1, where cod_alive is a column with 1 for alive and 0 for death animals)
living_population <- merged_data %>% filter(cod_alive == 1)

# Function to calculate Pedigree Completeness Levels (PCL)
calculate_pcl <- function(data, max_generations) {
  pcl <- sapply(0:max_generations, function(gen) {
    sum(data$MG >= gen) / nrow(data)
  })
  return(data.frame(Generation = 0:max_generations, PCL = pcl))
}

# Calculate the maximum generations and PCL for both populations
max_generations_anc <- max(ancestor_population$MG, na.rm = TRUE)
max_generations_living <- max(living_population$MG, na.rm = TRUE)

pcl_anc <- calculate_pcl(ancestor_population, max_generations_anc)
pcl_living <- calculate_pcl(living_population, max_generations_living)

# Function to plot PCL for ancestor and reference populations
plot_pcl <- function(pcl_ref, pcl_anc, title, filename) {
  plot <- ggplot() +
    geom_line(data = pcl_anc, aes(x = Generation, y = PCL, color = 'ANC'), size = 1) +
    geom_line(data = pcl_ref, aes(x = Generation, y = PCL, color = 'REF'), size = 1) +
    scale_color_manual(values = c('ANC' = 'red', 'REF' = 'blue')) +
    labs(title = title, x = "MG", y = "PCL", color = "Population") +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(plot)
  
  # Save the plot in a "results" folder within the project directory
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  ggsave(filename = file.path("results", filename), plot = plot, width = 8, height = 6, dpi = 300)
}

# Plot for living population and ancestor population with continuous lines
plot_pcl(pcl_living, pcl_anc, "PCL for Living Population and Ancestors", "figure_living_vs_anc.png")

# Final confirmation message
cat("Plots have been saved in the 'results' folder.\n")
