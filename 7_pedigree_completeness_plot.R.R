# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to calculate Pedigree Completeness Levels (PCL) for a population
calculate_pcl <- function(data, max_generations) {
  pcl <- sapply(0:max_generations, function(gen) {
    sum(data$MG >= gen) / nrow(data)
  })
  return(data.frame(Generation = 0:max_generations, PCL = pcl))
}

# Split dataset into ancestor (ANC) and reference (RP) populations based on cod_alive( living or death animals)  and year of birth (year)
ancestor_population <- merged_data %>% filter(cod_alive == 0)
reference_population <- merged_data %>% filter(cod_alive == 1)
#reference_population <- merged_data %>% filter(anno > 2006) # eventually reference can be calculated accordoing other factors

# Maximum generations for each population
max_generations_anc <- max(ancestor_population$MG, na.rm = TRUE)
max_generations_rp <- max(reference_population$MG, na.rm = TRUE)

# Calculate PCL for both populations
pcl_anc <- calculate_pcl(ancestor_population, max_generations_anc)
pcl_rp <- calculate_pcl(reference_population, max_generations_rp)

# Plot PCL for ANC and RP populations
pcl_plot <- ggplot() +
  geom_line(data = pcl_anc, aes(x = Generation, y = PCL, color = 'ANC'), size = 1) +
  geom_line(data = pcl_rp, aes(x = Generation, y = PCL, color = 'RP'), size = 1) +
  scale_color_manual(values = c('ANC' = 'red', 'RP' = 'blue')) +
  labs(title = "Pedigree Completeness Levels (PCL) over Maximum Generations Traced (MG)",
       x = "MG",
       y = "PCL",
       color = "Population") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the plot to the screen
print(pcl_plot)

# Create the 'results' directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Save the plot as PNG
ggsave(filename = "results/figure3_pcl_plot.png", plot = pcl_plot, width = 8, height = 6, dpi = 300)

cat("Plot saved in the 'results' directory.\n")
