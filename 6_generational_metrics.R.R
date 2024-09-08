# Load necessary libraries
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Function to calculate Maximum Generations (MG) and Complete Generations (CG)
maxgen <- function(pedigree_data) {
  if (!all(c("Progeny", "Sire", "Dam") %in% colnames(pedigree_data))) {
    stop("The dataset must contain 'Progeny', 'Sire', and 'Dam' columns.")
  }

  pedigree_data <- pedigree_data %>%
    mutate(MG = 0, CG = 0)

  calculate_generations <- function(indiv, generation_count) {
    if (is.na(indiv) || indiv == "") return(generation_count)
    individual_row <- pedigree_data[pedigree_data$Progeny == indiv, ]
    if (nrow(individual_row) == 0) return(generation_count)

    max_sire_gen <- calculate_generations(individual_row$Sire, generation_count + 1)
    max_dam_gen <- calculate_generations(individual_row$Dam, generation_count + 1)
    max(max_sire_gen, max_dam_gen)
  }

  for (i in 1:nrow(pedigree_data)) {
    indiv <- pedigree_data$Progeny[i]
    pedigree_data$MG[i] <- calculate_generations(indiv, 0)
  }

  calculate_complete_generations <- function(indiv, generation_count) {
    if (is.na(indiv) || indiv == "") return(generation_count)
    individual_row <- pedigree_data[pedigree_data$Progeny == indiv, ]
    if (nrow(individual_row) == 0 || is.na(individual_row$Sire) || is.na(individual_row$Dam)) return(generation_count)

    complete_sire_gen <- calculate_complete_generations(individual_row$Sire, generation_count + 1)
    complete_dam_gen <- calculate_complete_generations(individual_row$Dam, generation_count + 1)
    min(complete_sire_gen, complete_dam_gen)
  }

  for (i in 1:nrow(pedigree_data)) {
    indiv <- pedigree_data$Progeny[i]
    pedigree_data$CG[i] <- calculate_complete_generations(indiv, 0)
  }

  return(pedigree_data)
}

# Function to calculate Equivalent Generations (EG)
calculate_generational_metrics <- function(data) {
  ped <- maxgen(data)
  ped <- ped %>%
    mutate(EG = rowMeans(ped[, c("MG", "CG")], na.rm = TRUE))
  return(ped)
}

# Filter reference population (living animals, a column cod_alive contain 1 for alive and 0 for death animals)
reference_population <- merged_data %>% filter(cod_alive == 1)

# Calculate generational metrics
metrics_data <- calculate_generational_metrics(reference_population)

# Create density plots
metrics_data$Sex <- ifelse(metrics_data$sex_COD == 1, "male", "female")# if sex is codificated as number...

# Plot MG, CG, EG
plot_MG <- ggplot(metrics_data, aes(x = MG, fill = Sex)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge", color = "black") +
  labs(title = "A", x = "MG", y = "Count") +
  theme_minimal()

plot_CG <- ggplot(metrics_data, aes(x = CG, fill = Sex)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge", color = "black") +
  labs(title = "B", x = "CG", y = "Count") +
  theme_minimal()

plot_EG <- ggplot(metrics_data, aes(x = EG, fill = Sex)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge", color = "black") +
  labs(title = "C", x = "EG", y = "Count") +
  theme_minimal()

# Save plots
dir.create("results", showWarnings = FALSE)
ggsave(filename = "results/plot_MG_histogram.png", plot = plot_MG, width = 8, height = 6)
ggsave(filename = "results/plot_CG_histogram.png", plot = plot_CG, width = 8, height = 6)
ggsave(filename = "results/plot_EG_histogram.png", plot = plot_EG, width = 8, height = 6)

# Display plots
grid.arrange(plot_MG, plot_CG, plot_EG, ncol = 3)
ggsave(filename = "results/figure2_combined.png", plot = last_plot(), width = 8, height = 6)

# Create a table in Word
library(officer)
library(flextable)

output_table <- metrics_data %>%
  select(Progeny, Sex, MG, CG, EG)

ft <- flextable(output_table) %>%
  set_header_labels(Progeny = "Progeny", Sex = "Sex", MG = "Max Generations (MG)",
                    CG = "Complete Generations (CG)", EG = "Equivalent Generations (EG)") %>%
  autofit()

doc <- read_docx()
doc <- body_add_flextable(doc, ft)
doc <- body_add_par(doc, "Table: Figure 2 Tabular Data", style = "heading 1")
print(doc, target = "results/figure2_tabular_data.docx")

cat("The table has been saved as a Word document in the 'results' folder.\n")


##########################################################################################################################################################################

#alternative...not run
##################################following is the same script but plot are produced using density instead of direct count

# ################################## Alternative (Density Plot) ##################################
# # Load necessary libraries
# library(ggplot2)
# library(dplyr)
# 
# # Apply the functions to calculate MG, CG, and EG
# merged_data$MG <- calculate_mg(merged_data$Progeny, merged_data$Sire, merged_data$Dam)
# merged_data$CG <- calculate_cg(merged_data$Progeny, merged_data$Sire, merged_data$Dam)
# merged_data$EG <- calculate_eg(merged_data$Progeny, merged_data$Sire, merged_data$Dam)
# 
# # Add sex information as a factor
# merged_data$Sex <- factor(merged_data$SESSO_COD, labels = c("female", "male"))
# 
# # Plotting the results using density instead of direct count
# plot_mg <- ggplot(data, aes(x = MG, fill = Sex)) +
#   geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, position = 'identity') +
#   scale_fill_manual(values = c("female" = "salmon", "male" = "skyblue")) +
#   labs(x = "MG", y = "Density", fill = "Sex") +
#   theme_minimal() +
#   ggtitle("A")
# 
# plot_cg <- ggplot(data, aes(x = CG, fill = Sex)) +
#   geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, position = 'identity') +
#   scale_fill_manual(values = c("female" = "salmon", "male" = "skyblue")) +
#   labs(x = "CG", y = "Density", fill = "Sex") +
#   theme_minimal() +
#   ggtitle("B")
# 
# plot_eg <- ggplot(data, aes(x = EG, fill = Sex)) +
#   geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, position = 'identity') +
#   scale_fill_manual(values = c("female" = "salmon", "male" = "skyblue")) +
#   labs(x = "EG", y = "Density", fill = "Sex") +
#   theme_minimal() +
#   ggtitle("C")
# 
# # Arrange the plots in a grid
# grid.arrange(plot_mg, plot_cg, plot_eg, ncol = 3)
