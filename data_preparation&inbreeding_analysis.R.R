# Load necessary libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(segmented)
library(officer)
library(flextable)
library(readxl)

# Generalized path to the pedigree file
# Assuming the pedigree file is stored in the "data" directory with the structure:
# Columns: Progeny (animal ID), Sire, Dam, Year of Birth (yob)
# assuming that pedigree have been checked before for inconsistency realted on sex and sire/Dam asignment mistake

pedigree_file <- "data/pedigree_data.xlsx"

# Load pedigree data from an Excel file
pedigree_data <- read_excel(pedigree_file)

# Select and rename relevant columns
pedigree_cleaned <- pedigree_data[, c("Progeny", "Sire", "Dam", "anno")] %>%
  rename(
    animal = Progeny,
    sire = Sire,
    dam = Dam,
    yob = anno
  )

# Ensure there are no extra spaces or missing columns in the cleaned pedigree data
#not mandatory
output_file <- "pedigree_cleaned.txt"

# Write cleaned pedigree data to a text file
write.table(
  pedigree_cleaned,
  file = output_file,
  quote = FALSE,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE
)

print(paste("Formatted pedigree file saved to", output_file))

# Run INBUPGF90 to calculate inbreeding coefficients
# assuming INBUPGF90 is inthe PC patch

# FPED1: using method 1
system("inbupgf90 --pedfile pedigree_cleaned.txt --method 1 --yob --niter 10 --allped")
file.rename("pedigree_cleaned.txt.solinb", "FPED1_output.txt")

# FPED3: using method by Meuwissen and Luo (1992)
system("inbupgf90 --pedfile pedigree_cleaned.txt --method 3 --yob --niter 10 --allped")
file.rename("pedigree_cleaned.txt.solinb", "FPED3_output.txt")

# Load inbreeding coefficients from INBUPGF90 outputs
FPED1 <- fread("FPED1_output.txt", header = FALSE, col.names = c("ID", "FPED1"))
FPED3 <- fread("FPED3_output.txt", header = FALSE, col.names = c("ID", "FPED3"))

# Convert ID columns to character type for merging
FPED1$ID <- as.character(FPED1$ID)
FPED3$ID <- as.character(FPED3$ID)

# Merge the pedigree-based inbreeding coefficients
pedigree_inbreeding <- merge(FPED1, FPED3, by = "ID")

# Print the merged pedigree inbreeding coefficients
print(pedigree_inbreeding)

# Assuming Progeny column in the original pedigree data contains matching IDs for merging
pedigree_data$Progeny <- as.character(pedigree_data$Progeny)

# Merge pedigree data with inbreeding coefficients
merged_data <- pedigree_data %>%
  inner_join(pedigree_inbreeding, by = c("Progeny" = "ID"))

# Inspect merged data
str(merged_data)
head(merged_data)

# Calculate average inbreeding per year of birth
average_inbreeding_per_year <- merged_data %>%
  group_by(anno) %>%
  summarize(
    avg_FPED1 = mean(FPED1, na.rm = TRUE),
    avg_FPED3 = mean(FPED3, na.rm = TRUE),
    num_inbreed_individuals = sum(FPED3 > 0, na.rm = TRUE)
  )

# Print the calculated averages
print(average_inbreeding_per_year)

# Add a new column for province based on the CODASL column (replace this with your own logic if needed)
merged_data$Province <- substr(merged_data$CODASL, 4, 5)

# Define the provinces (or other clustering ) of interest
provinces_of_interest <- c("TA", "BA", "BR", "FG", "LE", "BT")

# Group data by province
merged_data$Province_Group <- ifelse(merged_data$Province %in% provinces_of_interest, 
                                     merged_data$Province, 
                                     "Other")

# Filter data to include only years from 1970 onwards
# In our case we filtered out in the plots data before of 1970 to improve plot readability and avoid very old records
filtered_data <- merged_data %>%
  filter(anno >= 1970)

# Combine provinces BT and BA, and LE and BR for further analysis
filtered_data$Province_Group <- ifelse(filtered_data$Province_Group == "BT", "BA", filtered_data$Province_Group)
filtered_data$Province_Group <- ifelse(filtered_data$Province_Group == "LE", "BR", filtered_data$Province_Group)

# Create time periods for analysis
# in out case we are centering the comparison with populatuon before 2005 as the last similar study on this population (Rizzi el al 2011) 
# used database until these period

filtered_data$Time_Period <- cut(filtered_data$anno,
                                 breaks = c(-Inf, 2005, Inf),
                                 labels = c("Until 2005", "After 2005"))

# Count the number of observations for each province group and time period
numerosity_data <- filtered_data %>%
  group_by(Province_Group, Time_Period) %>%
  summarize(count = n())

# Print numerosity data
print(numerosity_data)

# Create directory for saving results if it doesn't exist
dir.create("results", showWarnings = FALSE)

# Plot average inbreeding over time (FPED1 and FPED3) with the number of inbreeding individuals
# in our case we filterd out data before 1970 to allow better plot readabilityn and avoid data from very ancient records
#edit according requirement

average_inbreeding_per_year_filtered <- average_inbreeding_per_year %>%
  filter(anno >= 1970)

p1 <- ggplot(average_inbreeding_per_year_filtered, aes(x = anno)) +
  geom_line(aes(y = avg_FPED1, color = "FPED1")) +
  geom_line(aes(y = avg_FPED3, color = "FPED3")) +
  geom_line(aes(y = num_inbreed_individuals / max(num_inbreed_individuals) * max(avg_FPED3), color = "Inbred Count"), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~ . * max(average_inbreeding_per_year_filtered$num_inbreed_individuals) / max(average_inbreeding_per_year_filtered$avg_FPED3), name = "Number of Inbred Individuals")) +
  labs(
    title = "Inbreeding Coefficient Trends Over Time (1970 onwards)",
    x = "Year of Birth",
    y = "Average Inbreeding Coefficient",
    color = "Metric"
  ) +
  theme_minimal()

# Save the plot
ggsave("results/inbreeding_trends_filtered.png", plot = p1, width = 10, height = 6)

# Box plot for inbreeding by province group and time period
filtered_data$Province_Group <- factor(filtered_data$Province_Group, levels = c("BA", "BR", "FG", "LE", "TA", "Other"))

p2 <- ggplot(filtered_data, aes(x = Province_Group, y = FPED3, fill = Time_Period)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(data = numerosity_data, aes(x = Province_Group, y = -0.02, label = count), 
            color = "black", size = 4, position = position_dodge(width = 0.8), vjust = 1.5) +
  labs(
    title = "Inbreeding Coefficient (FPED3) by Province Group and Time Period",
    x = "Province Group",
    y = "Inbreeding Coefficient (FPED3)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Until 2005" = "#1f77b4", "After 2005" = "#2ca02c")) +
  ylim(-0.05, max(filtered_data$FPED3, na.rm = TRUE) + 0.05)

# Save the box plot
ggsave("results/inbreeding_boxplot.png", plot = p2, width = 10, height = 6)

# Density plot of inbreeding coefficients by sex
filtered_data$sex <- factor(filtered_data$SESSO_COD, levels = c(1, 2), labels = c("Male", "Female"))

p3 <- ggplot(filtered_data, aes(x = FPED3, fill = sex)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(
    title = "Density Plot of FPED3 Inbreeding Coefficient by Sex",
    x = "FPED3",
    y = "Density",
    fill = "Sex"
  ) +
  theme_minimal()

# Save the density plot
ggsave("results/density_inbreeding_plot.png", plot = p3, width = 10, height = 6)

# Calculate descriptive statistics for each province group and time period
descriptive_stats <- filtered_data %>%
  group_by(Province_Group, Time_Period) %>%
  summarize(
    Mean = mean(FPED3, na.rm = TRUE),
    Median = median(FPED3, na.rm = TRUE),
    SD = sd(FPED3, na.rm = TRUE),
    IQR = IQR(FPED3, na.rm = TRUE)
  )

# Print the descriptive statistics
print(descriptive_stats)

#plot and data are avaialble in the R environment and can be simply written in 
# the pc, alternatively follow the code below to write formatted MS Word table


# Save descriptive statistics to a Word document
doc <- read_docx()

# Add descriptive statistics and plots to the Word document
ft_desc <- flextable(descriptive_stats)

doc <- doc %>%
  body_add_par("Descriptive Statistics by Province Group and Time Period", style = "heading 1") %>%
  body_add_flextable(ft_desc) %>%
  body_add_par("Boxplot of Inbreeding Coefficient by Province Group and Time Period", style = "heading 1") %>%
  body_add_img(src = "results/inbreeding_boxplot.png", width = 6, height = 4) %>%
  body_add_par("Trends of Inbreeding Coefficient Over Time", style = "heading 1") %>%
  body_add_img(src = "results/inbreeding_trends_filtered.png", width = 6, height = 4)

# Save the Word document
print(doc, target = "results/inbreeding_analysis.docx")

cat("Analysis complete. Plots and report saved in 'results'.\n")
