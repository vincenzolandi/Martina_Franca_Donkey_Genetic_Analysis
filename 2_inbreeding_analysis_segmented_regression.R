# Load necessary libraries
library(ggplot2)
library(dplyr)
library(segmented)
library(officer)
library(flextable)

# 1. Load the cleaned data (generated from the data preparation script)

# Assuming the pedigree data is stored in a file "pedigree_data.xlsx" in the "data" directory
# Make sure to update this path if necessary

pedigree_cleaned <- read.table("data/pedigree_cleaned.txt", header = TRUE, sep = "\t")


# 2. Inbreeding Analysis with Segmented Regression ------------------------

# Aggregate the data by year of birth
annual_inbreeding <- pedigree_cleaned %>%
  group_by(yob) %>%
  summarize(mean_FPED3 = mean(FPED3, na.rm = TRUE))

# Fit the initial linear model
lm_model <- lm(mean_FPED3 ~ yob, data = annual_inbreeding)

# Fit the segmented regression model (adjust number of breakpoints if necessary)
seg_model <- segmented(lm_model, seg.Z = ~yob, npsi = 7)

# Extract the breakpoints and segment equations
breakpoint_estimates <- round(na.omit(seg_model$psi[, "Est."]))
print("Breakpoints:")
print(breakpoint_estimates)

# Calculate slopes and intercepts for each segment
segment_slopes <- slope(seg_model)$yob
segment_intercepts <- coef(seg_model)[1] + cumsum(c(0, segment_slopes[-length(segment_slopes)] * diff(c(min(annual_inbreeding$yob), breakpoint_estimates))))

# Define the range of years for each segment
segment_ranges <- c(
  paste0("< ", breakpoint_estimates[1]), 
  sapply(2:length(breakpoint_estimates), function(i) paste(breakpoint_estimates[i-1], "-", breakpoint_estimates[i])),
  paste0("> ", breakpoint_estimates[length(breakpoint_estimates)])
)

# Ensure correct number of segments
segment_slopes <- segment_slopes[1:length(segment_ranges)]

# Create a table with the equations for each segment
equations_table <- data.frame(
  Range = segment_ranges,
  Equation = sapply(seq_along(segment_slopes), function(i) {
    slope <- round(segment_slopes[i], 5)
    intercept <- round(segment_intercepts[i], 4)
    paste0("y = ", slope, "x + ", intercept)
  })
)

# Print the equations table
print("Equations Table:")
print(equations_table)

# Save the results (breakpoints and equations) to a file if necessary
write.table(equations_table, "results/equations_table.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# 3. Visualization and Report Generation ----------------------------------

# Create directory for saving results if it doesn't exist
dir.create("results", showWarnings = FALSE)

# Plot the segmented regression
p <- ggplot(annual_inbreeding, aes(x = yob, y = mean_FPED3)) +
  geom_point() +
  geom_line(aes(y = predict(seg_model)), color = "red") +
  geom_vline(xintercept = breakpoint_estimates, linetype = "dashed", color = "blue") +
  labs(
    title = "Segmented Regression of Inbreeding Coefficient (FPED3) Over Time",
    x = "Year",
    y = "Mean Inbreeding Coefficient (FPED3)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a PNG file
ggsave("results/inbreeding_plot.png", plot = p, width = 10, height = 6)

# Create a Word document to summarize the results
doc <- read_docx()

# Add the plot and equations table to the Word document
ft <- flextable(equations_table)

doc <- doc %>%
  body_add_par("Segmented Regression of Inbreeding Coefficient (FPED3) Over Time", style = "heading 1") %>%
  body_add_img(src = "results/inbreeding_plot.png", width = 6, height = 4) %>%
  body_add_par("") %>%
  body_add_par("Segment Equations", style = "heading 2") %>%
  body_add_flextable(ft)

# Save the Word document
print(doc, target = "results/inbreeding_report.docx")

cat("Analysis complete. Plot and report saved in 'results'.\n")
