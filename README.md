# Martina_Franca_Donkey_Genetic_Analysis
Insights into pedigree and genome-based inbreeding pattern in Martina franca donkey breed
The following scripts were used to generate figures and tables included in the manuscript:

data_preparation&inbreeding_analysis.R 
For scripts that involve loading, cleaning, and formatting the pedigree data;  inbreeding coefficients calculation (FPED1, FPED3) and or generating plots that visualize inbreeding trends over time; analyzing the inbreeding coefficients by province and time period.

inbreeding_analysis_segmented_regression.R 

This R script performs an inbreeding analysis on a pedigreed population, using segmented regression to identify breakpoints and trends in the inbreeding coefficient (FPED3) over time. It generates visualizations, calculates segment equations, and compiles the results into a Word report. The script is designed for use with pedigree data and inbreeding coefficients.

Family_statistics.R: 

  1- Data Correction: Handles missing or incorrectly formatted birth dates and ensures consistency in pedigree information by identifying and addressing missing sires and dams.
  2- Pedigree Analysis: Uses the pedigree and kinship2 packages to calculate inbreeding coefficients based on family relationships and 3-visualize the trends in inbreeding over time.
  3- Family Statistics: Calculates family statistics, such as the number and size of full-sibling families, and the number of founders with offspring, for the total population, ancestor population, and reference population.
  4- Output: Generates a summary table of the family statistics (similar to Table 2) and saves it as a Word document in a specified folder. Additionally, the script creates a plot of inbreeding coefficients over time.
