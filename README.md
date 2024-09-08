# script from the paper "Conservation and Genetic Analysis of the Endangered Martina Franca Donkey: Insights from a 15-Year Management Program"
**C. Pierini, V. Landi, P. De Palo
Department of Veterinary Medicine, University of Bari Aldo Moro, 70010 Valenzano, Italy

Insights into pedigree and genome-based inbreeding pattern in Martina franca donkey breed
The following scripts were used to generate figures and tables included in the manuscript:

-------

**1_data_preparation&inbreeding_analysis.R** – For scripts that involve loading, cleaning, and formatting the pedigree data;  inbreeding coefficients calculation (FPED1, FPED3) and or generating plots that visualize inbreeding trends over time; analyzing the inbreeding coefficients by province and time period.

**2_inbreeding_analysis_segmented_regression.R** - This R script performs an inbreeding analysis on a pedigreed population, using segmented regression to identify breakpoints and trends in the inbreeding coefficient (FPED3) over time. It generates visualizations, calculates segment equations, and compiles the results into a Word report. The script is designed for use with pedigree data and inbreeding coefficients.

**3_Family_statistics.R**: The script also computes family statistics, such as the number and size of full-sibling families, and the number of founders with offspring. 

**4_generation_interval.R**: This R script calculates the generation intervals for a population dataset, focusing on the differences between the total population (TP) and the reference population (RP). It processes sire and dam birth years, calculates generation intervals for sire-son, sire-daughter, dam-son, and dam-daughter paths, and averages these intervals. 

**5_population_trends.R**: This R script generates three visual components for Figure 1: (A) a bar plot of animal count by birth year periods and sex, (B) a yearly bar plot since 2006 to show the distribution of births by sex, and (C) a line plot showing the female-to-male ratio over time. The plots are arranged in a grid layout and saved as a PNG file in the results folder.

**6_generational_metrics.R**: This script calculates the Maximum Generations (MG), Complete Generations (CG), and Equivalent Generations (EG) from a pedigree dataset. It generates histograms for each metric, segmented by sex, and saves the plots as PNG files. 

**7_pedigree_completeness_plot.R**: This script calculates and plots Pedigree Completeness Levels (PCL) over Maximum Generations Traced (MG) for both the ancestor (ANC) and reference (RP) populations. The plot shows the comparison of pedigree completeness between the two populations

**8_genetic_diversity.R**: This script prepares pedigree data and calculates various genetic diversity metrics using the purgeR and optiSel packages. It computes inbreeding coefficients, generation numbers, and effective population sizes for both the reference and total populations.

-------
