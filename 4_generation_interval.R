# Load necessary libraries
library(dplyr)
library(lubridate)
library(flextable)

# Define the function to calculate generation intervals
calculate_generation_intervals <- function(data) {
  # Ensure necessary columns are formatted correctly
  data <- data %>%
    mutate(
      Progeny = trimws(as.character(Progeny)),
      Sire = trimws(as.character(Sire)),
      Dam = trimws(as.character(Dam)),
      BirthYear = anno  # Directly use 'anno' for birth year
    )
  
  # Create a dataset for Sire birth years
  sire_data <- data %>%
    dplyr::select(Progeny, BirthYear) %>%  # Use dplyr::select to avoid conflict
    rename(Sire = Progeny, Sire_Born = BirthYear) %>%
    distinct()
  
  # Join Sire birth years back to the main dataset
  data <- left_join(data, sire_data, by = "Sire")
  
  # Create a dataset for Dam birth years
  dam_data <- data %>%
    dplyr::select(Progeny, BirthYear) %>%  # Use dplyr::select to avoid conflict
    rename(Dam = Progeny, Dam_Born = BirthYear) %>%
    distinct()
  
  # Join Dam birth years back to the main dataset
  data <- left_join(data, dam_data, by = "Dam")
  
  # Filter complete cases where we have all necessary information
  complete_data <- data %>%
    filter(!is.na(Sire) & !is.na(Dam) & !is.na(Sire_Born) & !is.na(Dam_Born))
  
  # Calculate generation intervals
  complete_data <- complete_data %>%
    mutate(
      sire_son_GI = ifelse(sex == 1, BirthYear - Sire_Born, NA),
      sire_daughter_GI = ifelse(sex == 2, BirthYear - Sire_Born, NA),
      dam_son_GI = ifelse(sex == 1, BirthYear - Dam_Born, NA),
      dam_daughter_GI = ifelse(sex == 2, BirthYear - Dam_Born, NA)
    )
  
  # Calculate average generation intervals
  average_GIs_list <- list(
    sire_son = mean(complete_data$sire_son_GI, na.rm = TRUE),
    sire_daughter = mean(complete_data$sire_daughter_GI, na.rm = TRUE),
    dam_son = mean(complete_data$dam_son_GI, na.rm = TRUE),
    dam_daughter = mean(complete_data$dam_daughter_GI, na.rm = TRUE)
  )
  
  # Adding averages for both sire paths, both dam paths, and overall average
  average_GIs_list$sire_average <- mean(c(average_GIs_list$sire_son, average_GIs_list$sire_daughter), na.rm = TRUE)
  average_GIs_list$dam_average <- mean(c(average_GIs_list$dam_son, average_GIs_list$dam_daughter), na.rm = TRUE)
  average_GIs_list$overall_average <- mean(c(average_GIs_list$sire_son, average_GIs_list$sire_daughter,
                                             average_GIs_list$dam_son, average_GIs_list$dam_daughter), na.rm = TRUE)
  
  return(average_GIs_list)
}

# Calculate generation intervals for the total population
tp_intervals <- calculate_generation_intervals(total_population)

# Calculate generation intervals for the reference population, (cod_alive= a column contianing 1 for alive or 0 for death animals)
reference_population <- total_population %>% filter(cod_alive == 1)  # Assuming 'cod_alive == 1' indicates reference
rp_intervals <- calculate_generation_intervals(reference_population)

# Create a summary table similar to Table 3
generation_intervals_table <- data.frame(
  Path = c("Sire - Son",
           "Sire - Daughter",
           "Sire Average",
           "Dam - Son",
           "Dam - Daughter",
           "Dam Average",
           "Overall"),
  TP = c(tp_intervals$sire_son,
         tp_intervals$sire_daughter,
         tp_intervals$sire_average,
         tp_intervals$dam_son,
         tp_intervals$dam_daughter,
         tp_intervals$dam_average,
         tp_intervals$overall_average),
  RP = c(rp_intervals$sire_son,
         rp_intervals$sire_daughter,
         rp_intervals$sire_average,
         rp_intervals$dam_son,
         rp_intervals$dam_daughter,
         rp_intervals$dam_average,
         rp_intervals$overall_average)
)

# Print the summary table to the console (table 3 in the paper)
print(generation_intervals_table)

# Create the risultati_short directory if it doesn't exist
dir.create("risultati_short", showWarnings = FALSE)

# Create a Word document with the table
ft <- flextable(generation_intervals_table)
ft <- theme_booktabs(ft)
ft <- set_caption(ft, caption = "Table 3: Values of generation intervals for each path, calculated for horses in TP and RP.")
ft <- fontsize(ft, part = "all", size = 10)
ft <- font(ft, part = "all", fontname = "Arial")
ft <- autofit(ft)

# Save the table to a Word document in the risultati_short folder
save_as_docx(ft, path = "risultati_short/result_table_3.docx")
