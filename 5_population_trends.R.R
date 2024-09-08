#################################### FIGURE 1 ####################################
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Prepare data for plotting
# Ensure 'year' column exists and is properly formatted in the merged_data dataframe
merged_data <- merged_data %>% mutate(YearOfBirth = as.numeric(year))

# Filter data for 2006 onwards (focusing on the period after the last research by Rizzi et al. 2011)
data_post_2006 <- merged_data %>% filter(YearOfBirth >= 2006)

# Create periods for bar plot (Component A)
merged_data$Period <- cut(
  merged_data$YearOfBirth, 
  breaks = c(1900, 1950, 1970, 1990, 2010, 2023),
  labels = c("1900-1949", "1950-1969", "1970-1989", "1990-2009", "2010-2022"),
  right = FALSE
)

# Component A: Bar plot by defined periods
plot_A <- ggplot(merged_data, aes(x = Period, fill = factor(sex, labels = c("male", "female")))) +
  geom_bar(position = "dodge") +
  labs(x = "Period", y = "Count", fill = "Sex") +
  theme_minimal() +
  ggtitle("A") +
  theme(plot.title = element_text(hjust = 0.5))

# Component B: Yearly bar plot since 2006
plot_B <- ggplot(data_post_2006, aes(x = YearOfBirth, fill = factor(sex, labels = c("male", "female")))) +
  geom_bar(position = "stack") +
  labs(x = "Year of Birth", y = "Count", fill = "Sex") +
  theme_minimal() +
  ggtitle("B") +
  theme(plot.title = element_text(hjust = 0.5))

# Component C: Female-to-male ratio line plot since 2006
sex_ratio_data <- data_post_2006 %>%
  group_by(YearOfBirth) %>%
  summarise(
    females = sum(sex == 2, na.rm = TRUE),
    males = sum(sex == 1, na.rm = TRUE),
    ratio = ifelse(males > 0, females / males, NA)  # Avoid division by zero
  )

plot_C <- ggplot(sex_ratio_data, aes(x = YearOfBirth, y = ratio)) +
  geom_line(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(x = "Year of Birth", y = "Female-to-Male Ratio") +
  theme_minimal() +
  ggtitle("C") +
  theme(plot.title = element_text(hjust = 0.5))

# Create the directory for results if it doesn't exist
dir.create("results", showWarnings = FALSE)

# Arrange the plots and save
final_plot <- grid.arrange(plot_A, plot_C, plot_B, ncol = 2, layout_matrix = rbind(c(1, 1), c(2, 3)))

# Save the final plot to a file
ggsave("results/Figure_1.png", plot = final_plot, width = 12, height = 8)
