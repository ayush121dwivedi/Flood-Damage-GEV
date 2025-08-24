# Load Required Libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(stringr)
library(ggh4x)  # Required for facet_wrap2

# File Path
file_path <- "/Users/aayushdwivedi/Desktop/dissertation/Dissertation excel sheet/working old/CROP,UTIL,HOUSE -FAR, RR 95,75,55.xlsx"
data <- read_excel(file_path, sheet = "Total")

# Define time periods
data <- data %>%
  mutate(Period = factor(
    case_when(
      Year >= 1961 & Year <= 1990 ~ "1961-1990",
      Year >= 1991 & Year <= 2020 ~ "1991-2020"
    ),
    levels = c("1961-1990", "1991-2020")
  ))

# Remove NA values
data <- data %>% filter(!is.na(Period))

# Convert state names to Title Case
data <- data %>%
  mutate(ST = str_to_title(tolower(ST)))

# Select 15 states (excluding Sikkim)
selected_states <- c("Andhra Pradesh", "Assam", "Bihar", "Gujarat", "Haryana", 
                     "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", 
                     "Odisha", "Punjab", "Rajasthan", "Tamil Nadu", 
                     "Uttar Pradesh", "West Bengal")

data <- data %>% filter(ST %in% selected_states)

# Pivot to long format
data_long <- data %>%
  select(ST, Period, Util, Crop, Houses, Total) %>%
  pivot_longer(cols = c(Util, Crop, Houses, Total),
               names_to = "Variable", values_to = "Damage")

# Keep only positive finite values
data_long <- data_long %>%
  filter(is.finite(Damage), Damage > 0)

# Reorder states within each facet
data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(ST = reorder(ST, Damage, FUN = median))

# Ensure facet panels follow A, B, C, D order
data_long$Variable <- factor(
  data_long$Variable,
  levels = c("Total", "Crop", "Houses", "Util") # matches A, B, C, D
)

# Custom facet labels
variable_labels <- c(
  "Total" = "A. Total Damage",
  "Crop" = "B. Crop Damage",
  "Houses" = "C. Housing Damage",
  "Util" = "D. Public Utility Damage"
)

# Colors & line styles
custom_colors <- c("1961-1990" = "#F5DEB3", "1991-2020" = "#8B0000")
custom_border_colors <- c("1961-1990" = "black", "1991-2020" = "black")
custom_border_types <- c("1961-1990" = "dashed", "1991-2020" = "solid")

# Plot
p <- ggplot(data_long, aes(x = ST, y = Damage, fill = Period,
                           color = Period, linetype = Period)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, width = 0.7,
               position = position_dodge2(reverse = TRUE, padding = 0.2)) +
  scale_y_log10(labels = scales::comma, name = "Damage (Million Rs)") +
  scale_x_discrete(name = "State") +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_border_colors) +
  scale_linetype_manual(values = custom_border_types) +
  ggh4x::facet_wrap2(
    ~ Variable,
    scales = "free_y",
    ncol = 2,  # 2x2 layout
    labeller = as_labeller(variable_labels),
    strip.position = "top",
    axes = "all"
  ) +
  coord_flip(clip = "off") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),        
    strip.text = element_text(size = 16, face = "bold", color = "black"),
    panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),
    plot.margin = margin(10, 40, 10, 20)
  )

# Save corrected order plot
ggsave("/Users/aayushdwivedi/Desktop/dissertation/damage_boxplots_15states_corrected.png",
       p, width = 18, height = 12, dpi = 400, bg = "white")
