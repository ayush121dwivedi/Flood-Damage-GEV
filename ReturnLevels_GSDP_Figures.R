library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(ggrepel)
library(purrr)
library(viridis)

states <- c(
  "ANDHRA PRADESH","ASSAM","BIHAR","GUJARAT","HARYANA","KARNATAKA",
  "KERALA","MADHYA PRADESH","MAHARASHTRA","ODISHA","PUNJAB","RAJASTHAN",
  "TAMIL NADU","UTTAR PRADESH","WEST BENGAL"
)
state_abbr <- c("AP","AS","BR","GJ","HR","KA","KL","MP","MH","OD","PB","RJ","TN","UP","WB")

mu <- c(11.29, 36.55, 68.58, 11.90, 12.09, 3.96, 15.66, 3.22,
        1.97, 15.57, 8.34, 22.86, 6.85, 76.52, 47.34)
xi <- c(1.80, 1.78, 1.63, 2.72, 2.25, 1.67, 1.84, 2.49,
        2.30, 1.87, 2.23, 2.27, 2.84, 1.73, 2.24)
sigma <- c(1598.78, 64.80, 135.50, 33.21, 30.06, 317.69, 126.40, 8.50,
           7.89, 418.00, 25.92, 69.85, 21.27, 292.43, 128.19)

gsdp_values_million <- c(
  "Andhra Pradesh" = 30000000,
  "Assam" = 5702000,
  "Bihar" = 8544000,
  "Gujarat" = 24258000,
  "Haryana" = 10955000,
  "Karnataka" = 25007000,
  "Kerala" = 11461000,
  "Madhya Pradesh" = 13633000,
  "Maharashtra" = 40443000,
  "Odisha" = 8535000,
  "Punjab" = 7449000,
  "Rajasthan" = 15284000,
  "Tamil Nadu" = 27216000,
  "Uttar Pradesh" = 25479000,
  "West Bengal" = 17009000
)

T_vals <- c(2, 20, 35, 50, 70, 100)

gev_return_level <- function(mu, sigma, xi, T) {
  p <- 1 - 1/T
  y <- -log(p)
  mu + (sigma/xi) * (y^(-xi) - 1)
}
df_all <- purrr::map_dfr(seq_along(states), function(i) {
  rl <- sapply(T_vals, function(T) gev_return_level(mu[i], sigma[i], xi[i], T))
  state_name <- str_to_title(states[i])
  gsdp_val <- gsdp_values_million[state_name]
  pct <- (rl / gsdp_val) * 100
  data.frame(State = state_name,
             Abbr = state_abbr[i],
             ReturnPeriod = T_vals,
             Percent_GSDP = pct)
})
n_states <- length(unique(df_all$State))
light_colors <- viridis(n_states, option = "C", begin = 0.3, end = 0.9)
dark_colors  <- viridis(n_states, option = "C", begin = 0, end = 0.7)   
names(light_colors) <- names(dark_colors) <- sort(unique(df_all$State))

df_range <- df_all %>%
  group_by(ReturnPeriod) %>%
  summarise(
    ymin = min(Percent_GSDP, na.rm = TRUE),
    ymax = max(Percent_GSDP, na.rm = TRUE)
  )
panel1 <- ggplot(df_all) +
  geom_ribbon(aes(x = ReturnPeriod, ymin = 0, ymax = Percent_GSDP, fill = State),
              alpha = 0.65, color = NA) +
  geom_line(data = df_range, aes(x = ReturnPeriod, y = ymax),
            color = "darkred", size = 0.5) +
  geom_line(data = df_range, aes(x = ReturnPeriod, y = ymin),
            color = "darkblue", size = 0.5) +
  geom_text(
    data = df_range %>% filter(ReturnPeriod == max(ReturnPeriod)),
    aes(x = ReturnPeriod + 1.5, y = ymax, label = sprintf("%.2f", ymax)),
    color = "darkred", hjust = 0, size = 4, fontface = "bold"
  ) +
  geom_text(
    data = df_range %>% filter(ReturnPeriod == max(ReturnPeriod)),
    aes(x = ReturnPeriod + 1.5, y = ymin, label = sprintf("%.2f", ymin)),
    color = "darkblue", hjust = 0, size = 4, fontface = "bold"
  ) +
  scale_fill_manual(values = light_colors) +
  scale_x_continuous(breaks = T_vals) +
  labs(y = NULL, x = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(2, 2, 2, 2)
  ) +
  annotate("text", x = min(T_vals), y = max(df_all$Percent_GSDP) * 1.05, label = "A",
           fontface = "bold", size = 6, hjust = 0, vjust = 1)
ranked_states <- df_all %>%
  filter(ReturnPeriod == 100) %>%
  arrange(desc(Percent_GSDP)) %>%
  pull(State)
state_groups <- split(ranked_states, rep(1:3, each = 5))

make_group_plot <- function(states_subset, label) {
  dat <- filter(df_all, State %in% states_subset)
  p <- ggplot(dat, aes(x = ReturnPeriod, y = Percent_GSDP, color = State)) +
    geom_line(size = 0.5) +
    geom_text_repel(
      data = filter(dat, ReturnPeriod == max(ReturnPeriod)),
      aes(label = Abbr),
      fontface = "bold",  
      nudge_x = 5,
      segment.color = NA,
      size = 3.2,
      direction = "y",
      hjust = 0
    ) +
    scale_color_manual(values = dark_colors) +
    scale_x_continuous(breaks = T_vals, limits = c(min(T_vals), max(T_vals) + 12)) +
    labs(y = NULL, x = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(2, 2, 2, 2)
    ) +
    annotate("text", x = min(T_vals), y = max(dat$Percent_GSDP) * 1.05, label = label,
             fontface = "bold", size = 5, hjust = 0, vjust = 1)
  return(p)
}

panel2_combined <- plot_grid(
  make_group_plot(state_groups[[1]], "B"),
  make_group_plot(state_groups[[2]], "C"),
  make_group_plot(state_groups[[3]], "D"),
  ncol = 3
)
y_axis <- ggdraw() + draw_label("Damage (% of GSDP)", angle = 90, fontface = "bold", size = 14)
x_axis <- ggdraw() + draw_label("Return Period (Years)", fontface = "bold", size = 14)

plots_combined <- plot_grid(panel1, panel2_combined, ncol = 1, rel_heights = c(1, 1.1))
final_plot <- plot_grid(y_axis, plots_combined, ncol = 2, rel_widths = c(0.05, 1))
final_plot <- plot_grid(final_plot, x_axis, ncol = 1, rel_heights = c(1, 0.05))

print(final_plot)
ggsave("Nature_Style_Multipanel_Improved.png", final_plot, width = 14, height = 9, dpi = 300)
