library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggspatial)
library(shadowtext)
library(cowplot)
library(grid)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
india_shapefile <- st_read("/Users/aayushdwivedi/Desktop/dissertation/Classwork_Data_1/ind_adm1.shp")
print(colnames(india_shapefile))  # "ST_NM" = state names
data <- read.csv("/Users/aayushdwivedi/Desktop/dissertation/Dissertation excel sheet/working old/Scatterplot & RR/Scatter plot image - R/gis rr/cc_crop.csv")
cap_at_100 <- function(x) ifelse(x > 100, 100, x)
data <- data %>%
  mutate(
    RR...Socio.Economic..Housing = cap_at_100(RR...Socio.Economic..Housing),
    RR...Socio.Economic..Crop = cap_at_100(RR...Socio.Economic..Crop),
    RR...Socio.Economic..Public.utilities = cap_at_100(RR...Socio.Economic..Public.utilities)
  )
data$State <- recode(data$State,
                     "Orissa" = "Odisha",
                     "Uttaranchal" = "Uttarakhand",
                     "NCT of Delhi" = "Delhi")
india_data <- india_shapefile %>%
  left_join(data, by = c("ST_NM" = "State"))
color_palette_upper <- c("#006837", "#31a354", "#78c679", "#c2e699",
                         "#ffffcc", "#fed976", "#fd8d3c", "#e31a1c")
color_palette_lower <- c("#08306b", "#2171b5", "#6baed6", "#c6dbef",
                         "#f7f7f7", "#fdd0a2", "#f16913", "#bd0026")
fixed_limit <- c(0, 100)
selected_states <- c(
  "Andhra Pradesh", "Assam", "Bihar", "Gujarat", "Haryana",
  "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra",
  "Odisha", "Punjab", "Rajasthan", "Tamil Nadu",
  "Uttar Pradesh", "West Bengal"
)
create_map <- function(variable, colors, fixed_limit = NULL,
                       add_scale = TRUE, remove_axes = TRUE) {
  india_data_proj <- st_transform(india_data, crs = 32644)
  label_data <- india_data_proj %>%
    filter(ST_NM %in% selected_states & !is.na(.data[[variable]])) %>%
    mutate(geometry = st_point_on_surface(geometry)) %>%
    st_transform(crs = 4326) %>%
    cbind(st_coordinates(.))
  
  tamil_nadu_label <- label_data %>% filter(ST_NM == "Tamil Nadu") %>%
    mutate(X = X + 2.2, Y = Y - 1.5)
  
  other_labels <- label_data %>% filter(ST_NM != "Tamil Nadu")
  
  p <- ggplot(india_data) +
    geom_sf(aes(fill = !!sym(variable)), color = "black", linewidth = 0.1) +
    
    geom_shadowtext(data = other_labels,
                    aes(x = X, y = Y, label = ST_NM),
                    color = "black", bg.color = "white",
                    size = 4.2, fontface = "bold", check_overlap = TRUE) +
    
    geom_shadowtext(data = tamil_nadu_label,
                    aes(x = X, y = Y, label = ST_NM),
                    color = "black", bg.color = "white",
                    size = 4.2, fontface = "bold", check_overlap = TRUE) +
    
    scale_fill_gradientn(
      colors = colors,
      limits = fixed_limit,
      breaks = if (!is.null(fixed_limit))
        seq(fixed_limit[1], fixed_limit[2], length.out = 5)
      else pretty(range(india_data[[variable]], na.rm = TRUE), n = 5),
      na.value = "white",
      name = "Risk Ratio"
    ) +
    
    coord_sf(expand = FALSE, xlim = c(67, 99), ylim = c(6, 38), datum = NA) +
    
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = if(remove_axes) element_blank() else element_text(size = 8),
      axis.ticks = if(remove_axes) element_blank() else element_line(linewidth = 0.4),
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = "white", color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
      plot.title = element_blank(),
      legend.key.width = unit(2.5, "cm"),   # wider legend bar
      legend.key.height = unit(0.5, "cm")
    )
  
  if (add_scale) {
    p <- p + annotation_scale(location = "bl", width_hint = 0.25)
  }
  
  return(p)
}
get_legend_plot <- function(p) {
  cowplot::get_legend(
    p + theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.width = unit(3, "cm")   # stretch across maps
    )
  )
}
map1 <- create_map("RR...Socio.Economic..Housing", color_palette_upper, fixed_limit) + theme(legend.position = "bottom")
map2 <- create_map("RR...Socio.Economic..Crop", color_palette_upper, fixed_limit) + theme(legend.position = "none")
map3 <- create_map("RR...Socio.Economic..Public.utilities", color_palette_upper, fixed_limit) + theme(legend.position = "none")

map4 <- create_map("RR.Climate.Change..Housing", color_palette_lower) + theme(legend.position = "bottom")
map5 <- create_map("RR.Climate.Change..Crop", color_palette_lower) + theme(legend.position = "none")
map6 <- create_map("RR.Climate.Change..Public.utilities", color_palette_lower) + theme(legend.position = "none")

# ---- Extract legends ----
legend_top <- get_legend_plot(map1)
legend_bottom <- get_legend_plot(map4)

# ---- Arrange top row with legend ----
top_row <- plot_grid(
  map1 + theme(legend.position = "none"),
  map2,
  map3,
  ncol = 3,
  labels = c("A", "B", "C"),
  label_fontface = "bold",
  label_size = 16,
  label_x = 0.05,   # shift labels right
  label_y = 0.98    # shift labels down
)
top_with_legend <- plot_grid(top_row, legend_top, ncol = 1, rel_heights = c(1, 0.12))
bottom_row <- plot_grid(
  map4 + theme(legend.position = "none"),
  map5,
  map6,
  ncol = 3,
  labels = c("D", "E", "F"),
  label_fontface = "bold",
  label_size = 16,
  label_x = 0.05,   # shift labels right
  label_y = 0.98    # shift labels down
)
bottom_with_legend <- plot_grid(bottom_row, legend_bottom, ncol = 1, rel_heights = c(1, 0.12))
final_maps <- plot_grid(
  top_with_legend,
  bottom_with_legend,
  ncol = 1,
  rel_heights = c(1, 1)
)
ggsave(
  filename = "/Users/aayushdwivedi/Desktop/India_RiskMaps.pdf",
  plot = final_maps,
  width = 18,
  height = 14
)
