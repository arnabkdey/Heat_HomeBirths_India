# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the distribution of heatwave thresholds across PSUs covered in DHS 2019-2021 in India
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(here, readxl, dplyr, tidyverse, stringr, ggplot2, ggmap, sf, rnaturalearth, rnaturalearthdata, cowplot)

# set paths ----
source(here("paths.R"))

# load data ----
df <- read_excel(here(path_project, "data",
  "processed_data", "1.2.3_psu_cutoffs_summary_geo.xlsx"), 
  col_names = TRUE)
nrow(df)
# categorize into intervals ----
df$db_85_cat <- cut(df$cutoff_tmax_db_85, 
                               breaks = c(-Inf, 32, 34, 36, 38, 40, Inf),  # Define interval breakpoints
                               labels = c("< 32", "32-34", "34-36", "36-38", "38-40", "40-42"),
                               right = FALSE)  # Ensures left-closed intervals


df$wb_85_cat <- cut(df$cutoff_tmax_wb_85, 
                    breaks = c(-Inf, 23, 25, 27, 29, 31, Inf),  # Define interval breakpoints
                    labels = c("< 23", "23-25", "25-27", "27-29", "29-31", "31-33"),
                    right = FALSE)  # Ensures left-closed intervals


# Get India's boundary as an sf object ----
india_boundary <- ne_countries(scale = "medium", country = "India", returnclass = "sf")

# Define a custom color palette (approximate extracted colors) ----
custom_colors <- rev(c(
  "#b2182b", # Deep Red (Hot)
  "#d6604d", # Light Red
  "#f4a582", # Peach
  "#fddbc7", # Pale Peach
  "#f7f7f7", # White (Neutral)
  "#d1e5f0", # Light Blue
  "#92c5de", # Sky Blue
  "#4393c3", # Deep Blue
  "#2166ac"  # Dark Blue (Cold)
))


custom_colors <- c(
  "#FFEB7B",  # Yellow (~100)
  "#FDC04E",  # Orange-Yellow (~150)
  "#E8853A",  # Orange (~200)
  "#D6522B",  # Red-Orange (~250)
  "#B53422",  # Deep Red (~300)
  "#7D1D1C"   # Darkest Red (~350)
)

# Plot the data ----
## Plot for dry bulb temperature
plot_db <- ggplot() +
  # Add India's polygon
  geom_sf(data = india_boundary, fill = "darkgray", color = "darkgray", size = 0.8) +
  
  # Add temperature points
  geom_point(data = df, aes(x = long, y = lat, color = db_85_cat), size = 0.005) +
  
  # Apply the custom color scale
  scale_color_manual(values = custom_colors) +
 
   # Increase legend dot size
  guides(color = guide_legend(override.aes = list(size = 5))) +  # Adjust size value as needed
  
  # Labels and theme
  labs(title = "B. Dry Bulb Temperature 85\u1d57\u02b0 percentile threshold",
       x = "Longitude", y = "Latitude", color = "Dry Bulb Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))


## Wet bulb temperature plot
plot_wb <- ggplot() +
  # Add India's polygon
  geom_sf(data = india_boundary, fill = "darkgray", color = "darkgray", size = 0.8) +
  
  # Add temperature points
  geom_point(data = df, aes(x = long, y = lat, color = wb_85_cat), size = 0.005) +
 
  # Apply the custom color scale
  scale_color_manual(values = custom_colors) +
  
  # Increase legend dot size
  guides(color = guide_legend(override.aes = list(size = 5))) +  # Adjust size value as needed
  
  
  # Labels and theme
  labs(title = "A. Wet Bulb Globe Temperature 85\u1d57\u02b0 percentile threshold",
       x = "Longitude", y = "Latitude", color = "Wet Bulb Globe Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

library(cowplot)

# Combine the plots without individual legends ----
combined_plot <- plot_grid(
  plot_wb,
  plot_db,
  ncol = 2, align = "v"  # Stack vertically
)

ggsave(here(path_project, "outputs", "figures", "plot_maps_heatwave_distribution.png"), 
  plot = combined_plot, 
  width = 14, 
  height = 10, 
  dpi = 500,
  bg = "white")


