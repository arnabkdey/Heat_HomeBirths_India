# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the coefficients of the full models for dry-bulb and wet-bulb temperature
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(here, readxl, dplyr, tidyverse, stringr, ggpubr, cowplot, gtable, grid)

# set paths ----
source(here("paths.R"))

# load data ----
data <- read_excel(here(path_project, "outputs", 
  "models", "full_models", "coefs_all_models.xlsx"), 
  col_names = TRUE)

# data cleaning ----
## rename tertile to sample
data <- data |> 
  rename(sample = tertile)

## Clean threshold
data$threshold[data$threshold == "80"] <- "80\u1d57\u02b0 pc"
data$threshold[data$threshold=="825"] <- "82.5\u1d57\u02b0 pc"
data$threshold[data$threshold=="85"] <- "85\u1d57\u02b0 pc"
data$threshold[data$threshold=="875"] <- "87.5\u1d57\u02b0 pc"
data$threshold[data$threshold=="90"] <- "90\u1d57\u02b0 pc"
data$threshold[data$threshold=="925"] <- "92.5\u1d57\u02b0 pc"
data$threshold[data$threshold=="95"] <- "95\u1d57\u02b0 pc"

## Clean duration
data$duration[data$duration=="1-day"] <- "1 day"
data$duration[data$duration=="2-day"] <- "2 days"
data$duration[data$duration=="3-day"] <- "3 days"
data$duration[data$duration=="4-day"] <- "4 days"
data$duration[data$duration=="5-day"] <- "5 days"


# process data for the plots ----
## Dry-bulb results
data_main_db <- data |> 
  #filter(sample == "alldata") |> 
  filter(exposure_type == "Dry Bulb") |> 
  arrange(duration, threshold) |>
  mutate(HWD = 1:35) |>   
  mutate(HWD = as.factor(HWD))

HWD_lbs_db <- paste0("HWD", data_main_db$HWD)

data_main_db$HWD <- factor(data_main_db$HWD, 
                      levels = c(1:35), 
                      labels = HWD_lbs_db)

data_main_db$duration <- factor(data_main_db$duration, levels = c("1 day", "2 days", "3 days", "4 days", "5 days"), ordered = TRUE)

#### Define the limit
x_max <- 1.65  # Upper limit for x-axis (which is the y-axis due to coord_flip)

### Create new variables for capped conf_high and for marking values exceeding the limit
data_main_db <- data_main_db |>
  mutate(
    conf_high_capped = ifelse(conf_high > x_max, x_max, conf_high),  # Cap conf_high
    exceeds_limit = conf_high > x_max  # Flag for exceeding limit
  )


##  Wet-bulb results
data_main_wb <- data |> 
  #filter(sample == "alldata") |> 
  filter(exposure_type == "Wet Bulb") |> 
  arrange(duration, threshold) |>
  mutate(HWD = 36:70) |>   
  mutate(HWD = as.factor(HWD))

HWD_lbs_wb <- paste0("HWD", data_main_wb$HWD)

data_main_wb$HWD <- factor(data_main_wb$HWD, 
                           levels = c(36:70), 
                           labels = HWD_lbs_wb)

data_main_wb$duration <- factor(data_main_wb$duration, levels = c("1 day", "2 days", "3 days", "4 days", "5 days"), ordered = TRUE)


# Define color palette ----
colors <- c(
  "#F4A3A3",  # Very light red
  "#F08080",  # Light but distinct red
  "#E34234",  # Medium red (more contrast)
  "#CC241D",  # Strong, saturated red
  "#A11217",  # Darker red with high contrast
  "#80000A",  # Very deep red
  "#4D0006"   # Almost black-red, high contrast
)

# Plotting ----
## Wet-bulb temperature plot
plot_wb <- ggplot(data_main_wb, aes(y = HWD, x = estimate, color = threshold)) +
  geom_vline(xintercept = 1, color = "black") +  # Reference line at 1
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), position = position_dodge(width = 0.5), size = 0.6) +  # Error bars
  geom_point(size = 3) +  # Points
  scale_color_manual(values = colors) +  # Custom colors
  facet_wrap(~ duration, scales = "free_x", nrow = 1) +  # Separate panel by threshold variable
  labs(
    title = "A. Wet Bulb Globe Temperature",
    y = "",
    x = "OR and 95% CI",
    color = "Temperature threshold"
  ) +
  theme_minimal() +
  coord_flip() +
  xlim(0.9, 1.7) +  # Setting limits to y-axis (which is now x due to coord_flip)
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),  # Change title size and centering
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor.y = element_line(linetype = "dotted", color = "gray", size=0.4),  # Dotted major grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size=0.4),  # Dotted major grid lines
    axis.text.y = element_text(size = 8.5),
    axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),  # Rotate labels
    axis.title.x = element_text(size = 8.5),
    axis.title.y = element_text(size = 8.5),
    strip.text = element_text(size = 10, face = "plain"),  # Smaller, non-bold text
    strip.placement = "outside",  # Keeps the strips positioned outside the plot
    strip.text.x = element_text(hjust = 0, vjust = 1)  # Move facet titles to upper-left
  )

## Dry-bulb temperature plot
plot_db <- ggplot(data_main_db, aes(y = HWD , x = estimate, color = threshold )) +
  geom_vline(xintercept = 1, color = "black") +  # Reference line at 1
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high_capped), position = position_dodge(width = 0.5), size = 0.6) +  # Error bars
  # Add arrows for those exceeding the limit
  geom_segment(
    data = data_main_db |> filter(exceeds_limit),
    aes(x = conf_high_capped, xend = x_max + 0.05, y = HWD, yend = HWD),
    arrow = arrow(length = unit(0.15, "cm")),
    color = "black"
  ) +
  geom_point(size = 3) +  # Points
  scale_color_manual(values = colors) +  # Custom colors
  facet_wrap(~ duration, scales = "free_x", nrow = 1) +  # Separate panel by threshold variable
  labs(
    title = "B. Dry Bulb Temperature",
    y = "",
    x = "OR and 95% CI",
    color = "Temperature threshold"
  ) +
  theme_minimal() +
  coord_flip() +
  xlim(0.9, 1.7) +  # Setting limits to y-axis (which is now x due to coord_flip)
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),  # Change title size and centering
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor.y = element_line(linetype = "dotted", color = "gray", size=0.4),  # Dotted major grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size=0.4),  # Dotted major grid lines
    axis.text.x = element_text(size = 6.5, angle = 45, hjust = 1),  # Rotate labels
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8.5),
    axis.title.x = element_text(size = 8.5),
    axis.title.y = element_text(size = 8.5),
    strip.text = element_text(size = 10, face = "plain"),  # Smaller, non-bold text
    strip.placement = "outside",  # Keeps the strips positioned outside the plot
    strip.text.x = element_text(hjust = 0, vjust = 1)  # Move facet titles to upper-left
  )



## Extract the legend using gtable
extract_legend <- function(p) {
  g <- ggplotGrob(p)
  legends <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  
  if (length(legends) > 0) {
    return(legends[[1]])  # Select the first legend if multiple exist
  } else {
    return(NULL)  # Return NULL if no legend found
  }
}

## Get the legend from plot_db
legend <- extract_legend(
  plot_db + theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )
)

## Remove individual legends from plots
plot_db_no_legend <- plot_db + theme(legend.position = "none")
plot_wb_no_legend <- plot_wb + theme(legend.position = "none")

## Combine the plots without individual legends
combined_plot <- plot_grid(
  plot_wb_no_legend,
  plot_db_no_legend,
  ncol = 1, align = "v"  # Stack vertically
)

## Convert legend into a graphical object
legend_grob <- ggdraw(legend) + 
  theme(plot.background = element_rect(fill = "white", color = "white"))

## Create final plot with all backgrounds set to white
final_plot <- ggdraw() + 
  draw_plot(combined_plot, 0, 0.1, 1, 0.9) +  # Main plots
  draw_plot(legend_grob, 0, 0, 1, 0.1) +  # Legend at bottom
  theme(plot.background = element_rect(fill = "white", color = "white"))

# Save the plot
ggsave(here(
  path_project, "outputs", "figures", "plot_full_models.png"),
  plot = final_plot, 
  width = 8, 
  height = 6, 
  dpi = 500,
  bg = "white")



