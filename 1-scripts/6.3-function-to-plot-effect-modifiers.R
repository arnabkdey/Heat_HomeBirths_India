#' @ param data: data frame with the following columns: exp_label, OR, CILow, CIHigh, contrast, effect_modifier
#' @ param effect_modifier_col: column name of the effect modifier
#' @ param effect_modifier_value: value of the effect modifier to filter the data
#' @ return: a plot with the effect modifier as the facet

plot_effect_modifier <- function(data, effect_modifier_col, effect_modifier_value) {
  # Pre-filter the data based on the effect modifier provided by the user
  effect_modifier_col_sym <- sym(effect_modifier_col)
  filtered_data <- data %>% filter(!!effect_modifier_col_sym == effect_modifier_value)

  # Create OR and CI related variables
  filtered_data <- filtered_data %>%
    mutate(OR = exp(estimate),
         CILow  = exp(conf.low),
         CIHigh = exp(conf.high))

  # Calculate the min and max for CILow and CIHigh
  # y_min <- min(filtered_data$CILow, na.rm = TRUE)
  y_min <- 0
  y_max <- max(filtered_data$CIHigh, na.rm = TRUE)

  # Generate the plot with Segoe UI font
  plot <- ggplot(filtered_data, aes(y=contrast, x=OR)) +
    facet_wrap(~exp_label, labeller = label_wrap_gen(width=22), nrow = 1) +
    geom_vline(xintercept=1, colour="darkgray") +  
    geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65), size = 0.5) +
    labs(y=effect_modifier_value, x="aOR") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
    theme_classic() +
    theme(
      panel.grid.major = element_line(linewidth=0.25), 
      panel.grid.minor.x = element_line(linewidth=0.15),
      strip.background = element_blank(),  
      strip.placement = "outside",  
      strip.text.y.left = element_text(angle = 0, family="Calibri"),
      text = element_text(size = 13, family="Calibri"),  
      axis.ticks = element_blank(), 
      axis.title.x = element_text(margin = margin(t = 15)), # Increase gap bw text and x axis by adjusting the top margin
      axis.title.y = element_text(margin = margin(r = 15)), # Increase gap bw text and y axis by adjusting the top margin
      panel.border = element_blank(),
      legend.position="none",
      panel.spacing=unit(0, "cm"), 
      plot.title = element_text(size=13, family="Calibri"),
    ) +
    scale_x_continuous(limits = c(0.4, 1.6)) +
    # scale_x_continuous(limits = c(y_min, y_max)) +
    coord_flip()
  
  return(plot)
}

plot_wrapper <- function(df) {
  plot_caste <- plot_effect_modifier(df, "effect_modifier", "Caste")
  plot_religion <- plot_effect_modifier(df, "effect_modifier", "Religion")
  plot_residence <- plot_effect_modifier(df, "effect_modifier", "Residence")
  plot_wealth <- plot_effect_modifier(df, "effect_modifier", "Wealth")
  plot_lt_temp_mean <- plot_effect_modifier(df, "effect_modifier", "Long-term mean temperature tertiles")
  
  plot_out <- ggarrange(plot_caste, plot_religion, 
                          plot_residence, plot_wealth, 
                          plot_lt_temp_mean, 
                        align = "h", ncol = 1, nrow=5, 
                        labels = c("a", "b", "c", "d", "e")) 
  return(plot_out)
}
