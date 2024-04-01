plot_effect_modifier <- function(data, effect_modifier_col, effect_modifier_value) {
  # Pre-filter the data based on the effect modifier provided by the user
  effect_modifier_col_sym <- sym(effect_modifier_col)
  filtered_data <- data %>% filter(!!effect_modifier_col_sym == effect_modifier_value)
  
  # Generate the plot with Segoe UI font
  plot <- ggplot(filtered_data, aes(y=contrast, x=OR)) +
    facet_wrap(~exposure, labeller = label_wrap_gen(width=22), nrow = 1) +
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
      panel.border = element_blank(),
      legend.position="none",
      panel.spacing=unit(0, "cm"), 
      plot.title = element_text(size=13, family="Calibri")
    ) +
    scale_x_continuous(limits = c(0.4, 1.6)) +
    coord_flip()
  
  return(plot)
}
