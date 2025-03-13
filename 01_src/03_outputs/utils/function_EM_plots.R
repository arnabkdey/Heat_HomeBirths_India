# Helper functions to generate effect modification plots ----

## plot for Odds Ratio
create_or_plot <- function(data, modifier) {
  ggplot(data, aes(y = ModifierLevel)) +
    geom_linerange(aes(xmin = OR_LowerCI, xmax = OR_UpperCI, y = ModifierLevel), 
                   color = "#A11217", size = 1, position = position_dodge(width = 0.7)) +
    geom_point(aes(x = OR), size = 3.5, color = "#A11217", position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 1, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(title = modifier, y = "", x = "OR with 95% CI") +
    xlim(0.7, 1.4) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
}

## plot for RERI
create_reri_plot <- function(data, modifier) {
  ggplot(data, aes(y = ModifierLevel)) +
    geom_segment(aes(x = 0, xend = RERI, y = ModifierLevel, yend = ModifierLevel), 
                 size = 5, color = "#6FAEF5", position = position_dodge(width = 0.7)) +
    geom_linerange(aes(xmin = RERI_LowerCI, xmax = RERI_UpperCI, y = ModifierLevel), 
                   color = "#102A6B", size = 1.2, position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 0, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(y = "", x = "RERI with 95% CI") +
    xlim(-0.75, 1) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
}

## Combine OR and RERI plots
create_modifier_plots <- function(or_data, reri_data, modifiers) {
  final_plots <- list()
  
  for (modifier in modifiers) {
    # Filter datasets
    or_filtered <- or_data |> filter(Modifier == modifier)
    reri_filtered <- reri_data |> filter(Modifier == modifier)
    
    # Generate OR and RERI plots
    ggplot_or <- create_or_plot(or_filtered, modifier)
    ggplot_reri <- create_reri_plot(reri_filtered, modifier)
    
    # Combine OR and RERI plots
    final_plot <- plot_grid(
      ggplot_or,
      ggplot_reri,
      ncol = 1, 
      align = "v", 
      rel_heights = c(1, 0.7)
    )
    
    # Store in list
    final_plots[[modifier]] <- final_plot
  }
  
  return(final_plots)
}