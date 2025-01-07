# Function to full model ----
func_plot_full_model <- function(df_plot) {
        plot <- ggplot(df_plot, aes(x = estimate, y = duration_label)) +
                geom_point() +
                facet_wrap(~threshold_label, ncol = 1) +
                geom_vline(xintercept = 1, colour = "black", linetype = "dashed") +
                geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
                geom_text(aes(label = format(round(estimate, 2), nsmall = 2)), vjust = -1.0, size = 4) +
                labs(x = "Adjusted Odds Ratio [95% CI]", y = "Heatwave Duration") +
                scale_x_continuous(trans = "log", 
                                 breaks = c(0.95, 1.0, 1.1, 1.2, 1.3, 1.4),
                                 limits = c(0.95, 1.4),
                                 expand = c(0, 0)) +
                theme_classic(base_size = 14, base_family = "Times New Roman") +
                theme(
                    panel.grid.major = element_line(linewidth=0.25), 
                    panel.grid.minor.x = element_line(linewidth=0.15),
                    strip.background = element_blank(),  
                    strip.placement = "outside",  
                    strip.text.y.left = element_text(angle = 0),
                    strip.text = element_text(face = "bold", size = 14, hjust = 0), # Added hjust = 0
                    strip.text.x = element_text(margin = margin(l = 140)), # Adjust this to adjust the WBGTmax >= 30°C label position
                    axis.ticks = element_blank(), 
                    axis.title.x = element_text(margin = margin(t = 15)),
                    axis.title.y = element_text(margin = margin(r = 15)),
                    panel.border = element_blank(),
                    legend.position="none",
                    panel.spacing=unit(1.5, "cm"))
        return(plot)
}
