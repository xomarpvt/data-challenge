lorenz_plot <- function(lc_df) {
    return (
        ggplot(lc_df, aes(x = p, y = L)) +
            geom_ribbon(aes(ymin = L, ymax = p), fill = "steelblue", alpha = 0.15) +  # Area
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey25") +  # Linear Line
            geom_line(colour = "steelblue", linewidth = 1.25) +  # Lorenz Curve
            # annotate("text", x = 0.05, y = 0.92, hjust = 0, label = paste0("Gini = ", round(gini, 3)), colour = "grey25", size = 4.5) +
            scale_x_continuous(labels = percent_format(), expand = c(0, 0)) +
            scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
            # coord_equal() +
            labs(
                title = "Lorenz Curve of S&P 500 Company Weights",
                subtitle = "The shaded area is the inequality gap, the dashed line is perfect equality",
                x = "Cumulative share of companies",
                y = "Cumulative share of weight"
            ) +
            theme(
                plot.margin = margin(10, 20, 10, 10),
                plot.subtitle = element_text(colour = "grey40", size=10)
            )
    )
}