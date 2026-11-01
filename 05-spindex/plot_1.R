hist_plot = function(wt_sp500) {
    # --- Shared x limits ---
    x_lim <- range(wt_sp500$weight)
    
    # --- Histogram ---
    hist <- wt_sp500 |>
        ggplot(aes(x = weight)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "white") +
        coord_cartesian(xlim = x_lim) +
        labs(x = "Company weight (%)", y = "Number of companies")
    
    # --- Box plot ---
    box <- wt_sp500 |>
        ggplot(aes(x = weight)) +
        geom_boxplot(fill = "orange", alpha = 0.6, outlier.alpha = 0.5) +
        coord_cartesian(xlim = x_lim) +
        labs(x = NULL, y = NULL) +
        theme(  # Removing axis, ticks and y grid
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.subtitle = element_text(colour = "grey40", size=10)
        )
    
    return (
        # --- Composition ---
        box / hist +
            plot_layout(heights = c(1, 5)) +
            plot_annotation(
                title = "Distribution of S&P 500 Company Weights",
                subtitle = "The boxplot summarizes the weights, the histogram shows their frequency",
                theme = theme(plot.subtitle = element_text(colour = "grey40", size=10))
            )
    )
}