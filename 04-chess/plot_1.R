dens_plot <- function(rating = "PointRating", x_lbl = "Point") {
    
    if (rating == "PointRating") {
        x_scale <- scale_x_continuous(
            breaks = seq(0, 1, by = 0.1),
            limits = c(0, 1),
            expand = expansion(mult = c(0, 0.01))
        )
    } else {
        x_scale <- scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10),
            expand = expansion(mult = c(0.02, 0.02))
        )
    }
    
    return (
        ggplot(player_ratings, aes(x = .data[[rating]])) +
        geom_histogram(
            aes(y = after_stat(density)),
            bins = 50,
            boundary = 0,
            fill = "#2878B5",
            colour = "white",
            linewidth = 0.3,
            alpha = 0.8
        ) +
        geom_density(
            linewidth = 1.2,
            colour = "#E76F51",
            adjust = 1
        ) +
        x_scale +
        labs(
            title = paste("Distribution of Player", x_lbl, "Ratings"),
            subtitle = "Histogram with an overlaid kernel density estimate",
            x = paste(x_lbl, "rating"),
            y = "Density",
            caption = "Bars show the empirical distribution; the curve represents the estimated density"
        ) +
        theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(
                size = 18,
                face = "bold",
                colour = "#243447"
            ),
            plot.subtitle = element_text(
                size = 11,
                colour = "#667085"
            ),
            axis.title = element_text(
                face = "bold",
                colour = "#344054"
            ),
            axis.text = element_text(
                colour = "#475467"
            ),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(
                colour = "#E6EAF0",
                linewidth = 0.4
            ),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.caption = element_text(
                size = 9,
                colour = "#667085",
                hjust = 0
            ),
            plot.margin = margin(15, 18, 12, 12)
        )
    )
}