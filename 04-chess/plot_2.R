scat_plot <- function(rating = "PointRating", y_lbl = "Point") {
    
    if (rating == "PointRating") {
        y_scale <- scale_y_continuous(
            breaks = seq(0, 1, by = 0.25),
            limits = c(0, 1)
        )
        cpt <- paste("Dashed line indicates a", tolower(y_lbl), "rating of 0.5")
    } else {
        y_scale <- scale_y_continuous(
            limits = c(-150, 350),
            breaks = c(-100, 0, 100, 200, 300),
            minor_breaks = seq(-150, 350, by = 50)
        )
        cpt <- paste("Dashed line indicates an", tolower(y_lbl), "rating of 0")
    }
    
    return (
        ggplot(player_ratings, aes(x = Matches, y = .data[[rating]])) +
        geom_hline(
            yintercept = 0.5,
            linetype = "dashed",
            colour = "#E76F51",
            linewidth = 0.8
        ) +
        geom_point(
            colour = "#2878B5",
            alpha = 0.25,
            size = 1.8
        ) +
        scale_x_log10(
            breaks = c(1, 10, 100, 1000),
            labels = c("1", "10", "100", "1,000")
        ) +
        y_scale +
        labs(
            title = paste(y_lbl, "Rating vs. Match Experience"),
            subtitle = "How rating varies with the number of matches played",
            x = "Matches played (log scale)",
            y = paste(y_lbl, "rating"),
            caption = cpt
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
            axis.text = element_text(colour = "#475467"),
            #panel.grid.minor = element_blank(),
            panel.grid.major = element_line(
                colour = "#E6EAF0",
                linewidth = 0.4
            ),
            plot.caption = element_text(
                colour = "#667085",
                size = 9,
                hjust = 0
            ),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.margin = margin(15, 18, 12, 12)
        )
    )
}