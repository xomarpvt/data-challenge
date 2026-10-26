corr_plot <- function() {
    return (
    ggplot(player_ratings, aes(x = PointRating, y = EloRating, size = Matches)) +
        geom_vline(
            xintercept = 0.5,
            colour = "#98A2B3",
            linetype = "dashed",
            linewidth = 0.5,
            alpha = 0.75
        ) +
        geom_hline(
            yintercept = 0,
            colour = "#98A2B3",
            linetype = "dashed",
            linewidth = 0.5,
            alpha = 0.75
        ) +
        geom_smooth(
            method = "loess",
            formula = y ~ x,
            se = TRUE,
            span = 0.75,
            colour = "#8064A2",
            fill = "#B9A5D6",
            alpha = 0.40,
            linewidth = 1.1
        ) +
        geom_point(
            colour = "#168C83",
            alpha = 0.20
        ) +
        scale_size_continuous(
            range = c(1, 2.5),
            guide = "none"
        ) +
        scale_y_continuous(
            limits = c(-150, 350),
            breaks = c(-100, 0, 100, 200, 300),
            minor_breaks = seq(-150, 350, by = 50)
        ) +
        labs(
            title = "Point Rating vs. Elo Rating",
            subtitle = "Exploring the relationship between point and elo player ratings",
            x = "Point rating",
            y = "Elo rating",
            caption = "Dashed lines indicate Point Rating = 0.5 and Elo Rating = 0; LOESS curve highlights the local trend"
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