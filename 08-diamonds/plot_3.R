grouped_jitter_plot <- function(data) {
    data |>
        mutate(carat_bin = cut_number(carat, 5)) |>
        jitter_plot(
            quality, price,
            xlab = "Quality score", ylab = "Price",
            ttl = "Price vs quality within carat bins",
            log_y = TRUE,
            alpha = 0.05
        ) +
        geom_smooth(method = "lm", formula = y ~ x, colour = "red") +
        scale_x_continuous(breaks = c(0, 0.5, 1)) +
        facet_wrap(~carat_bin, nrow = 1) +
        theme_minimal() +
        theme(panel.spacing.x = unit(1, "lines"))
}