grouped_box_plot <- function(data) {
    data |>
        select(price, cut, color, clarity) |>
        pivot_longer(
            -price,
            names_to = "variable", values_to = "category",
            names_transform = \(x) factor(x, levels = c("cut", "color", "clarity")),
            values_transform = as.character
        ) |>
        mutate(category = factor(category, levels = c(cut_levels, color_levels, clarity_levels))) |>
        ggplot(aes(category, price)) +
        geom_boxplot(
            colour = "#1f4e79", fill = "#1f4e79", alpha = 0.15,
            outlier.alpha = 0.25, outlier.size = 0.25, outlier.colour = "#1f4e79"
        ) +
        stat_summary(fun = mean, geom = "point", colour = "red", size = 1.5) +
        facet_wrap(~variable, scales = "free_x") +
        scale_x_discrete(guide = guide_axis(angle = 45)) +
        scale_y_log10(labels = scales::label_comma()) +
        labs(x = "Level (worst to best)", y = "Price", title = "Price by quality level") +
        theme_minimal() +
        theme(panel.spacing.x = unit(1, "lines"))
}