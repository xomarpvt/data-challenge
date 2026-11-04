line_plot <- function(transfers) {
    return (
        transfers |>
            arrange(time) |>
            group_by(token) |>
            mutate(cum_volume = cumsum(amount)) |>
            ungroup() |>
            ggplot(aes(x = time, y = cum_volume, colour = token)) +
            geom_step(linewidth = 1.0) +
            scale_y_continuous(
                labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
            ) +
            labs(
                title = "Cumulative stablecoins transfer volume over one hour",
                x = "Time (UTC)",
                y = "Cumulative volume (USD)",
                colour = NULL
            ) +
            theme_minimal() +
            theme(panel.grid.minor = element_blank())
    )
}