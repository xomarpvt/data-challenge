violin_plot <- function(transfers) {
    return (
        transfers |>
            ggplot(aes(x = token, y = amount, fill = token)) +
            geom_violin(
                scale = "width",
                colour = NA,
                alpha = 0.60,
                show.legend = FALSE
            ) +
            geom_boxplot(
                width = 0.10,
                fill = "white",
                colour = "grey25",
                linewidth = 0.5,
                outlier.shape = NA,
                show.legend = FALSE
            ) +
            scale_y_log10(
                breaks = 10^seq(-2, 9, by = 1),
                labels = scales::label_number(
                    scale_cut = scales::cut_short_scale(),
                    prefix = "$",
                    accuracy = 0.01
                )
            ) +
            labs(
                title = "Distribution of stablecoins transfer sizes",
                subtitle = paste0("Transfers above $", dust_threshold,
                                  "; each box shows quartiles and median"),
                x = NULL,
                y = "Transfer size (USD, log scale)"
            ) +
            theme_minimal() +
            theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
            )
    )
}