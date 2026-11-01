tree_map <- function(wt_sp500) {
    return(
        wt_sp500 |>
            ggplot(aes(area = weight, fill = weight, label = symbol)) +
            geom_treemap(colour = "white", size = 0.5) +
            geom_treemap_text(
                aes(colour = "white"),
                place    = "centre",
                grow     = TRUE,
                padding.x = grid::unit(2.5, "mm"),
                padding.y = grid::unit(2.5, "mm")
            ) +
            scale_colour_identity() +
            scale_fill_distiller(palette = "Blues", direction = 1, trans = "sqrt") +
            labs(
                title = "Treemap of S&P 500 Company Weights",
                fill  = "Weight (%)"
            ) +
            theme(
                panel.grid = element_blank(),
                axis.text  = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()
            )
    )
}