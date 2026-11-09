jitter_plot <- function(
        data,
        x, y,  # column names
        xlab, ylab, ttl = NULL,  # labels
        log_x = FALSE, log_y = FALSE,  # scales
        width = 0.025, height = 0.025,  # horizontal and vertical jitter
        alpha = 0.25,
        size = 0.25,
        colour = "#1f4e79") {
    
    p <- ggplot(
        data,
        aes({{ x }}, {{ y }})) +
        geom_jitter(
            width = width, height = height,
            alpha = alpha, size = size, colour = colour
        ) +
        labs(x = xlab, y = ylab, title = ttl)
    
    if (log_x) p <- p + scale_x_log10(labels = scales::label_comma())
    if (log_y) p <- p + scale_y_log10(labels = scales::label_comma())
    
    return (p)
}
