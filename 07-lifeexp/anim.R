life_exp_anim <- function(
        df_country,
        df_continent,
        df_world,
        title = "Evolution of life expectancy: {round(frame_time)}",
        subtitle = paste0("Small dots: countries   |   ",
                          "Large colored dots: continents   |   ",
                          "Large black dot: the whole world"),
        base_size = 17,
        country_alpha = 0.5,
        use_country_colors = FALSE,
        nframes = 600, fps = 60, end_pause = 150,
        width = 800, height = 600,
        file = NULL
    ) {
    
    p <- ggplot()
    
    if (use_country_colors) {
        p <- p +
            geom_point(data = df_country,
                       aes(x = pop, y = lifeExp, color = country, group = country),
                       size = 4.5, alpha = country_alpha) +
            scale_color_manual(values = gapminder::country_colors, guide = "none")
    } else {
        p <- p +
            geom_point(data = df_country,
                       aes(x = pop, y = lifeExp, color = continent, group = country),
                       size = 4.5, alpha = country_alpha) +
            scale_color_manual(values = gapminder::continent_colors, guide = "none")
    }
    
    p <- p +
        geom_point(data = df_continent,
                   aes(x = pop, y = lifeExp, fill = continent, group = continent),
                   shape = 21, size = 10, stroke = 1.2, color = "black") +
        
        scale_fill_manual(values = continent_colors, name = NULL) +
        guides(fill = guide_legend(override.aes = list(size = 5))) +
        
        geom_point(data = df_world,
                   aes(x = pop, y = lifeExp, group = 1),
                   shape = 21, size = 16, stroke = 1.5, fill = "black", color = "white") +
        
        scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                      breaks = 10^(4:10),
                      expand = expansion(mult = 0.03)) +
        scale_y_continuous(expand = expansion(mult = 0.06)) +
        
        coord_cartesian(clip = "off") +
        labs(
            title = title,
            subtitle = subtitle,
            x = "Population (log scale)",
            y = "Life expectancy at birth (years)") +
        theme_le(base_size) +
        theme(plot.title = element_text(face = "bold", size = rel(1.4))) +
        transition_time(year) +
        ease_aes("linear")  # frame transition
    
    anim <- animate(
        p,
        nframes = nframes, fps = fps, end_pause = end_pause,
        width = width, height = height,
        renderer = gifski_renderer()
    )
    
    if (!is.null(file)) anim_save(file, anim)
    
    anim
}