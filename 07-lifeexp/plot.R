
# Continent Colors are the mean of the colors of their countries
continent_colors <- gapminder::gapminder |>
    distinct(country, continent) |>
    mutate(color = gapminder::country_colors[as.character(country)]) |>
    group_by(continent) |>
    summarise(
        color = {
            m <- rowMeans(col2rgb(color))
            rgb(m[1], m[2], m[3], maxColorValue = 255)
        },
        .groups = "drop"
    )
continent_colors <- setNames(continent_colors$color, as.character(continent_colors$continent))

# Base Plot Theme
theme_le <- function(base_size = 14) {
    theme_minimal(base_size = base_size) +
        theme(
            plot.margin = margin(15, 15, 15, 15, unit = "pt"),
            plot.title.position = "plot",
            plot.title          = element_text(face = "bold"),
            panel.grid.minor    = element_blank(),
            legend.position     = "bottom"
        )
}

# Unique Plot Function
life_exp_plot <- function(
        data,
        level = "country",
        ylim = NULL,
        title = NULL,
        subtitle = NULL,
        base_size = 14,
        country_alpha = 0.5
    ) {
    
    level <- match.arg(level, c("country", "continent", "world"))
    
    last <- filter(data, year == max(year))
    p <- ggplot(data, aes(x = year, y = lifeExp))
    
    if (level == "country") {
        p <- p +
            geom_line(aes(group = country, color = country), alpha = country_alpha) +
            scale_color_manual(values = country_colors, guide = "none") +
            # invisible points for the legend
            geom_point(aes(fill = continent), shape = 21, alpha = 0, size = 3) +
            # shared colors
            scale_fill_manual(values = continent_colors, name = NULL) +
            guides(fill = guide_legend(
                override.aes = list(alpha = country_alpha, size = 4, color = NA)))
    }
    
    if (level == "continent") {
        p <- p +
            geom_line(aes(color = continent), linewidth = 1.1) +
            geom_point(aes(color = continent, size = pop), alpha = 0.9) +
            geom_text(data = last, aes(label = continent, color = continent),
                      hjust = 0, nudge_x = 1.5, fontface = "bold", size = 4) +
            scale_color_manual(values = continent_colors, guide = "none") +
            scale_size(range = c(2, 7), guide = "none")
    }
    
    if (level == "world") {
        p <- p +
            geom_line(color = "black", linewidth = 1.2) +
            geom_point(aes(size = pop), color = "black", alpha = 0.9) +
            geom_text(data = last, label = " World",
                      hjust = 0, nudge_x = 1.5, fontface = "bold", size = 4) +
            scale_size(range = c(2, 7), guide = "none")
    }
    
    p +
        scale_x_continuous(breaks = seq(1950, 2010, 5),
                           labels = function(x) ifelse(x %% 10 == 0, x, ""),
                           expand = expansion(mult = c(0.01, if (level == "country") 0.01 else 0.1))) +
        scale_y_continuous(expand = expansion(mult = 0.06)) +
        coord_cartesian(ylim = ylim, clip = "off") +
        labs(title = title, subtitle = subtitle,
             x = "Year", y = "Life expectancy at birth (years)") +
        theme_le(base_size)
    
}