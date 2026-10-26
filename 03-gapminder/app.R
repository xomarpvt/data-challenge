#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(gapminder)
library(rlang)
library(dplyr)
library(ggplot2)
library(shiny)

gapminder <- gapminder::gapminder

aes_scales <- c(
    "lifeExp" = "linear",
    "pop" = "log",
    "gdpPercap" = "log"
)

aes_labels <- c(
    "Life expectancy" = "lifeExp",
    "Population" = "pop",
    "GDP per capita" = "gdpPercap"
)

gaplot <- function(year_fltr, cont_fltr, x_aes, y_aes, ttl) {
    
    gapminder |>
        filter(year == year_fltr) |>
        filter(cont_fltr == "All" | continent == cont_fltr) |>
        ggplot(
            aes(
                x = !!sym(x_aes),
                y = !!sym(y_aes),
                size = pop,
                colour = country
            )
        ) +
        scale_colour_manual(values = gapminder::country_colors) +
        scale_size(range = c(2, 20)) +
        (if (aes_scales[[x_aes]] == "log")
            scale_x_log10()
         else
             scale_x_continuous()
        ) +
        (if (aes_scales[[y_aes]] == "log")
            scale_y_log10()
         else
             scale_y_continuous()
        ) +
        geom_point(alpha = 0.5, show.legend = FALSE) +
        labs(
            title=ttl,
            x = names(aes_labels)[aes_labels == x_aes],
            y = names(aes_labels)[aes_labels == y_aes]
        ) +
        theme_minimal()
}

ui <- fluidPage(
    
    titlePanel("Shiny Gapminder"),
    
    sidebarLayout(
      
        sidebarPanel(
            
            sliderInput(
                inputId = "year",
                label = "Year",
                min = min(gapminder$year),
                max = max(gapminder$year),
                value = min(gapminder$year),
                step = 5
            ),
            
            selectInput(
                inputId = "cont",
                label = "Continent",
                choices = c(levels(gapminder$continent), "All"),
                selected = "All",
                selectize = TRUE
            ),
            
            radioButtons(
                inputId = "xvar",
                label = "X",
                choices = aes_labels,
                selected = "gdpPercap",
                inline = FALSE
            ),
            
            radioButtons(
                inputId = "yvar",
                label = "Y",
                choices = aes_labels,
                selected = "lifeExp",
                inline = FALSE
            )
            
        ),

        mainPanel(
           plotOutput("scatPlot")
        )
    )
)

server <- function(input, output) {

    output$scatPlot <- renderPlot({
        
        gaplot(input$year, input$cont, input$xvar, input$yvar, NULL)
        
    })
}

shinyApp(ui = ui, server = server)