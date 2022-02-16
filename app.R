library(shiny)
library(leaflet)
library(tidyverse)
library(viridis)
library(bslib)
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT) # For making fancy tables
library(shinyWidgets)
r_colors = rgb(t(col2rgb(colors()) / 255))
names(r_colors) = colors()
load("../../../Downloads/20220131/Gridded predictions/pred_2015.RData")


ui = fluidPage(
    theme = bs_theme(
        bg = "white",
        fg = "#009CDE",
        primary = "#009CDE",
        secondary = "#009CDE",
        base_font = font_google("Prompt")
    ),
    navbarPage(
        "Data integration model for air quality",
        tabPanel(
            "Gridded Predictions",
            sidebarLayout(
                sidebarPanel(
                    h3("Select a country"),
                    selectInput(
                        inputId = "countryselected",
                        label = "Select a Country",
                        choices = c("England", "France")
                    ),
                    br(),
                    selectInput(
                        inputId = "year",
                        label = "Select a Year",
                        choices = 2011:2016
                    ),
                    br(),
                    materialSwitch(
                        inputId = "mode",
                        label = icon("moon"),
                        right = TRUE,
                        status = "success"
                    )
                ),
                mainPanel(plotOutput("map")) #fin main panel
            )
        ),
        tabPanel("Ground Monitor Data"),
        tags$footer(
            HTML(
                "
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <div style=';background-color:#009CDE;'>
                           <a  style='text-align:centre !important;color:white !important' href='https://mdbootstrap.com/education/bootstrap/'> Made by the Enviromental Inteligence Lab, Exeter University</a>
                           </div>
                           <!-- Copyright -->

                           </footer>
                           <!-- Footer -->"
            )
        )
    )
)


server = function(input, output, session) {
    output$map = renderPlot({
        ggplot() + geom_raster(data = pred_2015, aes(
            x = Longitude,
            y = Latitude,
            fill = Mean
        )) + theme_minimal()

    })

    output$table = renderPlot({

    })

    output$hist = renderPlot({

    })

    observe(session$setCurrentTheme(if (isTRUE(input$mode)) {
        bs_theme(bootswatch = "superhero")
    } else {
        bs_theme(
            bg = "white",
            fg = "#009CDE",
            primary = "#009CDE",
            secondary = "#009CDE",
            base_font = font_google("Prompt")
        )
    }))
}

shinyApp(ui, server)
