##################################################################
##                            Set-Up                            ##
##################################################################

source("utils/set-up.r")

##################################################################
##                              UI                              ##
##################################################################
ui = fluidPage(
    theme = bs_theme(
        bg = "white",
        fg = "#009CDE",
        primary = "#009CDE",
        secondary = "#009CDE",
        base_font = font_google("Prompt")
    ),
    navbarPage(
        "DIMAQ",
        tabPanel(
            "Home",
            h1("Data Integration Model for Air Quality"),
            fluidRow(
                column(
                    8,
                    align = "left",
                    globeOutput("maphp") %>% withSpinner(type = 6, color = "#009CDE"),
                ),
                column(
                    4,
                    align = "left",
                    p("DIMAQ was developed by the members of the Data Integration Task Force, a multi-disciplinary group of experts established as part of the recommendations from the first meeting of the WHO Global Platform for Air Quality in Geneva, January 2014. The resulting Data Integration Task Force consists of the first, fourth–ninth and 12th–16th authors of this paper together with members of the WHO (the 10th, 11th and 17th authors)."),
                    h4("Paper"),
                    a(
                        href = "https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssc.12227",
                        "A Hierarchical Approach to the Global Estimation of Exposures to Ambient Air Pollution"
                    ),
                    h4("Latest Data:"),
                    downloadButton("downloadDataGP2016", "Gridded Predictions 2016"),
                    br(),
                    h4("Past Data:"),
                    downloadButton("downloadDataGP2015", "Gridded Predictions 2015"),
                    downloadButton("downloadDataGP2014", "Gridded Predictions 2014"),
                    downloadButton("downloadDataGP2013", "Gridded Predictions 2013"),
                    downloadButton("downloadDataGP2012", "Gridded Predictions 2012"),
                    downloadButton("downloadDataGP2011", "Gridded Predictions 2011"),
                    )
            ),
            h2("Modelled estimates of particulate matter air pollution"),
            p(
                "Estimation of global health risks from exposure to ambient air pollution requires a comprehensive set of air pollution exposure data covering all inhabited areas. The Data Integration Model for Air Quality (DIMAQ) – developed by the University of Exeter – has produced estimates based on data from ground measurements (see the Database on air quality)  together with information from other sources including data from satellite retrievals of aerosol optical depth and chemical transport models. It provides estimates of annual concentrations to PM2.5 at high spatial resolution (0.1° × 0.1°, which equates to approximately 11x11km at the equator) globally."
            ),
            p(
                "The sources of data include: Ground measurements from 9690 monitoring locations around the world, satellite remote sensing; population estimates; topography; and information on local monitoring networks and measures of specific contributors of air pollution from chemical transport models. Within DIMAQ, data from these sources are calibrated with ground measurements. The model provides estimates of air quality, expressed in terms of median concentrations of PM2.5, for all regions of the world, including areas in which PM2.5 monitoring is not available."
            )
        ),
        tabPanel(
            "Temporal",
            fluidRow(column(
                4,
                selectInput(
                    inputId = "scale",
                    choices = unique(excceed$Scale),
                    selected = "10",
                    label = "Scale"
                )
            ),
            column(
                4,
                selectInput(
                    inputId = "cat",
                    choices = unique(excceed$Category),
                    selected = "Country",
                    label = "Category"
                )
            ),
            column(
                4,
                selectInput(
                    inputId = "landclass",
                    choices = unique(excceed$UrbanRural),
                    selected = "Overall",
                    label = "Urban | Rural"
                )
            )
            ),
            fluidRow(
            column(
                12,
                align = "center",
                selectizeInput(
                    inputId = "countryex",
                    label = "Select a country",
                    choices = unique(excceed$ID),
                    selected = sample(countries,1),
                    multiple = TRUE
                ),
            )),
            plotlyOutput(outputId = "p")
        ),
        tabPanel(
            "Spatial",
            fluidRow(
                column(
                    6,
                    align = "center",
                    selectInput(
                        inputId = "country",
                        label = "Search for a country",
                        choices = countries,
                        selected = sample(countries, 1)

                    )
                ),

                column(
                    6,
                    align = "center",
                    selectInput(
                        inputId = "year",
                        label = "Select a Year",
                        choices = 2011:2016
                    )
                )
                # ,
                # column(
                #     2,
                #     materialSwitch(
                #         inputId = "mode",
                #         label = icon("moon"),
                #         right = TRUE,
                #         status = "success"
                #     )
                # )

            ),
            fluidRow(
                column(
                    12,
                    align = "center",
                    plotlyOutput("map") %>% withSpinner(type = 6, color = "#009CDE"),
                    dataTableOutput("table")
                )
            ),

        ),
        tags$footer(
            HTML(
                "
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <a  style='text-align:centre !important;color:white !important' href='https://mdbootstrap.com/education/bootstrap/
                           <div style=';background-color:#009CDE;'>'> Made by the Enviromental Inteligence Lab </br> Exeter University</a>
                           </div>
                           <!-- Copyright -->
                           </footer>
                           <!-- Footer -->"
            )
        )
    )
)

##################################################################
##                            Server                            ##
##################################################################

server = function(input, output, session) {
    # Reactive value for selected dataset ----

    output$maphp = renderGlobe({
        data = grid_prediction %>% dplyr::filter(Year == 2016) %>%
            select(Latitude, Longitude, Mean) %>%
            sample_n(30000)

        data$q <- as.numeric(
            cut(data$Mean,
                breaks=quantile(data$Mean, probs=c(0,0.90,0.95,0.99,1)),
                include.lowest=TRUE))

        # Colors for each level
        col = c("#0055ff","#00aaff","#00ffaa","#aaff00")[data$q]
        globejs(
                lat = data$Latitude,long = data$Longitude,
                value = data$Mean,
                color = col,
                pointsize = 0.5,
                atmosphere = TRUE,
                bg = "white")

    })



    output$map = renderPlotly({
        req(input$country)
        if (identical(input$country, ""))
            return(NULL)
        p = ggplot() + geom_tile(
                data = grid_prediction %>% filter(Year == input$year, CountryName == input$country),
                aes(
                    x = Longitude,
                    y = Latitude,
                    fill = Mean
                )
            ) + theme_bw() + theme(panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank())
        height = session$clientData$output_p_height
        width = session$clientData$output_p_width
        ggplotly(p, height = height, width = width)
    })

    output$table = renderDataTable({
        data = grid_prediction %>% filter(Year == input$year, CountryName == input$country)
        datatable(data,
                  options = list(scrollX = TRUE),
                  escape = FALSE)

    })

    output$p = renderPlotly({
        plot_ly(
            excceed %>% filter(
                UrbanRural == input$landclass,
                Category == input$cat,
                Scale == input$scale
            ),
            x = ~ Year,
            y = ~ Value,
            name = ~ ID
        ) %>%
            filter(ID %in% input$countryex) %>%
            group_by(ID) %>%
            add_lines()
    })

    observe({
        updateSelectInput(session, "countryex", choices =  excceed %>% filter(
            UrbanRural == input$landclass,
            Category == input$cat,
            Scale == input$scale
        ) %>% dplyr::select(ID))
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

#################################################################
##                            Build                            ##
#################################################################

shinyApp(ui, server)
