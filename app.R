##################################################################
##                            Set-Up                            ##
##################################################################

source("utils/set-up.r")

##################################################################
##                              UI                              ##
##################################################################
ui = fluidPage(
    theme = bs_theme(
        bg = "#FCC780", fg = "black", primary = "#FCC780",
        base_font = font_google("Open Sans"),
        code_font = font_google("Open Sans")
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
                    br(),
                    p(
                        "DIMAQ was developed by the members of the Data Integration Task Force, a multi-disciplinary group of experts established as part of the recommendations from the first meeting of the WHO Global Platform for Air Quality in Geneva, January 2014. The resulting Data Integration Task Force consists of the first, fourth–ninth and 12th–16th authors of this paper together with members of the WHO (the 10th, 11th and 17th authors)."
                    ),
                    h4("Paper"),
                    a(
                        href = "https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssc.12227",
                        "A Hierarchical Approach to the Global Estimation of Exposures to Ambient Air Pollution"
                    ),
                    h4("Latest Data:"),
                    downloadButton("downloadDataGP2016", "Gridded Predictions 2016"),
                    h4("R Package:"),
                    code('install.package("dimaqdata")'),
                    br(),
                    tags$head(
                        tags$style(
                            ".butt{background-color:#add8e6;} .butt{color: #337ab7;margin-top:2%}"
                        )
                    ),
                )
            ),
            h2("Modelled estimates of particulate matter air pollution"),
            p(
                "Estimation of global health risks from exposure to ambient air pollution requires a comprehensive set of air pollution exposure data covering all inhabited areas. The Data Integration Model for Air Quality (DIMAQ) – developed by the University of Exeter – has produced estimates based on data from ground measurements (see the Database on air quality)  together with information from other sources including data from satellite retrievals of aerosol optical depth and chemical transport models. It provides estimates of annual concentrations to PM2.5 at high spatial resolution (0.1° × 0.1°, which equates to approximately 11x11km at the equator) globally."
            ),
            p(
                "The sources of data include: Ground measurements from 9690 monitoring locations around the world, satellite remote sensing; population estimates; topography; and information on local monitoring networks and measures of specific contributors of air pollution from chemical transport models. Within DIMAQ, data from these sources are calibrated with ground measurements. The model provides estimates of air quality, expressed in terms of median concentrations of PM2.5, for all regions of the world, including areas in which PM2.5 monitoring is not available."
            ),
            h2("Project Partners"),
            fluidRow(
                column(
                    6,
                    img(src="World_Health_Organization_logo.png", align = "left", height = 140, width = 400)
                ),
                column(
                    6,
                    img(src="University-of-Exeter-logo-1000x600.png", align = "right", height = 140, width = 400)
                )
            ),
            br(),
            h4("Past Data:"),
            downloadButton("downloadDataGP2015", "Gridded Predictions 2015", class = "butt"),
            downloadButton("downloadDataGP2014", "Gridded Predictions 2014", class = "butt"),
            downloadButton("downloadDataGP2013", "Gridded Predictions 2013", class = "butt"),
            downloadButton("downloadDataGP2012", "Gridded Predictions 2012", class = "butt"),
            downloadButton("downloadDataGP2011", "Gridded Predictions 2011", class = "butt"),
        ),
        tabPanel("Exceedances",
                 titlePanel("Exceedances"),
                 p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"),
                 br(),
                 sidebarLayout(

                     sidebarPanel(
                         selectInput(
                             inputId = "cat",
                             choices = unique(excceed$Category),
                             selected = "Country",
                             label = "Country or Region"
                         ),
                         selectizeInput(
                             inputId = "countryex",
                             label = "Select a country",
                             choices = unique(excceed$ID),
                             selected = sample(countries, 1),
                             multiple = TRUE
                         ),
                         selectInput(
                             inputId = "landclass",
                             choices = unique(excceed$UrbanRural),
                             selected = "Overall",
                             label = "Urban | Rural"
                         ),
                         selectInput(
                             inputId = "scale",
                             choices = unique(excceed$Scale),
                             selected = "10",
                             label = "Scale"
                         )
                     ),

                     # Show a plot of the generated distribution
                     mainPanel(plotlyOutput(outputId =  "p") %>% withSpinner(type = 6, color = "#009CDE"))
                 )
                 ),
        tabPanel("Spatial",
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
                 ),),
        br(),
        br(),
        tags$footer(
            HTML(
                "
                    <!-- Footer -->
                           <footer class='page-footer font-large indigo'>
                           <!-- Copyright -->
                           <a  style='text-align:centre !important;color:white !important' href='https://mdbootstrap.com/education/bootstrap/
                           <div style=';background-color:#009CDE;'><p style='text-align:center;'> Built by the Environmental Intelligence CDT </br> Exeter University </p></a>
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
            dplyr::select(Latitude, Longitude, Mean) %>%
            sample_n(30000)

        data$q <- as.numeric(cut(
            data$Mean,
            breaks = quantile(data$Mean, probs = c(0, 0.90, 0.95, 0.99, 1)),
            include.lowest = TRUE
        ))

        # Colors for each level
        col = c("#0055ff", "#00aaff", "#00ffaa", "#aaff00")[data$q]
        globejs(
            lat = data$Latitude,
            long = data$Longitude,
            value = data$Mean,
            color = col,
            pointsize = 0.5,
            atmosphere = TRUE,
            bg = "#0b3d91"
        )

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
        updateSelectInput(
            session,
            "countryex",
            choices =  excceed %>% filter(
                UrbanRural == input$landclass,
                Category == input$cat,
                Scale == input$scale
            ) %>% dplyr::select(ID)
        )
    })

    observe({
        cat <- input$cat

        # Can use character(0) to remove all choices
        if (cat== "Country"){
            # Can also set the label and select items
            updateSelectInput(session, "countryex",
                              label = paste("Select Country"))
        } else {
            updateSelectInput(session, "countryex",
                              label = paste("Select Region"))
        }



    })

    observe(session$setCurrentTheme(if (isTRUE(input$mode)) {
        bs_theme(bootswatch = "superhero")
    } else {
        bs_theme(
            bg = "#0b3d91", fg = "white", primary = "#FCC780",
            base_font = font_google("Open Sans"),
            code_font = font_google("Open Sans")
        )
    }))
}

#################################################################
##                            Build                            ##
#################################################################

shinyApp(ui, server)
