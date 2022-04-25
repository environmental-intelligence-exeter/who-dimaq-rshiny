##################################################################
##                            Set-Up                            ##
##################################################################

source("utils/set-up.r")

##################################################################
##                              UI                              ##
##################################################################
ui = navbarPage(
  "The Data Integration Model for Air Quality ",
  # Create Right Side Logo/Image with Link
  tags$script(
    HTML(
      "var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"URL\"><img src=\"who.png\" alt=\"alt\" style=\"float:right;width:244px;height:82px;padding-top:10px;\"> </a></div>');
    console.log(header)"
    )
  ),
  theme = bs_theme(
    bg = "#0b3d91",
    fg = "white",
    primary = "#FCC780",
    base_font = font_google("Open Sans"),
    code_font = font_google("Open Sans")
  ),
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
    fluidRow(column(
      6,
      img(
        src = "who.png",
        align = "left",
        height = 140,
        width = 400
      )
    ),
    column(
      6,
      img(
        src = "University-of-Exeter-logo-1000x600.png",
        align = "right",
        height = 140,
        width = 400
      )
    )),
    br(),
    h4("Past Data:"),
    downloadButton("downloadDataGP2015", "Gridded Predictions 2015", class = "butt"),
    downloadButton("downloadDataGP2014", "Gridded Predictions 2014", class = "butt"),
    downloadButton("downloadDataGP2013", "Gridded Predictions 2013", class = "butt"),
    downloadButton("downloadDataGP2012", "Gridded Predictions 2012", class = "butt"),
    downloadButton("downloadDataGP2011", "Gridded Predictions 2011", class = "butt"),
  ),
  navbarMenu(
    "Temporal Data",
    tabPanel(
      "Global & Regional pm2.5 Concentraions",
      titlePanel("Global & Regional pm2.5 Concentraions"),
      p(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
      ),
      br(),
      sidebarLayout(
        #  Inputs excluded for brevity
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
          ),
          downloadButton("datadownload", "Download Data", class = "butt")


        ),

        mainPanel(tabsetPanel(
          tabPanel("Graph", plotlyOutput(outputId = "exceed_graph")),
          tabPanel("Table", dataTableOutput("exceed_table"))

        ))
      )
    ),
    tabPanel(
      "Global & Regional pm2.5 Exceedances",
      titlePanel("Global & Regional pm2.5 Exceedances"),
      p(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum"
      ),
      br(),
      sidebarLayout(
        #  Inputs excluded for brevity
        sidebarPanel(
          selectInput(
            inputId = "cat_conc",
            choices = unique(concentration$Category),
            selected = "Unweighted WHO Region",
            label = "Country or Region"
          ),
          selectizeInput(
            inputId = "country_conc",
            label = "Select a country",
            choices = unique(concentration$byvar),
            selected = "AFRO",
            multiple = TRUE
          ),
          selectInput(
            inputId = "landclass_conc",
            choices = unique(concentration$UrbanRural),
            selected = "Overall",
            label = "Urban | Rural"
          ),
          selectInput(
            inputId = "type_conc",
            choices = unique(concentration$Type),
            selected = "Population-weighted concentration",
            label = "Type"
          ),
          downloadButton("datadownloadex", "Download Data", class = "butt")


        ),

        mainPanel(tabsetPanel(
          tabPanel("Prediction Interval", plotOutput("conc_graph_ci")),
          tabPanel("Confidence Interval", plotOutput("conc_graph_pi")),
          tabPanel("Table", dataTableOutput("conc_table"))

        ))
      )

    ),
  ),
  navbarMenu(
    "Spatial Data",
    tabPanel(
      "Gridded Prediction Data",
      titlePanel("Gridded Prediction Data"),
      p(
        "Ground measurements were available for locations reported within the WHO ‘Air pollution in cities’ database (World Health Organization, 2016b) but, rather than using the city averages that are reported in that database, monitor-specific measurements are used. The result was measurements of PM10- and PM2.5-concentrations from 6003 ground monitors. "
      ),
      br(),
      sidebarLayout(
        selectInput(
          inputId = "grid_year",
          choices = 2011:2016,
          selected = 2011,
          label = "Select a year"
        ),

        mainPanel(tabsetPanel(
          tabPanel(
            "Graph",
            leafglOutput("my_leaf") %>% withSpinner(type = 6, color = "#009CDE")
          ),
          tabPanel("Table", dataTableOutput("grid_pred_table"))
        ))
      )
    ),
    tabPanel(
      "Ground Monitor Data",
      titlePanel("Ground Monitor Data"),
      p(
        "Ground measurements were available for locations reported within the WHO ‘Air pollution in cities’ database (World Health Organization, 2016b) but, rather than using the city averages that are reported in that database, monitor-specific measurements are used. The result was measurements of PM10- and PM2.5-concentrations from 6003 ground monitors. "
      ),
      br(),
#       sidebarLayout(
#         selectInput(
#           inputId = "ground_year",
#           choices = 2011:2016,
#           selected = 2011,
#           label = "Select a year"
#         ),
# #
# #         mainPanel(tabsetPanel(
# #           tabPanel(
# #             "Graph",
# #             leafglOutput("my_leaf") %>% withSpinner(type = 6, color = "#009CDE")
# #           ),
# #           tabPanel("Table", dataTableOutput("grid_pred_table"))
# #         ))
#       )
    ),
    tags$footer(
      HTML(
        "
                                    <!-- Footer -->
                                           <footer class='page-footer font-large indigo'>
                                           <!-- Copyright -->
                                           <a  style='text-align:centre !important;color:white !important' href='https://mdbootstrap.com/education/bootstrap/
                                           <div style=';background-color:#009CDE;'><p style='text-align:center;position:sticky;bottom:0'> Built by the Environmental Intelligence CDT, Exeter University </p></a>
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
  # Homepage globe
  output$maphp = renderGlobe({
    data = grid_prediction %>% dplyr::filter(Year == 2016) %>%
      dplyr::select(Latitude, Longitude, Mean) %>%
      sample_n(30000)

    data$q = as.numeric(cut(
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

  # Old map style for grid prediction
  # output$map = renderPlotly({
  #   req(input$country)
  #   if (identical(input$country, ""))
  #     return(NULL)
  #   p = ggplot() + geom_tile(
  #     data = grid_prediction %>% filter(Year == input$year, CountryName == input$country),
  #     aes(x = Longitude,
  #         y = Latitude,
  #         fill = Mean)
  #   ) + theme_bw() + theme(panel.grid.major = element_blank(),
  #                          panel.grid.minor = element_blank())
  #   height = session$clientData$output_p_height
  #   width = session$clientData$output_p_width
  #   ggplotly(p, height = height, width = width)
  # })
  # # old Table for grid prediction
  # output$table = renderDataTable({
  #   data = grid_prediction %>% filter(Year == input$year, CountryName == input$country)
  #   datatable(data,
  #             options = list(scrollX = TRUE),
  #             escape = FALSE)
  #
  # })

  # Exceedance Graph
  output$exceed_graph = renderPlotly({
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
  # Exceedance Table
  output$exceed_table =  renderDataTable({
    exceed_data =           excceed %>% filter(UrbanRural == input$landclass,
                                               Category == input$cat,
                                               Scale == input$scale) %>% filter(ID %in% input$countryex) %>%
      group_by(ID)
    datatable(exceed_data,
              options = list(scrollX = TRUE),
              escape = FALSE)

  })
  # Observe exceedance input
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

  # Concentration Graph confidence intervals
  output$conc_graph_ci = renderPlot({
    ggplot(
      concentration %>% dplyr::filter(byvar == input$country_conc) %>% dplyr::filter(Category == input$cat_conc) %>%
        dplyr::filter(Type == input$type_conc) %>% dplyr::filter(UrbanRural == input$landclass_conc),
      aes(Year, Mean)
    ) +        # ggplot2 plot with confidence intervals
      geom_point() +
      geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI))


  })
  # Concentration Table
  output$conc_table =  renderDataTable({
    concentration_data =   concentration %>% filter(
      UrbanRural == input$landclass_conc,
      Category == input$cat_conc,
      Type == input$type_conc
    ) %>% filter(byvar %in% input$country_conc) %>%
      group_by(byvar)
    datatable(concentration_data,
              options = list(scrollX = TRUE),
              escape = FALSE)

  })
  # Concentraion Graph predictor intervals
  output$conc_graph_pi = renderPlot({
    ggplot(
      concentration %>% dplyr::filter(byvar == input$country_conc) %>% dplyr::filter(Category == input$cat_conc) %>%
        dplyr::filter(Type == input$type_conc) %>% dplyr::filter(UrbanRural == input$landclass_conc),
      aes(Year, Mean)
    ) +        # ggplot2 plot with confidence intervals
      geom_point() +
      geom_errorbar(aes(ymin = LowerPI, ymax = UpperPI))


  })

  # Filtered df from grid_prediction
  df_filtered_grid = reactive({
    grid_prediction %>%
      dplyr::filter(Year == input$grid_year)
  })
  # Concentration Table
  output$ground_mon_table =  renderDataTable({
    datatable(df_filtered(),
              options = list(scrollX = TRUE),
              escape = FALSE)

  })

  # Leaflet map from grid prediction
  output$my_leaf = renderLeaflet({
    data_new = df_filtered() %>% dplyr::select("Longitude", "Latitude",  "Mean")

    r = raster::rasterFromXYZ(data_new)
    crs(r) = crs(who_world_map)

    leaflet(who_world_map) %>%
      addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        stroke = TRUE,
        fillOpacity = 0.3,
        color = "black",
        weight = 0.5
      ) %>%
      addRasterImage(r,  opacity = 0.5, colors = pal) %>%
      addLegend(pal = pal,
                values = values(r),
                title = "Mean Pm2.5 Prediction")

  })

  # Ground monitor reactive data
  df_filtered = reactive({
    grid_prediction %>%
      dplyr::filter(Year == input$ground_year)
  })
  # Concentration Table
  output$grid_pred_table =  renderDataTable({
    datatable(df_filtered(),
              options = list(scrollX = TRUE),
              escape = FALSE)

  })
  # Leaflet map from grid prediction
  # output$my_leaf = renderLeaflet({
  #   data_new = df_filtered() %>% dplyr::select("Longitude", "Latitude",  "Mean")
  #
  #   r = raster::rasterFromXYZ(data_new)
  #   crs(r) = crs(who_world_map)
  #
  #   leaflet(who_world_map) %>%
  #     addProviderTiles(provider = providers$Stamen.TerrainBackground) %>%
  #     addPolygons(
  #       stroke = TRUE,
  #       fillOpacity = 0.3,
  #       color = "black",
  #       weight = 0.5
  #     ) %>%
  #     addRasterImage(r,  opacity = 0.5, colors = pal) %>%
  #     addLegend(pal = pal,
  #               values = values(r),
  #               title = "Mean Pm2.5 Prediction")
  #
  # })

  # observe({
  #     updateSelectInput(
  #         session,
  #         "country_conc",
  #         choices =  concentration%>% filter(
  #             UrbanRural == input$landclass_conc,
  #             Category == input$cat_conc,
  #             Type == input$type_conc
  #         ) %>% dplyr::select(byvar)
  #     )
  # })

  # graph observe
  observe({
    cat = input$cat

    # Can use character(0) to remove all choices
    if (cat == "Country") {
      # Can also set the label and select items
      updateSelectInput(session, "countryex",
                        label = paste("Select Country"))
    } else {
      updateSelectInput(session, "countryex",
                        label = paste("Select Region"))
    }



  })

  observe({
    cat_conc = input$cat

    # Can use character(0) to remove all choices
    if (cat_conc == "Country") {
      # Can also set the label and select items
      updateSelectInput(session, "country_conc",
                        label = paste("Select Country"))
    } else {
      updateSelectInput(session, "country_conc",
                        label = paste("Select Region"))
    }



  })
}

#################################################################
##                            Build                            ##
#################################################################

shinyApp(ui, server)
