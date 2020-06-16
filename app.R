#################
# LOAD PACKAGES #
#################
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(dplyr)
library(shinyWidgets)
library(tigris)
library(DT)
library(rgdal)
library(dplyr)
library(countrycode)
library(shinycssloaders) #
library(data.table)
library(plotly)
library(shinyalert)

#########################
# IMPORT DATA FROM ECDC #
#########################
data <-
    read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
             header = TRUE)

#################
# DATA CLEANING #
#################
data$location <- gsub("Cote d'Ivoire", "Ivory Coast", data$location) 
data$date <- as.Date(data$date, format = "%Y-%m-%d")
todaydate = max(as.Date((data$date)))
startdate = "2020-01-01" #input it manually ~a month previous
options(scipen = 999)

####################
# IMPORT SHAPEFILE #
####################
world_spdf <- readOGR( #import spatial files
    dsn = paste0(getwd(), "/DATA/world_shape_file/") ,
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)

####
# SUBSET DATA FOR DATASET INCL. ONLY TODAYS  DATA
####
dataToday <- subset(data, date == todaydate)

###
# MERGE SHAPE FILE AND  DATASET
###
world_sf_merged <- geo_join(world_spdf, dataToday, "ISO3", "iso_code")


###
# CREATE LEAFLET POPUPS
###
popup_sb <-
    paste0(
        "<center>",
        "<strong>",
        "<code>",
        world_sf_merged$location,
        "</code>",
        "</strong>",
        "</center>",
        br(),
        "Total Cases: ",
        "<strong>",
        format(
            world_sf_merged$total_cases,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "Total Deaths: ",
        "<strong>",
        format(
            world_sf_merged$total_deaths,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "New Cases: ",
        "<strong>",
        format(
            world_sf_merged$new_cases,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "New Deaths: ",
        "<strong>",
        format(
            world_sf_merged$new_deaths,
            big.mark = ",",
            scientific = FALSE
        ),
        "</strong>",
        "<br/>",
        "Case Density: ",
        "<strong>",
        format(
            round(world_sf_merged$total_cases_per_million, 0),
            big.mark = ",",
            scientific = FALSE
        ),
        " per million individuals",
        "</strong>",
        "<br/>",
        "Death Density: ",
        "<strong>",
        format(
            round(world_sf_merged$total_deaths_per_million, 0),
            big.mark = ",",
            scientific = FALSE
        ),
        " per million individuals",
        "</strong>"
    )


##################
# GGPLOT WRITING #
##################

themeGG <- theme(
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(
        size = 25,
        family = "Avenir",
        color = 'black',
        hjust = 0.5,
        lineheight = 1.2
    ),
    axis.title.y = element_text(
        size = 15,
        family = "Avenir",
        color = "black"
    ),
    axis.title.x = element_text(
        size = 15,
        color = "black",
        family = "Avenir"
    ),
    axis.text.y = element_text(
        size = 10,
        family = "Avenir",
        color = "black"
    )
)

########################
# COLOR PALETTE GGPLOT #
########################
palC <-
    colorNumeric("YlOrBr",
                 domain = world_sf_merged@data$total_cases,
                 na.color = "transparent")
palD <-
    colorNumeric("YlOrBr",
                 domain = world_sf_merged@data$total_deaths,
                 na.color = "transparent")

palNC <-
    colorNumeric("YlOrBr",
                 domain = world_sf_merged@data$new_cases,
                 na.color = "transparent")

palCD <- colorNumeric("YlOrBr",
                      domain = world_sf_merged@data$total_cases_per_million,
                      na.color = "transparent")

palDD <- colorNumeric("YlOrBr",
                      domain = world_sf_merged@data$total_deaths_per_million,
                      na.color = "transparent")

#############
# START UI #
############
ui <- shinyUI(navbarPage(
    theme = shinytheme("flatly"),
    "COVID-19",
    tabPanel("Data Explorer",
             div(class = "outer",
                 fluidPage(fluidRow(
                     column(
                         12,
                         align = 'center',
                         br(),
                         fluidRow(
                             column(
                                 2,
                                 "",
                                 fluidRow(column(
                                     12,
                                     h5("Total Cases:", align = "center"),
                                     h1(textOutput("worldCase"), 
                                        style="color:darkblue",
                                        align = "center"),
                                 )),
                                 fluidRow(column(
                                     12,
                                     div(style = 'height: 220px; overflow-y: scroll',
                                         dataTableOutput("totalCasesTable"))
                                 )),
                                 fluidRow(column(
                                     12,
                                     hr(),
                                     useShinyalert(),
                                     selectInput(
                                         "country",
                                         "Country:",
                                         choices =
                                             dataToday$location,
                                         selected = "France",
                                     ),
                                     radioButtons(
                                         "radio",
                                         label = "Statistic:",
                                         inline = TRUE,
                                         selected = "Cases",
                                         choices = c("Cases", "Deaths", "New Cases", "Cases Per Million", "Deaths Per Million"),
                                     ),
                                     hr(),
                                 )),
                                 fluidRow(column(
                                     12,
                                     h6("Last Updated:", todaydate, ' at 6AM EST'),
                                     h6(
                                         "Source:",
                                         tags$a(href = "https://covid.ourworldindata.org/data/owid-covid-data.csv", "ECDC")
                                     ),
                                     tags$h6("Created by Elijah Silva", br(),
                                             a(href = "https://www.linkedin.com/in/elijahsilva/", "LinkedIn"), " | ",
                                             a(href = "https://github.com/Elijah-Silva/covid", "Github"), 
                                             align = "center")
                                 ))
                             ),
                             column(6,
                                    withSpinner(leafletOutput("map",
                                                  height = 711),type=6)),
                             column(4,
                                    "",
                                    fluidRow(
                                        column(
                                            6,
                                            h5("Total Deaths:", align = "center"),
                                            h1(textOutput("worldDeath"),
                                               style="color:darkred",
                                               align = "center"),
                                            div(style = 'height: 130px; overflow-y: scroll',
                                                dataTableOutput("totalDeathsTable"))
                                        ),
                                        column(
                                            6,
                                            h5("New Cases:", align = "center"),
                                            h1(textOutput("newCases"),
                                               style="color:darkorange",
                                               align = "center"),
                                            div(style = 'height: 130px; overflow-y: scroll;',
                                                dataTableOutput("totalNewCasesTable"))
                                        )
                                    ),
                                    hr(),
                                    fluidRow(column(
                                        12,
                                        withSpinner(plotlyOutput("plot1"),type=6),
                                        dateRangeInput(
                                            "dateRange",
                                            label = "Filter Date Range:",
                                            min = min(data$date),
                                            max = max(data$date),
                                            start = startdate,
                                            end = max(data$date)
                                        )
                                    )))
                             
                         )
                     )
                 ))))
))


################
# START SERVER #
################
server <- function(input, output, session) {
    location <- data$location
    dates <- format(data$date, format ="%Y-%m-%d")
    total_cases <- data$total_cases
    total_deaths <- data$total_deaths
    new_cases <- data$new_cases
    case_dens <- data$total_cases_per_million
    death_dens <- data$total_deaths_per_million
    
    df <-
        data.frame(dates, location, total_cases, total_deaths, new_cases, case_dens, death_dens)
    
    
    ##############
    # DATATABLES #
    ##############
    
    # Total Cases Table
    df_totalCases <- reactive({
        data %>%
            select(location, total_cases) %>%
            subset(as.Date(dates) == todaydate) %>%
            subset(location != "World") %>%
            arrange(desc(total_cases)) %>%
            format(data$total_cases,
                   big.mark = ",",
                   scientific = FALSE)
    })
    output$totalCasesTable <- renderDataTable({
        datatable(
            df_totalCases(),
            colnames = "",
            class = "compact",
            options = list(
                dom = 't',
                ordering = F,
                scrollX = TRUE,
                pageLength = 200
            ),
            rownames = FALSE,
            selection = "single",
        )
    })
    
    # Total Deaths Table
    df_totalDeaths <- reactive({
        data %>%
            select(location, total_deaths) %>%
            subset(as.Date(dates) == todaydate) %>%
            subset(location != "World") %>%
            arrange(desc(total_deaths)) %>%
            format(data$total_cases,
                   big.mark = ",",
                   scientific = FALSE)
    })
    output$totalDeathsTable <- renderDataTable({
        datatable(
            df_totalDeaths(),
            colnames = "",
            class = "compact",
            options = list(
                dom = 't',
                ordering = F,
                scrollX = TRUE,
                pageLength = 200
            ),
            rownames = FALSE,
            selection = "single"
        )
    })
    
    # Total New Cases Table
    df_totalNewCases <- reactive({
        data %>%
            select(location, new_cases) %>%
            subset(as.Date(dates) == todaydate) %>%
            subset(location != "World") %>%
            arrange(desc(new_cases)) %>%
            format(data$total_cases,
                   big.mark = ",",
                   scientific = FALSE)
    })
    output$totalNewCasesTable <- renderDataTable({
        datatable(
            df_totalNewCases(),
            class = "compact",
            colnames = "",
            options = list(
                dom = 't',
                ordering = F,
                scrollX = TRUE,
                pageLength = 200
            ),
            rownames = FALSE,
            selection = "single"
        )
    })
    
    #########################
    # TEXT ABOVE DATATABELS #
    #########################
    
    df_world <- reactive({
        subset(data, location == "World" &
                   as.Date(dates) == todaydate)
    })
    
    output$worldCase <- renderText({
        df1 <- df_world()$total_cases
        format(df1, big.mark = ",", scientific = FALSE)
    })
    
    output$worldDeath <- renderText({
        df2 <- df_world()$total_deaths
        format(df2, big.mark = ",", scientific = FALSE)
    })
    
    output$newCases <- renderText({
        df3 <- df_world()$new_cases
        format(df3, big.mark = ",", scientific = FALSE)
    })
    
    
    ############
    # PLOTTING #
    ############
    
    df_subset <- reactive({
        subset(
            df,
            location == input$country &
                as.Date(dates) >= input$dateRange[1] &
                as.Date(dates) <= input$dateRange[2]
        )
    })
    
    observe({
        if (input$country == "World") {
            shinyalert(
                title = "World Plot",
                text = "To create a plot for World, please update 'Filter Date Range' at the bottom right corner",
                closeOnEsc = TRUE,
                closeOnClickOutside = TRUE,
                html = FALSE,
                type = "",
                showConfirmButton = TRUE,
                showCancelButton = FALSE,
                confirmButtonText = "OK",
                confirmButtonCol = "#AEDEF4",
                timer = 0,
                imageUrl = "data:image/png;base64,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",
                imageWidth = 100,
                imageHeight = 100,
                animation = TRUE
            )
        }
    })
    
    observe({
        if (input$radio == "Cases") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggCases <- ggplot(data = df_subset(), aes(x = as.Date(dates))) +
                    geom_line(aes(y = total_cases),
                              color = 'darkred',
                              group = 5) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_x_date() +
                    themeGG +
                    labs(title = "Number of Cases",
                         y = "Number of Cases",
                         x = "Date")
                    
                    ggCases
                })
            })
        }
        if (input$radio == "Deaths") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggDeaths <- ggplot(data = df_subset(), aes(x = as.Date(dates))) +
                    geom_line(aes(y = total_deaths),
                              color = 'steelblue',
                              group = 5) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_x_date() +
                    labs(title = "Number of Deaths",
                         y = "Number of Deaths",
                         x = "Date") +
                    themeGG
                    
                    ggDeaths
                })
                
            })
        }
        if (input$radio == "New Cases") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggNew <- ggplot(data = df_subset(), aes(x = as.Date(dates), y = new_cases)) +
                    geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_x_date() +
                    labs(title = "New Cases (last 24 hours)",
                         y = "Number of New Cases",
                         x = "Date") +
                    themeGG
                    
                    ggNew
                })
            })
        }
        if (input$radio == "Cases Per Million") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggCD <- ggplot(data = df_subset(), aes(x = as.Date(dates), y = case_dens)) +
                    geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_x_date() +
                    labs(title = "Numer of Cases per Million",
                         y = "Number of Cases Per Million",
                         x = "Date") +
                    themeGG
                    
                    ggCD
                })
            })
        }
        if (input$radio == "Deaths Per Million") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggDD <- ggplot(data = df_subset(), aes(x = as.Date(dates), y = death_dens)) +
                    geom_bar(stat = "identity") +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    scale_x_date() +
                    labs(title = "Numer of Deaths per Million",
                         y = "Number of Deaths Per Million",
                         x = "Date") +
                    themeGG
                    
                    ggDD
                })
            })
        }
    })
    
    ###########
    # LEAFLET #
    ###########
    
    observe({
        if (input$radio == "Cases") {
            output$map <- renderLeaflet({
                coordCode <- countrycode(input$country, origin = 'country.name', destination = 'iso3c')
                lat <- subset(world_sf_merged@data$LAT, world_sf_merged@data$ISO3 == coordCode)
                lng <- subset(world_sf_merged@data$LON, world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat, lng = lng, zoom = 4) %>%
                    addPolygons(
                        data = world_sf_merged ,
                        fillColor = ~ palC(world_sf_merged$total_cases),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palC,
                        values = world_sf_merged$total_cases,
                        position = "bottomright",
                        title = "Total Cases"
                    )
            })
        }
        if (input$radio == "Deaths") {
            output$map <- renderLeaflet({
                coordCode <- countrycode(input$country, origin = 'country.name', destination = 'iso3c')
                lat <- subset(world_sf_merged@data$LAT, world_sf_merged@data$ISO3 == coordCode)
                lng <- subset(world_sf_merged@data$LON, world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat, lng = lng, zoom = 4) %>%
                    addPolygons(
                        data = world_sf_merged ,
                        fillColor = ~ palD(world_sf_merged$total_deaths),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palD,
                        values = world_sf_merged$total_deaths,
                        position = "bottomright",
                        title = "Total Deaths"
                    )
            })
        }
        if (input$radio == "New Cases") {
            output$map <- renderLeaflet({
                coordCode <- countrycode(input$country, origin = 'country.name', destination = 'iso3c')
                lat <- subset(world_sf_merged@data$LAT, world_sf_merged@data$ISO3 == coordCode)
                lng <- subset(world_sf_merged@data$LON, world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat, lng = lng, zoom = 4) %>%
                    addPolygons(
                        data = world_sf_merged ,
                        fillColor = ~ palNC(world_sf_merged$new_cases),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palNC,
                        values = world_sf_merged$new_cases,
                        position = "bottomright",
                        title = "New Cases"
                    )
            })
        }
        if (input$radio == "Cases Per Million") {
            output$map <- renderLeaflet({
                coordCode <- countrycode(input$country, origin = 'country.name', destination = 'iso3c')
                lat <- subset(world_sf_merged@data$LAT, world_sf_merged@data$ISO3 == coordCode)
                lng <- subset(world_sf_merged@data$LON, world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat, lng = lng, zoom = 4) %>%
                    addPolygons(
                        data = world_sf_merged ,
                        fillColor = ~ palCD(world_sf_merged$total_cases_per_million),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palCD,
                        values = world_sf_merged$total_cases_per_million,
                        position = "bottomright",
                        title = "Cases per Million"
                    )
            })
        }
        if (input$radio == "Deaths Per Million") {
            output$map <- renderLeaflet({
                coordCode <- countrycode(input$country, origin = 'country.name', destination = 'iso3c')
                lat <- subset(world_sf_merged@data$LAT, world_sf_merged@data$ISO3 == coordCode)
                lng <- subset(world_sf_merged@data$LON, world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat, lng = lng, zoom = 4) %>%
                    addPolygons(
                        data = world_sf_merged ,
                        fillColor = ~ palDD(world_sf_merged$total_deaths_per_million),
                        fillOpacity = 0.7,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        popup = ~ popup_sb
                    ) %>%
                    addLegend(
                        pal = palDD,
                        values = world_sf_merged$total_deaths_per_million,
                        position = "bottomright",
                        title = "Deaths per Million"
                    )
            })
        }
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
