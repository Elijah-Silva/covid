#================
# LOAD PACKAGES #
#================
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
library(shinycssloaders)
library(data.table)
library(plotly)
library(shinyalert)

#========================
# IMPORT DATA FROM ECDC #
#========================
data <-
    read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
             header = TRUE)

#================
# DATA CLEANING #
#================
data$location <-
    gsub("Cote d'Ivoire", "Ivory Coast", data$location)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
todaydate = max(as.Date((data$date)))
startdate = "2020-01-01"
options(scipen = 999)
dataToday <- subset(data, date == todaydate) #Subset for only today's date

#===================
# IMPORT SHAPEFILE #
#===================
world_spdf <- readOGR(
    #import spatial files
    dsn = paste0(getwd(), "/DATA/world_shape_file/") ,
    layer = "TM_WORLD_BORDERS_SIMPL-0.3",
    verbose = FALSE
)

world_sf_merged <-
    geo_join(world_spdf, dataToday, "ISO3", "iso_code") #Merge shapefile and dataset


#========================
# CREATE LEAFLET POPUPS #
#========================
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


#==============
# GGPLOT TEXT #
#==============
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

#=======================
# COLOR PALETTE GGPLOT #
#=======================
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

############
# START UI #
############
ui <- shinyUI(navbarPage(theme = shinytheme("flatly"),
                         "COVID-19",
                         tabPanel(
                             "Data Explorer",
                             div(class = "outer",
                                 fluidPage(fluidRow(
                                     column(12,
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
                                                           style = "color:darkblue",
                                                           align = "center"),
                                                    )),
                                                    fluidRow(column(
                                                        12,
                                                        div(style = 'height: 220px; overflow-y: scroll',
                                                            dataTableOutput("totalCasesTable"))
                                                    )),
                                                    fluidRow(
                                                        column(
                                                            12,
                                                            hr(),
                                                            useShinyalert(),
                                                            selectInput(
                                                                "country",
                                                                "Country:",
                                                                choices =
                                                                    dataToday$location,
                                                                selected = "World",
                                                            ),
                                                            radioButtons(
                                                                "radio",
                                                                label = "Statistic:",
                                                                inline = TRUE,
                                                                selected = "Cases",
                                                                choices = c(
                                                                    "Cases",
                                                                    "Deaths",
                                                                    "New Cases",
                                                                    "Cases per Million",
                                                                    "Deaths per Million"
                                                                ),
                                                            ),
                                                            actionButton("help", "Quick Tip!", style =
                                                                             'padding:4px; font-size:95%'),
                                                            hr(),
                                                        )
                                                    ),
                                                    fluidRow(column(
                                                        12,
                                                        h6("Last Updated:", todaydate, ' at 6AM EST'),
                                                        h6(
                                                            "Source:",
                                                            tags$a(href = "https://covid.ourworldindata.org/data/owid-covid-data.csv", "ECDC")
                                                        ),
                                                        tags$h6(
                                                            "Created by Elijah Silva",
                                                            br(),
                                                            a(href = "https://www.linkedin.com/in/elijahsilva/", "LinkedIn"),
                                                            " | ",
                                                            a(href = "https://github.com/Elijah-Silva/covid", "Github"),
                                                            align = "center"
                                                        )
                                                    ))
                                                ),
                                                column(6,
                                                       withSpinner(leafletOutput("map",
                                                                                 height = 711), type =
                                                                       6)),
                                                column(4,
                                                       "",
                                                       fluidRow(
                                                           column(
                                                               6,
                                                               h5("Total Deaths:", align = "center"),
                                                               h1(textOutput("worldDeath"),
                                                                  style = "color:darkred",
                                                                  align = "center"),
                                                               div(style = 'height: 130px; overflow-y: scroll',
                                                                   dataTableOutput("totalDeathsTable"))
                                                           ),
                                                           column(
                                                               6,
                                                               h5("New Cases:", align = "center"),
                                                               h1(textOutput("newCases"),
                                                                  style = "color:darkorange",
                                                                  align = "center"),
                                                               div(style = 'height: 130px; overflow-y: scroll;',
                                                                   dataTableOutput("totalNewCasesTable"))
                                                           )
                                                       ),
                                                       hr(),
                                                       fluidRow(
                                                           column(
                                                               12,
                                                               withSpinner(plotlyOutput("plot1"), type =
                                                                               6),
                                                               dateRangeInput(
                                                                   "dateRange",
                                                                   label = "Filter Date Range:",
                                                                   min = min(data$date),
                                                                   max = max(data$date),
                                                                   start = startdate,
                                                                   end = max(data$date)
                                                               )
                                                           )
                                                       ))
                                                
                                            ))
                                 )))
                         )))


################
# START SERVER #
################
server <- function(input, output, session) {
    location <- data$location
    dates <- format(data$date, format = "%Y-%m-%d")
    total_cases <- data$total_cases
    total_deaths <- data$total_deaths
    new_cases <- data$new_cases
    case_dens <- data$total_cases_per_million
    death_dens <- data$total_deaths_per_million
    
    df <-
        data.frame(dates,
                   location,
                   total_cases,
                   total_deaths,
                   new_cases,
                   case_dens,
                   death_dens)
    
    #================
    # ACTION BUTTON #
    #================
    observeEvent(input$help, {
        shinyalert(
            title = "Quick Tip!",
            text = "Click on any country to get a variety of summary statistics (cases, deaths, new cases, new deaths, case and death density per million). You also have the ability to choose 'World' under 'Country:' to get worldwide statistics",
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
    })
    
    #=============
    # DATATABLES #
    #=============
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
    
    #======================================
    # SUMMARY STATISTICS ABOVE DATATABLES #
    #======================================
    
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
    
    
    #===========
    # PLOTTING #
    #===========
    
    df_subset <- reactive({
        subset(
            df,
            location == input$country &
                as.Date(dates) >= input$dateRange[1] &
                as.Date(dates) <= input$dateRange[2]
        )
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
                    ggNew <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = new_cases)) +
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
                    ggCD <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = case_dens)) +
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
                    ggDD <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = death_dens)) +
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
    
    #=============
    # WORLD PLOT #
    #=============
    
    observe({
        if (input$radio == "Cases" & input$country == "World") {
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
        if (input$radio == "Deaths" & input$country == "World") {
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
        if (input$radio == "New Cases" & input$country == "World") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggNew <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = new_cases)) +
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
        if (input$radio == "Cases per Million" &
            input$country == "World") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggCD <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = case_dens)) +
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
        if (input$radio == "Deaths per Million" &
            input$country == "World") {
            output$plot1 <- renderPlotly({
                ggplotly({
                    ggDD <-
                        ggplot(data = df_subset(), aes(x = as.Date(dates), y = death_dens)) +
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
    
    #==========
    # LEAFLET #
    #==========
    
    observe({
        if (input$country != "World" & input$radio == "Cases") {
            output$map <- renderLeaflet({
                coordCode <-
                    countrycode(input$country,
                                origin = 'country.name',
                                destination = 'iso3c')
                lat <-
                    subset(world_sf_merged@data$LAT,
                           world_sf_merged@data$ISO3 == coordCode)
                lng <-
                    subset(world_sf_merged@data$LON,
                           world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat,
                            lng = lng,
                            zoom = 4) %>%
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
        if (input$country != "World" & input$radio == "Deaths") {
            output$map <- renderLeaflet({
                coordCode <-
                    countrycode(input$country,
                                origin = 'country.name',
                                destination = 'iso3c')
                lat <-
                    subset(world_sf_merged@data$LAT,
                           world_sf_merged@data$ISO3 == coordCode)
                lng <-
                    subset(world_sf_merged@data$LON,
                           world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat,
                            lng = lng,
                            zoom = 4) %>%
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
        if (input$country != "World" & input$radio == "New Cases") {
            output$map <- renderLeaflet({
                coordCode <-
                    countrycode(input$country,
                                origin = 'country.name',
                                destination = 'iso3c')
                lat <-
                    subset(world_sf_merged@data$LAT,
                           world_sf_merged@data$ISO3 == coordCode)
                lng <-
                    subset(world_sf_merged@data$LON,
                           world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat,
                            lng = lng,
                            zoom = 4) %>%
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
        if (input$country != "World" &
            input$radio == "Cases per Million") {
            output$map <- renderLeaflet({
                coordCode <-
                    countrycode(input$country,
                                origin = 'country.name',
                                destination = 'iso3c')
                lat <-
                    subset(world_sf_merged@data$LAT,
                           world_sf_merged@data$ISO3 == coordCode)
                lng <-
                    subset(world_sf_merged@data$LON,
                           world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat,
                            lng = lng,
                            zoom = 4) %>%
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
        if (input$country != "World" &
            input$radio == "Deaths per Million") {
            output$map <- renderLeaflet({
                coordCode <-
                    countrycode(input$country,
                                origin = 'country.name',
                                destination = 'iso3c')
                lat <-
                    subset(world_sf_merged@data$LAT,
                           world_sf_merged@data$ISO3 == coordCode)
                lng <-
                    subset(world_sf_merged@data$LON,
                           world_sf_merged@data$ISO3 == coordCode)
                leaflet() %>%
                    clearControls() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
                    setView(lat = lat,
                            lng = lng,
                            zoom = 4) %>%
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
    
    #================
    # WORLD LEAFLET #
    #================
    # Cases
    observe({
        if (input$country == "World" & input$radio == "Cases") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
    })
    
    # Deaths
    observe({
        if (input$country == "World" & input$radio == "Deaths") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
    })
    
    # New Cases
    observe({
        if (input$country == "World" & input$radio == "New Cases") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
    })
    
    # Cases per Million
    observe({
        if (input$country == "World" & input$radio == "Cases per Million") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
    })
    
    # Deaths per Million
    observe({
        if (input$country == "World" &
            input$radio == "Deaths per Million") {
            output$map <- renderLeaflet({
                leaflet() %>%
                    addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
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
