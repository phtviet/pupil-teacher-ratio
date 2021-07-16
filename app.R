library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(RColorBrewer)

# Read in .csv file
ratioTrend <- read_csv("ratio_trend.csv")

# Transform data into long form
ratioPerformance <- read_csv("ratio_performance.csv") %>%
    filter(!(Country %in% c("United States","Cyprus","Dominican Republic","Malta","Mauritius","Panama", "Switzerland"))) %>%
    select(-`Pupil-Teacher Ratio Secondary`:-`PISA Science Mean Performance`) %>%
    gather(key = "Metric", value = "Value", -Country, -Year, -Region)

# Read in shapefiles for choropleth map. Downloaded from naturalearth.com.
shape <- read_sf("shapefiles")

# Change to fit our data
shape$SOVEREIGNT[which(shape$SOVEREIGNT=="Macedonia")] <- "North Macedonia"
shape$SOVEREIGNT[which(shape$SOVEREIGNT=="Republic of Serbia")] <- "Serbia"

ui <- navbarPage(
    # Add a logo
    title = HTML(
        "
    <i class='fas fa-globe-americas' id='logo'></i>
    Pupil-teacher ratio and student performance
    "
    ),
    # Add theme
    # Design influenced by https://shiny.rstudio.com/gallery/masters.html
    theme = "theme.css",
    windowTitle = "Pupil-teacher ratio and student performance",
    header = tagList(
        useShinydashboard(),
        tags$link(href = "style.css", rel="stylesheet"),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        )
    ),
    # First tab
    tabPanel(
        title = "Trend of Ratio",
        fluidRow(
            # First text box in first tab
            box(
                width = 3, height = 680, solidHeader = TRUE, title = "What is the trend of Pupil-Teacher ratio over the years?",
                HTML("Over the past few decades, many world governments, such as the U.S., China, Greece, and Spain to name just a few,
        have all spent considerable resources and efforts to lower the size of their classrooms. <br><br>
        This trend seems to be hinged on a widespread belief that the lower the class size, the better the students perform. <br><br>
        This dashboard wil attempt to validate the existence of this trend and its impacts on secondary students' performance across multiple countries. <br><br>
        From our overall line graph, it is clear that there is a trend to lower class size across multiple regions and countries. <br><br>
        Latin America and the Caribbean, and Southeastern Asia stand out as the only regions that have somewhat of a flat trend. <br><br>
        This could likely be the case because these countries already have a number of existing educational problems that demand resoures, such as retention rate and lack of education inputs.
        " 
                )),
            # Output trend line graph
            box(
                width = 7, height = 680, solidHeader = TRUE, title = "Trend line graph of Pupil-teacher ratio over the years",
                plotlyOutput("ratioTrendGraph", height = 600)
            ),
            fluidRow(
                # Add region filter
                box(
                    width = 2, solidHeader = TRUE, title = "Region Filter",
                    uiOutput("ratioTrendFilter")
                ),
                # Add tips box
                box(
                    width = 2, solidHeader = TRUE, title = "Tips!",
                    HTML("Click on legend to filter out a country. <br>
               Double-click on legend to isolate one country. Double-click again to reverse.")
                )
            )
        ),
        fluidRow(
            # Second text box in first tab
            box(
                width = 3, solidHeader = TRUE, title = "Interaction with chart and map",
                HTML("Our dataset focuses only on ratio for secondary students. <br><br>
        Here we have somewhat complete data for about 51 countries in 12 different regions. Due to discrepancies in data collection, there are some missing values in different years for most countries. <br><br>
        Some interesting filters would be for Latin America and the Caribbean, and Southeastern Asia countries, to explore their trends over the years. <br><br>
        The animation of the choropleth map over the years while might be interesting, suffers from the lack of consistent data over the years and can be confusing at times. <br><br>
        Nevertheless, it manages to capture the essence of our message about the overall trend of Pupil-Teacher Ratio, and it does demonstrate that the trend for each country in a region tend to be similar. <br><br>
        <font size=-1> Data source: World Bank </font>")
            ),
            # Output choropleth map
            box(
                width = 7, height = 663, solidHeader = TRUE, title = "Choropleth map of pupil-teacher ratio",
                leafletOutput("ratioTrendMap", height = 600)
            ),
            # Add year filter with animation
            box(
                width = 2, solidHeader = TRUE, title = "Year Filter",
                div(
                    sliderInput(
                        inputId = "mapYearFilter",
                        label = "Year",
                        value = min(ratioTrend$Year),
                        min = min(ratioTrend$Year),
                        max = max(ratioTrend$Year),
                        step = 1,
                        animate = TRUE
                    ),
                    id = "mapYearFilterContainer"
                )
            )
        )
    ),
    # Second tab
    tabPanel(
        title = "Impact on Performance",
        fluidRow(
            # First text box in second tab
            box(
                width = 3, height = 680, solidHeader = TRUE, title = "Does lowering Pupil-Teacher Ratio have a significant impact on students' performance?",
                HTML("In the U.S. alone, the push to lower class size has led to a boom in the number of new teachers, 
             around a quarter million new teachers being hired between 1996 and 2004, resulting in an increase of 21 percent of per-pupil spending. <br><br>
             To explore whether focusing on lowering this ratio is justified, PISA Scores, 
             which is a standardised test measuring 15-year-olds' reading, mathematics, and science knowledge, will be used as our measure of student performance. <br><br>
             The first graph plots PISA Scores and Pupil-Teacher Ratio as 
             percent changes from the earliest year record available for each country to compare their magnitudes of change. 
             In this way, we could get a sense of how well these variables move together.<br><br>
             At a glance, we could see that the big dips and, in some cases, rises of Pupil-Teacher ratio do not translate to the same magnitude of change in PISA scores. 
             This suggests no significant relationship between the two for multiple countries.")
            ),
            # Output small multiple graph
            box(
                width = 7, height = 680, solidHeader = TRUE, title = "Pupil-Teacher Ratio and PISA Scores Change by Year as Percent of Earliest Year Record",
                plotlyOutput("ratioPerformanceGraph", height = 600)
            ),
            # Add a region filter
            box(
                width = 2, solidHeader = TRUE, title = "Region Filter",
                uiOutput("ratioPerformanceFilter")
            )
        ),
        fluidRow(
            # Second text box in second tab
            box(
                width = 3, solidHeader = TRUE, title = "Taking a closer look",
                HTML("Here we have data on 41 countries in 10 different regions, less than before because PISA tests have only been recently administered for a lot of countries. 
             Again, there are some missing values for most countries due to discrepancies. <br><br>
             Albania and Peru stand out as the only two countries that have somewhat similar magnitudes of change between the two variables, suggesting a negative correlation. 
             Even then, this is only a proportionate change, not significant. <br><br>
             The rest of the countries either show no change in PISA scores when Pupil-Teacher Ratio changes significantly, or they move along side each other, 
             which suggests that the belief about lowering classize leading to better outcomes is not true. <br><br>
             Some interesting filters would be for Albania and Peru as well as Latin America and the Caribbean, and Southeastern Asia countries, to explore their widely varied changes over the years. <br><br>
             <font size=-1> Data source: OECD and World Bank. </font>")
            ),
            # Output focus graph of small multiple graph
            column(
                width = 7,
                uiOutput("focusGraphBox")
            ),
            # Add a country filter
            box(
                width = 2, solidHeader = TRUE, title = "Country Filter",
                uiOutput("focusGraphCountrySelector")
            )
        )
    ),
    # Third tab
    tabPanel(
        title = "About",
        fluidRow(
            # Add text
            column(
                width = 8, offset = 2,
                box(
                    width = 12, solidHeader = FALSE,
                    HTML("Author: Viet Pham <br>
          Student number: 30925363 <br>
          Lab number: 19 <br>
          Tutors: Jeffery Liu, Niranjan Nanjunda <br><br>
          This is a final Narrative Visualisation Project for FIT5147. <br><br>
          It focuses on exploring the trend in Pupil-Teacher Ratio 
          for multiple different countries across the world and its impact on secondary students' performance, specifically PISA scores, 
          trying to get a clearer idea whether lowering the ratio leads to better outcomes as often suggested.<br><br>
          The intended audience are parents, education policy makers and any one interested in the education sector in general. <br><br>
          <p style=color:steelblue><font size=+2><b>Trend of Ratio</b></font></p>
          The <i>Trend of Ratio</i> will help illustrate the overall trend of Pupil-Teacher Ratio over the course of 48 years, 
          from 1970 to 2018 for a total of 51 countries in 12 different regions across the world. <br><br>
          In the trend line graph, the ratios are plotted for each year from 1980 to 2018. Each country is represented as one line. You can hover over the line to show info for each country, 
          as well as filter out countries by clicking on the legend. You can also isolate one single country by double clicking on its legend. <br><br>
          The graph can be grouped by region using the filter in the panel on the top-right of the page. Doing this will also update the choropleth map below. <br><br>
          In the choropleth map, the changes of Pupil-Teacher Ratio are shown further using animation by clicking on the play button. Geographical visualisation might help highlight trends or patterns
          that are hard to find with the trend line graph. <br><br>
          You can also zoom in, hover over each country to show info, 
          as well as pause the animation to explore each year in more details. Unfortunately, due to the lack of consistent data, the animation can be confusing at times. 
          However, it still manages to capture the essence of our message about the overall trend of Pupil-Teacher Ratio, and it does demonstrate the fact that the trend for each country in a region tend to be similar. <br><br>
          <p style=color:steelblue><font size=+2><b>Impact on Performance</b></font></p>
          The <i>Impact on Performance</i> page is all about visualising the impact of changes in Pupil-Teacher Ratio to student performance in different countries over time, using PISA scores 
          as a measure of performance. Howver, since PISA was recently introduced widely to countries outside of the OECD, and it is only held every 3 years, multiple countries suffer from a lack of data. <br><br>
          In the small multiple graph, PISA Scores and Pupil-Teacher Ratio are plotted as percent changes from the earliest year record available for each country to compare their magnitudes of change, usually it is from 2000, 
          which was when the PISA started, to 2015, latest available public data. <br><br>
          Some conclusions could be made about their relationships in this way. 
          Of course, this is in no way a rigorous statistical analysis, nor does it aim to be taking into account the intended audience, 
          but given the number of countries and the data available, I believe this could at least serve as a decent starting point. <br><br>
          You can hover over each individual line to view the info for each country in details. The graph can also be grouped by region using the filter in the panel on the top-right of the page. 
          Doing this will also update the graph below.<br><br>
          In the second graph on the page, this shows each small graph in the small multiple graph above in more details, 
          by allowing user to pick a country from the small multiple graph above using the filter on the right of the page. 
          The default is to show the first country in small multiple graph."
                    )
                )
            )
        )
    )
)

server <- function(input, output){
    # Create a reactive expression that responds to user input for our first dataset
    filterDataTab1 <- reactive({
        data <- if(input$selectedRegionTrend=="All"){
            ratioTrend
        } else {
            ratioTrend[which(ratioTrend$Region==input$selectedRegionTrend),]
        }
    })
    # Create a reactive expression that responds to user input for our second dataset
    filterDataTab2 <- reactive({
        data <- if(input$selectedRegionPerformance=="All"){
            ratioPerformance 
        } else {
            ratioPerformance[which(ratioPerformance$Region==input$selectedRegionPerformance),]
        }
    })
    # Render trend line graph with Plotly
    output$ratioTrendGraph <- renderPlotly({
        plot_ly(
            data = filterDataTab1(),
            x = ~Year,
            y = ~`Pupil-Teacher Ratio Secondary`,
            color = ~Country,
            type = "scatter",
            mode = "lines+markers",
            hoverinfo = ~paste0("Country: ", Country, "\n", "Year: ", Year, "\n", "Ratio: ", `Pupil-Teacher Ratio Secondary`)
        ) %>% 
            layout(
                showlegend = TRUE,
                yaxis = list(title = list(text = "Pupil-Teacher Ratio Secondary", standoff = 40L)),
                xaxis = list(title = list(text = "Year", standoff = 40L))
            )
    })
    # Render region filter for trend line graph
    output$ratioTrendFilter <- renderUI(
        selectInput(
            inputId = "selectedRegionTrend",
            label = "Select a region",
            choices = c("All", unique(ratioTrend$Region))
        )
    )
    # Set up choropleth map
    bins<- c(1, 5, 10, 15, 20, 25, 30, 35)
    colorCases <- colorBin(palette = "YlOrRd", domain = NULL, bins = bins, na.color = "transparent")
    # Render choropleth map with Leaflet
    output$ratioTrendMap <- renderLeaflet({
        data <- filterDataTab1() %>% filter(Year == input$mapYearFilter)
        data <- left_join(shape, data, by = c("SOVEREIGNT"="Country")) 
        
        leaflet(data, options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25)) %>%
            addPolygons(
                weight = 1,
                fillColor = ~colorCases(`Pupil-Teacher Ratio Secondary`),
                fillOpacity = 1,
                highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = T),
                label = ~lapply(paste0("Country: ", SOVEREIGNT, "</br>", "Year: ", Year, "</br>", "Ratio: ", `Pupil-Teacher Ratio Secondary`), HTML)
            ) %>%
            addLegend(
                pal=colorCases,
                values=~`Pupil-Teacher Ratio Secondary`,
                opacity=2,
                title = "Ratio",
                position = "topright"
            ) %>%
            setView(0,0, zoom=1.5)
    })
    # Render small multiple graph with Plotly
    output$ratioPerformanceGraph <- renderPlotly({
        ggplotly(
            ggplot(data = filterDataTab2()) +
                geom_line(mapping = aes(x=Year, y=Value, color=Metric)) +
                geom_point(mapping = aes(x=Year, y=Value, color=Metric)) +
                facet_wrap(~ Country) +
                scale_color_manual(
                    values = brewer.pal(8, "Dark2"),
                    guide = guide_legend(title = "Legend")
                ) +
                labs(y = "Percentage %",
                     x = "Year",
                     caption = "Base year (100%) is the earliest available year for that country. Data source: World Bank") +
                theme_minimal(base_size = 11) +
                theme(
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.title.y = element_text(hjust = 1, vjust = 0),
                    axis.title.x = element_text(hjust = 1, vjust = 0),
                    legend.position = "none"))
    })
    # Render region filter for small multiple graph
    output$ratioPerformanceFilter <- renderUI(
        selectInput(
            inputId = "selectedRegionPerformance",
            label = "Select a region",
            choices = c("All", unique(ratioPerformance$Region))
        )
    )
    # Render focus graph of small multiple graph
    output$focusGraphBox <- renderUI({
        data <- ratioPerformance %>%
            filter(Country==input$focusGraphCountry)
        box(
            width = 13, height = 663, solidHeader = TRUE, title = paste0(input$focusGraphCountry, " % Change"),
            ggplotly(
                p = ggplot(data = data) +
                    geom_line(mapping = aes(x=Year, y=Value, color=Metric)) +
                    geom_point(mapping = aes(x=Year, y=Value, color=Metric)) +
                    scale_color_manual(
                        values = brewer.pal(4, "Dark2"),
                        guide = guide_legend(title = "Legend")
                    ) +
                    labs(y = "Percentage %",
                         x = "Year") +
                    theme_minimal(base_size = 12),
                height = 600
            )
        )
    })
    # Rend country filter for focus graph
    output$focusGraphCountrySelector <- renderUI(
        selectInput(
            inputId = "focusGraphCountry",
            label = "Select a country",
            if(input$selectedRegionPerformance=='All'){
                choices = unique(ratioPerformance$Country)
            } else {
                choices = unique(ratioPerformance$Country[which(ratioPerformance$Region==input$selectedRegionPerformance)])
            })
    )
}

shinyApp(ui, server)

