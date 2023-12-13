 # setup -------------------------------------------------------------------

library(lubridate)
library(DT)
library(viridis)
library(plotly)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)

# data --------------------------------------------------------------------

# Read in Major Crime Indicator data:

# setwd('~/Documents/DSPP 2022-2024/gis/')

tt <-
  st_read('data/tt.geojson') %>% 
  rename(HOOD_140 = AREA_S_CD) %>% 
  mutate(HOOD_140 = 
           as.character(HOOD_140))


mci <-
  read_csv('data/mci_2018.csv') %>%
  filter(OCC_YEAR == 2018) %>% 
  select(-HOOD_140) %>% 
  st_as_sf(
    coords = c('LONG_WGS84', 'LAT_WGS84'),
    crs = 4326) %>%
  st_transform(st_crs(tt))

# user interface ----------------------------------------------------------

ui <- 
  dashboardPage(
    
    dashboardHeader(title = 'Toronto Crime App'),
    
    skin = "purple",
    
    dashboardSidebar(
      sidebarMenu(
        
        menuItem('Dashboard',
                 tabName = 'dashboard',
                 icon = icon('dashboard')),
        
        menuItem('Data',
                 icon = icon('database'),
                 tabName = 'datas'),
        
        menuItem('Map',
                 icon = icon('map'),
                 tabName = 'maps'),
        
        menuItem('Table',
                 icon = icon('table'),
                 tabName = 'tables'),
        
        menuItem('Trend',
                 icon = icon('chart-line'),
                 tabName = 'charts'),
        
        menuItem("Heatmap View", 
                 icon = icon("fire"),
                 tabName = "heatmap_view"),
        
        menuItem("Coda", 
                 icon = icon("book"),
                 tabName = "coda_view"))
    ),
    
    dashboardBody(
      tags$head(
        tags$link(
          rel = 'stylesheet',
          type = 'text/css',
          href = 'dashboard_styles.css'
        )
      ),
      
      tabItems(
        
        tabItem(
          tabName = 'dashboard',
          h2('Overview'),
          br(),
          
          h3('Decoding Crime Patterns: Analysis of Toronto\'s Crime Dynamics'),
          p('This app works for providing some insights about the', strong('Crimes in Toronto')),
          p('This interactive dashboard offers an examination of crime patterns in Toronto throughout 2018. 
            Created with the help of Shiny, this tool provides users with insights into', strong('the distribution and 
            trends of significant crimes within the city.'), 'Our aim is to create a user accessible platform that 
            enables citizens, policymakers, and researchers to comprehend and investigate crime dynamics in Toronto 
            ultimately fostering a safer and more knowledgeable community.'),
          img(src = "toronto-feat.jpeg", height = "500px", width = "1000px", align = 'center'),
          br(),
          h4('What You Could Take Away?'),
          tags$ul(
            tags$li(tags$b('Which city/neighborhoods are safer or more dangerous?')),
            tags$li(tags$b('Which month and which day has the highest number of cases?')),
            tags$li(tags$b('Is there any potential trend for different MCI?')),
            tags$li(tags$b('Which premises type is more possibility to meet offenses?')),
            tags$li(tags$b('Where to live could stay safe?')),
            tags$li(tags$b('Which area should be allocated more police resouces to prevent the occurance of incidents?'))
          ),
          br(),
          
          h3('Data Source'),
          p('The data displayed on this dashboard is based on the', a(strong("Major Crime Indicators (MCI)"), 
          href = "https://data.torontopolice.on.ca/datasets?t=MCI", target = "_blank"), 'dataset, 
            which was obtained from the', a(strong('Toronto Police Service Public Safety Data Portal.'),
            href = "https://data.torontopolice.on.ca/pages/open-data", target = "_blank"), 'This dataset 
            includes information about a range of major crimes that occurred in Toronto and were meticulously 
            documented and classified by the Toronto Police Service. It provides a view of the citys crime landscape 
            covering categories of criminal activities. For this dashboard tour, we will focus on the offenses across 
            2018, which reports 36,406 cases in 2018.'),
          
          br(),
          h3('Features of the Dashboard'),
          p('This dashboard includes several insight features to display the view of crime in Toronto:'),
          p(strong('Interactive Map:'), 'Visualize the geographic distribution of major crimes across Toronro. User 
            can input different types of crime indicators to see the distribution accross the neighborhood and see which
            neighborhood/street is more safer/dangerous.'),
          p(strong('Summary Table:'), 'Based on the neighborhood level, this table provide a detailed summary of crime 
            occurances. Use can use this table to check the name of neighborhood and the occurances of different crimes.'),
          p(strong('Trend Line Chart:'), 'Display monthly trends with dynamic chart. The x-axis represents days of the month,
            and the y-axis represents the number of cases. Earch Major Crime Indicator(MCI) is coded as different colored
            lines.'),
          
          br(),
          p('Feel free to explore the tour of Analysis of Crime in Toronto!')
        ),
        
        tabItem(
          tabName = 'datas',
          h2('Important Features'), 
          fluidRow(
              DTOutput("data_view")
          ),
          br(),
          h3('Quick View'),
          p('The shape of data is 36,406 rows and 30 variables. I choose features we are going to use:'),
          tags$ul(
            tags$li(tags$b('OCC_MONTH'), ' - The month that occur certain crimes'),
            tags$li(tags$b('OCC_DAY'), ' - The day of month that occur certain crimes'),
            tags$li(tags$b('MCI_CATEGORY'), ' - The assigned Major Crime Indicator category'),
            tags$li(tags$b('PREMISES_TYPE'), ' - The types of premises that occur certain crimes'), 
            tags$li(tags$b('HOOD_140'), ' - For administrative purposes, the City of Toronto divides the city into 140 neighbourhoods. These divisions are used for internal planning purposes. Each neighborhood has unique number' ),
            tags$li(tags$b('NEIGHBORHOOD_140'), ' - The full name of neighborhoods corresponding to the number')
          )
        ),
        
        tabItem(
          tabName = 'maps',
          h2('Distribution of MCI across Neighborhoods'),
          
          fluidRow(
            box(
              title = "Inputs", 
              width = 7,
              status = "warning", 
              solidHeader = TRUE,
              
              selectInput("mciCategory",
                          "Select MCI Category:",
                          choices = unique(mci$MCI_CATEGORY))),
            box(
              width = 5,
              status = "warning",
              p(strong('Church-Yonge Corridor stands out with high rates across several categories, including assault, robbery, and break and enter.')),
              p(strong('This could indicate a higher risk for residents and visitors, potentially due to its role as a bustling commercial and residential area that draws large crowds.'))
            )
            ),
          
          fluidRow(
            box(
              title = "Distribution Map", 
              width = 7,
              status = "info", 
              solidHeader = TRUE,
              tmapOutput(outputId = 'crime_map')),
            
            box(
              title = "Top 5 with Highest Crime Rate", 
              width = 5,
              status = "info", 
              solidHeader = TRUE,
              dataTableOutput(outputId = "top_crime_table")),
            fluidRow(
              box(
                width = 12,
                h3('The Situations Accross Neighbohoods'),
                p('As the neighborhoods of Toronto pulse, with life an intriguing connection between crime and community is revealed in this visualization. This visualization weaves a tapestry of incidents across the urban landscape.'),
                br(),
                p('The visualization thickens as we delve into downtown, where the', strong('Church Yonge Corridor'), 'takes stage amidst its activity. The prevalence of', strong('assaults, robberies and break ins'), 'in this area speaks to the vulnerability of its streets and alleys. Amidst the nightlife and thriving commerce these statistics unveil hidden struggles that call for vigilance.'),
                p('Expanding our view across the city map, we discover that', strong('Waterfront Communities-The Island'), 'also plays an important role. This district, popular, among both locals and tourists alike mirrors the challenges faced by the Corridor with its occurrences of', strong('assaults and break ins'), '. It seems that the very vibrancy that attracts people also entices elements.'),
                p('Shifting our focus to the outskirts a new chapter unfolds.The neighborhood of', strong('West Humber Clairville'), 'which may seem quieter at glance actually reveals a twist with its high rates of', strong('auto theft'), '. This suggests the presence of a kind of element that takes advantage of the area industrial spaces and less crowded nature to carry out grand thefts.'),
                p('As we observe the interplay between the neighborhoods and the outlying districts we come across contrasting scenes.', strong('Islington City Centre West'), 'and', strong('Kensington Chinatown'), 'with their combination of commercial presence face their own challenges with', strong('auto thefts and break ins'), '. This implies a narrative of crime that leverages both opportunity and the anonymity provided by settings.'),
                br(),
                p('The data tells us more than numbers; it paints a picture of a living city grappling with issues and challenges. It urges policymakers, law enforcement agencies and communities to delve into the underlying factors contributing to these patterns—whether its lighting quality, policing strategies, community cohesion or urban design.')
              )
            )
            )
          ),
        
        tabItem(
          tabName = 'tables',
          h3('Summary Table'),
          fluidRow(
            box(
              width = 12,
              p(strong('This table is used for searching for the specific neighborhood name becasue we can only see the unique identification number on the map.')),
              p(strong('For example: ')),
              p('If you want to know what the number 32 neighborhood is, you can search', strong('032'), 'and you will see the area name for that neighborhood.')
            )
           ),
          fluidRow(
            dataTableOutput(outputId = "summaryTable"))
          ),
        
        tabItem(
          tabName = 'charts',
          h2('Monthly Offense Trend'),
          fluidRow(
            box(
              title = "Inputs", 
              width = 8,
              status = "warning", 
              solidHeader = TRUE,
              selectInput("month",
                          "Select Month:",
                          choices = month.name),
              br(),
              downloadButton(
                outputId = "download_trend",
                label = "Download trend plot")),
            
            valueBoxOutput("highest_offense_day"),
            valueBoxOutput("total_offenses")
            ),
          fluidRow(
            box(
            title = "Monthly Trend Chart", 
            background = "light-blue", 
            width = 12,
            solidHeader = TRUE,
            plotOutput(outputId = "monthly_trend")
            )
          ),
          fluidRow(
            box(
              width = 12,
              h3('Seasonal Concerns and Findings'),
              p('We already know what the safer/more dangerous neighborhoods are. Are there any patterns that ebbs and flows with the seasons and crimes?'),
              p('The waves often crest as the city comes alive with people and activity—highlighting a correlation with the city pulse, but there are no impactful findings. At least this visualization can be used as a wake-up call afterwards'),
              br(),
              p('As the year progresses, each offense marks the passage of time, a calendar not of dates, but of incidents that map out the challenges faced by a city in motion. Yet, this is not just a story of crime; it is also one of resilience. The data holds within it the keys to understanding, prevention, and intervention. Patterns emerge that can inform smarter policing, more targeted social services, and community actions that can strengthen the very fabric of Toronto neighborhoods.'),
              p('The trends observed over the months provide a roadmap for the future. As the city prepares to turn the page to a new year, it carries with it the knowledge gleaned from these patterns—a powerful tool in the quest to write a safer, more secure story for all its residents.'),
              p('This comprehensive view, then, is not just a reflection of what has been, but a prologue to what could be—a city that learns, adapts, and evolves, using the lessons of the past to forge a better tomorrow.')
            )
          )
        ),
        
        tabItem(
          tabName = "heatmap_view",
          h2('Heatmap of MCI by Premises Types'),
          fluidRow(
            box(
              width = 12,
              h3('Distribution of Crimes Accross Premises Type'),
              p('The comprehensive narrative woven from January to December reveals not just the prevalence of crime, but the settings where individuals are most vulnerable or where opportunities for crime are most abundant. The darker the hues across the heatmaps represent not only the higher incidence of crime but also the concentration of distress.'),
              p('The heatmaps also underscore the need for a holistic approach to crime prevention, one that includes environmental design, community-based strategies, and social services. It reveals a city that is alive, not just with the day-to-day bustle of its citizens but with the challenges they face in the form of crime.'),
              p('As the year closes, the comprehensive story told by these heatmaps becomes a catalyst for reflection and planning. It offers a chance to analyze the effectiveness of measures taken and to strategize anew for the coming year. Each cell of the heatmap, each color gradient, is a piece of evidence in the ongoing effort to create a safer, more secure Toronto.')
            )
          ),
          fluidRow(
            box(
              title = "Inputs", 
              width = 7,
              status = "warning", 
              solidHeader = TRUE,
              
              selectInput("hmonth",
                          "Select Month:",
                          choices = month.name))),
          fluidRow(
            box(
              title = "Monthly Trend Chart", 
              background = "light-blue", 
              width = 12,
              solidHeader = TRUE,
              plotlyOutput("heatmap_plot"))
          ),
          
          fluidRow(
            box(
              width = 12,
              h3('Assults'),
              p('Throughout the year, assaults are consistently prominent, especially in apartment complexes and outside locations, suggesting a pattern tied to residential density and social interactions. The winter months do not seem to deter this type of crime, indicating a complexity that cold weather alone cannot avoid.'),
              br(),
              h3('Break and Enters, Theft over, and Robbery'),
              p('Commercial areas are recurrent hotspots for break and enters, thefts over, and robberies, aligning with the notion that places of commerce are targeted for their assets. Educational premises, while touched by crime, remain relatively lighter in color, indicating lower crime rates which may speak to the structured environment and active presence of security measures.'),
              br(),
              h3('Auto Theft'),
              p('The outer areas and transit points of the city stand out for auto theft, which peaks as the year wanes, hinting at a seasonal trend or perhaps a shift in criminal focus as other crimes decrease.'),
              br(),
              p('This year-long story of crime across the urban sprawl of Toronto offers a multi-dimensional understanding of the issue. The heatmaps serve as a guide, a tool for those tasked with safeguarding heart and homes of the city. For policymakers, it is a call to reinforce the vulnerable points, for community leaders, a sign to foster stronger neighborhood watches, and for law enforcement, a map to deploy resources more effectively.')
            ))
        ),
        
        tabItem(
          tabName = "coda_view",
          fluidRow(
            h2("Coda"),
            h3("Decoding Crime Patterns: Analysis of Toronto's Crime Dynamics"),
            box(
              width = 7,
              p('When we examine the crime data for Toronto in 2018, we can gain a nuanced understanding. It involves analyzing the relationship between crime rates, their geographical distribution changes over time, and the specific types of locations that are affected. By studying maps, monthly trends, and heatmaps we can uncover insights about the city\'s crime landscape.')),
            box(
              width = 7,
              h4('Distribution of MCI across Neighborhoods'),
              p('The interactive map reveals variations in assault rates across neighborhoods. This highlights the importance of implementing targeted strategies to prevent crimes in these areas. It emphasizes the significance of interventions tailored to locations.'),
              h4('Monthly Offense Trend'),
              p('By studying trends, we can observe fluctuations in crime rates throughout the year. Certain types of crimes have peak periods which could guide us in allocating law enforcement resources and determining when community awareness programs would be most impactful.'),
              h4('Heatmap of MCI by Premises Type'),
              p('Heatmaps offer a representation of how different types of crimes are distributed across various locations. We observe that assaults tend to concentrate around apartment complexes while robberies occur frequently in transit areas. This information urges us to reconsider security measures and explore how environmental design can help mitigate these risks.')),
            img(src = "toronto_safe.jpeg", height = "500px", width = "500px"),
            br(),
            box(
              width = 12,
              p('In summary, this analysis provides a foundation for research and policy development based on data-driven insights. It indicates that effective crime prevention requires an approach that takes into account factors such as environment, timing, and socio-economic conditions. It will serve as a resource for policymakers, law enforcement agencies, and individuals who prioritize safety. It provides a framework for understanding the intricacies of crime and devising targeted strategies to address the needs of Toronto\'s diverse communities.')
            )
          )
        )
        )
      )
    )


# server -------------------------------------------------------------------

server <-
  function(input, output) { 

    # Load mci data --------------------------------------------------------
    
    mci_data <-
      tt %>%
      st_join(mci) %>%
      as_tibble()
    
    crimes_t <-
      mci_data %>%
      summarize(
        count = n(), 
        .by = c(HOOD_140, MCI_CATEGORY)) %>%
      pivot_wider(
        names_from = MCI_CATEGORY, 
        values_from = count, 
        values_fill = 0) %>%
      left_join(
        tt, 
        .,
        by = "HOOD_140") %>%
      st_as_sf()
    
    # Make reactive portion for output ------------------------------------
    
    filtered_data <- reactive({
      crimes_t %>%
        select(HOOD_140, input$mciCategory) %>%
        rename(count = input$mciCategory)
    })
    
    trend_plot <- reactive({
      ggplot(monthly_plot_data(), 
             aes(x = OCC_DAY, 
                 y = count, 
                 color = MCI_CATEGORY)) +
        geom_line() +
        labs(title = 
               paste("Daily Count of Offenses in", 
                     input$month),
             x = "Day of the Month",
             y = "Count of Offenses") +
        theme_minimal()
    })
    
    top_neighborhoods_data <- reactive({
      crimes_t %>%
        st_set_geometry(., NULL) %>% 
        select(AREA_NAME, HOOD_140, input$mciCategory) %>%
        arrange(desc(.[[input$mciCategory]])) %>%
        head(5)
    })
    
    # summary table
    
    summary_data <- reactive({
      crimes_t %>%
        st_set_geometry(., NULL)
    })
    
    monthly_plot_data <- reactive({
      mci %>%
        filter(OCC_MONTH == input$month) %>%
        group_by(OCC_DAY, MCI_CATEGORY) %>%
        summarize(count = n(), .groups = 'drop') %>%
        arrange(OCC_DAY)
    })
    
    highest_offense <- reactive({
      mci %>%
        filter(OCC_MONTH == input$month,
               !OCC_DAY == 1) %>%
        group_by(OCC_DAY) %>%
        summarize(count = n(), .groups = 'drop') %>%
        arrange(desc(count)) %>%
        slice(1) %>%
        pull(OCC_DAY)
    })
    
    total_offense <- reactive({
      mci %>%
        filter(OCC_MONTH == input$month) %>%
        nrow()
    })
    
    heatmap_data <- reactive({
      mci_data %>%
        filter(OCC_MONTH == input$hmonth) %>% 
        summarize(
          count = n(), 
          .by = c(PREMISES_TYPE, MCI_CATEGORY)) %>%
        pivot_wider(
          names_from = MCI_CATEGORY, 
          values_from = count, 
          values_fill = 0) %>% 
        pivot_longer(
          cols = -PREMISES_TYPE, 
          names_to = "Crime", 
          values_to = "Count")
    })
    
     # Output result --------------------------------------------------------
    
    output$data_view <- renderDT({
      datatable(mci_data %>% 
                  select(OCC_MONTH, 
                         OCC_DAY, 
                         MCI_CATEGORY,
                         PREMISES_TYPE,
                         HOOD_140, 
                         AREA_NAME,
                         HOOD_158
                         ), 
                options = list(
                  columnDefs = list(list(visible=FALSE, targets=-1)),
                  dom = 'Bfrtip',
                  buttons = list('colvis') 
                )
      )
    })
    
    output$crime_map <- renderTmap({
      tm_basemap(
        c('OpenStreetMap',
          'Esri.WorldImagery')) +
      tm_shape(filtered_data(),
               name = 'Neighborhoods') +
        tm_polygons(
          col = "count", 
          palette = 'Blues',
          breaks = 
            c(0, 
              20, 
              50, 
              90, 
              130, 
              300, 
              1000),
          title = paste(input$mciCategory, "Count"),
          alpha = 0.8) 
    })
    
    output$top_crime_table <- renderDataTable({
      top_neighborhoods_data()
      })
    
    output$summaryTable <- renderDataTable(
      summary_data())
    
    output$monthly_trend <- renderPlot({
      trend_plot()
    })
    
    output$download_trend <- downloadHandler(
      filename = function() {
        stringr::str_glue("monthly_trend_{input$month}.png")
      },
      
      content = function(file) {
        ggsave(
          file,
          trend_plot(),
          width = 8,
          height = 5,
          dpi = 300)
      }
    )
    
    output$highest_offense_day <- renderValueBox({
      valueBox(
        value = highest_offense(),
        subtitle = "Date with Highest Offenses",
        icon = icon("calendar-day"),
        color = "aqua"
      )
    })
    
    output$total_offenses <- renderValueBox({
      valueBox(
        value = total_offense(),
        subtitle = "Total Offenses in the Month",
        icon = icon("chart-bar"),
        color = 
          if_else(
            total_offense() > 3000, 
            "yellow", 
            "aqua")
      )
    })
    
    output$heatmap_plot <- renderPlotly({
      gg <- 
        ggplot(heatmap_data(), 
                   aes(x = PREMISES_TYPE, y = Crime, fill = Count)) +
        geom_tile() +
        scale_fill_viridis(name = "Total Count", option = "C") +
        geom_text(aes(label = Count), color='white') +
        theme_minimal() +
        ggtitle("Major Crime Indicator by Premises Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Heatmap of Crimes by Premises Type", 
             x = "Premises Type", 
             y = "Crime Type")
      
      ggplotly(gg, tooltip = c("x", "y", "fill"))
    })
    
    
  }


# Run the application 
shinyApp(ui = ui, server = server)
