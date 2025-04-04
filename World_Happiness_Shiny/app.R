library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(zoo)
library(CausalImpact)
library(forecast)
library(plotly)
library(corrplot)
library(sf)
library(tmap)
library(leaflet)
library(rnaturalearth)
library(rworldmap)
library(readr)
library(gridExtra)
library(plm)
library(tibble)
library(DT)
library(spdep)
library(shinyWidgets)
library(gt)
library(gtExtras)
library(ggridges)
library(ggfortify)
library(fpc)
library(dtw)
library(reshape2)
library(clValid)
library(cluster)
library(clusterSim)
library(TSclust)
library(networkD3)
library(factoextra)
library(RColorBrewer)
library(rworldmap)


# Load happiness data
happiness_df <- read.csv("data/world_happiness.csv") %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% 2014:2024)


happiness_df <- happiness_df %>% na.omit()


fe_model <- plm(ladder_score ~ economy_score + social_score + lifeexpectancy_score +
                  freedom_score + generosity_score + corrperception_score, 
                data = happiness_df, model = "within")

coef_df <- as.data.frame(coef(summary(fe_model))) %>%
  rownames_to_column(var = "Feature") %>%
  rename(Coefficient = Estimate)

# Prepare geospatial data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_happy <- world %>%
  left_join(happiness_df, by = c("name" = "country"))

# For clustering
data <- read_csv("data/world_happiness.csv")

# Country remapping for ne
country_corrections <- c(
  "Bolivia, Plurinational State of" = "Bolivia",
  "Congo" = "Republic of Congo",
  "Congo, Democratic Republic of the" = "Democratic Republic of the Congo",
  "Czechia" = "Czech Republic",
  "Côte d'Ivoire" = "Ivory Coast",
  "Eswatini" = "Swaziland",
  "Iran, Islamic Republic of" = "Iran",
  "Korea, Democratic People's Republic of" = "North Korea",
  "Lao People's Democratic Republic" = "Laos",
  "Moldova, Republic of" = "Moldova",
  "Netherlands, Kingdom of the" = "Netherlands",
  "North Macedonia" = "Macedonia",
  "Palestine, State of" = "Palestine",
  "Russian Federation" = "Russia",
  "Somaliland Region" = "Somalia",
  "Syrian Arab Republic" = "Syria",
  "Taiwan, Province of China" = "Taiwan",
  "Tanzania, United Republic of" = "Tanzania",
  "Türkiye" = "Turkey",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "United States of America" = "United States",
  "Venezuela, Bolivarian Republic of" = "Venezuela",
  "Viet Nam" = "Vietnam"
)

data$country_fixed <- ifelse(data$country %in% names(country_corrections),
                             country_corrections[data$country],
                             data$country)

ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Visualization"),
  dashboardSidebar(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")),  # Load custom CSS
    sidebarMenu(
      menuItem("Time Series", icon = icon("chart-area"), startExpanded = FALSE,
               menuSubItem("Trend", tabName = "ts_trend"),
               menuSubItem("Forecast", tabName = "ts_forecast"),
               menuSubItem("Causal Impact", tabName = "ts_causal")
      ),
      menuItem("Panel Model", icon = icon("sliders-h"), startExpanded = FALSE,
               menuSubItem("Feature Importance", tabName = "panel_feature_importance"),
               menuSubItem("Happiness Trend", tabName = "panel_happiness_trend"),
               menuSubItem("Panel Data Insights", tabName = "panel_data_insights")
      ),
      menuItem("Clustering", icon = icon("sitemap"), startExpanded = FALSE,
               menuSubItem("Multivariate", tabName = "clustering_multivariate"),
               menuSubItem("Time-Series", tabName = "clustering_timeseries")
      ),
      
      # Collapsible Geospatial Section
      menuItem("Geospatial", icon = icon("globe"), startExpanded = FALSE,
               menuSubItem("Choropleth & Proportional", tabName = "geo_choropleth"),
               menuSubItem("LISA & Moran's I", tabName = "geo_lisa"),
               menuSubItem("Aspatial", tabName = "geo_aspatial")
      ),
  
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")),
    tabItems(
      tabItem(tabName = "ts_trend",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("country", "Search and Select Countries:", 
                                   choices = unique(happiness_df$country), 
                                   multiple = TRUE, 
                                   options = list(maxItems = 5, placeholder = "Select countries")),
                    sliderInput("year_range", "Select Year Range:", 
                                min = 2014, max = 2024, value = c(2014, 2024),
                                step = 1, animate = TRUE, pre = "", sep = "")
                  ),
                  mainPanel(
                    h2("Happiness Trend"),
                    plotlyOutput("trend_plot"),
                    uiOutput("trend_explanation")
                  )
                )
              )
      ),
      
      tabItem(tabName = "ts_forecast",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("country", "Search and Select Countries:", 
                                   choices = unique(happiness_df$country), 
                                   multiple = TRUE, 
                                   options = list(maxItems = 5, placeholder = "Select countries")),
                    sliderInput("year_range", "Select Year Range:", 
                                min = 2014, max = 2024, value = c(2014, 2024),
                                step = 1, animate = TRUE, pre = "", sep = "")
                  ),
                  mainPanel(
                    h2("Happiness Forecast"),
                    plotlyOutput("forecast_plot"),
                    uiOutput("forecast_explanation")
                  )
                )
              )
      ),
      
      tabItem(tabName = "ts_causal",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("country", "Search and Select Countries:", 
                                   choices = unique(happiness_df$country), 
                                   multiple = TRUE, 
                                   options = list(maxItems = 5, placeholder = "Select countries")),
                    sliderInput("year_range", "Select Year Range:", 
                                min = 2014, max = 2024, value = c(2014, 2024),
                                step = 1, animate = TRUE, pre = "", sep = "")
                  ),
                  mainPanel(
                    h2("Causal Impact Analysis"),
                    plotOutput("causal_impact_plot"),
                    uiOutput("causal_impact_explanation")
                  )
                )
              )
      ),
    
    
  
  
      
      tabItem(tabName = "panel_feature_importance",
              fluidPage(
                titlePanel("Feature Importance"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country_select_multi", "Select Up to 2 Countries:", 
                                choices = unique(happiness_df$country), 
                                multiple = TRUE, 
                                selectize = TRUE)
                  ),
                  mainPanel(
                    plotlyOutput("feature_importance_plot_panel1"),
                    uiOutput("feature_importance_explanation")
                  )
                )
              )
      ),
      
      tabItem(tabName = "panel_happiness_trend",
              fluidPage(
                titlePanel("Happiness Trend"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country", "Select a Country:", choices = unique(happiness_df$country)),
                    checkboxGroupInput("factors", "Select Factors to Compare:", 
                                       choices = c("Ladder Score" = "ladder_score",
                                                   "Economy Score" = "economy_score",
                                                   "Social Score" = "social_score",
                                                   "Life Expectancy Score" = "lifeexpectancy_score",
                                                   "Freedom Score" = "freedom_score",
                                                   "Generosity Score" = "generosity_score",
                                                   "Corruption Perception Score" = "corrperception_score"),
                                       selected = "ladder_score")  # Default selection
                  ),
        
                  mainPanel(
                    plotlyOutput("happiness_trend_plot"),
                    uiOutput("happiness_trend_explanation")
                  )
                )
              )
      ),
      
      tabItem(tabName = "panel_data_insights",
              fluidPage(
                titlePanel("Panel Data Insights"),
                sidebarLayout(
                  sidebarPanel(
      
                    conditionalPanel(
                      condition = "input.subtabs == 'Panel Data Table'",
                      h4("Do you want to save the data?"),
                      downloadButton("downloadData", "Download Data as CSV")
        
                    ),
                    conditionalPanel(
                      condition = "input.subtabs == 'Top Improvement'",
                      br(), br(), br(),
                      box(
                        title = "🔍 Insights",
                        status = "info",
                        solidHeader = TRUE,
                        width = 12,
                        p("The top improvement in happiness is often driven by factors such as improvements in social support, economic stability, and governance."),
                        br(),
                        p("For more details on global happiness trends, check out the resources below:"),
                        tags$ul(
                          tags$li(a("World Happiness Report", href = "https://worldhappiness.report/", target = "_blank")),
                          tags$li(a("UN Happiness Index", href = "https://www.un.org/sustainabledevelopment/happiness/", target = "_blank"))
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.subtabs == 'What-If Analysis'",
                      selectInput("country", "Select a Country:", choices = unique(happiness_df$country)),
                      selectInput("year", "Select year:", choices = unique(happiness_df$year)),
                      
                      h4("Predicted Happiness Score:"),
                      verbatimTextOutput("what_if_prediction")
                    )
                  ),
                  mainPanel(
                    tabsetPanel(id = "subtabs",
                                tabPanel("Panel Data Table", DTOutput("panel_data_table")),
                                tabPanel("Top Improvement", 
                                         conditionalPanel(
                                           condition = "input.subtabs == 'Top Improvement'",
                                           fluidRow(
                                             column(8,
                                                    uiOutput("top_improvement")  # Your dynamic message here
                                             )
                                            
                                             )
                                           )
                                         ),
                                
                    
                  
                                
                                tabPanel("What-If Analysis",
                                         h3("What-If Analysis: Adjust Factors"),
                                         wellPanel(
                                           sliderInput("economy", "Economy Score",
                                                       min = min(happiness_df$economy_score, na.rm = TRUE),
                                                       max = max(happiness_df$economy_score, na.rm = TRUE),
                                                       value = mean(happiness_df$economy_score, na.rm = TRUE),
                                                       step = 0.01),
                                           
                                           sliderInput("social", "Social Support Score",
                                                       min = min(happiness_df$social_score, na.rm = TRUE),
                                                       max = max(happiness_df$social_score, na.rm = TRUE),
                                                       value = mean(happiness_df$social_score, na.rm = TRUE),
                                                       step = 0.01),
                                           
                                           sliderInput("lifeexp", "Life Expectancy Score",
                                                       min = min(happiness_df$lifeexpectancy_score, na.rm = TRUE),
                                                       max = max(happiness_df$lifeexpectancy_score, na.rm = TRUE),
                                                       value = mean(happiness_df$lifeexpectancy_score, na.rm = TRUE),
                                                       step = 0.01),
                                           
                                           sliderInput("freedom", "Freedom Score",
                                                       min = min(happiness_df$freedom_score, na.rm = TRUE),
                                                       max = max(happiness_df$freedom_score, na.rm = TRUE),
                                                       value = mean(happiness_df$freedom_score, na.rm = TRUE),
                                                       step = 0.01),
                                           
                                           sliderInput("generosity", "Generosity Score",
                                                       min = min(happiness_df$generosity_score, na.rm = TRUE),
                                                       max = max(happiness_df$generosity_score, na.rm = TRUE),
                                                       value = mean(happiness_df$generosity_score, na.rm = TRUE),
                                                       step = 0.01),
                                           
                                           sliderInput("corruption", "Corruption Perception Score",
                                                       min = min(happiness_df$corrperception_score, na.rm = TRUE),
                                                       max = max(happiness_df$corrperception_score, na.rm = TRUE),
                                                       value = mean(happiness_df$corrperception_score, na.rm = TRUE),
                                                       step = 0.01)
                                           
                                         )
                                )
                    )
                  )
                )
              )
      ),
      
      # Multivariate clustering
      tabItem(tabName = "clustering_multivariate",
              fluidPage(
                titlePanel("World Happiness Clustering Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    h4("Clustering Parameters"),
                    selectInput("year", "Select Year", choices = sort(unique(data$year))),
                    selectInput("method", "Clustering Method", choices = c("K-Means", "Hierarchical")),
                    selectInput("k", "Number of Clusters", choices = 2:8, selected = 3),
                    hr(),
                    h4("PCA Variables Selection"),
                    selectInput("var1", "Variable 1", choices = names(data)[4:9], selected = "economy_score"),
                    selectInput("var2", "Variable 2", choices = names(data)[4:9], selected = "social_score"),
                    selectInput("var3", "Variable 3", choices = names(data)[4:9], selected = "lifeexpectancy_score"),
                    helpText("Note: For optimal PCA visualization, select 2 different variables the 2D plot and 3 different variables for  the 3D plot")
                  ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Yearly Cluster Exploration",
                           leafletOutput("map_section1", height = "500px"),
                           uiOutput("validation_stats"),
                           DTOutput("cluster_summary")
                  ),
                  tabPanel("PCA Analysis 2D",
                           div(
                             style = "border: 1px solid #b3cce6; 
                                     border-radius: 8px; 
                                     padding: 10px; 
                                     background-color: #f5faff; 
                                     margin-top: 10px; 
                                     box-shadow: 1px 1px 4px rgba(0,0,0,0.05);",
                             strong("What is PCA (Principal Component Analysis)?"),
                             p("PCA is a technique that reduces the number of variables while preserving most of the important information (variance) in the data. 
                               It creates new variables, called Principal Components, which are combinations of the original variables. 
                               The first few components usually capture most of the variation. PCA is used here to project the clustered data into a 2D space, making it easier to visualize how well-separated the clusters are."),
                             p(em("Think of PCA as finding the best viewing angle to observe patterns in complex data."))
                           ),
                           br(),
                           plotlyOutput("pca_plot_2d", height = "350px"),
                           br(),
                           tabsetPanel(
                             tabPanel("Loadings Matrix",
                                      DTOutput("pca_loadings_2d")
                             ),
                             tabPanel("Interpretation",
                                      uiOutput("pca_explanation_2d")
                             )
                           )
                  ),
                  tabPanel("PCA Analysis 3D",
                        div(
                           style = "border: 1px solid #b3cce6; 
                                     border-radius: 8px; 
                                     padding: 10px; 
                                     background-color: #f5faff; 
                                     margin-top: 10px; 
                                     box-shadow: 1px 1px 4px rgba(0,0,0,0.05);",
                             h5("💡 Food for thought: Is the separation between clusters apparent?")
                           ),
                           br(),
                           plotlyOutput("pca_plot_3d", height = "350px"),
                           br(),
                           tabsetPanel(
                             tabPanel("Loadings Matrix",
                                      DTOutput("pca_loadings_3d")
                             ),
                             tabPanel("Interpretation",
                                      uiOutput("pca_explanation_3d")
                             )
                           )
                  ),
                  tabPanel("PCA Full Analysis",
                        div(
                           style = "border: 1px solid #b3cce6; 
                                     border-radius: 8px; 
                                     padding: 10px; 
                                     background-color: #f5faff; 
                                     margin-top: 10px; 
                                     box-shadow: 1px 1px 4px rgba(0,0,0,0.05);",
                           h5("ℹ️ This PCA plot uses all clustering variables for analysis")
                           ),
                           br(),
                           plotlyOutput("pca_plot_full", height = "350px"),
                           br(),
                           tabsetPanel(
                             tabPanel("Loadings Matrix",
                                      DTOutput("pca_loadings_full")
                             ),
                             tabPanel("Interpretation",
                                      uiOutput("pca_explanation_full")
                             )
                           )
                  ),
                  tabPanel("Clustering Over Time",
                        div(
                          style = "border: 1px solid #b3cce6; 
                                     border-radius: 8px; 
                                     padding: 10px; 
                                     background-color: #f5faff; 
                                     margin-top: 10px; 
                                     box-shadow: 1px 1px 4px rgba(0,0,0,0.05);",
                           strong("📈 Temporal Analysis"),
                           p("Clusters are formed separately for each year. 
                             This plot tracks how the proportion of countries in each cluster changes over time,
                             allowing observerations of stability or shifts in global patterns.")
                           ),
                           br(),
                           plotOutput("temporal_plot", height = "350px"),
                           DTOutput("cluster_profile_section3")
                  )
                  # tabPanel("Cluster Transition Sankey",
                  #    h5("Sankey Diagram of Cluster Transitions"),
                  #    sankeyNetworkOutput("sankey_plot", height = "600px")
                  # )
                )
              )
            )
          )
        ),
      
    # Time-series Clustering
      tabItem(tabName = "clustering_timeseries",
              fluidPage(
                useShinyjs(),
                titlePanel("World Happiness Time Series Clustering"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("clusterMethod", "Clustering Method:",
                                choices = c("Feature-based Clustering", 
                                            "DTW-based Clustering",
                                            "Shape-based Clustering"),
                                selected = "Feature-based Clustering"),
                    sliderInput("numClusters", "Number of Clusters:",
                                min = 2, max = 8, value = 3, step = 1),
                    conditionalPanel(
                      condition = "input.clusterMethod == 'Feature-based Clustering'",
                      checkboxGroupInput("features", "Select Features:",
                                         choices = c("Mean", "Variance", "Trend", "Autocorrelation"),
                                         selected = c("Mean", "Trend"))
                    ),
                    conditionalPanel(
                      condition = "input.clusterMethod == 'DTW-based Clustering'",
                      selectInput("dtwDistance", "DTW Distance Method:",
                                  choices = c("Euclidean", "Manhattan"),
                                  selected = "Euclidean")
                    ),
                    actionButton("runClustering", "Run Clustering", class = "btn-primary", style = "margin-top: 15px; width: 100%"),
                    uiOutput("clusteringDescription")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Optimal Clusters", 
                               fluidRow(
                                 column(6, plotOutput("elbowPlot")),
                                 column(6, plotOutput("silhouettePlot"))
                               )
                      ),
                      tabPanel("Cluster Results", 
                               plotlyOutput("clusterPlot", height = "500px")
                      ),
                      tabPanel("Geographical Distribution", 
                               leafletOutput("mapPlot", height = "500px")
                      ),
                      tabPanel("Statistical Analysis", 
                               selectInput("explVar", "Select Variable:",
                                           choices = c("Economy", "Social Support", "Life Expectancy", "Freedom", "Generosity", "Corruption Perception"), selected = "Economy"),
                               plotOutput("varBoxPlot", height = "300px"),
                               br(),
                               uiOutput("statSummary"),
                               plotOutput("corrPlot", height = "400px")
                      )
                    )
                  )
                )
              )
            ),
    
  # Choropleth & Proportional
  tabItem(tabName = "geo_choropleth",
          fluidPage(
            titlePanel("Geospatial Analysis: Choropleth & Proportional Maps"),
            sidebarLayout(
              sidebarPanel(
                selectInput("selected_year", "Select Year:",
                            choices = sort(unique(world_happy$year)), selected = 2024),
                selectInput("selected_region", "Filter by Region:", choices = NULL),
                selectInput("selected_country", "Search Country:", choices = NULL),
                hr(),
                h4("Chart Interpretation"),
                HTML("The <b>Choropleth Map</b> uses color gradients to represent the overall happiness score of each country. 
 Darker shades indicate higher happiness, while lighter shades indicate lower happiness. This allows for a quick visual comparison across countries.<br><br>
 
 The <b>Proportional Symbol Map</b> overlays circles on each country, where the <b>size of the circle</b> corresponds to the happiness score. 
 Larger circles represent happier countries. This helps emphasize magnitude and enables easy identification of extreme values.<br><br>
 
 Together, these maps provide complementary insights — the choropleth captures regional trends through shading, 
 while the proportional symbols highlight individual country scores more directly.")
                
              ),
              mainPanel(
                fluidRow(
                  column(6, h4("Choropleth Map"), tmapOutput("choropleth_map", height = "500px")),
                  column(6, h4("Proportional Symbol Map"), leafletOutput("prop_map", height = "500px"))
                )
              )
            )
          )
      ),
      
      # LISA & Moran's I
      tabItem(tabName = "geo_lisa",
              fluidPage(
                titlePanel("Geospatial Analysis: LISA & Moran's I"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("selected_year_lisa", "Select Year:",
                                choices = sort(unique(happiness_df$year)), selected = 2024),
                    hr(),
                    h4("Chart Interpretation"),
                    HTML("The Moran scatterplot shows how each country's happiness score correlates with its neighbors'.<br><br>
                           The LISA Cluster map highlights statistically significant spatial clusters:<br>
                           - <b style='color:red;'>High-High</b>: Top right quadrant - Happy countries near other happy countries<br>
                           - <b style='color:blue;'>Low-Low</b>: Bottom left quadrant - Unhappy countries near unhappy neighbors<br>
                           - <b style='color:#78c679;'>Low-High</b>: Top left quadrant - Potential outliers<br>
                           - <b style='color:#c2e699;'>High-Low</b>: Bottom right quandrant - Potential outliers<br>
                           - <b style='color:#ffffcc;'>Insignificant</b>: No strong spatial pattern")
                  ),
                  mainPanel(
                    fluidRow(
                      column(6, plotOutput("moran_plot", height = "500px")),
                      column(6, tmapOutput("lisa_map", height = "500px")),
                      column(12, h4("Proportional Symbol Map (LISA Context)"), leafletOutput("prop_map_lisa", height = "500px"))
                    )
                  )
                )
              )
      ),
      
      # Aspatial
      tabItem(tabName = "geo_aspatial",
              fluidPage(
                titlePanel("Geospatial Analysis: Aspatial Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("year", "Select Year:", choices = sort(unique(happiness_df$year)), selected = 2024),
                    pickerInput(
                      inputId = "region",
                      label = "Search and Select Region(s):",
                      choices = unique(happiness_df$region),
                      selected = unique(happiness_df$region),
                      options = list(`actions-box` = TRUE, `live-search` = TRUE),
                      multiple = TRUE
                    ),
                    hr(),
                    h4("Chart Interpretation"),
                    HTML("The <b>Left Map</b> provides a simple geographic overview of the selected regions without considering spatial relationships like proximity or clustering. 
                   It helps orient users geographically while remaining neutral to spatial dependence.<br><br>

                   The <b>Region-Level Ridgeline Plot Chart</b> (top right) compares average happiness scores across selected regions. 
                   This enables users to quickly identify which regions are generally happier or less happy.<br><br>

                   The <b>Summary Table</b> complements the visuals by providing exact happiness values, 
                   averages, and rankings, allowing for precise data inspection.")
                  ),
                  mainPanel(
                    width = 9,
                    fluidRow(
                      column(6, tmapOutput("map", height = "400px")),
                      column(6, plotOutput("regionPlot", height = "400px"))
                    ),
                    fluidRow(
                      column(12, gt_output("summaryTable"))
                    )
                  )
                )
              )
      ),
      
      
      
      tabItem(tabName = "about",
              fluidPage(
                titlePanel("About This Dashboard"),
                
                box(
                  title = "🌍 Project Overview", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  tags$ul(
                    tags$li(HTML("This interactive dashboard visualizes global happiness trends from <strong>2014 to 2024</strong>.")),
                    tags$li("It enables users to explore country-level happiness scores, compare across regions, and identify key drivers using modeling and spatial analysis.")
                  )
                ),
                
                
                box(
                  title = "📊 Features", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  tags$ul(
                    tags$li("📈 Time Series: Explore happiness trends, forecasts, and causal impacts"),
                    tags$li("📊 Panel Model: Fixed effects regression to assess feature importance"),
                    tags$li("⚙️ What-If Analysis: Simulate changes to happiness factors"),
                    tags$li("🔍 Clustering: Group similar countries based on happiness indicators"),
                    tags$li("🗺️ Geospatial: View Choropleth, Proportional, LISA, and Aspatial maps"),
                  )
                ),
                
                box(
                  title = "📁 Data Source", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  tags$ul(
                    tags$li(HTML('The data is sourced from the <a href="https://worldhappiness.report" target="_blank">World Happiness Report</a>, which evaluates well-being across countries using metrics like GDP per capita, social support, healthy life expectancy, freedom, generosity, and corruption perception.'))
                  )
                ),
                
                box(
                  title = "👩‍💻 Developer Info", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  tags$ul(
                    tags$li(HTML("Created by <strong>Andrea Yeo, Dhreeti Shah, and Ou Yi Ming</strong> as part of the <strong>ISSS608 Visual Analytics Applications (VAA)</strong> subject in the Master of Information Technology program (MITB) at SMU.")),
                    tags$li(HTML("This project combines statistical modeling, data visualization, and geospatial techniques using <strong>R Shiny</strong>.")),
                    tags$li(HTML("🙏 Special thanks to <strong>Prof Kam Tin Seong</strong> for his insightful guidance, encouragement, and support throughout the development of this dashboard."))
                  )
                ),
                
                
                
                box(
                  title = "📅 Last Updated", width = 12, solidHeader = TRUE, collapsible = TRUE,
                  p("April 2025")
                )
              )
      )
      
      
      
    ) 
  ) 
) 


server <- function(input, output, session) {
  # --- Time Series Filtering ---
  filtered_data <- reactive({
    happiness_df %>%
      filter(country %in% input$country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$trend_explanation <- renderUI({
    HTML("<div style='border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;'>
            <h4>Trend Analysis</h4>
            <p>This plot shows the trend of happiness scores over time across selected countries. Use the filters to select a specific country and year range to explore the trend.</p>
          </div>")
  })
  
  # Explanation for Forecast tab
  output$forecast_explanation <- renderUI({
    HTML("<div style='border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;'>
            <h4>Forecasting Happiness</h4>
            <p>This plot provides a forecast of future happiness scores based on past trends. Adjust the year range to explore potential future outcomes.</p>
          </div>")
  })
  
  # Explanation for Causal Impact tab
  output$causal_impact_explanation <- renderUI({
    HTML("<div style='border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;'>
            <h4>Causal Impact Analysis</h4>
            <p>This plot shows the causal impact of events (e.g., COVID-19, economic crises) on happiness scores. Use this to understand how external factors may have influenced the happiness trends.</p>
          </div>")
  })

  output$trend_plot <- renderPlotly({
    filtered_data_trendplot <- happiness_df %>%
      filter(country %in% input$country & 
               year >= input$year_range[1] & 
               year <= input$year_range[2])
    
    # Plot the filtered data
    plot_ly(data = filtered_data_trendplot, x = ~year, y = ~ladder_score, 
            color = ~country, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Happiness Trend",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Happiness Score"),
             legend = list(title = list(text = "Country")))
  })
  

  output$forecast_plot <- renderPlotly({
    
    # Create a filtered dataset specific to this plot
    filtered_data_forecast <- happiness_df %>%
      filter(country %in% input$country & 
               year >= input$year_range[1] & 
               year <= input$year_range[2])
    
    
    # Initialize an empty Plotly object
    p <- plot_ly()
    
    # Loop through each selected country and generate forecasts
    for (country_name in unique(filtered_data_forecast$country)) {
      
      country_data <- filtered_data_forecast %>% filter(country == country_name)
      ts_data <- ts(country_data$ladder_score, start = min(country_data$year), frequency = 1)
      
      if (length(ts_data) > 5) {
        model <- auto.arima(ts_data)
        forecast_data <- forecast(model, h = 5)
        future_years <- seq(max(country_data$year) + 1, by = 1, length.out = 5)
        
        p <- p %>%
          add_lines(x = country_data$year, y = ts_data, name = paste(country_name, "- Observed")) %>%
          add_lines(x = future_years, y = forecast_data$mean, name = paste(country_name, "- Forecasted"), 
                    line = list(dash = "dash"))
      }
    }
    
    # Layout settings
    p %>% layout(title = "Happiness Forecast Comparison", 
                 xaxis = list(title = "Year"), 
                 yaxis = list(title = "Happiness Score"))
  })
  
  
  output$causal_impact_plot <- renderPlot({
    pre_period <- c(2014, 2019)
    post_period <- c(2020, 2024)
    plots <- list()
    for (country_name in input$country) {
      impact_data <- happiness_df %>% filter(country == country_name, year >= 2014 & year <= 2024) %>% na.omit()
      if (nrow(impact_data) > 5) {
        ts_data <- zoo(impact_data$ladder_score, order.by = impact_data$year)
        impact <- CausalImpact(ts_data, pre.period = pre_period, post.period = post_period)
        impact_df <- data.frame(
          year = impact_data$year,
          actual = impact$series$response,
          predicted = impact$series$point.pred,
          lower = impact$series$point.pred.lower,
          upper = impact$series$point.pred.upper
        )
        plots[[country_name]] <- ggplot(impact_df, aes(x = year)) +
          geom_line(aes(y = actual, color = "Actual")) +
          geom_line(aes(y = predicted, color = "Predicted"), linetype = "dashed") +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
          labs(title = paste("Causal Impact -", country_name)) +
          theme_minimal()
      }
    }
    if (length(plots) > 0) do.call(grid.arrange, c(plots, ncol = 2)) else ggplot() + ggtitle("Not enough data")
  })
  
  output$feature_importance_explanation <- renderUI({
    HTML("<div style='border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;'>
          <h4>Feature Importance</h4>
          <p>This plot highlights the most significant factors influencing happiness scores. Larger bars indicate greater importance in predicting happiness levels.</p>
        </div>")
  })
  
  # Explanation for Happiness Trend
  output$happiness_trend_explanation <- renderUI({
    HTML("<div style='border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;'>
          <h4>Happiness Trend</h4>
          <p>This plot shows the changes in happiness scores over time. Use this to analyze trends, compare countries, and identify patterns in happiness levels.</p>
        </div>")
  })
  
  output$feature_importance_plot_panel1 <- renderPlotly({
    # Ensure at least one or two countries are selected
    req(input$country_select_multi)
    
    selected_countries <- input$country_select_multi
    
    # Limit selection to exactly two countries
    if (length(selected_countries) != 2) {
      return(NULL) # Do not render if less/more than 2 countries are selected
    }
    
    # Subset data for the selected countries
    filtered_data <- happiness_df %>% 
      filter(country %in% selected_countries)
    
    # Run Fixed Effects Model for each country
    fe_model_1 <- plm(ladder_score ~ economy_score + social_score + lifeexpectancy_score + 
                        freedom_score + generosity_score + corrperception_score, 
                      data = filtered_data %>% filter(country == selected_countries[1]), 
                      index = c("country", "year"), model = "within")
    
    fe_model_2 <- plm(ladder_score ~ economy_score + social_score + lifeexpectancy_score + 
                        freedom_score + generosity_score + corrperception_score, 
                      data = filtered_data %>% filter(country == selected_countries[2]), 
                      index = c("country", "year"), model = "within")
    
    # Extract coefficients
    coefs_1 <- as.data.frame(coef(summary(fe_model_1))) %>%
      rownames_to_column(var = "Feature") %>%
      rename(Coefficient = Estimate) %>%
      mutate(Country = selected_countries[1])
    
    coefs_2 <- as.data.frame(coef(summary(fe_model_2))) %>%
      rownames_to_column(var = "Feature") %>%
      rename(Coefficient = Estimate) %>%
      mutate(Country = selected_countries[2])
    
    # Combine both dataframes
    coefs_df <- bind_rows(coefs_1, coefs_2)
    
    # Create ggplot
    p <- ggplot(coefs_df, aes(x = Coefficient, y = reorder(Feature, Coefficient), fill = Country)) +
      geom_col(position = "dodge") +  # Side-by-side bars
      scale_fill_manual(values = c("steelblue", "darkorange")) +  # Colors for each country
      labs(title = "Feature Importance (Fixed Effects Model)", x = "Coefficient", y = "Feature") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  
  
  output$happiness_trend_plot <- renderPlotly({
    req(input$country, input$factors)
    
    # Filter for selected country
    country_data <- happiness_df %>% 
      filter(country == input$country)
    
    # Confirm selected columns + year exist
    available_cols <- c("year", input$factors)
    missing_cols <- setdiff(available_cols, colnames(country_data))
    
    if (length(missing_cols) > 0) {
      stop(paste("Missing column(s):", paste(missing_cols, collapse = ", ")))
    }
    
    # Extract only needed columns and sort by year
    plot_data <- country_data[, available_cols] %>%
      arrange(year)
    
    # Pivot to long format
    long_data <- tidyr::pivot_longer(
      plot_data,
      cols = -year,
      names_to = "Factor",
      values_to = "Value"
    )
    
    # Plot
    p <- ggplot(long_data, aes(x = year, y = Value, color = Factor)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = paste("Happiness Trends in", input$country),
           x = "Year", y = "Score") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  output$panel_data_table <- renderDT({
    happiness_df %>% filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      datatable(options = list(scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_happiness_data.csv", sep = "")  # File name
    },
    content = function(file) {
      # Capture the search query from the data table's search bar
      search_query <- input$panel_data_table_search
      
      # Filter the data based on the search query
      data_to_export <- happiness_df
      
      # If there's a search query, apply it to filter the data across all columns
      if (!is.null(search_query) && search_query != "") {
        data_to_export <- data_to_export %>%
          filter(grepl(search_query, country, ignore.case = TRUE) |
                   grepl(search_query, ladder_score, ignore.case = TRUE) |
                   grepl(search_query, economy_score, ignore.case = TRUE) |
                   grepl(search_query, social_score, ignore.case = TRUE) |
                   grepl(search_query, lifeexpectancy_score, ignore.case = TRUE) |
                   grepl(search_query, freedom_score, ignore.case = TRUE) |
                   grepl(search_query, generosity_score, ignore.case = TRUE) |
                   grepl(search_query, corrperception_score, ignore.case = TRUE))
      }
      
      # Write the filtered data to the CSV file
      write.csv(data_to_export, file, row.names = FALSE)
    }
  )


  
  output$top_improvement <- renderUI({
    
    improvement_df <- happiness_df %>%
      group_by(country) %>%
      summarize(improvement = max(ladder_score) - min(ladder_score), .groups = "drop") %>%
      arrange(desc(improvement))
    
    # Top country with the highest improvement
    top_country <- improvement_df$country[1]
    top_change <- round(improvement_df$improvement[1], 2)
    
    # Country with the lowest improvement
    bottom_country <- improvement_df$country[nrow(improvement_df)]
    bottom_change <- round(improvement_df$improvement[nrow(improvement_df)], 2)
    
    # Average improvement across all countries
    avg_improvement <- round(mean(improvement_df$improvement), 2)
    
    # Construct the UI output with multiple boxes for each message
    fluidPage(
      box(
        title = "🏆 Top Happiness Improvement",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        paste0(
          "**The country with the highest happiness improvement** from ", min(happiness_df$year),
          " to ", max(happiness_df$year), " is **", top_country, "** with an increase of ",
          top_change, " in the Ladder Score. This demonstrates a significant shift in happiness levels, ",
          "potentially due to factors like improved economy, social support, or governance."
        )
      ),
      
      box(
        title = "📉 Least Happiness Improvement",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        paste0(
          "On the other hand, the country with the **least improvement** in happiness is **", 
          bottom_country, "**, with a modest increase of only ", bottom_change, "."
        )
      ),
      
      box(
        title = "📊 Average Improvement",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        paste0(
          "📝 **On average**, countries saw an improvement of **", avg_improvement, "** in happiness levels over this period. ",
          "This data provides valuable insights into global trends in well-being."
        )
      )
    )
  })
  
  
  
  output$what_if_prediction <- renderText({
    
    req(input$country, input$year)  # Ensure inputs are available
    
    # Extract coefficients from the model
    coef_values <- coef(fe_model)
    
    # Get the existing ladder score for the selected country and year
    existing_score <- happiness_df %>%
      filter(country == input$country, year == input$year) %>%
      pull(ladder_score)
    
    # If no data is found, return a message
    if (length(existing_score) == 0) {
      return("No data available for the selected country and year.")
    }
    
    # Compute the new predicted happiness score based on user adjustments
    new_ladder_score <- existing_score + (
      (coef_values["economy_score"] * input$economy) +
        (coef_values["social_score"] * input$social) +
        (coef_values["lifeexpectancy_score"] * input$lifeexp) +
        (coef_values["freedom_score"] * input$freedom) +
        (coef_values["generosity_score"] * input$generosity) +
        (coef_values["corrperception_score"] * input$corruption)
    )
    
    # Compute the difference
    score_change <- new_ladder_score - existing_score
    
    # Generate output message
    paste0(
      "Selected Country: ", input$country, "\n",
      "Selected Year: ", input$year, "\n",
      "Existing Happiness Score: ", round(existing_score, 2), "\n",
      "New Predicted Happiness Score: ", round(new_ladder_score, 2), "\n",
      "Change: ", round(score_change, 2)
    )
  })
  
  # --- MULTIVARIATE CLUSTERING BLOCK ---
  df_year <- reactive({
    data %>% filter(year == input$year)
  })
  
  clustering_result <- reactive({
    req(input$year)
    req(input$k)
    df <- df_year() %>% as_tibble()
    # Extract only variables used for clustering
    df_vars <- df %>%
      dplyr::select(economy_score, social_score, lifeexpectancy_score, freedom_score, generosity_score, corrperception_score)
    # Scale
    df_scaled <- scale(df_vars)
    k_val <- as.numeric(input$k)
    
    # Clustering
    if(input$method == "K-Means"){
      km <- kmeans(df_scaled, centers = k_val, nstart = 25)
      df$cluster <- as.factor(km$cluster)
      return(list(data = df, cluster = km$cluster, scaled = df_scaled))
    } else {
      hc <- hclust(dist(df_scaled), method = "ward.D2")
      df$cluster <- as.factor(cutree(hc, k = input$k))
      return(list(data = df, cluster = cutree(hc, k = input$k), scaled = df_scaled))
    }
  })
  
  # Get world map data using rnaturalearth
  world_map <- reactive({
    ne_countries(scale = "medium", returnclass = "sf")
  })
  
  # Render the map
  output$map_section1 <- renderLeaflet({
    req(clustering_result())
    
    # Get the cluster results
    cluster_data <- clustering_result()$data
    # Get world map data
    world <- world_map()
    # Match countries by name
    world$cluster <- NA
    for(i in 1:nrow(cluster_data)) {
      idx <- which(world$name == cluster_data$country_fixed[i] | 
                     world$name_long == cluster_data$country_fixed[i])
      if(length(idx) > 0) {
        world$cluster[idx] <- cluster_data$cluster[i]
      }
    }
    
    # Create color palette for clusters
    #pal1 <- colorFactor(
    #  palette = brewer.pal(max(as.numeric(cluster_data$cluster), 8), "Set1"),
    #  domain = cluster_data$cluster
    #)
    # Define color palette
    pal <- colorFactor("Set1", domain = cluster_data$cluster, na.color = "lightgray")
    
    # Create leaflet map
    leaflet(world) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(cluster),
        weight = 0.5,
        color = "black",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "blue",
          bringToFront = TRUE
        ),
        popup = ~paste0(
          "<strong>Country:</strong> ", name, "<br>",
          "<strong>Cluster:</strong> ", ifelse(is.na(cluster), "No Data", cluster)
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = cluster_data$cluster,
        title = "Cluster"
      )
  })
  
  clustering_validation <- function(data, cluster_assignments, scaled_data) {
    if(is.factor(cluster_assignments)) {
      cluster_assignments <- as.numeric(as.character(cluster_assignments))
    }
    validation_stats <- list()
    # Silhouette score
    sil <- cluster::silhouette(cluster_assignments, dist(scaled_data))
    validation_stats$avg_silhouette <- mean(sil[, "sil_width"])
    # Dunn Index
    validation_stats$dunn_index <- clValid::dunn(dist(scaled_data), cluster_assignments)
    # Calinski-Harabasz Index (CH Index)
    validation_stats$ch_index <- fpc::calinhara(scaled_data, cluster_assignments)
    # Davies-Bouldin Index
    validation_stats$db_index <- clusterSim::index.DB(scaled_data, cluster_assignments)$DB
    # Calculate cluster summary statistics
    cluster_summary <- data.frame(
      Cluster = sort(unique(cluster_assignments)),
      Count = as.numeric(table(cluster_assignments)),
      stringsAsFactors = FALSE
    )
    # Average ladder score
    ladder_means <- aggregate(data$ladder_score, by = list(cluster = cluster_assignments), FUN = mean)
    cluster_summary$avg_ladder_score <- ladder_means$x
    # Calculate means for each cluster for the original variables
    variables <- colnames(scaled_data)
    for(var in variables) {
      var_means <- aggregate(data[[var]], by=list(cluster=cluster_assignments), FUN=mean)
      cluster_summary[[paste0(var, "_mean")]] <- var_means$x
      var_sds <- aggregate(data[[var]], by=list(cluster=cluster_assignments), FUN=sd)
      cluster_summary[[paste0(var, "_sd")]] <- var_sds$x
    }
    # Add top 3 countries per cluster
    cluster_summary$top_countries <- sapply(sort(unique(cluster_assignments)), function(cl) {
      countries <- data$country_fixed[cluster_assignments == cl]
      if(length(countries) > 3) {
        paste(head(countries, 3), collapse=", ")
      } else {
        paste(countries, collapse=", ")
      }
    })
    
    # Return both validation stats and cluster summary
    return(list(
      validation = validation_stats,
      summary = cluster_summary
    ))
  }
  
  clustering_stats <- reactive({
    req(clustering_result())
    # Get clustering results
    cluster_result <- clustering_result()
    # Calculate validation stats
    clustering_validation(
      data = cluster_result$data,
      cluster_assignments = cluster_result$cluster,
      scaled_data = cluster_result$scaled
    )
  })
  
  # Validation statistics card
  output$validation_stats <- renderUI({
    req(clustering_stats())
    stats <- clustering_stats()$validation
    div(
      class = "panel panel-default",
      div(class = "panel-heading", h4("Clustering Validation Statistics")),
      div(class = "panel-body",
          fluidRow(
            column(3, 
                   div(class = "well text-center", 
                       h4("Silhouette Score"),
                       h3(style = "color: #2C3E50", round(stats$avg_silhouette, 3)),
                       p("Higher is better")
                   )
            ),
            column(3, 
                   div(class = "well text-center", 
                       h4("Dunn Index"),
                       h3(style = "color: #2C3E50", round(stats$dunn_index, 3)),
                       p("Higher is better")
                   )
            ),
            column(3, 
                   div(class = "well text-center", 
                       h4("CH Index"),
                       h3(style = "color: #2C3E50", round(stats$ch_index, 1)),
                       p("Higher is better")
                   )
            ),
            column(3, 
                   div(class = "well text-center", 
                       h4("Davies-Bouldin"),
                       h3(style = "color: #2C3E50", round(stats$db_index, 3)),
                       p("Lower is better")
                   )
            )
          ),
          tags$hr(style = "border-top: 1px solid #ccc; margin: 5px 0;"),
          p("These metrics help evaluate clustering quality:"),
          tags$ul(
            tags$li(strong("Silhouette Score:"), "Measures how similar objects are to their own cluster compared to other clusters. Range from -1 to 1."),
            tags$li(strong("Dunn Index:"), "Ratio of the smallest distance between observations in different clusters to the largest intra-cluster distance."),
            tags$li(strong("Calinski-Harabasz Index:"), "Ratio of between-cluster variance to within-cluster variance."),
            tags$li(strong("Davies-Bouldin Index:"), "Average similarity between each cluster and its most similar cluster. Lower values indicate better clustering.")
          )
      )
    )
  })
  
  # Cluster summary table
  output$cluster_summary <- renderDT({
    req(clustering_stats())
    summary_df <- clustering_stats()$summary
    display_df <- data.frame(
      Cluster = summary_df$Cluster,
      "No. of Countries" = summary_df$Count,
      "Example Countries" = summary_df$top_countries,
      "ladder_score" = round(summary_df$avg_ladder_score, 2),
      stringsAsFactors = FALSE
    )
    # Add mean values for all variables (rounded)
    variables <- colnames(clustering_result()$scaled)
    for(var in variables) {
      var_col <- paste0(var, "_mean")
      if(var_col %in% colnames(summary_df)) {
        display_df[[var]] <- round(summary_df[[var_col]], 2)
      }
    }
    # Create datatable with formatting
    DT::datatable(
      display_df,
      options = list(
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; font-size: 14px; font-weight: bold;',
        'Cluster Summary Statistics'
      )
    )
  })
  
  # 2D PCA Plot
  output$pca_plot_2d <- renderPlotly({
    req(clustering_result())
    req(input$var1, input$var2)
    # Validation: variables must be different
    validate(need(input$var1 != input$var2, "Please select two different variables for the PCA plot."))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2)
    # Extract selected variables and scale
    pca_data <- df %>% dplyr::select(all_of(vars)) %>% scale()
    # Run PCA
    pca_res <- prcomp(pca_data)
    # Extract loadings
    loadings <- round(pca_res$rotation[,1:2], 3)
    loadings_df <- as.data.frame(loadings)
    loadings_df$Variable <- rownames(loadings_df)
    # Create PCA plot
    pca_plot <- autoplot(pca_res, data = df, colour = 'cluster', loadings = TRUE, loadings.label = TRUE) +
      theme_minimal() +
      labs(
        x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)"),
        y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)"),
        color = "Cluster"
      )
    # Convert to plotly
    ggplotly(pca_plot)
  })
  
  output$pca_loadings_2d <- renderDT({
    req(clustering_result())
    req(input$var1, input$var2)
    # Validation: variables must be different
    validate(need(input$var1 != input$var2, ""))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2)
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    # Create loadings matrix
    loadings <- as.data.frame(round(pca_res$rotation, 3))
    loadings$Variable <- rownames(loadings)
    loadings <- loadings[, c("Variable", "PC1", "PC2")]
    # Display as datatable
    DT::datatable(loadings,
                  options = list(dom = 't', pageLength = nrow(loadings)),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-size: 14px; font-weight: bold;',
                    'PCA Loadings Matrix (Selected 2 Variables)')
    ) %>%
      DT::formatStyle(
        columns = c("PC1", "PC2"),
        backgroundColor = styleInterval(
          c(-0.7, 0.7), 
          c("#ffeda0", NA, "#ffeda0")
        ),
        color = "black"
      )
  })
  
  output$pca_explanation_2d <- renderUI({
    req(clustering_result())
    req(input$var1, input$var2)
    # Validation: variables must be different
    validate(need(input$var1 != input$var2, ""))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2)
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    loadings <- as.data.frame(pca_res$rotation[,1:2])
    variance_explained <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)
    interpret_pc <- function(pc_index) {
      pc_loads <- loadings[[pc_index]]
      abs_loads <- abs(pc_loads)
      
      # Get top 2 variables
      top2_idx <- order(abs_loads, decreasing = TRUE)[1:2]
      top2_vars <- rownames(loadings)[top2_idx]
      top2_signs <- ifelse(pc_loads[top2_idx] > 0, "positively", "negatively")
      div(
        style = "border-radius: 15px; background-color: #f0f8ff; padding: 15px; margin: 5px; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);",
        HTML(paste0(
          "<b>PC", pc_index, "</b>", " ( ",
          variance_explained[pc_index], "% variance explained )<br/><br/>",
          "- ", top2_signs[1], " driven by <b>", top2_vars[1], "</b><br/>",
          "- ", top2_signs[2], " driven by <b>", top2_vars[2], "</b>"
        ))
      )
    }
    
    fluidRow(
      div(class = "col-sm-6", interpret_pc(1)),
      div(class = "col-sm-6", interpret_pc(2))
    )
  })
  
  output$pca_plot_full <- renderPlotly({
    req(clustering_result())
    df <- clustering_result()$data
    vars <- c("economy_score", "social_score", "lifeexpectancy_score", "freedom_score", "generosity_score", "corrperception_score")
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    pca_df <- as.data.frame(pca_res$x[, 1:3]) # PC1, PC2, PC3
    pca_df$cluster <- df$cluster
    pca_df$country <- df$country_fixed
    plot_ly(pca_df, 
            x = ~PC1, 
            y = ~PC2, 
            z = ~PC3, 
            color = ~cluster,
            type = "scatter3d", 
            mode = "markers",
            marker = list(size = 4),
            text = ~paste("Country:", country,
                          "<br>Cluster:", cluster,
                          "<br>PC1:", round(PC1, 2),
                          "<br>PC2:", round(PC2, 2),
                          "<br>PC3:", round(PC3, 2)))
  })
  
  output$pca_loadings_full <- renderDT({
    req(clustering_result())
    df <- clustering_result()$data
    vars <- c("economy_score", "social_score", "lifeexpectancy_score", "freedom_score", "generosity_score", "corrperception_score")
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    loadings <- as.data.frame(round(pca_res$rotation[, 1:3], 3))
    loadings$Variable <- rownames(loadings)
    loadings <- loadings[, c("Variable", "PC1", "PC2", "PC3")]
    DT::datatable(loadings,
                  options = list(dom = 't', pageLength = nrow(loadings)),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold;',
                    'PCA Loadings Matrix (All Variables)')
    ) %>%
      DT::formatStyle(
        columns = c("PC1", "PC2", "PC3"),
        backgroundColor = styleInterval(
          c(-0.7, 0.7), 
          c("#ffeda0", NA, "#ffeda0")
        ),
        color = "black"
      )
  })
  
  output$pca_explanation_full <- renderUI({
    req(clustering_result())
    df <- clustering_result()$data
    vars <- c("economy_score", "social_score", "lifeexpectancy_score", "freedom_score", "generosity_score", "corrperception_score")
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    loadings <- as.data.frame(pca_res$rotation[,1:3])
    variance_explained <- round(summary(pca_res)$importance[2, 1:3] * 100, 1)
    interpret_pc <- function(pc_index) {
      pc_loads <- loadings[[pc_index]]
      abs_loads <- abs(pc_loads)
      top2_idx <- order(abs_loads, decreasing = TRUE)[1:2]
      top2_vars <- rownames(loadings)[top2_idx]
      top2_signs <- ifelse(pc_loads[top2_idx] > 0, "positively", "negatively")
      # Bubble card
      div(
        style = "border-radius: 15px; background-color: #f0f8ff; padding: 15px; margin: 5px; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);",
        HTML(paste0(
          "<b>PC", pc_index, "</b>", " ( ",
          variance_explained[pc_index], "% variance explained )<br/><br/>",
          "- ", top2_signs[1], " driven by <b>", top2_vars[1], "</b><br/>",
          "- ", top2_signs[2], " driven by <b>", top2_vars[2], "</b>"
        ))
      )
    }
    fluidRow(
      div(class = "col-sm-4", interpret_pc(1)),
      div(class = "col-sm-4", interpret_pc(2)),
      div(class = "col-sm-4", interpret_pc(3))
    )
  })
  
  # 3D PCA Plot
  output$pca_plot_3d <- renderPlotly({
    req(clustering_result())
    req(input$var1, input$var2, input$var3)
    # Validation: variables must be all different
    validate(need(length(unique(c(input$var1, input$var2, input$var3))) == 3, 
                  "Please select three different variables for the 3D PCA plot."))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2, input$var3)
    # Extract selected variables and scale
    pca_data <- df %>% dplyr::select(all_of(vars)) %>% scale()
    # Run PCA on the selected variables
    pca_res <- prcomp(pca_data)
    # Get the principal components (up to 3)
    pca_df <- as.data.frame(pca_res$x[, 1:3])
    # Add cluster and country information
    pca_df$cluster <- df$cluster
    pca_df$country <- df$country_fixed
    loadings <- round(pca_res$rotation[,1:3], 3)
    # Create color palette for clusters
    cluster_colors <- colorRampPalette(brewer.pal(8, "Set1"))(length(unique(pca_df$cluster)))
    # Full 3D plot
    plot_ly(pca_df, 
            x = ~PC1, 
            y = ~PC2, 
            z = ~PC3, 
            color = ~cluster,
            colors = cluster_colors,
            type = "scatter3d", 
            mode = "markers",
            marker = list(size = 5),
            hoverinfo = "text",
            text = ~paste("Country:", country, 
                          "<br>Cluster:", cluster,
                          "<br>PC1:", round(PC1, 2),
                          "<br>PC2:", round(PC2, 2),
                          "<br>PC3:", round(PC3, 2)
            )) %>%
      layout(
        scene = list(
          xaxis = list(title = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100, 1), "%)")),
          yaxis = list(title = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100, 1), "%)")),
          zaxis = list(title = paste0("PC3 (", round(summary(pca_res)$importance[2,3]*100, 1), "%)"))
        ))
  })
  
  output$pca_loadings_3d <- renderDT({
    req(clustering_result())
    req(input$var1, input$var2, input$var3)
    validate(need(length(unique(c(input$var1, input$var2, input$var3))) == 3, ""))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2, input$var3)
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    # Extract loadings matrix
    loadings <- as.data.frame(round(pca_res$rotation[, 1:3], 3))
    loadings$Variable <- rownames(loadings)
    loadings <- loadings[, c("Variable", "PC1", "PC2", "PC3")]  
    DT::datatable(loadings,
                  options = list(dom = 't', pageLength = nrow(loadings)),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-size: 14px; font-weight: bold;',
                    'PCA Loadings Matrix (Selected 3 Variables)')
    ) %>%
      DT::formatStyle(
        columns = c("PC1", "PC2", "PC3"),
        backgroundColor = styleInterval(
          c(-0.7, 0.7), 
          c("#ffeda0", NA, "#ffeda0")
        ),
        color = "black"
      )
  })
  
  output$pca_explanation_3d <- renderUI({
    req(clustering_result())
    req(input$var1, input$var2, input$var3)
    validate(need(length(unique(c(input$var1, input$var2, input$var3))) == 3, ""))
    df <- clustering_result()$data
    vars <- c(input$var1, input$var2, input$var3)
    pca_res <- prcomp(df %>% dplyr::select(all_of(vars)) %>% scale())
    loadings <- as.data.frame(pca_res$rotation[,1:3])
    variance_explained <- round(summary(pca_res)$importance[2, 1:3] * 100, 1)
    # Function to describe each PC
    interpret_pc <- function(pc_index) {
      pc_loads <- loadings[[pc_index]]
      abs_loads <- abs(pc_loads)
      
      # Get top 2 contributors
      top2_idx <- order(abs_loads, decreasing = TRUE)[1:2]
      top2_vars <- rownames(loadings)[top2_idx]
      top2_signs <- ifelse(pc_loads[top2_idx] > 0, "positively", "negatively")
      div(
        style = "border-radius: 15px; background-color: #f0f8ff; padding: 15px; margin: 5px; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);",
        HTML(paste0(
          "<b>PC", pc_index, "</b>", " ( ",
          variance_explained[pc_index], "% variance explained )<br/><br/>",
          "- ", top2_signs[1], " driven by <b>", top2_vars[1], "</b><br/>",
          "- ", top2_signs[2], " driven by <b>", top2_vars[2], "</b>"
        ))
      )
    }
    
    fluidRow(
      div(class = "col-sm-4", interpret_pc(1)),
      div(class = "col-sm-4", interpret_pc(2)),
      div(class = "col-sm-4", interpret_pc(3))
    )
  })
  
  
  cluster_one_year <- function(df, year, method, k) {
    df_year <- df %>% filter(year == year)
    df_vars <- df_year %>%
      dplyr::select(economy_score, social_score, lifeexpectancy_score, freedom_score, generosity_score, corrperception_score)
    df_scaled <- scale(df_vars)
    if(method == "K-Means") {
      km <- kmeans(df_scaled, centers = k, nstart = 25)
      df_year$cluster <- as.factor(km$cluster)
    } else {
      hc <- hclust(dist(df_scaled), method = "ward.D2")
      df_year$cluster <- as.factor(cutree(hc, k = k))
    }
    return(df_year)
  }
  
  cluster_all_years <- reactive({
    req(input$method, input$k)
    years <- sort(unique(data$year))
    clustered_list <- lapply(years, function(yr){
      cluster_one_year(data, yr, input$method, as.numeric(input$k))
    })
    clustered_df <- do.call(rbind, clustered_list)
    clustered_df
  })
  
  output$temporal_plot <- renderPlot({
    df_clustered <- cluster_all_years()
    
    ggplot(df_clustered, aes(x = as.factor(year), fill = cluster)) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(title = "Temporal Cluster Distribution (Yearly Clustering)",
           x = "Year",
           y = "Proportion of countries in each cluster")
  })
  
  output$cluster_profile_section3 <- renderDT({
    req(input$method, input$k)
    df_clustered <- cluster_all_years()
    years <- sort(unique(df_clustered$year))
    clusters <- sort(unique(df_clustered$cluster))  # could be factor or numeric
    full_grid <- expand.grid(
      year = years,
      cluster = clusters
    )
    summary_table <- df_clustered %>%
      group_by(year, cluster) %>%
      summarise(avg_ladder_score = round(mean(ladder_score, na.rm = TRUE), 2))
    summary_complete <- full_grid %>%
      left_join(summary_table, by = c("year", "cluster"))
    # Step 4: Pivot to wide
    summary_wide <- summary_complete %>%
      pivot_wider(names_from = year, values_from = avg_ladder_score) %>%
      arrange(cluster)
    #summary_wide[is.na(summary_wide)] <- "No Data"
    # Dynamically detect actual range
    ladder_range <- range(summary_wide[,-1], na.rm = TRUE)
    cuts <- seq(ladder_range[1], ladder_range[2], length.out=99)
    colors <- colorRampPalette(c("#deebf7", "#3182bd"))(100)
    DT::datatable(summary_wide, 
                  options = list(dom = 't'),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold;',
                    'Average Ladder Scores by Cluster and Year (Yearly Clustering)'
                  )
    ) %>%
      # Apply heatmap style
      DT::formatStyle(
        columns = colnames(summary_wide)[-1],
        backgroundColor = DT::styleInterval(cuts, colors),
        color = 'black'  # text color to contrast well
      )
  })
  
  
  # sankey_data <- reactive({
  #   df_clustered <- cluster_all_years()
  #   df_clustered <- df_clustered %>%
  #     arrange(country_fixed, year) %>%
  #     dplyr::select(country_fixed, year, cluster)
  #   # Create pairs for transitions
  #   df_sankey <- df_clustered %>%
  #     group_by(country_fixed) %>%
  #     mutate(cluster_prev = lag(cluster), year_prev = lag(year)) %>%
  #     filter(!is.na(cluster_prev)) %>%
  #     ungroup() %>%
  #     mutate(
  #       source = paste0("Year ", year_prev, " | Cluster ", cluster_prev),
  #       target = paste0("Year ", year, " | Cluster ", cluster)
  #     ) %>%
  #     count(source, target, name = "value")
  #   # Create node list
  #   nodes <- data.frame(name = unique(c(df_sankey$source, df_sankey$target)))
  #   # Map nodes to indices
  #   df_sankey$IDsource <- match(df_sankey$source, nodes$name) - 1
  #   df_sankey$IDtarget <- match(df_sankey$target, nodes$name) - 1
  #   list(links = df_sankey, nodes = nodes)
  # })
  # 
  # output$sankey_plot <- renderSankeyNetwork({
  #   sankey_content <- sankey_data()
  #   
  #   sankeyNetwork(
  #     Links = sankey_content$links,
  #     Nodes = sankey_content$nodes,
  #     Source = "IDsource",
  #     Target = "IDtarget",
  #     Value = "value",
  #     NodeID = "name",
  #     fontSize = 12,
  #     nodeWidth = 30
  #   )
  # })

  # --- TIME-SERIES CLUSTERING BLOCK ---
  output$clusteringDescription <- renderUI({
    desc <- switch(input$clusterMethod,
                   "Feature-based Clustering" = "Feature-based clustering summarizes each time series into statistical features such as mean, variance, trend, and autocorrelation, then applies k-means clustering on these features.",
                   
                   "DTW-based Clustering" = "DTW-based clustering compares the shape of time series directly by aligning them in time (even if shifted or warped), then groups them based on similarity.",
                   
                   "Shape-based Clustering" = "Shape-based clustering uses correlation-based distances to compare overall patterns and shapes of time series, grouping series with similar profiles."
    )
    div(
      style = "border: 1px solid #b3cce6; 
                   border-radius: 8px; 
                   padding: 10px; 
                   margin-top: 15px; 
                   background-color: #f5faff; 
                   font-size: 0.9em; 
                   box-shadow: 1px 1px 4px rgba(0,0,0,0.05);",
      strong("Method Info:"),
      p(desc)
    )
  })
  
  # Prepare time series data
  time_series_data <- reactive({
    wide_data <- data %>%
      dplyr::select(country, year, ladder_score) %>%
      pivot_wider(names_from = year, values_from = ladder_score)
    wide_data <- wide_data %>%
      filter(!if_any(everything(), is.na))
    ts_matrix <- as.matrix(wide_data[, -1])
    rownames(ts_matrix) <- wide_data$country
    list(wide_data = wide_data, matrix = ts_matrix)
  })
  
  perform_clustering <- function(method, k, ts_matrix, features_selected = NULL) {
    
    if (method == "Feature-based Clustering") {
      
      # ---- Feature extraction ----
      features <- matrix(0, nrow = nrow(ts_matrix), ncol = length(features_selected))
      rownames(features) <- rownames(ts_matrix)
      colnames(features) <- features_selected
      
      for (i in 1:nrow(ts_matrix)) {
        series <- ts_matrix[i, ]
        if ("Mean" %in% features_selected) features[i, "Mean"] <- mean(series, na.rm = TRUE)
        if ("Variance" %in% features_selected) features[i, "Variance"] <- var(series, na.rm = TRUE)
        if ("Trend" %in% features_selected) {
          years <- as.numeric(colnames(ts_matrix))
          valid_idx <- !is.na(series)
          features[i, "Trend"] <- ifelse(sum(valid_idx) > 1, coef(lm(series[valid_idx] ~ years[valid_idx]))[2], 0)
        }
        if ("Autocorrelation" %in% features_selected) {
          valid_series <- series[!is.na(series)]
          features[i, "Autocorrelation"] <- ifelse(length(valid_series) > 1, acf(valid_series, plot = FALSE)$acf[2], 0)
        }
      }
      
      # ---- Clustering ----
      scaled_features <- scale(features)
      km <- kmeans(scaled_features, centers = k, nstart = 25)
      
      dist_mat <- dist(scaled_features)
      
      return(list(
        cluster = km$cluster,
        centers = km$centers,
        dist = dist_mat,
        method = "Feature-based",
        data = scaled_features
      ))
      
    } else if (method == "DTW-based Clustering") {
      
      # ---- DTW Distance ----
      dtw_dist <- proxy::dist(ts_matrix, method = function(x, y) {
        dtw(x, y, distance.only = TRUE)$distance
      })
      
      hc <- hclust(dtw_dist, method = "complete")
      clusters <- cutree(hc, k = k)
      
      return(list(
        cluster = clusters,
        centers = NULL,
        dist = dtw_dist,
        method = "DTW-based",
        data = ts_matrix
      ))
      
    } else if (method == "Shape-based Clustering") {
      
      # ---- Shape Distance ----
      shape_dist <- diss(ts_matrix, METHOD = "COR")
      
      hc <- hclust(shape_dist, method = "complete")
      clusters <- cutree(hc, k = k)
      
      return(list(
        cluster = clusters,
        centers = NULL,
        dist = shape_dist,
        method = "Shape-based",
        data = ts_matrix
      ))
    }
  }
  
  clustering_results <- eventReactive(input$runClustering, {
    perform_clustering(
      method = input$clusterMethod,
      k = input$numClusters,
      ts_matrix = time_series_data()$matrix,
      features_selected = input$features
    )
  })
  
  validation_metrics <- function(clustering_result, max_k = 8) {
    
    dist_mat <- clustering_result$dist
    data_mat <- clustering_result$data
    
    # WSS Calculation
    wss <- sapply(1:max_k, function(k) {
      if (clustering_result$method == "Feature-based") {
        if (k > nrow(data_mat)) return(NA)
        kmeans(data_mat, centers = k, nstart = 25)$tot.withinss
      } else {
        if (k > length(clustering_result$cluster)) return(NA)
        cl <- cutree(hclust(dist_mat, method = "complete"), k = k)
        within_ss <- sum(sapply(1:k, function(i) {
          idx <- which(cl == i)
          if (length(idx) > 1) sum(as.matrix(dist_mat)[idx, idx]^2) / (2 * length(idx)) else 0
        }))
        return(within_ss)
      }
    })
    
    # Silhouette Calculation
    sil <- sapply(2:max_k, function(k) {
      if (k >= nrow(data_mat)) return(NA)
      if (clustering_result$method == "Feature-based") {
        km_tmp <- kmeans(data_mat, centers = k, nstart = 25)
        mean(silhouette(km_tmp$cluster, dist_mat)[, 3])
      } else {
        cl <- cutree(hclust(dist_mat, method = "complete"), k = k)
        mean(silhouette(cl, dist_mat)[, 3])
      }
    })
    
    list(
      wss = wss,
      silhouette = c(NA, sil) # align index with k=1:10
    )
  }
  
  validation_results <- reactive({
    req(clustering_results())
    validation_metrics(clustering_results())
  })
  
  output$elbowPlot <- renderPlot({
    if (input$runClustering == 0) {
      plot.new()
      return()
    }
    plot(1:8, validation_results()$wss, type = "b", 
         xlab = "Number of Clusters", ylab = "WSS", main = "Elbow Method")
    abline(v = input$numClusters, col = "red", lty = 2)
  })
  
  output$silhouettePlot <- renderPlot({
    if (input$runClustering == 0) {
      plot.new()
      return()
    }
    plot(2:8, validation_results()$silhouette[-1], type = "b",
         xlab = "Number of Clusters", ylab = "Average Silhouette Width", main = "Silhouette Plot")
    abline(v = input$numClusters, col = "red", lty = 2)
  })
  
  output$clusterPlot <- renderPlotly({
    req(clustering_results())
    
    cluster_assignments <- clustering_results()$cluster
    ts_data <- time_series_data()$wide_data
    
    # Prepare long format
    plot_data <- ts_data %>%
      mutate(Cluster = factor(cluster_assignments[as.character(country)])) %>%
      filter(!is.na(Cluster)) %>%
      pivot_longer(-c(country, Cluster), names_to = "Year", values_to = "Score")
    
    plot_data$Year <- as.numeric(as.character(plot_data$Year))
    
    p <- ggplot(plot_data, aes(x = Year, y = Score, color = Cluster, group = country)) +
      geom_line(alpha = 0.6) +
      facet_wrap(~Cluster) +
      theme_minimal() +
      labs(title = paste("Time Series Clusters (", clustering_results()$method, ")", sep = ""),
           x = "Year", y = "Ladder Score")
    
    ggplotly(p)
  })
  
  output$mapPlot <- renderLeaflet({
    req(clustering_results())
    # Prepare cluster assignment data
    cluster_assignments <- clustering_results()$cluster
    map_data <- data.frame(
      country = names(cluster_assignments),
      Cluster = factor(cluster_assignments)
    )
    # Merge with country_fixed
    map_data <- map_data %>%
      left_join(
        data %>% dplyr::select(country, country_fixed) %>% distinct(),
        by = "country"
      ) %>%
      filter(!is.na(country_fixed))
    # Load world map as sf object
    world <- ne_countries(scale = "medium", returnclass = "sf")
    # Merge cluster info
    world_map <- world %>%
      left_join(map_data, by = c("name" = "country_fixed"))
    # Define color palette
    pal <- colorFactor("Set1", domain = world_map$Cluster, na.color = "lightgray")
    # Create leaflet map
    leaflet(world_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(Cluster),
        weight = 0.5,
        color = "black",
        fillOpacity = 0.7,
        label = ~paste0(name, ": Cluster ", Cluster),
        highlightOptions = highlightOptions(weight = 2, color = "blue", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Cluster, opacity = 0.7, title = "Cluster", position = "bottomleft")
  })
  
  output$varBoxPlot <- renderPlot({
    req(clustering_results())
    cluster_assignments <-clustering_results()$cluster
    # Map UI input to column
    var_map <- c(
      "Economy" = "economy_score", 
      "Social Support" = "social_score", 
      "Life Expectancy" = "lifeexpectancy_score", 
      "Freedom" = "freedom_score",
      "Generosity" = "generosity_score", 
      "Corruption Perception" = "corrperception_score"
    )
    var_col <- var_map[input$explVar]
    
    # Prepare Data
    plot_data <- data %>%
      filter(year == max(year)) %>%
      mutate(Cluster = factor(cluster_assignments[as.character(country)])) %>%
      filter(!is.na(Cluster))
    
    ggplot(plot_data, aes(x = Cluster, y = .data[[var_col]], fill = Cluster)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = paste("Distribution of", input$explVar, "by Cluster"),
           x = "Cluster", y = input$explVar) +
      theme_minimal()
  })
  
  output$statSummary <- renderUI({
    req(clustering_results())
    cluster_assignments <- clustering_results()$cluster
    df <- data %>%
      filter(year == max(year)) %>%
      mutate(Cluster = factor(cluster_assignments[as.character(country)])) %>%
      filter(!is.na(Cluster))
    var_names <- c("economy_score", "social_score", "lifeexpectancy_score", 
                   "freedom_score", "generosity_score", "corrperception_score")
    var_labels <- c("Economy", "Social Support", "Life Expectancy", 
                    "Freedom", "Generosity", "Corruption Perception")
    results <- lapply(seq_along(var_names), function(i) {
      formula <- as.formula(paste(var_names[i], "~ Cluster"))
      model <- aov(formula, data = df)
      tidy_result <- broom::tidy(model)
      tidy_result$Variable <- var_labels[i]
      return(tidy_result)
    })
    
    results_df <- do.call(rbind, results) %>%
      dplyr::filter(term == "Cluster") %>%
      dplyr::mutate(
        F = round(statistic, 3),
        `p-value` = signif(p.value, 3),
        Significance = case_when(
          p.value <= 0.001 ~ "***",
          p.value <= 0.01 ~ "**",
          p.value <= 0.05 ~ "*",
          TRUE ~ "ns"
        )
      ) %>%
      dplyr::select(Variable, F, `p-value`, Significance)
    
    tagList(
      div(
        style = "border: 1px solid #cce0f5; 
                         border-radius: 15px; 
                         padding: 15px; 
                         background-color: #e6f0fa; 
                         box-shadow: 1px 1px 8px rgba(0,0,0,0.1); 
                         margin-bottom: 20px;",
        h4("ANOVA Summary", style = "color: #004080;"),
        p("Null Hypothesis: The means of the variable are equal across all clusters."),
        p("Alternative Hypothesis: At least one cluster has a different mean."),
        DT::datatable(results_df,
                      rownames = FALSE,
                      options = list(
                        dom = 't',
                        ordering = FALSE,
                        columnDefs = list(list(className = 'dt-center', targets = 1:3))
                      )
        )
      )
    )
  })

  # --- FIXED GEOSPATIAL BLOCK ---
  observe({
    data_2024 <- world_happy %>%
      filter(year == 2024 & !is.na(ladder_score))
    
    regions <- sort(unique(data_2024$region))
    countries <- data_2024 %>% filter(region == "Asia") %>% pull(name) %>% unique() %>% sort()
    
    updateSelectInput(session, "selected_region", choices = c("All", regions), selected = "Asia")
    updateSelectInput(session, "selected_country", choices = countries, selected = "Afghanistan")
  })
  
  geo_filtered_data <- reactive({
    data <- world_happy %>% filter(year == input$selected_year & !is.na(ladder_score))
    if (input$selected_region != "All") {
      data <- data %>% filter(region == input$selected_region)
    }
    data <- data %>% filter(!st_is_empty(geometry))
    return(data)
  })
  
  observeEvent(input$selected_region, {
    countries <- geo_filtered_data() %>% pull(name) %>% unique() %>% sort()
    updateSelectInput(session, "selected_country", choices = countries, selected = countries[1])
  })
  
  observeEvent(input$selected_country, {
    selected_region <- world_happy %>%
      filter(name == input$selected_country, year == input$selected_year) %>%
      pull(region) %>% unique()
    if (!is.null(selected_region) && length(selected_region) == 1) {
      updateSelectInput(session, "selected_region", selected = selected_region)
    }
  })
  
  output$choropleth_map <- renderTmap({
    tmap_mode("view")
    
    data <- geo_filtered_data()
    
    # Safeguard: ensure we have valid data
    if (is.null(data) || nrow(data) == 0 || all(is.na(data$geometry))) {
      return(tm_shape(world) + tm_borders() + tm_text("name", size = 0.5))
    }
    
    # Try to get selected country geometry
    selected_geom <- data %>% filter(name == input$selected_country)
    
    # Use fallback bbox if country not found
    if (nrow(selected_geom) == 0 || all(is.na(selected_geom$geometry))) {
      bbox_zoom <- st_bbox(data)
    } else {
      bbox_zoom <- st_bbox(selected_geom)
    }
    
    tm_shape(data, bbox = bbox_zoom) +
      tm_polygons(
        col = "ladder_score",
        palette = "YlGnBu",
        id = "name",
        popup.vars = c("Country" = "name", "Happiness" = "ladder_score"),
        title = paste("Happiness Score:", input$selected_year)
      )
  })
  
  
  
  output$prop_map <- renderLeaflet({
    data <- geo_filtered_data()
    if (nrow(data) == 0) return(leaflet() %>% addTiles() %>% addPopups(0, 0, "No valid spatial data"))
    centroids <- st_centroid(data)
    coords <- centroids %>%
      mutate(
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      )
    leaflet(coords) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~ladder_score * 3,
        color = "black",
        fillColor = ~colorNumeric("YlGnBu", domain = coords$ladder_score)(ladder_score),
        fillOpacity = 0.6,
        stroke = TRUE,
        weight = 0.5,
        popup = ~paste0(
          "<b>Country:</b> ", name, "<br/>",
          "<b>Happiness Score:</b> ", round(ladder_score, 2), "<br/>",
          "<b>Economy:</b> ", round(economy_score, 2), "<br/>",
          "<b>Life Expectancy:</b> ", round(lifeexpectancy_score, 2), "<br/>",
          "<b>Freedom:</b> ", round(freedom_score, 2), "<br/>",
          "<b>Region:</b> ", region
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("YlGnBu", domain = coords$ladder_score),
        values = ~ladder_score,
        title = "Happiness Score",
        opacity = 1
      )
  })
  
  observeEvent(input$selected_country, {
    data <- geo_filtered_data()
    if (nrow(data) == 0) return()
    centroids <- st_centroid(data)
    coords <- centroids %>%
      mutate(
        lon = st_coordinates(geometry)[, 1],
        lat = st_coordinates(geometry)[, 2]
      )
    selected_data <- coords %>% filter(name == input$selected_country)
    if (nrow(selected_data) > 0 && !is.na(selected_data$lon) && !is.na(selected_data$lat)) {
      leafletProxy("prop_map") %>%
        setView(lng = selected_data$lon, lat = selected_data$lat, zoom = 5) %>%
        clearPopups() %>%
        addPopups(
          lng = selected_data$lon,
          lat = selected_data$lat,
          popup = paste0(
            "<b>Country:</b> ", selected_data$name, "<br/>",
            "<b>Happiness Score:</b> ", round(selected_data$ladder_score, 2), "<br/>",
            "<b>Economy:</b> ", round(selected_data$economy_score, 2), "<br/>",
            "<b>Life Expectancy:</b> ", round(selected_data$lifeexpectancy_score, 2), "<br/>",
            "<b>Freedom:</b> ", round(selected_data$freedom_score, 2), "<br/>",
            "<b>Region:</b> ", selected_data$region
          )
        )
    }
  })
  
  # --- LISA & Moran's I Functionality ---
  world_data <- reactive({
    data <- happiness_df %>% filter(year == input$selected_year_lisa)
    world %>%
      left_join(data, by = c("name" = "country")) %>%
      filter(!is.na(ladder_score))
  })
  
  local_moran <- reactive({
    data <- world_data()
    nb <- poly2nb(data, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    localMI <- localmoran(data$ladder_score, lw, zero.policy = TRUE)
    
    lagged_score <- lag.listw(lw, data$ladder_score)
    centered_lag <- lagged_score - mean(lagged_score)
    centered_localMI <- localMI[, 1] - mean(localMI[, 1])
    
    quadrant <- vector(mode = "numeric", length = nrow(data))
    quadrant[centered_lag < 0 & centered_localMI > 0] <- 1  # Low-Low
    quadrant[centered_lag > 0 & centered_localMI < 0] <- 2  # Low-High
    quadrant[centered_lag < 0 & centered_localMI < 0] <- 3  # High-Low
    quadrant[centered_lag > 0 & centered_localMI > 0] <- 4  # High-High
    quadrant[localMI[, 5] > 0.05] <- 0                      # Not significant
    
    data$quadrant <- quadrant
    data$cluster_label <- factor(
      quadrant,
      levels = 0:4,
      labels = c("Insignificant", "Low-Low", "Low-High", "High-Low", "High-High")
    )
    
    list(data = data, lw = lw)
  })
  
  output$moran_plot <- renderPlot({
    dat <- local_moran()
    moran.plot(dat$data$ladder_score, dat$lw,
               labels = dat$data$name,
               xlab = paste("Happiness Score (", input$selected_year_lisa, ")", sep = ""),
               ylab = "Spatially Lagged Happiness Score",
               zero.policy = TRUE)
  })
  
  output$lisa_map <- renderTmap({
    tmap_mode("view")
    tm_shape(local_moran()$data) +
      tm_fill(
        col = "cluster_label",
        palette = c(
          "Insignificant" = "#ffffcc",
          "Low-Low" = "blue",
          "Low-High" = "#78c679",
          "High-Low" = "#c2e699",
          "High-High" = "red"
        ),
        title = paste("LISA Cluster (", input$selected_year_lisa, ")", sep = ""),
        style = "cat",
        id = "name",
        popup.vars = c(
          "Country" = "name",
          "Cluster Type" = "cluster_label",
          "Happiness Score" = "ladder_score"
        )
      ) +
      tm_borders(alpha = 0.4) +
      tm_layout(frame = FALSE, legend.outside = TRUE)
  })
  
  output$prop_map_lisa <- renderLeaflet({
    data <- world_data()
    if (nrow(data) == 0) return(leaflet() %>% addTiles())
    
    centroids <- suppressWarnings(st_centroid(data))
    coords <- cbind(data, st_coordinates(centroids)) %>%
      rename(lon = X, lat = Y)
    
    leaflet(coords) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~ladder_score * 3,
        color = "black",
        fillColor = ~colorNumeric("YlGnBu", domain = coords$ladder_score)(ladder_score),
        fillOpacity = 0.6, stroke = TRUE, weight = 0.5,
        popup = ~paste0(
          "<b>Country:</b> ", name, "<br/>",
          "<b>Happiness Score:</b> ", round(ladder_score, 2), "<br/>",
          "<b>Region:</b> ", region
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = colorNumeric("YlGnBu", domain = coords$ladder_score),
        values = ~ladder_score,
        title = "Happiness Score",
        opacity = 1
      )
  })
  
  # --- ASPATIAL TAB ---
  filtered_data <- reactiveVal({
    happiness_df %>%
      filter(year == 2024, region %in% unique(happiness_df$region))
  })  
  observeEvent({input$year; input$region}, {
    new_data <- happiness_df %>%
      filter(year == input$year, region %in% input$region)
    filtered_data(new_data)
  }, ignoreInit = FALSE)
  
  output$map <- renderTmap({
    tmap_mode("view")
    happiness_latest <- filtered_data()
    
    req(nrow(happiness_latest) > 0)
    
    world_happy_latest <- left_join(world, happiness_latest, by = c("name" = "country")) %>%
      filter(!is.na(ladder_score))
    
    req(nrow(world_happy_latest) > 0)
    
    tm_shape(world_happy_latest) +
      tm_polygons(
        col = "ladder_score",
        palette = "YlGnBu",
        id = "name",
        title = paste("Happiness Score (", input$year, ")", sep = "")
      )
  })
  
  output$regionPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = ladder_score, y = region, fill = region)) +
      ggridges::geom_density_ridges(alpha = 0.7, scale = 1.2) +
      theme_minimal() +
      labs(
        title = paste("Distribution by Region (", input$year, ")", sep = ""),
        x = "Happiness Score (Ladder Score)",
        y = "Region"
      )
  })
  
  
  output$summaryTable <- render_gt({
    df <- happiness_df %>% filter(region %in% input$region)
    
    summary_df <- df %>%
      group_by(country) %>%
      summarise(
        MIN = min(ladder_score, na.rm = TRUE),
        MAX = max(ladder_score, na.rm = TRUE),
        AVERAGE = mean(ladder_score, na.rm = TRUE)
      )
    
    latest_scores <- df %>%
      filter(year == max(year)) %>%
      dplyr::select(country, ACTUAL = ladder_score)
    
    combined_df <- summary_df %>%
      left_join(latest_scores, by = "country") %>%
      arrange(country) %>%
      mutate(bullet_chart = ACTUAL) %>%
      dplyr::select(country, MIN, MAX, AVERAGE, ACTUAL, bullet_chart)
    
    combined_df %>%
      gt() %>%
      cols_label(
        country = "COUNTRY",
        MIN = "Min Score\n(2014–24)",
        MAX = "Max Score\n(2014–24)",
        AVERAGE = "Avg Score\n(2014–24)",
        ACTUAL = "2024\nScore",
        bullet_chart = "2024 vs Avg"
      ) %>%
      gt_plt_bullet(column = bullet_chart, target = AVERAGE, palette = c("lightblue", "black")) %>%
      tab_header(title = "Happiness Score Dashboard: 2014–2024")
  })
}


shinyApp(ui, server)
