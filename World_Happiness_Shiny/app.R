library(shiny)
library(shinydashboard)
library(ggplot2)
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
library(readr)
library(gridExtra)
library(plm)
library(tibble)
library(DT)

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

print(coef_df)


# Prepare geospatial data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_happy <- world %>%
  left_join(happiness_df, by = c("name" = "country"))

ui <- dashboardPage(
  dashboardHeader(title = "Happiness Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-line")),
      menuItem("Panel Model", tabName = "panel_model", icon = icon("chart-line")),
      menuItem("EDA", tabName = "eda", icon = icon("search")),
      menuItem("CDA", tabName = "cda", icon = icon("cogs")),
      menuItem("Geospatial", tabName = "geospatial", icon = icon("globe")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "time_series",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("country", "Search and Select Countries:", 
                                   choices = unique(happiness_df$country), 
                                   multiple = TRUE, 
                                   options = list(maxItems = 5, placeholder = "Select countries")),
                    sliderInput("year_range", "Select Year Range:", 
                                min = 2014, max = 2024, value = c(2014, 2024),
                                step = 1, animate = TRUE)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Trend", plotlyOutput("trend_plot")),
                      tabPanel("Forecast", plotlyOutput("forecast_plot")),
                      tabPanel("Causal Impact", plotOutput("causal_impact_plot"))
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "panel_model",
              fluidPage(
                titlePanel("Panel Data Model Insights"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("country_select", "Select Country:", choices = unique(happiness_df$country)),
                    sliderInput("year_range", "Select Year Range:", 
                                min = min(happiness_df$year), max = max(happiness_df$year), 
                                value = c(min(happiness_df$year), max(happiness_df$year)), step = 1),
                    checkboxGroupInput("factor_select", "Select Happiness Factors:", 
                                       choices = c("ladder_score", "economy_score", "social_score", 
                                                   "lifeexpectancy_score", "freedom_score", 
                                                   "generosity_score", "corrperception_score"),
                                       selected = "ladder_score")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Feature Importance",plotlyOutput("feature_importance_plot")),
                      tabPanel("Predicted vs Actual",plotlyOutput("pred_vs_actual_plot")),
                      tabPanel("Happiness Trend",plotlyOutput("happiness_trend_plot")),
                      tabPanel("Panel Data Table",DTOutput("panel_data_table")),
                      tabPanel("Top Improvement",verbatimTextOutput("top_improvement"))
                    )
                  )
                )
              )),
      
      
      tabItem(tabName = "eda",
              fluidPage(
                titlePanel("Exploratory Data Analysis"),
                plotOutput("eda_plot"),
                dataTableOutput("eda_table")
              )
      ),
      
      tabItem(tabName = "cda",
              fluidPage(
                titlePanel("Causal Data Analysis"),
                tabsetPanel(
                  tabPanel("Correlation Heatmap", plotlyOutput("corr_plot")),
                  tabPanel("Feature Importance", plotlyOutput("feature_importance_plot")),
                  tabPanel("Stationarity Check", plotOutput("stationary_plot"))
                )
              )
      ),
      
      tabItem(tabName = "geospatial",
              fluidPage(
                titlePanel("Geospatial Analysis"),
                tabsetPanel(
                  tabPanel("Choropleth & Proportional Maps",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("selected_year", "Select Year:",
                                           choices = sort(unique(world_happy$year)), selected = 2024),
                               selectInput("selected_region", "Filter by Region:", choices = NULL),
                               selectInput("selected_country", "Search Country:", choices = NULL)
                             ),
                             mainPanel(
                               fluidRow(
                                 column(6, h4("Choropleth Map"), tmapOutput("choropleth_map", height = "500px")),
                                 column(6, h4("Proportional Symbol Map"), leafletOutput("prop_map", height = "500px"))
                               )
                             )
                           )
                  ),
                  tabPanel("LISA & Moran's I", 
                           plotOutput("geo_plot")
                  )
                )
              )
      ),
      
      tabItem(tabName = "about",
              fluidPage(
                titlePanel("About"),
                p("This is a dashboard to analyze happiness data across different countries and years.")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    happiness_df %>%
      filter(country %in% input$country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$trend_plot <- renderPlotly({
    filtered <- filtered_data()
    if (nrow(filtered) > 0) {
      plot_ly(data = filtered, x = ~year, y = ~ladder_score, color = ~country, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Happiness Trend",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Happiness Score"),
               legend = list(title = list(text = "Country")))
    } else {
      plot_ly() %>% add_text(text = "No data available for the selected country/year range")
    }
  })
  
  output$forecast_plot <- renderPlotly({
    filtered <- filtered_data()
    p <- plot_ly()
    for (country_name in unique(filtered$country)) {
      country_data <- filtered %>% filter(country == country_name)
      ts_data <- ts(country_data$ladder_score, start = min(country_data$year), frequency = 1)
      if (length(ts_data) > 5) {
        model <- auto.arima(ts_data)
        forecast_data <- forecast(model, h = 5)
        future_years <- seq(max(country_data$year) + 1, by = 1, length.out = 5)
        p <- p %>%
          add_lines(x = country_data$year, y = ts_data, name = paste(country_name, "- Observed")) %>%
          add_lines(x = future_years, y = forecast_data$mean, name = paste(country_name, "- Forecasted"), line = list(dash = "dash"))
      }
    }
    p %>% layout(title = "Happiness Forecast Comparison", xaxis = list(title = "Year"), yaxis = list(title = "Happiness Score"))
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
  
  observe({
    updateSelectInput(session, "selected_region", choices = c("All", sort(unique(world_happy$region))), selected = "All")
  })
  
  geo_filtered_data <- reactive({
    data <- world_happy %>% filter(year == input$selected_year & !is.na(ladder_score))
    if (input$selected_region != "All") {
      data <- data %>% filter(region == input$selected_region)
    }
    data
  })
  
  observeEvent(input$selected_region, {
    countries <- geo_filtered_data() %>% pull(name) %>% unique() %>% sort()
    updateSelectInput(session, "selected_country", choices = countries, selected = countries[1])
  })
  
  observeEvent(input$selected_country, {
    selected_region <- world_happy %>% filter(name == input$selected_country, year == input$selected_year) %>% pull(region) %>% unique()
    if (!is.null(selected_region)) updateSelectInput(session, "selected_region", selected = selected_region)
  })
  
  output$choropleth_map <- renderTmap({
    tmap_mode("view")
    selected_geom <- geo_filtered_data() %>% filter(name == input$selected_country)
    bbox_zoom <- if (nrow(selected_geom) > 0) st_bbox(selected_geom) else st_bbox(geo_filtered_data())
    tm_shape(geo_filtered_data(), bbox = bbox_zoom) +
      tm_polygons(col = "ladder_score", palette = "YlGnBu", id = "name",
                  popup.vars = c("Country" = "name", "Happiness" = "ladder_score"))
  })
  
  output$prop_map <- renderLeaflet({
    centroids <- st_centroid(geo_filtered_data())
    coords <- centroids %>% mutate(lon = st_coordinates(geometry)[,1], lat = st_coordinates(geometry)[,2])
    leaflet(coords) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = ~ladder_score * 3,
        fillColor = ~colorNumeric("YlGnBu", domain = coords$ladder_score)(ladder_score),
        fillOpacity = 0.6, stroke = TRUE, color = "black", weight = 0.5,
        popup = ~paste0("<b>Country:</b> ", name, "<br/>",
                        "<b>Happiness Score:</b> ", round(ladder_score, 2), "<br/>",
                        "<b>Region:</b> ", region)
      ) %>%
      addLegend("bottomright", pal = colorNumeric("YlGnBu", coords$ladder_score), values = ~ladder_score,
                title = "Happiness Score")
  })
 
  output$feature_importance_plot <- renderPlotly({
    p <- ggplot(coef_df, aes(x = Coefficient, y = reorder(Feature, Coefficient))) +
      geom_col(fill = "steelblue") +
      labs(title = "Feature Importance (Fixed Effects Model)", x = "Coefficient", y = "Feature") +
      theme_minimal()
    
    ggplotly(p)  # Convert ggplot to interactive plot
  })
  
  output$pred_vs_actual_plot <- renderPlotly({
    happiness_df$predicted <- predict(fe_model)  # Ensure predictions are calculated
    
    plot_ly(happiness_df, 
            x = ~ladder_score, 
            y = ~predicted, 
            text = ~paste("Country:", country, "<br>Year:", year), 
            hoverinfo = "text",
            type = "scatter", 
            mode = "markers",
            marker = list(size = 7, color = 'blue', opacity = 0.7)) %>%
      layout(title = "Predicted vs. Actual Happiness Scores",
             xaxis = list(title = "Actual Happiness Score"),
             yaxis = list(title = "Predicted Happiness Score"),
             hovermode = "closest")  # Ensures better hover behavior
  })
  
  
  output$happiness_trend_plot <- renderPlotly({
    df_filtered <- happiness_df %>% filter(country == input$country_select, 
                                 year >= input$year_range[1], year <= input$year_range[2])
    
    plot <- plot_ly(df_filtered, x = ~year)
    
    for (factor in input$factor_select) {
      plot <- plot %>% add_trace(y = df_filtered[[factor]], name = factor, type = "scatter", mode = "lines+markers")
    }
    
    plot %>%
      layout(title = paste("Happiness Trends in", input$country_select),
             xaxis = list(title = "Year"), 
             yaxis = list(title = "Score"),
             legend = list(x = 0, y = 1))
  })
  
  output$panel_data_table <- renderDT({
    happiness_df %>% filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      datatable(happiness_df, 
                          options = list(scrollX = TRUE, autoWidth = TRUE),
                          rownames = FALSE)
  })
  
  
  output$top_improvement <- renderText({
    improvement_df <- happiness_df %>%
      group_by(country) %>%
      summarize(improvement = max(ladder_score) - min(ladder_score), .groups = "drop") %>%
      arrange(desc(improvement))
    
    top_country <- improvement_df$country[1]
    top_change <- round(improvement_df$improvement[1], 2)
    
    paste("The country with the highest happiness improvement from", 
          min(happiness_df$year), "to", max(happiness_df$year), "is", top_country, 
          "with an increase of", top_change, "in Ladder Score.")
  })
  
  
}

shinyApp(ui, server)
