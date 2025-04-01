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
library(spdep)
library(shinyWidgets)
library(gt)
library(gtExtras)
library(ggridges)




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
  dashboardHeader(title = "World Happiness Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-area")),
      menuItem("Panel Model", tabName = "panel_model", icon = icon("sliders-h")),
      menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")),
      
      # Collapsible Geospatial Section
      menuItem("Geospatial", icon = icon("globe"), startExpanded = FALSE,
               menuSubItem("Choropleth & Proportional", tabName = "geo_choropleth"),
               menuSubItem("LISA & Moran's I", tabName = "geo_lisa"),
               menuSubItem("Aspatial", tabName = "geo_aspatial")
      ),
      
      # menuItem("Geospatial", tabName = "geospatial", icon = icon("globe")),
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
                    # Country selection remains visible for all tabs
                    selectInput("country_select", "Select Country:", choices = unique(happiness_df$country)),
                    
                    # Show these filters only for main panel analysis
                    conditionalPanel(
                      condition = "input.subtabs != 'What-If Analysis'",
                      sliderInput("year_range", "Select Year Range:", 
                                  min = min(happiness_df$year), max = max(happiness_df$year), 
                                  value = c(min(happiness_df$year), max(happiness_df$year)), step = 1),
                      checkboxGroupInput("factor_select", "Select Happiness Factors:", 
                                         choices = c("ladder_score", "economy_score", "social_score", 
                                                     "lifeexpectancy_score", "freedom_score", 
                                                     "generosity_score", "corrperception_score"),
                                         selected = "ladder_score")
                    ),
                    conditionalPanel(
                      condition = "input.subtabs == 'What-If Analysis'",
                      h4("Predicted Happiness Score:"),
                      textOutput("what_if_prediction")
                    )
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("Feature Importance", plotlyOutput("feature_importance_plot_panel1")),
                                tabPanel("Predicted vs Actual", plotlyOutput("pred_vs_actual_plot")),
                                tabPanel("Happiness Trend", plotlyOutput("happiness_trend_plot")),
                                tabPanel("Panel Data Insights",
                                         tabsetPanel(id = "subtabs",
                                                     tabPanel("Panel Data Table", DTOutput("panel_data_table")),
                                                     tabPanel("Top Improvement", verbatimTextOutput("top_improvement")),
                                                     tabPanel("What-If Analysis",
                                                              h3("What-If Analysis: Adjust Factors"),
                                                              wellPanel(
                                                                sliderInput("economy_adj", "Economy Score:", min = 0, max = 2, value = 1, step = 0.1),
                                                                sliderInput("social_adj", "Social Score:", min = 0, max = 2, value = 1, step = 0.1),
                                                                sliderInput("lifeexp_adj", "Life Expectancy Score:", min = 0, max = 2, value = 1, step = 0.1),
                                                                sliderInput("freedom_adj", "Freedom Score:", min = 0, max = 2, value = 1, step = 0.1),
                                                                sliderInput("generosity_adj", "Generosity Score:", min = 0, max = 2, value = 1, step = 0.1),
                                                                sliderInput("corrperc_adj", "Corruption Perception Score:", min = 0, max = 2, value = 1, step = 0.1)
                                                              )
                                                              
                                                     )
                                         )
                                )
                    )
                  )
                )
              )
      ),
      
      
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
      
      # Choropleth & Proportional
      tabItem(tabName = "geo_choropleth",
              fluidPage(
                titlePanel("Geospatial Analysis: Choropleth & Proportional Maps"),
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
                    )
                  ),
                  mainPanel(
                    width = 9,
                    fluidRow(
                      column(6, tmapOutput("map", height = "400px")),
                      column(6, plotOutput("regionPlot", height = "400px"))
                    ),
                    fluidRow(
                      column(12,
                             div(style = "overflow-x: auto;",
                                 plotOutput("countryPlot", height = "1000px", width = "1500px", inline = TRUE)
                             )
                      )
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
                titlePanel("About"),
                p("This is a dashboard to analyze happiness data across different countries and years.")
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
  
  output$feature_importance_plot_panel1 <- renderPlotly({
    
    coef_df <- as.data.frame(coef(summary(fe_model))) %>%
      rownames_to_column(var = "Feature") %>%
      rename(Coefficient = Estimate)
    
    ggplot(coef_df, aes(x = Coefficient, y = reorder(Feature, Coefficient))) +
      geom_col(fill = "steelblue") +
      labs(title = "Feature Importance (Fixed Effects Model)", x = "Coefficient", y = "Feature") +
      theme_minimal()
  })
  
  output$pred_vs_actual_plot <- renderPlotly({
    happiness_df$predicted <- predict(fe_model)
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
             hovermode = "closest")
  })
  
  output$happiness_trend_plot <- renderPlotly({
    df_filtered <- happiness_df %>% filter(country == input$country_select, 
                                           year >= input$year_range[1], year <= input$year_range[2])
    plot <- plot_ly(df_filtered, x = ~year)
    for (factor in input$factor_select) {
      plot <- plot %>% add_trace(y = df_filtered[[factor]], name = factor, type = "scatter", mode = "lines+markers")
    }
    plot %>% layout(title = paste("Happiness Trends in", input$country_select),
                    xaxis = list(title = "Year"), 
                    yaxis = list(title = "Score"),
                    legend = list(x = 0, y = 1))
  })
  
  output$panel_data_table <- renderDT({
    happiness_df %>% filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      datatable(options = list(scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
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
  
  output$what_if_prediction <- renderText({
    
    # Extract coefficients from the model
    coef_values <- coef(fe_model)
    
    # Compute the new predicted happiness score
    new_ladder_score <- (coef_values["economy_score"] * input$economy_adj) +
      (coef_values["social_score"] * input$social_adj) +
      (coef_values["lifeexpectancy_score"] * input$lifeexp_adj) +
      (coef_values["freedom_score"] * input$freedom_adj) +
      (coef_values["generosity_score"] * input$generosity_adj) +
      (coef_values["corrperception_score"] * input$corrperc_adj)
    
    paste("Predicted Happiness Score based on selected factors:", round(new_ladder_score, 2))
  })
  
  
  # --- FIXED GEOSPATIAL BLOCK ---
  observe({
    regions <- sort(unique(world_happy$region))
    updateSelectInput(session, "selected_region", choices = c("All", regions), selected = "All")
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
    if (nrow(data) == 0) return(tmap::tm_shape(world) + tm_text("No valid data"))
    selected_geom <- data %>% filter(name == input$selected_country)
    bbox_zoom <- if (nrow(selected_geom) > 0) st_bbox(selected_geom) else st_bbox(data)
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
  filtered_data <- reactiveVal()
  
  observeEvent({input$year; input$region}, {
    new_data <- happiness_df %>%
      filter(year == input$year, region %in% input$region)
    filtered_data(new_data)
  }, ignoreInit = TRUE)
  
  output$map <- renderTmap({
    tmap_mode("view")
    happiness_latest <- filtered_data()
    world_happy_latest <- left_join(world, happiness_latest, by = c("name" = "country")) %>%
      filter(!is.na(ladder_score))
    
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
      geom_density_ridges(alpha = 0.7, scale = 1.2) +
      theme_minimal() +
      labs(title = paste("Distribution by Region (", input$year, ")", sep = ""),
           x = "Happiness Score (Ladder Score)",
           y = "Region")
  })
  
  output$countryPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    df$country <- factor(df$country, levels = sort(unique(df$country)))
    
    ggplot(df, aes(x = ladder_score, y = country, fill = country)) +
      geom_density_ridges(alpha = 0.7, scale = 1.2, stat = "binline", bins = 30) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Distribution of Happiness Scores by Country (", input$year, ")", sep = ""),
           x = "Happiness Score (Ladder Score)",
           y = "Country") +
      theme(legend.position = "none",
            plot.margin = unit(c(10, 20, 10, 20), "pt"),
            plot.title = element_text(size = 16, face = "bold"))
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
      select(country, ACTUAL = ladder_score)
    
    combined_df <- summary_df %>%
      left_join(latest_scores, by = "country") %>%
      arrange(country) %>%
      mutate(bullet_chart = ACTUAL) %>%
      select(country, MIN, MAX, AVERAGE, ACTUAL, bullet_chart)
    
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
