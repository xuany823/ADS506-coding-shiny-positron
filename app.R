library(shiny)
library(fpp3)
library(readr)
library(tidyverse)
library(here)
library(DT)

# Data load & prep
aus_wine <- read_csv(
  here("data", "AustralianWines.csv"),
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, "-", "-01-")) |> yearmonth())

aus_wine_ts <- aus_wine |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

# Date limits (as per QMD requirements)
start_fixed <- as.Date("1980-01-01")
max_allowed <- as.Date("1994-12-31")
min_date <- start_fixed
max_date <- as.Date(max(as.Date(aus_wine_ts$Month)))
end_date <- min(max_date, max_allowed)
varietals_all <- sort(unique(aus_wine_ts$Varietal))

# Pre-fit models once at startup
train_cutoff <- yearmonth("1994 Jan")
data_trn <- aus_wine_ts |> filter(Month < train_cutoff)
fit_models <- data_trn |>
  model(
    TSLM = TSLM(Sales ~ trend() + season()),
    ETS = ETS(Sales),
    ARIMA = ARIMA(Sales)
  )
fc_all <- fit_models |> forecast(h = "1 year")

# UI
ui <- fluidPage(
  titlePanel("ADS 506 - Week 5: Australian Wine Sales Analysis"),
  tabsetPanel(
    # Tab 1: Visualization
    tabPanel(
      "Visualization",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("select_all_tab1", "Select all varietals", value = TRUE),
          selectizeInput(
            "varietal_tab1", "Varietal (searchable)",
            choices = varietals_all,
            selected = varietals_all,
            multiple = TRUE,
            options = list(placeholder = "Search varietals...")
          ),
          dateRangeInput(
            "daterange_tab1", "Date range",
            start = start_fixed,
            end = end_date,
            min = start_fixed,
            max = max_allowed,
            format = "yyyy-mm-dd"
          ),
          checkboxInput("show_points_tab1", "Show points", value = FALSE),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(12, plotOutput("ts_plot_tab1"))
          )
        )
      )
    ),
    
    # Tab 2: Model Building
    tabPanel(
      "Model Building",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("show_specs", "Show model specifications", value = FALSE),
          checkboxInput("show_train_acc", "Show training accuracy", value = FALSE),
          checkboxInput("show_forecast_acc", "Show forecasting accuracy", value = TRUE),
          width = 3
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.show_specs",
            h3("Model Specifications"),
            DTOutput("model_specs_dt")
          ),
          conditionalPanel(
            condition = "input.show_train_acc",
            h3("Training Accuracy"),
            DTOutput("train_accuracy_dt")
          ),
          conditionalPanel(
            condition = "input.show_forecast_acc",
            h3("Forecasting Accuracy"),
            DTOutput("forecast_accuracy_dt")
          )
        )
      )
    ),
    
    # Tab 3: Forecast
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "model_type_forecast",
            "Select model for visualization",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = "ARIMA"
          ),
          selectInput(
            "model_type_table",
            "Select model for forecast table",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = "ARIMA"
          ),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(12, 
              h3("Forecast Visualization"),
              plotOutput("forecast_plot", height = "800px")
            )
          ),
          fluidRow(
            column(12,
              h3("Forecast Table (1994)"),
              DTOutput("forecast_table")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1: Select all logic
  observeEvent(input$select_all_tab1, {
    if (isTRUE(input$select_all_tab1)) {
      updateSelectizeInput(session, "varietal_tab1", selected = varietals_all)
    } else {
      updateSelectizeInput(session, "varietal_tab1", selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$varietal_tab1, {
    is_all <- setequal(sort(input$varietal_tab1 %||% character(0)), varietals_all)
    if (!identical(is_all, isTRUE(input$select_all_tab1))) {
      updateCheckboxInput(session, "select_all_tab1", value = is_all)
    }
  }, ignoreInit = TRUE)
  
  # Tab 1: Filtered data
  filtered_ts_tab1 <- reactive({
    req(input$varietal_tab1, input$daterange_tab1)
    aus_wine_ts |>
      filter(
        Varietal %in% input$varietal_tab1,
        as.Date(Month) >= input$daterange_tab1[1],
        as.Date(Month) <= input$daterange_tab1[2]
      )
  })
  
  # Tab 1: Plot
  output$ts_plot_tab1 <- renderPlot({
    df <- filtered_ts_tab1()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = as.Date(Month), y = Sales, colour = Varietal)) +
      geom_line(size = 0.9) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales", colour = "Varietal") +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y Jan",
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_colour_brewer(palette = "Set1") +
      theme_gray(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(size = 12),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
      ) +
      geom_vline(
        xintercept = as.Date("1994-01-01"),
        colour = "red",
        linetype = "dashed",
        size = 0.8
      )
    
    if (isTRUE(input$show_points_tab1)) p <- p + geom_point(size = 1.2)
    p
  }, res = 96, height = function() {
    n <- max(1, length(input$varietal_tab1 %||% varietals_all))
    base <- 280
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(3000, p_h)
  })
  
  # Tab 2: Model specifications
  output$model_specs_dt <- renderDT({
    specs <- fit_models |>
      pivot_longer(cols = c(TSLM, ETS, ARIMA), names_to = ".model", values_to = "fit") |>
      mutate(specification = format(fit)) |>
      select(Varietal, .model, specification)
    
    datatable(specs, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Tab 2: Training accuracy
  output$train_accuracy_dt <- renderDT({
    train_acc <- fit_models |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(train_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 2: Forecast accuracy
  output$forecast_accuracy_dt <- renderDT({
    fc_acc <- fc_all |>
      accuracy(aus_wine_ts) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(fc_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    trn_start <- train_cutoff - 24
    
    fc_filtered <- fc_all |>
      filter(.model == input$model_type_forecast)
    
    fc_filtered |>
      autoplot(aus_wine_ts |> filter(Month >= trn_start)) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(
        title = paste("Australian Wine Sales Forecasts by Varietal -", input$model_type_forecast),
        y = "Sales",
        x = "Month"
      ) +
      theme_minimal()
  })
  
  # Tab 3: Forecast table
  output$forecast_table <- renderDT({
    fc_table <- fc_all |>
      filter(.model == input$model_type_table, year(Month) == 1994) |>
      as_tibble() |>
      select(Varietal, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean)
    
    datatable(fc_table, options = list(scrollX = TRUE)) |>
      formatRound(columns = -1, digits = 0)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}library(shiny)
library(fpp3)
library(readr)
library(tidyverse)
library(here)
library(DT)

# Data load & prep
aus_wine <- read_csv(
  here("data", "AustralianWines.csv"),
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, "-", "-01-")) |> yearmonth())

aus_wine_ts <- aus_wine |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

# Date limits (as per QMD requirements)
start_fixed <- as.Date("1980-01-01")
max_allowed <- as.Date("1994-12-31")
min_date <- start_fixed
max_date <- as.Date(max(as.Date(aus_wine_ts$Month)))
end_date <- min(max_date, max_allowed)
varietals_all <- sort(unique(aus_wine_ts$Varietal))

# Pre-fit models once at startup
train_cutoff <- yearmonth("1994 Jan")
data_trn <- aus_wine_ts |> filter(Month < train_cutoff)
fit_models <- data_trn |>
  model(
    TSLM = TSLM(Sales ~ trend() + season()),
    ETS = ETS(Sales),
    ARIMA = ARIMA(Sales)
  )
fc_all <- fit_models |> forecast(h = "1 year")

# UI
ui <- fluidPage(
  titlePanel("ADS 506 - Week 5: Australian Wine Sales Analysis"),
  tabsetPanel(
    # Tab 1: Visualization
    tabPanel(
      "Visualization",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("select_all_tab1", "Select all varietals", value = TRUE),
          selectizeInput(
            "varietal_tab1", "Varietal (searchable)",
            choices = varietals_all,
            selected = varietals_all,
            multiple = TRUE,
            options = list(placeholder = "Search varietals...")
          ),
          dateRangeInput(
            "daterange_tab1", "Date range",
            start = start_fixed,
            end = end_date,
            min = start_fixed,
            max = max_allowed,
            format = "yyyy-mm-dd"
          ),
          checkboxInput("show_points_tab1", "Show points", value = FALSE),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(12, plotOutput("ts_plot_tab1"))
          )
        )
      )
    ),
    
    # Tab 2: Model Building
    tabPanel(
      "Model Building",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("show_specs", "Show model specifications", value = FALSE),
          checkboxInput("show_train_acc", "Show training accuracy", value = FALSE),
          checkboxInput("show_forecast_acc", "Show forecasting accuracy", value = TRUE),
          width = 3
        ),
        mainPanel(
          conditionalPanel(
            condition = "input.show_specs",
            h3("Model Specifications"),
            DTOutput("model_specs_dt")
          ),
          conditionalPanel(
            condition = "input.show_train_acc",
            h3("Training Accuracy"),
            DTOutput("train_accuracy_dt")
          ),
          conditionalPanel(
            condition = "input.show_forecast_acc",
            h3("Forecasting Accuracy"),
            DTOutput("forecast_accuracy_dt")
          )
        )
      )
    ),
    
    # Tab 3: Forecast
    tabPanel(
      "Forecast",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "model_type_forecast",
            "Select model for visualization",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = "ARIMA"
          ),
          selectInput(
            "model_type_table",
            "Select model for forecast table",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = "ARIMA"
          ),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(12, 
              h3("Forecast Visualization"),
              plotOutput("forecast_plot", height = "800px")
            )
          ),
          fluidRow(
            column(12,
              h3("Forecast Table (1994)"),
              DTOutput("forecast_table")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1: Select all logic
  observeEvent(input$select_all_tab1, {
    if (isTRUE(input$select_all_tab1)) {
      updateSelectizeInput(session, "varietal_tab1", selected = varietals_all)
    } else {
      updateSelectizeInput(session, "varietal_tab1", selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$varietal_tab1, {
    is_all <- setequal(sort(input$varietal_tab1 %||% character(0)), varietals_all)
    if (!identical(is_all, isTRUE(input$select_all_tab1))) {
      updateCheckboxInput(session, "select_all_tab1", value = is_all)
    }
  }, ignoreInit = TRUE)
  
  # Tab 1: Filtered data
  filtered_ts_tab1 <- reactive({
    req(input$varietal_tab1, input$daterange_tab1)
    aus_wine_ts |>
      filter(
        Varietal %in% input$varietal_tab1,
        as.Date(Month) >= input$daterange_tab1[1],
        as.Date(Month) <= input$daterange_tab1[2]
      )
  })
  
  # Tab 1: Plot
  output$ts_plot_tab1 <- renderPlot({
    df <- filtered_ts_tab1()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = as.Date(Month), y = Sales, colour = Varietal)) +
      geom_line(size = 0.9) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales", colour = "Varietal") +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y Jan",
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_colour_brewer(palette = "Set1") +
      theme_gray(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(size = 12),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
      ) +
      geom_vline(
        xintercept = as.Date("1994-01-01"),
        colour = "red",
        linetype = "dashed",
        size = 0.8
      )
    
    if (isTRUE(input$show_points_tab1)) p <- p + geom_point(size = 1.2)
    p
  }, res = 96, height = function() {
    n <- max(1, length(input$varietal_tab1 %||% varietals_all))
    base <- 280
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(3000, p_h)
  })
  
  # Tab 2: Model specifications
  output$model_specs_dt <- renderDT({
    specs <- fit_models |>
      pivot_longer(cols = c(TSLM, ETS, ARIMA), names_to = ".model", values_to = "fit") |>
      mutate(specification = format(fit)) |>
      select(Varietal, .model, specification)
    
    datatable(specs, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Tab 2: Training accuracy
  output$train_accuracy_dt <- renderDT({
    train_acc <- fit_models |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(train_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 2: Forecast accuracy
  output$forecast_accuracy_dt <- renderDT({
    fc_acc <- fc_all |>
      accuracy(aus_wine_ts) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(fc_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    trn_start <- train_cutoff - 24
    
    fc_filtered <- fc_all |>
      filter(.model == input$model_type_forecast)
    
    fc_filtered |>
      autoplot(aus_wine_ts |> filter(Month >= trn_start)) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(
        title = paste("Australian Wine Sales Forecasts by Varietal -", input$model_type_forecast),
        y = "Sales",
        x = "Month"
      ) +
      theme_minimal()
  })
  
  # Tab 3: Forecast table
  output$forecast_table <- renderDT({
    fc_table <- fc_all |>
      filter(.model == input$model_type_table, year(Month) == 1994) |>
      as_tibble() |>
      select(Varietal, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean)
    
    datatable(fc_table, options = list(scrollX = TRUE)) |>
      formatRound(columns = -1, digits = 0)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}