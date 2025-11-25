library(shiny)
library(fpp3)
library(readr)
library(tidyverse)
library(here)
library(DT)
library(urca)

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

# UI
ui <- fluidPage(
  titlePanel("ADS506-Assignment5.1: Australian Wine Sales Shiny App"),
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
          sliderInput(
            "train_cutoff_year",
            "Training cutoff year",
            min = 1980,
            max = 1995,
            value = 1994,
            step = 1,
            sep = ""
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
    
    # Tab 2: Model Building (side by side)
    tabPanel(
      "Model Building",
      fluidRow(
        # Left column: Model Specifications
        column(
          6,
          wellPanel(
            h3("Model Specifications"),
            checkboxInput("show_specs", "Show model specifications", value = TRUE),
            hr(),
            conditionalPanel(
              condition = "input.show_specs",
              DTOutput("model_specs_dt")
            )
          )
        ),
        
        # Right column: Training Accuracy
        column(
          6,
          wellPanel(
            h3("Training Accuracy"),
            checkboxInput("show_train_acc", "Show training accuracy", value = TRUE),
            hr(),
            conditionalPanel(
              condition = "input.show_train_acc",
              DTOutput("train_accuracy_dt")
            )
          )
        )
      )
    ),
    
    # Tab 3: Forecast (side by side)
    tabPanel(
      "Forecast",
      fluidRow(
        # Left column: Forecast Accuracy
        column(
          6,
          wellPanel(
            h3("Forecasting Accuracy"),
            checkboxInput("show_forecast_acc", "Show forecasting accuracy", value = TRUE),
            checkboxGroupInput(
              "model_type_acc",
              "Select models for accuracy table:",
              choices = c("TSLM", "ETS", "ARIMA"),
              selected = c("TSLM", "ETS", "ARIMA")
            ),
            hr(),
            conditionalPanel(
              condition = "input.show_forecast_acc",
              DTOutput("forecast_accuracy_dt")
            )
          )
        ),
        
        # Right column: Forecast Visualization
        column(
          6,
          wellPanel(
            h3("Forecast Visualization"),
            checkboxGroupInput(
              "model_type_viz",
              "Select models for visualization:",
              choices = c("TSLM", "ETS", "ARIMA"),
              selected = "ARIMA"
            ),
            hr(),
            plotOutput("forecast_plot", height = "600px")
          )
        )
      )
    ),

    tabPanel(
      "About",
      fluidPage(
        h2("About This App"),
        p("The dataset was originally compiled by Rob J Hyndman and colleagues and is commonly used in time series forecasting examples. "),
        p("For this assignment, the data are provided as 'AustralianWines.csv' in the course materials and loaded from the local 'data/' folder."),
        p("This Shiny app was developed for ADS506 Assignment 5.1 to explore, model, and forecast monthly Australian wine sales."),
        p("It includes three main components:"),
        tags$ul(
          tags$li("Visualization of time-series patterns across six major wine varietals"),
          tags$li("Side-by-side comparison of TSLM, ETS, and ARIMA models"),
          tags$li("Forecasting and evaluation of future wine sales")
        ),
        h3("Modeling Choices"),
        p("The training period is controlled by the 'Training cutoff year' slider in the Visualization tab. All three models are re-estimated on the data up to the selected cutoff and then used to generate 12-month-ahead forecasts."),
        p("Model performance is summarised using RMSE, MAE, and MAPE for both the training period and the 1-year forecast horizon. These metrics allow users to compare how well TSLM, ETS, and ARIMA perform for each varietal."),
        h3("How to Use the App"),
        p("1. Use the Visualization tab to filter varietals and date ranges."),
        p("2. Use the Model Building tab to examine model forms and training accuracy."),
        p("3. Use the Forecast tab to compare forecast accuracy and visualize predictions."),
        p("4. by choosing different training cutoff,")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive training cutoff based on slider
  train_cutoff_reactive <- reactive({
    yearmonth(paste(input$train_cutoff_year, "Jan"))
  })
  
  # Reactive training data
  data_trn <- reactive({
    aus_wine_ts |> filter(Month < train_cutoff_reactive())
  })
  
  # Reactive fitted models
  fit_models <- reactive({
    data_trn() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  # Reactive forecasts
  fc_all <- reactive({
    fit_models() |> forecast(h = "1 year")
  })
  
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
    
    # Get cutoff date from slider
    cutoff_date <- as.Date(paste0(input$train_cutoff_year, "-01-01"))
    
    p <- ggplot(df, aes(x = as.Date(Month), y = Sales, colour = Varietal)) +
      geom_line(linewidth = 0.9) +
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
        axis.text.x = element_text(hjust = 1),
        plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
      ) +
      geom_vline(
        xintercept = cutoff_date,
        colour = "red",
        linetype = "dashed",
        linewidth = 0.8
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
    specs <- fit_models() |>
      pivot_longer(cols = c(TSLM, ETS, ARIMA), names_to = ".model", values_to = "fit") |>
      mutate(specification = format(fit)) |>
      as_tibble() |>
      select(Varietal, .model, specification)
    
    datatable(specs, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Tab 2: Training accuracy
  output$train_accuracy_dt <- renderDT({
    train_acc <- fit_models() |>
      accuracy() |>
      as_tibble() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(train_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 3: Forecast accuracy
  output$forecast_accuracy_dt <- renderDT({
    req(input$model_type_acc)
    fc_acc <- fc_all() |>
      filter(.model %in% input$model_type_acc) |>
      accuracy(aus_wine_ts) |>
      as_tibble() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE)
    
    datatable(fc_acc, options = list(pageLength = 10)) |>
      formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 1)
  })
  
  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    req(input$model_type_viz)
    trn_start <- train_cutoff_reactive() - 24
    
    fc_filtered <- fc_all() |>
      filter(.model %in% input$model_type_viz)
    
    fc_filtered |>
      autoplot(aus_wine_ts |> filter(Month >= trn_start)) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(
        title = paste("Forecasts -", paste(input$model_type_viz, collapse = ", ")),
        y = "Sales",
        x = "Month"
      ) +
      theme_minimal()
  })
}


shinyApp(ui = ui, server = server)