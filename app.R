library(shiny)
library(bslib)
library(fpp3)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(here)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Australian Wine — Visualization, Modeling, Forecasts"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("Controls appear per tab. Use the varietal selector and date range on Visualization.")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Visualization",
          value = "viz",
          sidebarLayout(
            sidebarPanel(
              selectizeInput("viz_varietals", "Varietals", choices = NULL, multiple = TRUE),
              dateRangeInput(
                "viz_daterange", "Date range",
                start = as.Date(yearmonth("1980 Jan")),
                end = as.Date(yearmonth("1994 Dec")),
                min = as.Date(yearmonth("1980 Jan")),
                max = as.Date(yearmonth("1994 Dec"))
              ),
              width = 3
            ),
            mainPanel(
              plotOutput("viz_plot", height = "900px")
            )
          )
        ),
        tabPanel(
          "Modeling",
          value = "model",
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput("model_types", "Model types to show", 
                                 choices = c("TSLM", "ETS", "ARIMA"),
                                 selected = c("TSLM", "ETS", "ARIMA")),
              actionButton("fit_models", "Fit models (training set)"),
              checkboxInput("show_training_acc", "Show training accuracy table", TRUE),
              width = 3
            ),
            mainPanel(
              DTOutput("model_specs"),
              conditionalPanel("input.show_training_acc == true", DTOutput("training_acc"))
            )
          )
        ),
        tabPanel(
          "Forecast",
          value = "forecast",
          sidebarLayout(
            sidebarPanel(
              radioButtons("forecast_model", "Model for forecast plot/table",
                           choices = c("TSLM", "ETS", "ARIMA"), selected = "ARIMA"),
              width = 3
            ),
            mainPanel(
              plotOutput("forecast_plot", height = "700px"),
              hr(),
              DTOutput("forecast_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load & prepare data (same transformations as qmd)
  aus_wine <- read_csv(here::here("data/AustralianWines.csv"),
                       col_types = cols(Rose = col_number()), show_col_types = FALSE) |>
    fill(Rose, .direction = "down") |>
    mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())
  
  aus_wine_ts <- aus_wine |>
    pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
    as_tsibble(index = Month, key = Varietal)
  
  # expose choices
  observe({
    vars <- unique(aus_wine_ts$Varietal)
    updateSelectizeInput(session, "viz_varietals", choices = vars, selected = vars)
  })
  
  # training / test splits (fixed as in qmd)
  train <- aus_wine_ts |> filter_index("1980 Jan" ~ "1993 Dec")
  test  <- aus_wine_ts |> filter_index("1994 Jan" ~ "1994 Dec")
  
  # Reactive filtered tsibble for viz based on inputs
  viz_ts <- reactive({
    req(input$viz_varietals, input$viz_daterange)
    start_ym <- yearmonth(input$viz_daterange[1])
    end_ym   <- yearmonth(input$viz_daterange[2])
    aus_wine_ts |>
      filter(Varietal %in% input$viz_varietals, Month >= start_ym, Month <= end_ym)
  })
  
  output$viz_plot <- renderPlot({
    req(viz_ts())
    viz_ts() |>
      autoplot(Sales) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = as.Date(yearmonth("1994 Jan")), color = "red", linetype = "dashed")
  })
  
  # Reactive storage for fitted models (mdl_df)
  fits <- reactiveVal(NULL)
  observeEvent(input$fit_models, {
    # fit on training data but restricted to currently selected varietals in viz control
    sel_vars <- isolate(input$viz_varietals)
    train_sel <- train
    if (!is.null(sel_vars) && length(sel_vars) > 0) {
      train_sel <- train |> filter(Varietal %in% sel_vars)
    }
    # Fit all three models (we will filter which ones to display later)
    fit_res <- train_sel |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    fits(fit_res)
  })
  
  output$model_specs <- renderDT({
    req(fits())
    specs <- fits() |> select(Varietal, contains("(")) |> as_tibble()
    datatable(specs, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$training_acc <- renderDT({
    req(fits())
    acc <- fits() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(RMSE)
    # filter by selected model types
    if (!is.null(input$model_types)) {
      acc <- acc |> filter(.model %in% input$model_types)
    }
    datatable(acc, rownames = FALSE, options = list(pageLength = 12))
  })
  
  # Forecasting (based on last fitted models)
  fc <- reactive({
    req(fits())
    # forecast using the test set for the same varietals that were fit
    fits() |> forecast(test)
  })
  
  output$forecast_plot <- renderPlot({
    req(fc())
    # show forecasts together with recent training history (from 1993 Jan)
    fc() |>
      autoplot(train |> filter(Month >= yearmonth("1993 Jan"))) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(title = "Australian Wine Sales Forecasts by Varietal", y = "Sales", x = "Month") +
      theme_minimal() +
      geom_vline(xintercept = as.Date(yearmonth("1994 Jan")), color = "red", linetype = "dashed")
  })
  
  output$forecast_table <- renderDT({
    req(fc())
    model_choice <- input$forecast_model
    fc_tbl <- fc() |>
      filter(.model == model_choice, year(Month) == 1994) |>
      as_tibble() |>
      select(Varietal, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean)
    datatable(fc_tbl, rownames = FALSE, options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)