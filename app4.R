# ...existing code...

ui <- fluidPage(
  titlePanel("Australian Wine — Shiny App"),
  tabsetPanel(
    # Tab 1: Visualization ------------------------------------------------
    # ...existing code (unchanged) ...

    # Tab 2: Modeling -----------------------------------------------------
    tabPanel("Modeling",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "models2", "Models to include",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = c("TSLM", "ETS", "ARIMA")
          ),
          # moved ARIMA-table toggle here so Tab3 does not own it
          checkboxInput("show_arima3", "Show ARIMA forecast table (Tab 3)", value = TRUE),
          helpText("Tables show selected models. Training accuracy from training set; forecasting accuracy validated against full series."),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Model specifications", br(), DTOutput("model_specs_dt")),
            tabPanel("Training accuracy", br(), DTOutput("train_acc_dt")),
            tabPanel("Forecasting accuracy", br(), DTOutput("fc_acc_dt"))
          )
        )
      )
    ),

    # Tab 3: Forecast -----------------------------------------------------
    tabPanel("Forecast",
      sidebarLayout(
        sidebarPanel(
          selectInput("model3", "Model for forecast plot", choices = c("TSLM", "ETS", "ARIMA"), selected = "ARIMA"),
          checkboxInput("select_all3", "Select all varietals", value = TRUE),
          selectizeInput("varietal3", "Varietals to plot", choices = varietals_all,
                         selected = varietals_all, multiple = TRUE,
                         options = list(placeholder = "Select varietals...")),
          helpText("ARIMA forecast table shows 1994 forecast (wide)."),
          width = 3
        ),
        mainPanel(
          plotOutput("forecast_plot3", height = "900px"),
          hr(),
          h4("ARIMA forecasts for 1994 (wide)"),
          DTOutput("arima_fc_dt")
        )
      )
    )
  )
)
# ...existing code...