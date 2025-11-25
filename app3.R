library(shiny)
library(fpp3)
library(readr)
library(tidyverse)
library(here)
library(DT)

# Data load & prep -------------------------------------------------------
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

# Date limits (QMD requirements)
start_fixed <- as.Date("1980-01-01")
max_allowed <- as.Date("1994-12-31")
min_date <- start_fixed
max_date <- as.Date(max(as.Date(aus_wine_ts$Month)))
end_date <- min(max_date, max_allowed)

varietals_all <- sort(unique(aus_wine_ts$Varietal))

# Training split and model fits (fit once on server start)
train_cutoff <- yearmonth("1994 Jan")
data_trn <- aus_wine_ts |> filter(Month < train_cutoff)

fit_models <- data_trn |>
  model(
    TSLM = TSLM(Sales ~ trend() + season()),
    ETS = ETS(Sales),
    ARIMA = ARIMA(Sales)
  )

fc_all <- fit_models |> forecast(h = "1 year")

# UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Australian Wine — Shiny App"),
  tabsetPanel(
    # Tab 1: Visualization ------------------------------------------------
    tabPanel("Visualization",
      sidebarLayout(
        sidebarPanel(
          checkboxInput("select_all1", "Select all varietals", value = TRUE),
          selectizeInput(
            "varietal1", "Varietal (searchable)", choices = varietals_all,
            selected = varietals_all, multiple = TRUE,
            options = list(placeholder = "Search varietals...")
          ),
          dateRangeInput(
            "daterange1", "Date range",
            start = min_date,
            end = end_date,
            min = min_date,
            max = max_allowed,
            format = "yyyy-mm-dd"
          ),
          checkboxInput("show_points1", "Show points", value = FALSE),
          width = 3
        ),
        mainPanel(
          plotOutput("ts_plot1", height = "800px")
        )
      )
    ),

    # Tab 2: Modeling -----------------------------------------------------
    tabPanel("Modeling",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "models2", "Models to include",
            choices = c("TSLM", "ETS", "ARIMA"),
            selected = c("TSLM", "ETS", "ARIMA")
          ),
          helpText("Tables show selected models. Training accuracy from training set; forecasting accuracy validated against full series."),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Model specifications", br(), 
            DTOutput("model_specs_dt")),
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
          helpText("Use the plot to inspect forecasts; ARIMA table removed from this tab."),
          width = 3
        ),
        mainPanel(
          plotOutput("forecast_plot3", height = "900px")
        )
      )
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  # Tab1 select all logic
  observeEvent(input$select_all1, {
    updateSelectizeInput(session, "varietal1",
                         selected = if (isTRUE(input$select_all1)) varietals_all else character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$varietal1, {
    is_all <- setequal(sort(input$varietal1 %||% character(0)), varietals_all)
    if (!identical(is_all, isTRUE(input$select_all1))) {
      updateCheckboxInput(session, "select_all1", value = is_all)
    }
  }, ignoreInit = TRUE)

  filtered_ts1 <- reactive({
    req(input$varietal1, input$daterange1)
    aus_wine_ts |>
      filter(
        Varietal %in% input$varietal1,
        as.Date(Month) >= input$daterange1[1],
        as.Date(Month) <= input$daterange1[2]
      )
  })

  output$ts_plot1 <- renderPlot({
    df <- filtered_ts1()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = as.Date(Month), y = Sales, colour = Varietal)) +
      geom_line(size = 0.9) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(x = "Month", y = "Sales", colour = "Varietal") +
      scale_x_date(date_breaks = "5 years", date_labels = "%Y Jan",
                   expand = expansion(mult = c(0.01, 0.01))) +
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
      geom_vline(xintercept = as.Date("1994-01-01"), colour = "red", linetype = "dashed", size = 0.8)

    if (isTRUE(input$show_points1)) p <- p + geom_point(size = 1.2)
    p
  }, res = 96, height = function() {
    n <- max(1, length(input$varietal1 %||% varietals_all))
    base <- 280
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(3000, p_h)
  })

  # Tab2: Model specifications (wide table like .qmd)
  models_selected <- reactive({
    input$models2 %||% character(0)
  })

  model_specs_wide <- reactive({
    sel <- models_selected()
    # defensive: if no models selected return empty frame
    if (length(sel) == 0) return(tibble(Varietal = character()))

    # convert mdl_df to plain tibble (Varietal + model list-columns)
    base <- as_tibble(fit_models)

    # keep only requested model columns that exist
    model_cols <- intersect(sel, names(base))
    if (length(model_cols) == 0) return(tibble(Varietal = character()))

    # create character spec for each model column safely
    specs <- base |>
      transmute(
        Varietal,
        across(all_of(model_cols), ~
          map_chr(.x, function(m) {
            if (is.null(m) || length(m) == 0) return(NA_character_)
            tryCatch(
              paste(capture.output(report(m)), collapse = " | "),
              error = function(e) NA_character_
            )
          })
        )
      )

    specs
  })
  output$model_specs_dt <- renderDT({
  req(input$models2)

  # Build models
  fit <- data_trn |>
    model(
      TSLM  = TSLM(Sales ~ trend() + season()),
      ETS   = ETS(Sales),
      ARIMA = ARIMA(Sales)
    )

  # Keep only selected models
  fit <- fit[, input$models2]

  # Extract clean model labels
  tbl <- fit |>
    mutate(
      TSLM  = if ("TSLM" %in% input$models2)  map_chr(TSLM,  ~ "TSLM"),
      ETS   = if ("ETS" %in% input$models2)   map_chr(ETS,   ~ format(.x)),
      ARIMA = if ("ARIMA" %in% input$models2) map_chr(ARIMA, ~ format(.x))
    ) |>
    as_tibble() |>
    select(Varietal, all_of(input$models2))

  datatable(tbl, options = list(pageLength = 10))
})
  output$model_specs_txt <- renderPrint({
    dat <- model_specs_wide()
    if (nrow(dat) == 0) {
      cat("No models selected.")
    } else {
      for (i in seq_len(nrow(dat))) {
        cat(paste("Varietal:", dat$Varietal[i]), "\n")
        for (j in seq_along(dat[i, -1])) {
          if (!is.na(dat[i, j + 1])) {
            cat(paste(names(dat)[j + 1], ":", dat[i, j + 1]), "\n")
          }
        }
        cat("\n")
      }
    }
  })

  train_acc_df <- reactive({
    fit_models |> accuracy() |>
      filter(.model %in% models_selected()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(RMSE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~ round(.x, 1)))
  })

  output$train_acc_dt <- renderDT({
    datatable(train_acc_df(), options = list(pageLength = 10), rownames = FALSE)
  })

  fc_acc_df <- reactive({
    fc_all |> accuracy(aus_wine_ts) |>
      filter(.model %in% models_selected()) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(RMSE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~ round(.x, 1)))
  })

  output$fc_acc_dt <- renderDT({
    datatable(fc_acc_df(), options = list(pageLength = 10), rownames = FALSE)
  })

  # Tab3: Forecast plot and ARIMA table
  # select all logic for tab3 varietals
  observeEvent(input$select_all3, {
    updateSelectizeInput(session, "varietal3",
                         selected = if (isTRUE(input$select_all3)) varietals_all else character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$varietal3, {
    is_all <- setequal(sort(input$varietal3 %||% character(0)), varietals_all)
    if (!identical(is_all, isTRUE(input$select_all3))) {
      updateCheckboxInput(session, "select_all3", value = is_all)
    }
  }, ignoreInit = TRUE)

  output$forecast_plot3 <- renderPlot({
    req(input$model3, input$varietal3)
    trn_start <- train_cutoff - 24
    fc_sub <- fc_all |> filter(.model == input$model3, Varietal %in% input$varietal3)
    hist_sub <- aus_wine_ts |> filter(Month >= trn_start, Varietal %in% input$varietal3)

    autoplot(fc_sub, hist_sub) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(title = paste("Forecasts by varietal —", input$model3),
           y = "Sales", x = "Month") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, height = function() {
    n <- max(1, length(input$varietal3 %||% varietals_all))
    base <- 800
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(4000, p_h)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}