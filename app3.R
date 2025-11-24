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

# UI defaults from data (Month -> Date for inputs)
min_date <- as.Date(min(as.Date(aus_wine_ts$Month)))
max_date <- as.Date(max(as.Date(aus_wine_ts$Month)))

# cap the selectable end date at 1994-12-31
max_allowed <- as.Date("1994-12-31")
end_date <- min(max_date, max_allowed)

varietals <- sort(unique(aus_wine_ts$Varietal))

ui <- fluidPage(
  titlePanel("Australian Wine — Sales by Varietal"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("select_all", "Select all varietals", value = TRUE),
      selectizeInput(
        "varietal", "Varietal (searchable)", choices = varietals,
        selected = varietals, multiple = TRUE,
        options = list(placeholder = "Search varietals...")
      ),
      dateRangeInput(
        "daterange", "Date range",
        start = min_date,
        end = end_date,
        min = min_date,
        max = max_allowed,
        format = "yyyy-mm-dd"
      ),
      checkboxInput("show_points", "Show points", value = FALSE),
      width = 3
    ),
    mainPanel(
      # Put plot in its own full-width row
      fluidRow(
        column(12,
          plotOutput("ts_plot")
        )
      )
      # data table removed
    )
  )
)

server <- function(input, output, session) {
  # select all logic
  observeEvent(input$select_all, {
    if (isTRUE(input$select_all)) {
      updateSelectizeInput(session, "varietal", selected = varietals)
    } else {
      updateSelectizeInput(session, "varietal", selected = character(0))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$varietal, {
    is_all <- setequal(sort(input$varietal %||% character(0)), varietals)
    if (!identical(is_all, isTRUE(input$select_all))) {
      updateCheckboxInput(session, "select_all", value = is_all)
    }
  }, ignoreInit = TRUE)

  filtered_ts <- reactive({
    req(input$varietal, input$daterange)
    aus_wine_ts |>
      filter(
        Varietal %in% input$varietal,
        as.Date(Month) >= input$daterange[1],
        as.Date(Month) <= input$daterange[2]
      )
  })

  output$ts_plot <- renderPlot({
    df <- filtered_ts()
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

    if (isTRUE(input$show_points)) p <- p + geom_point(size = 1.2)
    p
  }, res = 96, height = function() {
    n <- max(1, length(input$varietal %||% varietals))
    base <- 280
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(3000, p_h)
  })
}

if (interactive()) {
  shinyApp(ui, server)
}

  # --- Tab 2 tables --------------------------------------------------------
  # model specifications: pivot longer and create readable spec using report()
  model_specs_df <- reactive({
    fit_models |>
      pivot_longer(cols = -Varietal, names_to = ".model", values_to = "model") |>
      mutate(spec = map_chr(model, ~ paste(capture.output(report(.x)), collapse = " | "))) |>
      select(Varietal, .model, spec)
  })

  output$model_specs_dt <- renderDT({
    dat <- model_specs_df()
    datatable(dat, options = list(pageLength = 10), rownames = FALSE)
  })

  # training accuracy
  train_acc_df <- reactive({
    fit_models |> accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(RMSE)
  })

  output$train_acc_dt <- renderDT({
    datatable(train_acc_df(), options = list(pageLength = 10), rownames = FALSE)
  })

  # forecasting accuracy (validate against full series)
  fc_acc_df <- reactive({
    fc_all |> accuracy(aus_wine_ts) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(RMSE)
  })

  output$fc_acc_dt <- renderDT({
    datatable(fc_acc_df(), options = list(pageLength = 10), rownames = FALSE)
  })

  # --- Tab 3: Forecast plotting & ARIMA table ------------------------------
  output$forecast_plot <- renderPlot({
    req(input$model_select)
    # show recent history from 1992 Jan (train_cutoff - 24 months)
    trn_start <- train_cutoff - 24
    autoplot(
      fc_all |> filter(.model == input$model_select),
      aus_wine_ts |> filter(Month >= trn_start)
    ) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      labs(title = paste("Forecasts by varietal —", input$model_select),
           y = "Sales", x = "Month") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, height = function() {
    n <- max(1, length(input$varietal %||% varietals))
    base <- 800
    extra_per_plot <- 160
    p_h <- base + extra_per_plot * (n - 1)
    min(4000, p_h)
  })

  # ARIMA forecast table for 1994 (wide)
  output$arima_fc_table <- renderDT({
    arima_wide <- fc_all |>
      filter(.model == "ARIMA", year(Month) == 1994) |>
      as_tibble() |>
      select(Varietal, Month, .mean) |>
      mutate(Month = format(as.Date(Month), "%Y-%m")) |>
      pivot_wider(names_from = Month, values_from = .mean)

    datatable(arima_wide, options = list(pageLength = 10), rownames = FALSE)
  })

}

if (interactive()) {
  shinyApp(ui, server)
}