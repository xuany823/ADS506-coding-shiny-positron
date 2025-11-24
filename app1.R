library(shiny)
library(fpp3)
library(readr)
library(tidyverse)
library(DT)
library(here)

# data import / tsibble creation (same pipeline used in your qmd)
aus_wine <- read_csv(here::here("data/AustralianWines.csv"),
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, "-", "-01-")) |> yearmonth())

aus_wine_ts <- aus_wine |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

min_date <- as.Date(min(as.Date(aus_wine_ts$Month)))
max_date <- as.Date(max(as.Date(aus_wine_ts$Month)))
varietals <- sort(unique(aus_wine_ts$Varietal))

ui <- fluidPage(
  titlePanel("Australian Wine Sales — by Varietal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("varietal", "Varietal (pick one or more):",
                  choices = varietals, selected = varietals[1], multiple = TRUE),
      dateRangeInput("daterange", "Date range:",
                     start = min_date, end = max_date,
                     min = min_date, max = max_date, format = "yyyy-mm"),
      width = 3
    ),
    mainPanel(
      plotOutput("ts_plot", height = "500px"),
      hr(),
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  filtered_ts <- reactive({
    req(input$varietal)
    aus_wine_ts |>
      filter(
        Varietal %in% input$varietal,
        Month >= tsibble::yearmonth(input$daterange[1]),
        Month <= tsibble::yearmonth(input$daterange[2])
      )
  })

output$ts_plot <- renderPlot({
  df <- filtered_ts()
  req(nrow(df) > 0)
  ggplot(df, aes(x = as.Date(Month), y = Sales, color = Varietal)) +
      geom_line() +
      geom_point(size = 0.9) +
      labs(x = "Month", y = "Sales", color = "Varietal") +
      scale_x_date(date_labels = "%Y-%m", expand = expansion(mult = c(0.01, 0.01))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

  output$data_table <- renderDT({
    filtered_ts() |>
      arrange(Month, Varietal) |>
      mutate(Month = as.Date(Month)) |>
      select(Month, Varietal, Sales)
  }, options = list(pageLength = 10))
}

shinyApp(ui, server)