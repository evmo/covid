library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(ggrepel)

# Read in state meta info
state_meta <- read_csv("states.csv", col_types = "cdc")

# Read in live COVID-19 Tracking data
api <- "https://covidtracking.com/api/"
states_daily <- read_csv(glue::glue(api, "states/daily.csv")) %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  left_join(state_meta, by = c("state" = "abbrev"))

data <- states_daily %>%
  # Days since 100th case
  filter(positive >= 100) %>%
  group_by(state) %>%
  mutate(date_100th_case = min(date)) %>%
  ungroup %>%
  mutate(
    since_100th_case = as.integer(date - date_100th_case),
    case_rate = positive / (pop / 100000)
  ) %>%
  # Days since 10th death for each state
  filter(death >= 10) %>%
  group_by(state) %>%
  mutate(date_10th_death = min(date)) %>%
  ungroup %>%
  mutate(
    since_10th_death = as.integer(date - date_10th_death),
    death_rate = death / (pop / 100000)
  )

server <- function(input, output, session) {

  rv <- reactiveValues()

  observe({
    rv$x <- ifelse(input$cases_or_deaths == "Cases",
                   "since_100th_case",
                   "since_10th_death")
    rv$xlab <- ifelse(input$cases_or_deaths == "Cases",
                      "Days since 100th case",
                      "Days since 10th death")
    rv$y <- case_when(
      input$cases_or_deaths == "Cases" & input$cum_or_rate == "Cumulative" ~
        "positive",
      input$cases_or_deaths == "Cases" & input$cum_or_rate == "Rate" ~
        "case_rate",
      input$cases_or_deaths == "Deaths" & input$cum_or_rate == "Cumulative" ~
        "death",
      input$cases_or_deaths == "Deaths" & input$cum_or_rate == "Rate" ~
        "death_rate"
    )
    rv$ylab <- ifelse(input$cum_or_rate == "Cumulative",
                      "Total",
                      "Per Capita")
    rv$scale <- ifelse(input$lin_or_log == "Log",
                       scale_y_log10,
                       scale_y_continuous)
  })

  output$plot <- renderPlot({
    req(rv$x, rv$y, rv$xlab, rv$ylab, rv$scale)
    ggplot(data) +
      geom_line(aes_string(x = rv$x, y = rv$y, label = "state", color = "state")) +
      ggrepel::geom_text_repel(
        data = filter(data, date == max(date), rv$x > 1),
        aes_string(x = rv$x, y = rv$y, label = "state", color = "state"),
        nudge_x = 0.5) +
      theme(legend.position = "none") +
      xlab(rv$xlab) +
      ylab(rv$ylab) +
      rv$scale()
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("cases_or_deaths", "", c("Cases", "Deaths")),
      radioButtons("cum_or_rate", "", c("Cumulative", "Rate")),
      radioButtons("lin_or_log", "", c("Log", "Linear"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

shinyApp(ui = ui, server = server)
