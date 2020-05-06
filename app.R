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
  select(date, state, positive, death, pop) %>%
  mutate(
    has_n_cases = positive >= 200,
    has_n_deaths = death >= 10
  ) %>%
  group_by(state, has_n_cases) %>%
  mutate(date_nth_case = min(date[which(has_n_cases)], na.rm = T)) %>%
  ungroup %>%
  group_by(state, has_n_deaths) %>%
  mutate(date_nth_death = min(date[which(has_n_deaths)], na.rm = T)) %>%
  ungroup %>%
  mutate(
    since_nth_case = as.integer(date - date_nth_case),
    case_rate = positive / (pop / 1000000),
    since_nth_death = as.integer(date - date_nth_death),
    death_rate = death / (pop / 1000000)
  )

server <- function(input, output, session) {

  rv <- reactiveValues()

  observe({
    rv$x <- case_when(
      input$cases_or_deaths == "Cases" & input$date_or_since == "date" ~ "date",
      input$cases_or_deaths == "Deaths" & input$date_or_since == "date" ~ "date",
      input$cases_or_deaths == "Cases" & input$date_or_since == "since" ~ "since_nth_case",
      input$cases_or_deaths == "Deaths" & input$date_or_since == "since" ~ "since_nth_death"
    )

    # rv$x <- ifelse(input$cases_or_deaths == "Cases",
    #                "since_nth_case",
    #                "since_nth_death")

    rv$xlab <- case_when(
      input$cases_or_deaths == "Cases" & input$date_or_since == "date" ~ "Date",
      input$cases_or_deaths == "Deaths" & input$date_or_since == "date" ~ "Date",
      input$cases_or_deaths == "Cases" & input$date_or_since == "since" ~ "Days since 200th case",
      input$cases_or_deaths == "Deaths" & input$date_or_since == "since" ~ "Days since 10th death"
    )

    rv$y <- case_when(
      input$cases_or_deaths == "Cases" & input$total_or_adj == "tot" ~ "positive",
      input$cases_or_deaths == "Cases" & input$total_or_adj == "pop" ~ "case_rate",
      input$cases_or_deaths == "Deaths" & input$total_or_adj == "tot" ~ "death",
      input$cases_or_deaths == "Deaths" & input$total_or_adj == "pop" ~ "death_rate"
    )

    rv$ylab <- ifelse(input$total_or_adj == "tot",
                      "Total",
                      "Per Million")

    rv$scale <- ifelse(input$lin_or_log == "Log",
                       scale_y_log10,
                       scale_y_continuous)
  })

  output$plot <- renderPlot({
    ggplot(data %>% filter(!is.na(rv$y))) +
      geom_line(aes_string(x = rv$x, y = rv$y, color = "state")) +
      ggrepel::geom_text_repel(
        data = filter(data, date == max(date), !!sym(rv$x) > 5),
        aes_string(x = rv$x, y = rv$y, label = "state", color = "state"),
        nudge_x = 0.5) +
      theme(legend.position = "none") +
      xlab(rv$xlab) +
      ylab(rv$ylab) +
      rv$scale()
  }, width = 700, height = 700)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("cases_or_deaths", "", c("Cases", "Deaths")),
      radioButtons("total_or_adj", "", choiceValues = c("tot", "pop"),
                   choiceNames = c("Total", "Population Adjusted")),
      radioButtons("lin_or_log", "", c("Log", "Linear")),
      radioButtons("date_or_since", "", choiceValues = c("date", "since"),
                   choiceNames = c("Date", "Days Since"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

shinyApp(ui = ui, server = server)
