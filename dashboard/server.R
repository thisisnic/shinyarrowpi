library(arrow)
library(shinydashboard)
library(shinydashboardPlus)
library(fontawesome)
library(ggplot2)
library(dplyr)
library(tidyr)


server <- function(input, output, session) {
  enviro_data <- read_feather("../data/testdata.feather")
  output$temp <- renderInfoBox(
    infoBox(
      "Temperature",
      value = "15°C",
      icon = icon("thermometer-2")
    )
  )

  output$humid <- renderInfoBox(
    infoBox(
      "Humidity",
      value = "60%",
      icon = icon("tint")
    )
  )

  output$pressure <- renderInfoBox(
    infoBox(
      "Pressure",
      value = "658.6 hPa",
      icon = fa_i("fas fa-tachometer-alt")
    )
  )

  output$gas_over_time <- renderPlot({
    enviro_long <- enviro_data %>%
      select(datetime, oxidised, reducing, nh3) %>%
      pivot_longer(!datetime, names_to = "measure", values_to = "value")

    ggplot(enviro_long, aes(x = datetime, y = value)) +
      geom_line() +
      facet_grid(rows = vars(measure), scales = "free_y")

  })

}
