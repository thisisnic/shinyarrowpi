library(shiny)
library(leaflet)
library(DT)
library(arrow)

ui <- fluidPage(
  textOutput("mean_temp")
)

server <- function(input, output, session) {

  get_values <- reactive({
    open_dataset("data/generated-parquet/") %>%
      summarise(mean_temp = mean(temperature)) %>%
      collect()
  })

  output$mean_temp <- renderText(
    get_values()%>%
      pull(mean_temp)
  )
}

shinyApp(ui, server)
