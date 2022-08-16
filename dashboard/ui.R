library(shinydashboard)
library(shinydashboardPlus)

shinydashboardPlus::dashboardPage(
  skin = "midnight",
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    infoBoxOutput("temp"),
    infoBoxOutput("humid"),
    infoBoxOutput("pressure"),
    plotOutput("gas_over_time")
  )
)
