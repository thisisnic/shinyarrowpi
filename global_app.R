library(shiny)
library(leaflet)
library(DT)

locations = tibble::tribble(
                      ~city,     ~long,     ~lat,
                 "London", 51.5072, -0.1275,
             "Birmingham",   52.48, -1.9025,
             "Manchester", 53.4794, -2.2453,
                  "Leeds", 53.7997, -1.5492,
              "Newcastle", 55.0077, -1.6578,
               "Birstall", 52.6736,   -1.12,
                "Glasgow", 55.8609, -4.2514,
              "Liverpool",    53.4, -2.9833,
             "Portsmouth", 50.8058, -1.0872,
            "Southampton",    50.9,    -1.4,
             "Nottingham",   52.95,   -1.15,
                "Bristol",   51.45, -2.5833,
              "Sheffield", 53.3833, -1.4667,
     "Kingston upon Hull", 53.7444, -0.3325,
              "Leicester", 52.6333, -1.1333,
              "Edinburgh",  55.953,  -3.189,
               "Caerdydd", 51.4833, -3.1833,
         "Stoke-on-Trent",      53, -2.1833,
               "Coventry", 52.4081, -1.5106,
                "Reading", 51.4542, -0.9731,
                "Belfast", 54.5964,   -5.93,
                  "Derby", 52.9167, -1.4667,
               "Plymouth", 50.3714, -4.1422,
          "Wolverhampton", 52.5833, -2.1333,
               "Abertawe", 51.6167,   -3.95,
          "Milton Keynes",   52.04,   -0.76,
               "Aberdeen",   57.15,   -2.11,
                "Norwich",   52.63,   1.297,
                  "Luton", 51.8783, -0.4147,
              "Islington",  51.544, -0.1027,
                "Swindon",   51.56,   -1.78,
                "Croydon", 51.3727, -0.1099,
               "Basildon", 51.5761,  0.4886,
            "Bournemouth",   50.72,   -1.88,
               "Worthing", 50.8147, -0.3714,
                "Ipswich", 52.0594,  1.1556,
          "Middlesbrough", 54.5767, -1.2355,
             "Sunderland", 54.9061, -1.3811,
                 "Ilford", 51.5588,  0.0855,
             "Warrington", 53.3917, -2.5972,
                 "Slough",   51.51,   -0.59,
           "Huddersfield",  53.645, -1.7798,
                 "Oxford", 51.7519, -1.2578,
                   "York", 53.9583, -1.0803,
                  "Poole", 50.7167, -1.9833,
                 "Harrow", 51.5836, -0.3464,
                 "Dundee",  56.462, -2.9707,
           "Saint Albans",  51.755,  -0.336,
                "Telford", 52.6766, -2.4469,
              "Blackpool", 53.8142, -3.0503,
               "Brighton", 50.8429, -0.1313,
                   "Sale",  53.424,  -2.322,
                "Enfield", 51.6522, -0.0808,
              "Tottenham",  51.588,  -0.072,
                 "Bolton",  53.578,  -2.429,
                "Newport", 51.5833,      -3,
           "High Wycombe", 51.6287, -0.7482,
                 "Exeter", 50.7167, -3.5333,
               "Solihull",  52.413,  -1.778,
                "Romford", 51.5768,  0.1801,
                "Preston",  53.759,  -2.699,
              "Gateshead",   54.95,    -1.6,
              "Blackburn",  53.748,  -2.482,
             "Cheltenham",    51.9, -2.0667,
            "Basingstoke", 51.2667, -1.0876,
              "Maidstone",  51.272,   0.529,
             "Colchester", 51.8917,   0.903,
             "Chelmsford", 51.7361,  0.4798,
            "Wythenshawe",  53.392,  -2.264,
              "Doncaster",  53.515,  -1.133,
              "Rotherham",   53.43,  -1.357,
            "Walthamstow",   51.59,       0,
               "Rochdale", 53.6136,  -2.161,
                "Bedford",  52.135,   -0.47,
                "Crawley", 51.1092, -0.1872,
              "Mansfield",   53.15,    -1.2,
               "Dagenham", 51.5397,  0.1422,
              "Stockport", 53.4083, -2.1494,
             "Darlington",  54.527, -1.5526,
                "Fyfield",  51.739,  0.2722,
             "Gillingham",  51.385,    0.55,
                "Salford",  53.483, -2.2931,
             "Eastbourne",   50.77,    0.28,
                  "Wigan", 53.5448, -2.6318,
               "Hounslow", 51.4668,  -0.375,
                "Wembley", 51.5528, -0.2979,
           "Saint Helens", 53.4541, -2.7461,
              "Worcester",  52.192,   -2.22,
              "Wakefield",   53.68,   -1.49,
                "Lincoln", 53.2344, -0.5386,
        "Hemel Hempstead", 51.7526, -0.4692,
                "Watford",  51.655, -0.3957,
                 "Oldham", 53.5444, -2.1169,
       "Sutton Coldfield",  52.563,  -1.822,
              "Kettering", 52.3931, -0.7229,
               "Hastings",   50.85,    0.57,
             "Hartlepool",   54.69,   -1.21,
                   "Hove", 50.8352, -0.1758,
               "Barnsley", 53.5537, -1.4791,
  ) %>%
  mutate(failed_readings = c(0, 0, 0, 928, 0, 338, 35, 0, 965, 0, 519, 27, 0, 5, 868, 37,
0, 32, 0, 57, 0, 0, 6, 45, 3, 1, 17, 16, 0, 4, 27, 8, 23, 14,
0, 80, 6, 72, 0, 0, 4, 343, 51, 0, 22, 44, 0, 0, 45, 7, 42, 0,
0, 65, 261, 0, 35, 21, 0, 32, 46, 0, 0, 19, 504, 0, 0, 0, 0,
0, 46, 26, 10, 0, 722, 0, 0, 40, 31, 0, 0, 0, 0, 16, 14, 0, 25,
1, 65, 11, 29, 473, 43, 0, 0, 0, 0, 0, 0))


ui <- fluidPage(
  numericInput("n_failures", "Min Failures", min = 0, max = 1000, step = 1, value = 0),
  leafletOutput("mymap"),
  dataTableOutput("data_table")
)

server <- function(input, output, session) {

  input_data <- reactive({
    locations %>%
      filter(failed_readings > input$n_failures)
  })

  # TODO: don't draw the whole map every time; just add/remove points
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = as.matrix(input_data()[, c("lat", "long")]))
  })

  output$data_table <- renderDataTable(
    input_data() %>%
      select(city, failed_readings)
  )
}

shinyApp(ui, server)
