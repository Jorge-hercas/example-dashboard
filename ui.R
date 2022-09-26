


bootstrapPage(
  
  absolutePanel(
    top = 10, left = 50, style = "z-index:500; text-align: right;",
    tags$h2("An example of a performance dashboard")
  ),
  theme = theme,
  #useShinyalert(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 100, left = 50, style = "z-index:500; text-align: right;",
    pickerInput("market","Select a Market", choices = unique(orders$Market), multiple = TRUE,
                options = opts, selected = "LATAM"),
    pickerInput("region","Select a Region", choices = unique(orders$Region), multiple = TRUE,
                options = opts),
    pickerInput("country","Select a Country", choices = unique(orders$Country), multiple = TRUE,
                options = opts),
    pickerInput("state","Select a State", choices = unique(orders$State), multiple = TRUE,
                options = opts),
    pickerInput("city","Select a City", choices = unique(orders$City), multiple = TRUE,
                options = opts),
    airDatepickerInput("date","Select a time frame", range = TRUE,
                       value = c(as.Date("2017-01-01" ),as.Date("2017-12-01") )
                       ),
    reactableOutput("tabla")
    
    ),
  absolutePanel(
    top = 100, right = 50, style = "z-index:500; text-align: right;",
    echarts4rOutput("t_series", width = 400, height = 300),
    echarts4rOutput("pie", width = 400, height = 300),
    echarts4rOutput("top", width = 400, height = 300)
  )
  
  
  
)
