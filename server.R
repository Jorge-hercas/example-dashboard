

function(input, output, session){
  
  region <- reactive({
    unique(orders$Region[orders$Market %in% input$market ])
  })
  
  observe(
    {
      req(region())
      updatePickerInput(session, "region", selected = region())
    }
  )
  country <- reactive({
    unique(orders$Country[orders$Region %in% input$region ])
  })
  
  observe(
    {
      req(country())
      updatePickerInput(session, "country", selected = country())
    }
  )
  state <- reactive({
    unique(orders$State[orders$Country %in% input$country ])
  })
  
  observe(
    {
      req(state())
      updatePickerInput(session, "state", selected = state())
    }
  )
  
  city <- reactive({
    unique(orders$City[orders$State %in% input$state ])
  })
  
  observe(
    {
      req(city())
      updatePickerInput(session, "city", selected = city())
    }
  )
  
  output$map <- renderLeaflet({
    leaflet(orders) |> 
      addProviderTiles(
        providers$CartoDB.DarkMatter
      ) |> 
      setView(lng = -103, lat = 23.6, zoom = 3) |> 
      addCircleMarkers(
        lng = orders$lng,
        lat = orders$lat,
        radius = 1,
        clusterOptions = markerClusterOptions()
      )
  })
  
  bounds <- reactive({
    
    c(min(orders$lng[orders$City %in% input$city],na.rm = TRUE),
      min(orders$lat[orders$City %in% input$city],na.rm = TRUE),
      max(orders$lng[orders$City %in% input$city],na.rm = TRUE)+0.1,
      max(orders$lat[orders$City %in% input$city],na.rm = TRUE)+0.1)
    
  })
  
  
  data <- reactive({
    
    orders |> 
      filter(
        `Order Date` >= input$date[1]
        & `Order Date` <= input$date[2]
        & City %in% input$city
      )
    
  })
  
  
  observe({
    
    req(data())
    
    leafletProxy("map") |> 
      clearMarkers() |> 
      clearMarkerClusters() |> 
      flyToBounds(bounds()[1],
                  bounds()[2],
                  bounds()[3],
                  bounds()[4]
                  ) |> 
      addCircleMarkers(
        data = data(),
        lng = data()$lng,
        lat = data()$lat,
        radius = 1,
        clusterOptions = markerClusterOptions()
      )
    
    
  })
  
  output$t_series <- renderEcharts4r({
    
    orders |> 
      filter(
        `Order Date` >= input$date[1]
        & `Order Date` <= input$date[2]
        & City %in% input$city
      ) |> 
      group_by(`Order Date`) |> 
      summarise(
        total = sum(Quantity, na.rm = TRUE)
      ) |> 
      e_charts(`Order Date`, dispose = FALSE) |> 
      e_line(total, symbol = "none", name = "Total Orders") |> 
      e_tooltip(trigger = "axis") |> 
      e_theme("auritus") |> 
      e_color(color = "#B34646") |> 
      e_legend(top = 0,right = 1, orient = "vertical",
               textStyle = list(fontFamily = "Roboto Condensed", 
                                color = "gray",
                                fontSize = 10)) |> 
      e_title("Trend of the sales", left = "center",
              textStyle = list(fontFamily = "Roboto Condensed", 
                               color = "gray")
              
      )
    
    
  })
  output$pie <- renderEcharts4r({
    
    
    orders |> 
      filter(
        `Order Date` >= input$date[1]
        & `Order Date` <= input$date[2]
        & City %in% input$city
      ) |> 
      group_by(Category) |> 
      summarise(
        total = sum(Quantity, na.rm = TRUE)
      ) |> 
      e_charts(Category, dispose = FALSE) |> 
      e_pie(total, radius = c("50%", "70%"),
            textStyle = list(fontFamily = "Roboto Condensed", 
                             color = "gray",
                             fontSize = 10)
      ) |> 
      e_tooltip(trigger = "item") |> 
      e_theme("auritus") |> 
      e_legend(top = 0,right = 1, orient = "vertical",
               textStyle = list(fontFamily = "Roboto Condensed", 
                                color = "gray",
                                fontSize = 10)) |> 
      e_title("Number of sales by category", left = "center",
              textStyle = list(fontFamily = "Roboto Condensed", 
                               color = "gray")
              
      )
    
    
  })
  
  output$top <- renderEcharts4r({
    
    orders |> 
      filter(
        `Order Date` >= input$date[1]
        & `Order Date` <= input$date[2]
      ) |> 
      group_by(
        Country
      ) |> 
      summarise(
        Top = sum(Quantity, na.rm = TRUE)
      ) |> 
      arrange(desc(Top)) |> 
      head(10) |> 
      e_charts(Country) |> 
      e_bar(Top) |> 
      e_tooltip(trigger = "axis") |> 
      e_theme("auritus") |> 
      e_color(color = "#53AB4D") |> 
      e_legend(top = 0,right = 1, orient = "vertical",
               textStyle = list(fontFamily = "Roboto Condensed", 
                                color = "gray",
                                fontSize = 10)) |> 
      e_title("Top 10 countries by quantity of products", left = "center",
              textStyle = list(fontFamily = "Roboto Condensed", 
                               color = "gray")
              
      )
    
    
  })
  
  
  
  output$tabla <- renderReactable({
    
    orders |> 
      filter(
        `Order Date` >= input$date[1]
        & `Order Date` <= input$date[2]
      ) |> 
      summarise(
        `Total sales` = sum(Sales, na.rm = TRUE),
        `Total orders` = sum(Quantity, na.rm = TRUE),
        `Avg sales` = mean(Sales, na.rm = TRUE),
        `Avg orders` = mean(Quantity, na.rm = TRUE),
        `Total of devolutions` = n_distinct(`Order ID`[Returned == "Yes"])
      ) |> 
      t() |> 
      as.data.frame() |> 
      tibble::rownames_to_column("x") |> 
      setNames(c("Metric", "Value")) |> 
      reactable(
        theme = reactablefmtr::espn(
          background_color = "transparent"
        ),
        columns = list(
          Value = colDef(format = colFormat(separators = TRUE, digits = 0))
        )
      )
    
    
  })
  
  
}


