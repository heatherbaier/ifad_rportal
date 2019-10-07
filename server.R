shinyServer(function(input, output, session) {


# TabPanel I: Country Context ---------------------------------------------
# Output: Establish Sidebar Project Map (sidebar_map) ---------------------
  output$sidebar_map <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "OSM (default)") %>%
        addTiles('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', group = "Terrain") %>%
        addTiles("https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}{r}.png", group = "Street") %>%
        addLayersControl(
            baseGroups = c("OSM (default)", "Terrain", "Street"),
            options = layersControlOptions(collapsed = TRUE))
  })
  

# Observe: Update Ongoing Project Choices on Sidebar Map ------------------
  observe({
      if (input$country %in% ongoing_countries) {
          ongoing_pattern <- paste("WHERE display_name = '", input$country, "';", sep = '')
          project_choices <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                                 geom = "the_geom",
                                                 clause = (ongoing_pattern))
          updateSelectInput(session, "project", choices = c("All Projects" = "", project_choices$name))
      } else {
          updateSelectInput(session, "project", choices = c("No Ongoing Projects" = ""))
      }
  })
  
  
# Observe: Update Sidebar Project Map when user changes country -----------
  observe({
      country_key <- input$country
      if (input$country %in% ongoing_countries) {
          if (input$project == '') {
              #get ongoing project boundaries from postgres
              ongoing_pattern <- paste("WHERE display_name = '", country_key, "';", sep = '')
              ongoing_projects <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                                      geom = "the_geom",
                                                      clause = (ongoing_pattern))
          } else {
              ongoing_pattern <- paste("WHERE name = '", input$project, "';", sep = '')
              ongoing_projects <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                                        geom = "the_geom",
                                                        clause = (ongoing_pattern))
          }
          #get country boundaries from postgres
          pattern <- paste("WHERE adm0_name = '", country_key, "';", sep = '')
          country_boundaries <- rpostgis::pgGetGeom(con, "ifad_regions",
                                                    geom = "the_geom",
                                                    clause = (pattern))
          #fit map bounds
          map_bounds <- bbox(ongoing_projects)
          
          leafletProxy("sidebar_map", data = ongoing_projects) %>%
              clearShapes() %>%
              fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4], options = list()) %>%
              addPolygons(data = country_boundaries, color = "black", fillOpacity = 0, weight = 4) %>%
              addPolygons(data = ongoing_projects, color = "#003870",
                          weight = 2, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          highlightOptions = highlightOptions(color = "white", weight = 1,
                                                              bringToFront = TRUE),
                          popup = ~paste("<b>Project Name:</b>", ongoing_projects$name, "<br/>",
                                         "<b>Status:</b>", ongoing_projects$status, "<br/>",
                                         "<b>Sector:</b>", ongoing_projects$sector_name, "<br/>",
                                         "<b>Start Date:</b>", ongoing_projects$start_date, "<br/>",
                                         "<b>End Date:</b>", ongoing_projects$end_date, "<br/>",
                                         "<b>Direct Benficiaries:</b>", ongoing_projects$direct_benefiting))
          
        } else {
            #get country boundaries from postgres
            pattern <- paste("WHERE adm0_name = '", country_key, "';", sep = '')
            country_boundaries <- rpostgis::pgGetGeom(con, "ifad_regions",
                                                      geom = "the_geom",
                                                      clause = (pattern))
            #fit map bounds
            map_bounds <- bbox(country_boundaries)
            
            leafletProxy("sidebar_map", data = country_boundaries) %>%
                clearShapes() %>%
                fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4], options = list()) %>%
                addPolygons(data = country_boundaries, color = "black", fillOpacity = 0, weight = 4)
      }
  })
  
  
# Reactive: Selected Country Name (indicator_key) ---------------------------------
  indicator_key <- reactive({
      as.character(input$country)
  }) 


# Reactive: Selected Country Shapefile (shapefile_react) --------------------------
  shapefile_react <- reactive({
      pattern <- paste("WHERE adm0_name = '", input$country, "';", sep = '')
      country_boundaries <- rpostgis::pgGetGeom(con, "ifad_regions",
                                                geom = "the_geom",
                                                clause = (pattern))
  })
  

  output$dem_indicators_chart <- renderHighchart({
    
      #validate to make sure the country is included in the world bank api & raise a warning message if not
      shiny::validate(
        need(input$country %in% no_wb_api == FALSE, paste("No World Bank Demographic Data Available for", indicator_key(), sep = " "))
      )
      
      #find iso2 code in csv
      country_iso2 <- country_csv[country_csv$Country == input$country,]
      
      #make chart
      dem_data <- WDI(indicator = input$dem_indicator, country = country_iso2$Key, start=2000, end=2017)
      dem_data <- dem_data %>% select(3:4)
      
      print(dem_data)
      
      print(class(dem_data))
      
      highchart() %>% 
        hc_chart(type = "line") %>% 
        hc_colors(colors = c("#003870")) %>% 
        hc_xAxis(categories = dem_data$year) %>% 
        hc_add_series(data = dem_data[,1],
                      name = input$dem_indicator) %>% 
        hc_credits(enabled = TRUE, text = "World Bank Development Indicators",
                   href = "http://datatopics.worldbank.org/world-development-indicators/")

  })
  
  
  

  
  
# Output: Agricultural Indicators Chart (ag_indicators_chart) -------------------------
  output$ag_indicators_chart <- renderHighchart({
    
      #validate to make sure the country is included in the world bank api & raise a warning message if not
      shiny::validate(
        need(input$country %in% no_wb_api == FALSE, paste("No World Bank Agriculture Data Available for", indicator_key(), sep = " "))
      )     
      
      #find iso2 code in csv
      country_iso2 <- country_csv[country_csv$Country == input$country,]
      
      #make chart
      ag_data <- WDI(indicator = input$ag_indicator, country = country_iso2$Key, start=2000, end=2017)
      ag_data <- ag_data %>% select(3:4)

      print(ag_data)

      highchart() %>% 
          hc_chart(type = "line") %>% 
          hc_colors(colors = c("#003870")) %>% 
          hc_xAxis(categories = ag_data$year) %>% 
          hc_add_series(data = ag_data[,1],
                        name = input$ag_indicator) %>%
          hc_credits(enabled = TRUE, text = "World Bank Development Indicators",
                     href = "http://datatopics.worldbank.org/world-development-indicators/")
    
  })
  
  
# Output: Poverty Indicators Chart (poverty_indicators_chart) -----------------------------
  output$poverty_indicators_chart <- renderHighchart({
    
      #validate to make sure the country is included in the world bank api & raise a warning message if not
      shiny::validate(
        need(input$country %in% no_wb_api == FALSE, paste("No World Bank Poverty Data Available for", indicator_key(), sep = " "))
      )        
      
      #find iso2 code in csv
      country_iso2 <- country_csv[country_csv$Country == input$country,]
      
      #make chart
      pov_data <- WDI(indicator = input$pov_indicator, country = country_iso2$Key, start=2000, end=2017)
      pov_data <- pov_data %>% select(3:4)
      print(pov_data)
      
      highchart() %>% 
          hc_chart(type = "line") %>% 
          hc_colors(colors = c("#003870")) %>% 
          hc_xAxis(categories = pov_data$year) %>% 
          hc_add_series(data = pov_data[,1],
                        name = input$pov_indicator) %>%
          hc_credits(enabled = TRUE, text = "World Bank Development Indicators",
                     href = "http://datatopics.worldbank.org/world-development-indicators/")
    
  })
  

# Output: NDVI Plot (ndvi_plot) -------------------------------------------------------------
  output$ndvi_plot <- renderHighchart({
    
      #validate that ndvi data is available for chosen country & raise warning message if not
      shiny::validate(
        need(input$country %in% ndvi_csv$adm0_name == TRUE, paste("No NDVI Data Available for", indicator_key(), sep = " "))
      )
      
      #make ndvi chart
      ndvi_data <- ndvi_csv[ndvi_csv$adm0_name == input$country,]
      ndvi_data %>%
        hchart(type = "line", hcaes(x = year, y = mean)) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Mean NDVI"))
    
  })
  
  
# Reactive: NDVI Reative Raster (ndvi_react) -------------------------------------------------
  ndvi_react <- reactive({
      rast <- crop(ndvi_mean_tif, extent(shapefile_react()))
      rast <- mask(rast, shapefile_react())
      return(rast)
  })
  
  
# Output: NDVI Map (ndvi_map) ----------------------------------------------------------------
  output$ndvi_map <- renderLeaflet({
    
      #shiny::validate that the raster overlaps with chosen country & raise warning message if not
      shiny::validate(
        need(input$country %in% not_covered == FALSE, paste("NDVI raster does not overlap with", indicator_key(), sep = " "))
      )
      
      #create raster color palette
      pal <- colorNumeric(palette = "Greens", values(ndvi_react()), na.color = "transparent")
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(ndvi_react(), colors = pal, opacity = 0.7) %>%
        addPolygons(data = shapefile_react(), color = "#003870",
                    weight = 2, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0) %>%
        addLegend(pal = pal, values = values(ndvi_react()),
                  title = "Mean NDVI")
    
  })
  
  
  # Reactive: NDVI Reative Raster (ndvi_react) -------------------------------------------------
  ndvi_cov_country_react <- reactive({
    rast <- crop(ndvi_cov_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: NDVI Map (ndvi_map) ----------------------------------------------------------------
  output$ndvi_cov_country_map <- renderLeaflet({
    
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("NDVI raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #create raster color palette
    pal <- colorNumeric(palette = "Greens", values(ndvi_cov_country_react()), na.color = "transparent")
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(ndvi_cov_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(ndvi_cov_country_react()),
                title = "NDVI CoV",
                labFormat = labelFormat(suffix = " %"))
    
  })
  

  # Reactive: NDVI Reative Raster (ndvi_react) -------------------------------------------------
  ndvi_trend_country_react <- reactive({
    rast <- crop(ndvi_trend_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: NDVI Map (ndvi_map) ----------------------------------------------------------------
  output$ndvi_trend_country_map <- renderLeaflet({
    
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("NDVI raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #create raster color palette
    pal <- colorNumeric(palette = "Greens", values(ndvi_trend_country_react()), na.color = "transparent")
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(ndvi_trend_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(ndvi_trend_country_react()),
                title = "NDVI Trend")
    
  })
  
  
# Output: Rainfall Chart (rainfall_plot) ----------------------------------
  output$rainfall_plot <- renderHighchart({
      #shiny::validate that rainfall data is available for chosen country & raise warning message if not
      shiny::validate(
        need(input$country %in% rainfall_csv$adm0_name == TRUE, paste("No Rainfall Data Available for", indicator_key(), sep = " "))
      )
      
      #make rainfall chart
      rainfall_data <- rainfall_csv[rainfall_csv$adm0_name == input$country,]
      rainfall_data %>%
        hchart(type = "line", hcaes(x = year, y = mean)) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(title = list(text = "Mean Precipitation (MM)"))
  })
  
  
# Reactive: Rainfall Raster (rainfall_react) ------------------------------
  rainfall_react <- reactive({
      rast <- crop(precip_mean_tif, extent(shapefile_react()))
      rast <- mask(rast, shapefile_react())
      return(rast)
  })


# Output: Rainfall Map (precip_map) ---------------------------------------
  output$precip_map <- renderLeaflet({
    
      #shiny::validate that the raster overlaps with chosen country & raise warning message if not
      shiny::validate(
        need(input$country %in% not_covered == FALSE, paste("Rainfall raster does not overlap with", indicator_key(), sep = " "))
      )
      
      pal <- colorNumeric(palette = "Blues", values(rainfall_react()), na.color = "transparent")
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(rainfall_react(), colors = pal, opacity = 0.7) %>%
        addPolygons(data = shapefile_react(), color = "#003870",
                    weight = 2, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0) %>%
        addLegend(pal = pal, values = values(rainfall_react()),
                  title = "Mean Precipitation (mm)",
                  labFormat = labelFormat(suffix = " mm"))
    
  })
  
  # Reactive: Rainfall Raster (rainfall_react) ------------------------------
  rainfall_cov_country_react <- reactive({
    rast <- crop(precip_cov_trend, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Rainfall Map (precip_map) ---------------------------------------
  output$precip_cov_map_country <- renderLeaflet({
    
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Rainfall raster does not overlap with", indicator_key(), sep = " "))
    )
    
    pal <- colorNumeric(palette = "Blues", values(rainfall_cov_country_react()), na.color = "transparent")
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(rainfall_cov_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(rainfall_cov_country_react()),
                title = "Precipitation CoV (%)",
                labFormat = labelFormat(suffix = " %"))
    
  })
  
  # Reactive: Rainfall Raster (rainfall_react) ------------------------------
  rainfall_trend_country_react <- reactive({
    rast <- crop(precip_trend_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Rainfall Map (precip_map) ---------------------------------------
  output$precip_trend_map_country <- renderLeaflet({
    
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Rainfall raster does not overlap with", indicator_key(), sep = " "))
    )
    
    pal <- colorNumeric(palette = "Blues", values(rainfall_trend_country_react()), na.color = "transparent")
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(rainfall_trend_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(rainfall_trend_country_react()),
                title = "Precipitation Trend")
    
  })
  
  
# Output: Minimum Temperature Plot (mintemp_plot) -------------------------
  output$mintemp_plot <- renderHighchart({
      #shiny::validate that minimum temperature data is available for chosen country & raise warning message if not
      shiny::validate(
          need(input$country %in% mintemp_csv$adm0_name == TRUE, paste("No Minimum Temperature Data Available for", indicator_key(), sep = " "))
      )
      
      #make chart
      mintemp_data <- mintemp_csv[mintemp_csv$adm0_name == input$country,]
      mintemp_data %>%
          hchart(type = "line", hcaes(x = year, y = mean_min)) %>%
          hc_xAxis(title = list(text = "Year")) %>%
          hc_yAxis(title = list(text = "Mean Annual Minimum Temperature (°C)"))
  })
  
  
# Reactive: Minimum Temperature Raster (mintemp_react) --------------------
  mintemp_react <- reactive({
      rast <- crop(mintemp_mean_tif, extent(shapefile_react()))
      rast <- mask(rast, shapefile_react())
      return(rast)
  })
  
  
# Output: Minimum Temperature Map (mintemp_map) ---------------------------
  output$mintemp_map <- renderLeaflet({
      #shiny::validate that the raster overlaps with chosen country & raise warning message if not
      shiny::validate(
        need(input$country %in% not_covered == FALSE, paste("Minimum Temperature raster does not overlap with", indicator_key(), sep = " "))
      )
      
      #make raster color palette
      pal <- colorNumeric(palette = "inferno", values(mintemp_react()), na.color = "transparent", reverse = TRUE)
      
      leaflet() %>%
          addTiles() %>%
          addRasterImage(mintemp_react(), colors = pal, opacity = 0.7) %>%
          addPolygons(data = shapefile_react(), color = "#003870",
                      weight = 2, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0) %>%
          addLegend(pal = pal, values = values(mintemp_react()),
                    title = "Mean Min. Temp.",
                    labFormat = labelFormat(suffix = " °C"))
  })
  
  
  # Reactive: Minimum Temperature Raster (mintemp_react) --------------------
  mintemp_std_country_react <- reactive({
    rast <- crop(mintemp_std_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Minimum Temperature Map (mintemp_map) ---------------------------
  output$mintemp_std_map_country <- renderLeaflet({
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Minimum Temperature raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #make raster color palette
    pal <- colorNumeric(palette = "inferno", values(mintemp_std_country_react()), na.color = "transparent", reverse = TRUE)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(mintemp_std_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(mintemp_std_country_react()),
                title = "Min. Temp StD",
                labFormat = labelFormat(suffix = " °C"))
  })
  
  
  # Reactive: Maximum Temperature Raster (maxtemp_react) --------------------
  mintemp_trend_country_react <- reactive({
    rast <- crop(mintemp_trend_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Maximum Temperature Map (maxtemp_map) ---------------------------
  output$mintemp_trend_map_country <- renderLeaflet({
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Trend in Minimum Temperature raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #make raster color palette
    pal <- colorNumeric(palette = "inferno", values(mintemp_trend_country_react()), na.color = "transparent", reverse = TRUE)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(mintemp_trend_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(mintemp_trend_country_react()),
                title = "Min. Temp. Trend",
                labFormat = labelFormat(suffix = " °C"))
    
  })
  

# Output: Maximum Temperature Plot (maxtemp_plot) -------------------------
  output$maxtemp_plot <- renderHighchart({
      #shiny::validate that maximum temperature data is available for chosen country & raise warning message if not
      shiny::validate(
          need(input$country %in% maxtemp_csv$adm0_name == TRUE, paste("No Maximum Temperature Data Available for", indicator_key(), sep = " "))
      )
      
      #make chart
      maxtemp_data <- maxtemp_csv[maxtemp_csv$adm0_name == input$country,]
      maxtemp_data %>%
          hchart(type = "line", hcaes(x = year, y = mean_max)) %>%
          hc_xAxis(title = list(text = "Year")) %>%
          hc_yAxis(title = list(text = "Mean Annual Maximum Temperature (°C)"))
  })
  

# Reactive: Maximum Temperature Raster (maxtemp_react) --------------------
  maxtemp_react <- reactive({
      rast <- crop(maxtemp_mean_tif, extent(shapefile_react()))
      rast <- mask(rast, shapefile_react())
      return(rast)
  })
  
  
# Output: Maximum Temperature Map (maxtemp_map) ---------------------------
  output$maxtemp_map <- renderLeaflet({
      #shiny::validate that the raster overlaps with chosen country & raise warning message if not
      shiny::validate(
          need(input$country %in% not_covered == FALSE, paste("Maximum Temperature raster does not overlap with", indicator_key(), sep = " "))
      )
      
      #make raster color palette
      pal <- colorNumeric(palette = "inferno", values(maxtemp_react()), na.color = "transparent", reverse = TRUE)
      
      leaflet() %>%
          addTiles() %>%
          addRasterImage(maxtemp_react(), colors = pal, opacity = 0.7) %>%
          addPolygons(data = shapefile_react(), color = "#003870",
                      weight = 2, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0) %>%
          addLegend(pal = pal, values = values(maxtemp_react()),
                    title = "Mean Max. Temp.",
                    labFormat = labelFormat(suffix = " °C"))
    
  })
  
  
  # Reactive: Maximum Temperature Raster (maxtemp_react) --------------------
  maxtemp_trend_country_react <- reactive({
    rast <- crop(maxtemp_trend_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Maximum Temperature Map (maxtemp_map) ---------------------------
  output$maxtemp_trend_map_country <- renderLeaflet({
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Trend in Maximum Temperature raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #make raster color palette
    pal <- colorNumeric(palette = "inferno", values(maxtemp_trend_country_react()), na.color = "transparent", reverse = TRUE)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(maxtemp_trend_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(maxtemp_trend_country_react()),
                title = "Max. Temp. Trend",
                labFormat = labelFormat(suffix = " °C"))
    
  })
  
  
  # Reactive: Minimum Temperature Raster (mintemp_react) --------------------
  maxtemp_std_country_react <- reactive({
    rast <- crop(maxtemp_std_tif, extent(shapefile_react()))
    rast <- mask(rast, shapefile_react())
    return(rast)
  })
  
  
  # Output: Minimum Temperature Map (mintemp_map) ---------------------------
  output$maxtemp_std_map_country <- renderLeaflet({
    #shiny::validate that the raster overlaps with chosen country & raise warning message if not
    shiny::validate(
      need(input$country %in% not_covered == FALSE, paste("Minimum Temperature raster does not overlap with", indicator_key(), sep = " "))
    )
    
    #make raster color palette
    pal <- colorNumeric(palette = "inferno", values(maxtemp_std_country_react()), na.color = "transparent", reverse = TRUE)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(maxtemp_std_country_react(), colors = pal, opacity = 0.7) %>%
      addPolygons(data = shapefile_react(), color = "#003870",
                  weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0) %>%
      addLegend(pal = pal, values = values(maxtemp_std_country_react()),
                title = "Max. Temp StD",
                labFormat = labelFormat(suffix = " °C"))
  })
  
  

# Output: Download Country Overview PDF -----------------------------------
  output$downloadReport <- downloadHandler(
      filename = function() {
          paste('my-report.pdf')
      },
      
      content = function(file) {
          src <- normalizePath('report.Rmd')
          
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'report.Rmd', overwrite = TRUE)
          
          out <- render('report.Rmd', pdf_document())
          file.rename(out, file)
      }
  )
  
  
# Output: Download Country Overview PowerPoint ----------------------------
  output$downloadppt <- downloadHandler(
      filename = function() {
          paste('my-presentation.pptx')
      },
      
      content = function(file) {
          src <- normalizePath('presentation.Rmd')
          
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'presentation.Rmd', overwrite = TRUE)
          
          out <- render('presentation.Rmd')
          file.rename(out, file)
      }
  )
  
  
# Output: Download Country Overview Word Document --------------------------
  output$downloadword <- downloadHandler(
      filename = function() {
        paste('my-presentation.docx')
      },
      
      content = function(file) {
        src <- normalizePath('document.Rmd')
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'document.Rmd', overwrite = TRUE)
        
        out <- render('document.Rmd')
        file.rename(out, file)
      }
  )
  
  
# Output: Download Country Climate CSV ------------------------------------
  output$downloadclimatecsv <- downloadHandler(
      filename = function() {
        paste(indicator_key(), '_climate_data', '.csv', sep='')
      },
      content = function(file) {
        
        ndvi_data <- filter(ndvi_csv, adm0_name == input$country & year >= 2001)
        rainfall_data <- filter(rainfall_csv, adm0_name == input$country & year >= 2001)
        mintemp_data <- filter(mintemp_csv, adm0_name == input$country & year >= 2001)
        maxtemp_data <- filter(maxtemp_csv, adm0_name == input$country & year >= 2001)
        
        ndvi_data <- ndvi_data[c("adm0_name", "year", "mean")]
        rainfall_data <- rainfall_data[c("adm0_name", "mean")]
        mintemp_data <- mintemp_data[c("adm0_name", "mean_min")]
        maxtemp_data <- maxtemp_data[c("adm0_name", "mean_max")]
        
        ndvi_data <- setNames(ndvi_data, c("Country", "Year", "Mean_NDVI"))
        
        ndvi_data$Mean_Rainfall <- rainfall_data$mean
        ndvi_data$Mean_Minimum_Temperature <- mintemp_data$mean_min
        ndvi_data$Mean_Maximum_Temperature <- maxtemp_data$mean_max
        
        write.csv(ndvi_data, file)
      }
  )
  

  # Output: Downlaod Error Message (download_error) -------------------------
  output$download_error <- renderText({

      shiny::validate(
          need(input$country %in% mintemp_csv$adm0_name == TRUE, paste("Reports and data cannot be downloaded for", indicator_key(), sep = " "))
      )
      
      shiny::validate(
          need(input$country %in% not_covered == FALSE, paste("Reports and data cannot be downloaded for", indicator_key(), sep = " "))
      )
    
  })
  
  

# TabPanel III: Suitability Modeling ---------------------------------------
  observe({
    
      updateSelectInput(session, 'suitability_country', selected = input$country)
    
  })
  
  r <- reactiveValues(my_color = "green")
  
  # Establish Suitability Map -----------------------------------------------
  output$suitability_map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  
  observe({
    pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
    clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                   geom = "the_geom",
                                   clause = (pattern))
    bounding_box <- bbox(clipped)
    leafletProxy('suitability_map') %>%
      clearShapes() %>%
      fitBounds(bounding_box[1], bounding_box[2], bounding_box[3], bounding_box[4]) %>%
      addPolygons(data = clipped, fillOpacity = 0, color = "black")
  })
  
  
# Update Linear Water Types Selection ---------------------------------------------
  observe({
    
    water_types <- subset(water_linear, adm0_name == input$suitability_country)
    
    water_types <- unique(water_types$waterway)
    
    updateCheckboxGroupInput(session, 'linear_water_choices', choices = water_types)
    
  })
  
  

# Update Climatic Zones Selection ---------------------------------------------
  observe({
    
    pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
    
    clim_zones <- rpostgis::pgGetGeom(con, "koppen_geiger_world_cliamtic_zones_global_ifad_countries",
                                      geom = "the_geom",
                                      clause = (pattern))
    
    clim_zones <- unique(clim_zones$kopp_class)
    
    updateCheckboxGroupInput(session, 'climatic_zone_choices', choices = clim_zones)
    
  })
  
  

# Update EcoRegions Selection ---------------------------------------------
  observe({
    
    pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
    
    ecoregions <- rpostgis::pgGetGeom(con, "wwf_terrestrial_ecoregions_gloabl_ifad_countries",
                                   geom = "the_geom",
                                   clause = (pattern))
    
    ecoregions <- unique(ecoregions$econame)
    
    updateCheckboxGroupInput(session, 'eco_region_choices', choices = ecoregions)
  })
  
  

# Update Country Specifc Data Selection -----------------------------------
  observe({
    
        files <- subset(files, geometry_type == "MULTIPOLYGON")
    
        matched_files <- list()
        
        for (i in files$table_name) {
          
            if (length(grep(input$suitability_country, i, ignore.case = TRUE)) != 0) {
              
                matched_files <- rlist::list.append(matched_files, i)
                
            }
          
        }
        
        print(matched_files)
        
        if (length(matched_files) != 0) {
            
            updateCheckboxGroupInput(session, 'country_spec_data_choices', choices = matched_files)
          
        } else {
          
            updateCheckboxGroupInput(session, 'country_spec_data_choices', choices = c("No Country Specific Data Available in the Geonode"))
          
        }
        
  })
  
  # Add/Remove Water ------------------------------------------------
  observeEvent(input$add_linear_water, {
    
    water <- subset(water_linear, adm0_name == input$suitability_country)
    
    water <- subset(water, waterway %in% input$linear_water_choices)
    
    if (dim(water)[1] != 0) {
      
        print(water)
        
        print("in linear water if statement")
          
        water_polys <- sf::as_Spatial(water)
          
        if (input$linear_water_buffer_size != 0) {
            
            water_polys <- raster::buffer(water_polys, width = input$linear_water_buffer_size)
            
        }
          
        leafletProxy('suitability_map') %>%
            clearGroup("linear_water") %>%
            addPolylines(data = water_polys, 
                         group = 'linear_water', 
                         fillColor = "#0000FF",
                         color = "#0000FF",
                         weight = 1,
                         fillOpacity = .1)
      
    }
    
  })
  
  
  observeEvent(input$remove_linear_water, {
    
    leafletProxy('suitability_map') %>%
      clearGroup("linear_water")
    
  })
  
   
# Observe: Update Polygonal Water Types Selection ---------------------------------------------
  observe({
    
    # water_types <- subset(water_polygonal, adm0_name == input$suitability_country)
    
    
    # 
    
    tryCatch(
      {
        water_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
        
        water_types <- rpostgis::pgGetGeom(con, "africa_water_polygonal",
                                           geom = "the_geom",
                                           clause = (water_pattern))
        
        water_types <- unique(water_types$waterway)
        
        updateCheckboxGroupInput(session, 'polygonal_water_choices', choices = water_types)
        
        message("in water bodies try")
        
      },
      error=function(cond) {
        print('no geometries found')
        return(NA)
      }
    ) 

  })
  
  
  # Add/Remove Water ------------------------------------------------
  observeEvent(input$add_polygonal_water, {
    
    # water <- subset(water_polygonal, adm0_name == input$suitability_country)
    # 
    # water <- subset(water, waterway %in% input$polygonal_water_choices)
    
    tryCatch(
      {
        water_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
        
        water <- rpostgis::pgGetGeom(con, "africa_water_polygonal",
                                     geom = "the_geom",
                                     clause = (water_pattern))
        
        water_polys <- subset(water, waterway %in% input$polygonal_water_choices)
        
        message("in water bodies try")
        
      },
      error=function(cond) {
        
        print('no geometries found')
        
        water <- data.frame()
        
        return(water)
        
      }
    ) 
    
    if (dim(water)[1] != 0) {
      
        # water_polys <- sf::as_Spatial(water)
        
        if (input$polygonal_water_buffer_size != 0) {
          
            water_polys <- raster::buffer(water_polys, width = input$polygonal_water_buffer_size)
          
        }
        
        leafletProxy('suitability_map') %>%
          clearGroup("polygonal_water") %>%
          addPolygons(data = water_polys, 
                      group = 'polygonal_water', 
                      fillColor = "#0000FF",
                      color = "#0000FF",
                      weight = 1,
                      fillOpacity = .1)
    }
    
  })
  
  observeEvent(input$remove_polygonal_water, {
    
    leafletProxy('suitability_map') %>%
      clearGroup("polygonal_water")
    
  })
  
  
# Add/Remove Primary Roads ------------------------------------------------
  observeEvent(input$add_proads, {
    
    tryCatch(
      
      {
        pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
        
        highways <- rpostgis::pgGetGeom(con, "highways_clean",
                                        geom = "the_geom",
                                        clause = (pattern))
        
        message("in highways try")
        
      },
      error=function(cond) {
        
        print('no geometries found')
        
        highways <- data.frame()
        
        return(highways)
        
      }
    ) 
    
    # highways <- subset(highways, adm0_name == input$suitability_country)
    # 
    if (dim(highways)[1] != 0) {
      
      proad_polys <- subset(highways, highway == "primary")

    # proad_polys <- sf::as_Spatial(highways)
      
        if (input$praods_buffer_size != 0) {

            proad_polys <- raster::buffer(proad_polys, width = input$praods_buffer_size)
          
        }
        
        leafletProxy('suitability_map') %>%
          clearGroup("proads") %>%
          addPolygons(data = proad_polys, 
                      group = 'proads',
                      fillColor = "#011936",
                      color = "#011936",
                      popup = "Primary Road")
    
    }
    
  })
  
  observeEvent(input$remove_proads, {
    
    leafletProxy('suitability_map') %>%
      clearGroup("proads")
    
  })
  
  
  
  # Add/Remove Secondary Roads ----------------------------------------------
  observeEvent(input$add_sroads, {
    
    tryCatch(
      
      {
        pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
        
        highways <- rpostgis::pgGetGeom(con, "highways_clean",
                                        geom = "the_geom",
                                        clause = (pattern))
        
        message("in highways try")
        
      },
      error=function(cond) {
        
        print('no geometries found')
        
        highways <- data.frame()
        
        return(highways)
        
      }
    ) 
    
    if (dim(highways)[1] != 0) {
      
        sroad_polys <- subset(highways, highway == "secondary")

        # sroad_polys <- sf::as_Spatial(highways)
        
        if (input$sraods_buffer_size != 0) {
          
            sroad_polys <- raster::buffer(sroad_polys, width = input$sraods_buffer_size)
          
        }
      
      
        leafletProxy('suitability_map') %>%
          clearGroup("sroads") %>%
          addPolygons(data = sroad_polys, 
                      group = 'sroads',
                      fillColor = "#465362",
                      color = "#465362",
                      popup = "Secondary Road")
      
    }
    
  })
  
  observeEvent(input$remove_sroads, {
    leafletProxy('suitability_map') %>%
      clearGroup("sroads")
  })
  
  
  # Add/Remove Tertiary Roads -----------------------------------------------
  observeEvent(input$add_troads, {
    
    tryCatch(
      
      {
        pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
        
        highways <- rpostgis::pgGetGeom(con, "highways_clean",
                                        geom = "the_geom",
                                        clause = (pattern))
        
        message("in highways try")
        
      },
      error=function(cond) {
        
        print('no geometries found')
        
        highways <- data.frame()
        
        return(highways)
        
      }
    ) 
    
    if (dim(highways)[1] != 0) {
      
    troad_polys <- subset(highways, highway == "tertiary")
      
      if (input$traods_buffer_size != 0) {
        
          troad_polys <- raster::buffer(troad_polys, width = input$sraods_buffer_size)
        
      }
    
      leafletProxy('suitability_map') %>%
        addPolygons(data = troad_polys, 
                    group = 'troads',
                    fillColor = "#82a3a1",
                    color = "#82a3a1",
                    popup = "Tertiary Road")
    
    }
    
  })
  
  observeEvent(input$remove_troads, {
    leafletProxy('suitability_map') %>%
      clearGroup("troads")
  })
  
  
  
  observeEvent(input$add_ecoregions, {
    
      print("adding ecoregions")
    
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      
      ecoregions <- rpostgis::pgGetGeom(con, "wwf_terrestrial_ecoregions_gloabl_ifad_countries",
                                        geom = "the_geom",
                                        clause = (pattern))
      
      ecoregions <- subset(ecoregions, econame %in% input$eco_region_choices)
      
      leafletProxy('suitability_map') %>%
        clearGroup('ecoregions') %>%
        addPolygons(data = ecoregions, 
                    group = 'ecoregions',
                    fillColor = "#82a3a1",
                    color = "#82a3a1",
                    popup = paste("Econame: ", ecoregions$econame))
    
  })
  
  
  observeEvent(input$remove_ecoregions, {
    
      print("removing ecoregions")
    
      leafletProxy('suitability_map') %>%
        clearGroup("ecoregions")
      
      
  })
  
  
  
  
  
  observeEvent(input$add_climaticzones, {
    
      print("adding climatic zones")
      
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      
      clim_zones <- rpostgis::pgGetGeom(con, "koppen_geiger_world_cliamtic_zones_global_ifad_countries",
                                        geom = "the_geom",
                                        clause = (pattern))
      
      clim_zones <- subset(clim_zones, kopp_class %in% input$climatic_zone_choices)
      
      leafletProxy('suitability_map') %>%
        clearGroup('climaticzones') %>%
        addPolygons(data = clim_zones, 
                    group = 'climaticzones',
                    fillColor = "#82a3a1",
                    color = "#82a3a1",
                    popup = paste("Climatic Zone: ", clim_zones$kopp_class))
    
  })
  
  
  observeEvent(input$remove_climaticzones, {
    
      print("removing climatic zones")
      
      leafletProxy('suitability_map') %>%
        clearGroup("climaticzones")
    
    
  })
  
  
  observeEvent(input$add_cs, {
    
        print("in observe cs event")
    
        for (i in input$country_spec_data_choices) {
          
              print(i)
              
              selected_file <- subset(files, table_name == i)

              if (selected_file$geometry_type == "MULTIPOLYGON") {

                    polygons <- rpostgis::pgGetGeom(con, i, geom = selected_file$geom_column)
                    
                    print(polygons)
  
                    leafletProxy('suitability_map') %>%
                      addPolygons(data = polygons, group = 'cs_data')

              } else if (selected_file$geometry_type == "POINT" | selected_file$geometry_type == "MULTIPOINT") {

                    points <- rpostgis::pgGetGeom(con, i, geom = selected_file$geom_column)
                    
                    print(points)
                    
                    leafletProxy('suitability_map') %>%
                      addMarkers(data = points, group = 'cs_data')

                }
          
        }
    
  })
  
  
  observeEvent(input$remove_cs, {
    
    leafletProxy('suitability_map') %>%
      clearGroup("cs_data")
    
  })
    
  observe({
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      country <- rpostgis::pgGetGeom(con, "ifad_regions",
                                     geom = "the_geom",
                                     clause = (pattern))
      clipped <- crop(maxtemp_mean_tif, extent(country))
      min <- raster::cellStats(x = clipped, stat = 'min')
      max <- raster::cellStats(x = clipped, stat = 'max')
      updateSliderInput(session, 'choose_maxtemp', min = ceiling(min), max = ceiling(max), value = c(ceiling(min), ceiling(max)))
  })
  
  
  observe({
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      country <- rpostgis::pgGetGeom(con, "ifad_regions",
                                     geom = "the_geom",
                                     clause = (pattern))
      clipped <- crop(mintemp_mean_tif, extent(country))
      min <- raster::cellStats(x = clipped, stat = 'min')
      max <- raster::cellStats(x = clipped, stat = 'max')
      updateSliderInput(session, 'choose_mintemp', min = ceiling(min), max = ceiling(max), value = c(ceiling(min), ceiling(max)))
  })
  
  
  observe({
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      country <- rpostgis::pgGetGeom(con, "ifad_regions",
                                     geom = "the_geom",
                                     clause = (pattern))
      clipped <- crop(ndvi_mean_tif, extent(country))
      min <- raster::cellStats(x = clipped, stat = 'min')
      max <- raster::cellStats(x = clipped, stat = 'max')
      updateSliderInput(session, 'choose_ndvi', min = ceiling(min), max = ceiling(max), value = c(ceiling(min), ceiling(max)))
  })
  
  
  observe({
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      country <- rpostgis::pgGetGeom(con, "ifad_regions",
                                     geom = "the_geom",
                                     clause = (pattern))
      clipped <- crop(precip_mean_tif, extent(country))
      min <- raster::cellStats(x = clipped, stat = 'min')
      max <- raster::cellStats(x = clipped, stat = 'max')
      updateSliderInput(session, 'choose_rainfall', min = ceiling(min), max = ceiling(max), value = c(ceiling(min), ceiling(max)))
  })
    
  
  # Create Climate Raster Outside of Suit Calculation -----------------------------------------------------
  
  raster_choose_react <- reactive({
    
    # There's an error in here somewere where the mintemp raster shows up with values already filtered out
    pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
    
    clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                   geom = "the_geom",
                                   clause = (pattern))
    
    print("conning ndvi")
    
    ndvi_raster_con <- raster_con(input$choose_ndvi[1], input$choose_ndvi[2], ndvi_mean_tif, clipped)
    
    print("conning rainfall")
    
    rainfall_raster_con <- raster_con(input$choose_rainfall[1], input$choose_rainfall[2], precip_mean_tif, clipped, ndvi_raster_con)
    
    print("conning mintemp")
    
    mintemp_raster_con <- raster_con(input$choose_mintemp[1], input$choose_mintemp[2], mintemp_mean_tif, clipped, ndvi_raster_con)
    
    print("conning maxtemp")
    
    maxtemp_raster_con <- raster_con(input$choose_maxtemp[1], input$choose_maxtemp[2], maxtemp_mean_tif, clipped, ndvi_raster_con)
    
    # You might needa come back later and re-con the final added raster so that only pixel values of 4 are shown in the final raster
    # Or add a legend so that ppl know whether the pixel is 1, 2, 3 or 4
    # final_raster <- (rainfall_raster_con + ndvi_raster_con + mintemp_raster_con + maxtemp_raster_con)
    
    final_raster <- raster()
    
    if ("Rainfall" %in% input$suit_clim_vars) {
      
      print("adding rainfall")

      if (hasValues(final_raster) == TRUE) {
        
        final_raster <- final_raster + rainfall_raster_con
        
      } else {
        
        final_raster <- rainfall_raster_con
        
      }
      
    }
    
    if ("NDVI" %in% input$suit_clim_vars) {
      
      print("adding ndvi")

      if (hasValues(final_raster) == TRUE) {
        
        final_raster <- final_raster + ndvi_raster_con
        
      } else {
        
        final_raster <- ndvi_raster_con
        
      }
      
    }
    
    if ("Minimum Temperature" %in% input$suit_clim_vars) {
      
      print("adding mintemp")

      if (hasValues(final_raster) == TRUE) {
        
        final_raster <- final_raster + mintemp_raster_con
        
      } else {
        
        final_raster <- mintemp_raster_con
        
      }
      
    }
    
    if ("Maximum Temperature" %in% input$suit_clim_vars) {
      
      print("adding maxtemp")

      if (hasValues(final_raster) == TRUE) {
        
        final_raster <- final_raster + maxtemp_raster_con
        
      } else {
        
        final_raster <- maxtemp_raster_con
        
      }
      
    }
    
    print("returning rasters")
    
    return(final_raster)
    
  })
  
  
  # Add/Remove Climate Rasters ----------------------------------------------
  observeEvent(input$add_rasters, {
    
    print('mapping raster')
    
    leafletProxy('suitability_map') %>%
      clearImages() %>%
      addRasterImage(raster_choose_react(), group = 'climate_rasters')
    
  })
  
  observeEvent(input$remove_rasters, {
    
    leafletProxy('suitability_map') %>%
      clearGroup("climate_rasters")
    
  })
  

# NEED TO TITLE EVERYTHING ABOVE HERE -------------------------------------

  
  observe({
    
    adm2_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
    
    adm2 <- rpostgis::pgGetGeom(con, "ifad_gaul_level_2_2017",
                                geom = "the_geom",
                                clause = (adm2_pattern))
    
    adm2 <- adm2[!duplicated(adm2$adm2_name), ]
    
    adm2_names <- unique(adm2$adm2_name)
    
    updateSelectInput(session, 'adm2_suit_profile', choices = adm2_names)
    
  })
  

# Reactive: Create Final Suitability Polygons (final_suit_polygons --------
# If you want ott cimplify the code, the porcess of creating final_polygons and the nthe final_raster can be simplfied into two for loops y iterating through a list
# of the slected variables to be included in the model. IMPORTANT: I did not have time to account for a model that acutally produces no suitable polygons. In its 
# current state, the app willc rash if the user has chosen parameters that give no sutibale areas. It shouldn't be difficult to account for this though. 
  final_suit_polygons_react <- reactive({
  
      # Here I establish an empty polygosn frame that I reference back to as i'm creating the suitbaile polgyons. My reaosning for this: because you 
      # can't really predict which variables the user is going ot include in the model (ie. a model with just eco and climatic regions vs. a model with every
      # checkbox clicked) and you cant' create a model for every possible comobination of inputs, I set up the process using checkbox inputs. I check in the 
      # main conditional statements whehter a checkbox is clicked and if it is, I pull the polygons from the potgres.
      final_polygons <- SpatialPolygons(list())
      
      if (input$osm_proads) {
        
          print("adding primary roads")
          
          # Within the tryCatch block I pull the polygons from postgres if the user wants primary roads included in the suitbaility model. It is in a tryCatch
          # block because if the postgres query returns nothing, the whole app will crash. Therefore, I wrapped the query in tryCatch and if it returns nothing 
          # from the database, it will create and return an empty dataframe that then tell the next conditonal not to run and to move out of primary roads (and 
          # in turn not include them in the model).
          tryCatch(
            
            {
              
              phighways_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
              
              phighways <- rpostgis::pgGetGeom(con, "highways_clean",
                                               geom = "the_geom",
                                               clause = (phighways_pattern))
              
              proad_polys <- subset(phighways, highway == "primary")
              
              message("in phighways try")
              
            },
            
            error = function(cond) {
              
              # Here is where I create and return the empty data frame is no polygons are returned for the postgres query.
              print('no geometries found')
              
              phighways <- data.frame()
              
              return(phighways)
              
            }
            
          ) 

          # If the query was successful, I buffer the input (in this case primary roads) if the user wants a buffer. If you think about it logically,
          # the user would in thoery always need to buffer a PolyLines because if they are not buffered, there will be no acutal area that overlaps 
          # with anything to create polgyons. I don't account for that currently in my code becuase I ran out of time, but it's very simple to add 
          # and just needs to be implemented.
          if (dim(phighways)[1] != 0) {
            
              if (input$praods_buffer_size != 0) {
                
                  proad_polys <- raster::buffer(proad_polys, width = input$praods_buffer_size)
                
              }
              
              # Okay so here is pretty key. I use the empty SPatialPolgyons object i established earlier and I check to see if it's still empty. This isn't
              # wuite critical in primary roads because it's the first input I check for, so final_polygons will always be empty at this point, but once we 
              # get to the secondary roads input, if the user did not choose to include primary raods in the model, then final_polygons will still be empty. 
              # If final_polygons is empty, I restablish the final_polygons object to be, in this case, the primary roads polygons (or whatever input we are 
              # currenlty looking at). If final_polygons is not empty, then I simply intersect my current polgyons (again, primary roads here) with the 
              # non-empty final_polygons that currently exist.
              if (length(final_polygons) == 0) {
                
                  final_polygons <- proad_polys
                  
                  print('re-establish final polygons')
                
              } else if (length(final_polygons) != 0) {
                
                  final_polygons <- rgeos::gIntersection(final_polygons, proad_polys)
                
              }
              
          }
        
      }
      
      if (input$osm_sroads) {
        
          print("adding secondary roads")
          
          tryCatch(
            
            {
              shighways_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
              
              shighways <- rpostgis::pgGetGeom(con, "highways_clean",
                                               geom = "the_geom",
                                               clause = (shighways_pattern))
              
              sroad_polys <- subset(shighways, highway == "secondary")
              
              message("in shighways try")
              
            },
            
            error = function(cond) {
              
              print('no geometries found')
              
              shighways <- data.frame()
              
              return(shighways)
              
            }
            
          ) 
          
          if (dim(shighways)[1] != 0) {
            
              if (input$sraods_buffer_size != 0) {
                
                  sroad_polys <- raster::buffer(sroad_polys, width = input$sraods_buffer_size)
                
              }
              
              if (length(final_polygons) == 0) {
                
                  final_polygons <- sroad_polys
                
              # Okay, so heere, if the user has chosen to include both primary and seocndary raods in their model, then the former conditonal block
              # qould have re-established final_polygons as primary roads and here we will intest the scondary roads with primary roads
              } else if (length(final_polygons) != 0) {
                
                  print('unioning')
                  
                  final_polygons <- rgeos::gIntersection(final_polygons, sroad_polys)
                
              }
            
          }
        
      }

      # I  repeat this inetesecting process fo rall of the polygonal/lineal inputs. Hence why a for loop would make sense here, but also the code works 
      # fine as it is now.
      if (input$osm_troads) {
        
          print("adding tertiary roads")
          
          tryCatch(
            
            {
              thighways_pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
              
              thighways <- rpostgis::pgGetGeom(con, "highways_clean",
                                               geom = "the_geom",
                                               clause = (thighways_pattern))
              
              troad_polys <- subset(thighways, highway == "secondary")
              
              message("in shighways try")
              
            },
            
            error = function(cond) {
              
              print('no geometries found')
              
              thighways <- data.frame()
              
              return(thighways)
              
            }
            
          ) 
          
          if (dim(thighways)[1] != 0) {
            
              if (input$traods_buffer_size != 0) {
                
                  troad_polys <- raster::buffer(troad_polys, width = input$sraods_buffer_size)
                
              }
            
              if (length(final_polygons) == 0) {
                
                  final_polygons <- troad_polys
                
              } else if (length(final_polygons) != 0) {
                
                  print('unioning')
                
                  final_polygons <- rgeos::gIntersection(final_polygons, troad_polys)
                
              }
          
          }
        
      }
      
      if (input$polygonal_osm_water) {
        
          print("adding polygonal water")

          tryCatch(
            
            {
              
              water_pattern <- paste("WHERE adm0_name = '", "Lesotho", "';", sep = '')
              
              water <- rpostgis::pgGetGeom(con, "africa_water_polygonal",
                                           geom = "the_geom",
                                           clause = (water_pattern))
              
              pwater_polys <- subset(water, waterway %in% input$polygonal_water_choices)
              
              message("in pwater_polys try")
              
            },
            
            error = function(cond) {
              
              print('no geometries found')
              
              pwater <- data.frame()
              
              return(pwater)
              
            }
            
          ) 
          
          print(pwater)
          
          if (dim(pwater)[1] != 0) {
            
              if (input$polygonal_water_buffer_size != 0) {
                
                  pwater_polys <- raster::buffer(pwater_polys, width = input$polygonal_water_buffer_size)
                
              }
            
              if (length(final_polygons) == 0) {
                
                  final_polygons <- pwater_polys
                
              } else if (length(final_polygons) != 0) {
                
                  print('unioning')
                
                  final_polygons <- rgeos::gIntersection(final_polygons, pwater_polys)
                
              }
            
          }
        
      }
      
      if (input$linear_osm_water) {
        
          # Linear water has not been uploaded to the postgres because when I tried, it said there was a broken pipe or something so I grab this locally still.
          lwater <- subset(water_linear, adm0_name == input$suitability_country)
          
          lwater <- subset(lwater, waterway %in% input$linear_water_choices)
          
          if (dim(lwater)[1] != 0) {
            
              print(water)
              
              print("in linear water if statement")
              
              lwater_polys <- sf::as_Spatial(lwater)
              
              if (input$linear_water_buffer_size != 0) {
                
                  lwater_polys <- raster::buffer(lwater_polys, width = input$linear_water_buffer_size)
                
              }
              
              if (length(final_polygons) == 0) {
                
                  final_polygons <- lwater_polys
                
              } else if (length(final_polygons) != 0) {
                
                  print('unioning')
                
                  final_polygons <- rgeos::gIntersection(final_polygons, lwater_polys)
                
              }
            
          }
        
      }
      
      if (input$eco_regions) {
        
          print("adding ecoregions")
          
          pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
          
          ecoregions <- rpostgis::pgGetGeom(con, "wwf_terrestrial_ecoregions_gloabl_ifad_countries",
                                            geom = "the_geom",
                                            clause = (pattern))
          
          ecoregions <- subset(ecoregions, econame %in% input$eco_region_choices)
          
          if (dim(ecoregions)[1] != 0) {
    
            if (length(final_polygons) == 0) {
              
              print('establishing final polygons as ecoregions')
    
              final_polygons <- ecoregions
              
            } else if (length(final_polygons) != 0) {
              
              print('unioning ecoregions')
              
              final_polygons <- rgeos::gIntersection(final_polygons, ecoregions)
            
          }
          
        }
        
      }

      if (input$climatic_zones) {
        
          print("adding climatic zones")
        
        tryCatch(
          
          {
            
            pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
            
            clim_zones <- rpostgis::pgGetGeom(con, "koppen_geiger_world_cliamtic_zones_global_ifad_countries",
                                              geom = "the_geom",
                                              clause = (pattern))
            
            clim_zones <- subset(clim_zones, kopp_class %in% input$climatic_zone_choices)
            
            message("in pwater_polys try")
            
          },
          
          error = function(cond) {
            
            print('no geometries found')
            
            clim_zones <- data.frame()
            
            return(clim_zones)
            
          }
          
        )
          
          if (dim(clim_zones)[1] != 0) {
              
              if (length(final_polygons) == 0) {
                
                  print('establishing final polygons as climatic zones')
                  
                  final_polygons <- clim_zones
                
              } else if (length(final_polygons) != 0) {
                
                  print('unioning climatic zones')
                  
                  final_polygons <- rgeos::gIntersection(final_polygons, clim_zones)
                
              }
            
          }
        
      }
      
      if (input$country_spec_data) {
        
        print("adding country specific data")
  
        # A big concern many people had with creating the suitbaility tool was the issue of being able to integrate local data, which is fair. 
        # This was a tad tricky to implement concceptually but I have written this so that as long as files are uploaded to the postgres and have 
        # the country's name somewhere in the title, I look  throgh the files list and provide the file names that have the country names in them
        # as options to inlcude in the model. checkBoxGroupInputs return a vector/list/character type object with all of the checked inputs. I use 
        # a for loop to iterate through every object in this list...
        if (length(input$country_spec_data_choices) != 0) {
          
              if (length(final_polygons) != 0) {
            
                    for (i in input$country_spec_data_choices) {
                          
                          print(i)
                          
                          selected_file <- subset(files, table_name == i)
                            
                          # I then grab the geometry from postgres...
                          polygons <- rpostgis::pgGetGeom(con, i, geom = selected_file$geom_column)
                            
                          print(polygons)
                            
                          # I was using Cambodia as an example in my last demo and the flood extent files has a tond of extermely small polygons that due to
                          # there size were causing R much difficulty. To account for this, I simplify the polygons.
                          if (i == "wfp_flood_extent_cambodia") {
                              
                                polygons <- gSimplify(polygons, .4, topologyPreserve=FALSE)
                              
                          }
                            
                          # I don't implemnet this in every tinput type, but I think it shoul dbe an option for everything and I wanted to implement 
                          # it at least once as an exmaple. There may be areas that users actually want to exclude in their suitbaility model, like common 
                          # flood areas in Cambodia. I provide a radioButton input that lets the user choose whetter to include of exclude the input in their model. 
                          # If they include, the process works like normal with a simple internsection with the final_polygons.
                          if (input$include_cs == "Include") {
                              
                                final_polygons <- rgeos::gIntersection(final_polygons, polygons)
                              
                          # If the user chooses to exclude the input, simply clip the shapes out of the final_polygons using gDifference. 
                          } else if (input$include_cs == "Exclude") {
                              
                                final_polygons <- gDifference(final_polygons, polygons)
                              
                          }
                          
                    }
            
                }
      
           }
        
      }
      
      # I follow a pretty simalr workflow with the climate rasters. 
      if (input$climate_overlay) {
          
          print("conning ndvi")
          
          # The big thing here is that when you do your fianl raster calculation at the end, the rasters need to all have the same cell size. I could not find a 
          # reliable function/package to con rasters, so I wote my own which is in the global file. Read that code to see hwo the next few lines work. I  resample 
          # everything to the ndvi raster pixel size
          ndvi_raster_con <- raster_con(input$choose_ndvi[1], input$choose_ndvi[2], ndvi_mean_tif, country_shp_react())
          
          print("conning rainfall")
          
          rainfall_raster_con <- raster_con(input$choose_rainfall[1], input$choose_rainfall[2], precip_mean_tif, country_shp_react(), ndvi_raster_con)
          
          print("conning mintemp")
          
          mintemp_raster_con <- raster_con(input$choose_mintemp[1], input$choose_mintemp[2], mintemp_mean_tif, country_shp_react(), ndvi_raster_con)
          
          print("conning maxtemp")
          
          maxtemp_raster_con <- raster_con(input$choose_maxtemp[1], input$choose_maxtemp[2], maxtemp_mean_tif, country_shp_react(), ndvi_raster_con)
          
          # Like with the final_polygons, establish an empty raster.
          final_raster <- raster()
          
          print("adding rainfall")
          
          if ("Rainfall" %in% input$suit_clim_vars) {
            
              # If the final_raster is not empty, add the current raster to it.
              if (hasValues(final_raster) == TRUE) {
                
                  final_raster <- final_raster + rainfall_raster_con
                
              # If the final_raster is empty, re-establish it to be the curernt raster.
              } else {
                
                  final_raster <- rainfall_raster_con
                
              }
            
          }
          
          print("adding ndvi")
          
          if ("NDVI" %in% input$suit_clim_vars) {
            
              if (hasValues(final_raster) == TRUE) {
                
                  final_raster <- final_raster + ndvi_raster_con
                
              } else {
                
                  final_raster <- ndvi_raster_con
                
              }
            
          }
          
          print("adding mintemp")
          
          if ("Minimum Temperature" %in% input$suit_clim_vars) {
            
              if (hasValues(final_raster) == TRUE) {
                
                  final_raster <- final_raster + mintemp_raster_con
                
              } else {
                
                  final_raster <- mintemp_raster_con
                
              }
            
          }
          
          print("adding maxtemp")
          
          if ("Maximum Temperature" %in% input$suit_clim_vars) {
            
              if (hasValues(final_raster) == TRUE) {
                
                  final_raster <- final_raster + maxtemp_raster_con
                
              } else {
                
                  final_raster <- maxtemp_raster_con
                
              }
            
          }
          
          clipped_raster <- raster::mask(final_raster, final_polygons)
          
          pol <- rasterToPolygons(clipped_raster, fun=function(x){x>0})
          
          final_polygons <- rgeos::gIntersection(final_polygons, pol)
          
      } 
      
      return(final_polygons)
      
  })
  

# Reactive: Suitability Country Shapefile (country_shp_react) -------------
  country_shp_react <- reactive({
      pattern <- paste("WHERE adm0_name = '", input$suitability_country, "';", sep = '')
      clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                     geom = "the_geom",
                                     clause = (pattern))
      return(clipped)
  })


# Reactive: Convert Suitable Polygons Shapefile to Data Frame (suit_polygons_df_react) --------
  suit_polygons_df_react <- reactive({
      shp_dis <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
      shp_dis <- disaggregate(shp_dis)
      shp_dis <- sf::st_as_sf(shp_dis)
      print(shp_dis)
      shp_dis$ID <- NA
      shp_dis$ID <- c(1:dim(shp_dis)[1])
      print(shp_dis)
      return(shp_dis)
  })
  

# Output: Final Suitable Polygons Map (suit_polys_extract_map) ------------
  output$suit_polys_extract_map <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      # The final suitable polygons return as a single polgyons in a the dataframe, regardless of it there are many actually disconnected polgyons. Here I convert
      # the polgyons to the proper data type, then I use dissagreate to convert them into individual polgyons and recreate the dataframe. I do this so that in the
      # analytics below the final polgyons map, a user can choose to get statistics for individual polgyons.  
      shp <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
      shp_dis <- disaggregate(shp)
      new_df <- as.data.frame(1:dim(shp_dis)[1])
      colnames(new_df) <- c("ID")
      print(new_df)
      print(shp_dis)
      shp_dis@data <- new_df
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = shp_dis,
                    popup = ~paste(shp_dis$ID))
    
  })
  

# ObserveEvent: Calculate Suitability Model (calc_suitability) ------------
  observeEvent(input$calc_suitability, {
      r$my_color <- "red"
  })
  

# ObserveEvent: Reset Suitability Model (reset_suitability) ---------------
  observeEvent(input$reset_suitability, {
      r$my_color <- "green"
  })  
  

# Output: Download Suitable Polygons Shapefile (download_suitable_ --------
  output$download_suitable_polygons <- downloadHandler(
    filename = 'shpExport.zip',
    content = function(file) {
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
      # proj4string(value$drawnPoly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      shp_dis <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
      writeOGR(shp_dis, dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
      zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))
      file.copy("shpExport.zip", file)
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
    }
  )
  

# ObserveEvent: Update the list of extracted polygons to choose from (calc_suitability) --------
  observeEvent(input$calc_suitability, {
      shp_dis <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
      shp_dis <- disaggregate(shp_dis)
      len <- dim(shp_dis)[1]
      print(len)
      updateSelectInput(session, 'extracted_polygon', choices = c(1:len))
  })
  

# Reactive: Disagregate & Extract the User Selected Suitable Polygon (extracted_polygons_profile_react) --------
  extracted_polygons_profile_react <- reactive({
      print("subsetting suitable polygons")
      shp_dis <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
      shp_dis <- disaggregate(shp_dis)
      print(shp_dis)
      shp_dis$ID <- NA
      shp_dis$ID <- c(1:dim(shp_dis)[1])
      print(shp_dis)
      shp_dis <- subset(shp_dis, ID == input$extracted_polygon)
      return(shp_dis)
  })
  
  
# Reactive: Stunting Raster Clipped to Suitable Polygons (stunting_rast_polys_react) --------
  stunting_rast_polys_react <- reactive({
      if (input$stunting_calc_choice_polys == "Select one year"){
          base <- "./gis_files/stunting/IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_"
          base2 <- "_PREVALENCE_Y2018M02D28.TIF"
          pattern <- paste(base, input$stunting_year_polys, base2, sep = '')
          print(pattern)
          stunting_rast <- raster(pattern)
          stunting_rast <- crop(stunting_rast, extent(extracted_polygons_profile_react()))
      } else if (input$stunting_calc_choice_polys == "Calculate change over time") {
          base <- "./gis_files/stunting/IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_"
          base2 <- "_PREVALENCE_Y2018M02D28.TIF"
          pattern <- paste(base, input$stunting_change_year1_polys, base2, sep = '')
          print(pattern)
          pattern2 <- paste(base, input$stunting_change_year2_polys, base2, sep = '')
          print(pattern2)
          stunting_rast1 <- raster(pattern)
          stunting_rast2 <- raster(pattern2)
          stunting_rast1 <- crop(stunting_rast1, extent(extracted_polygons_profile_react()))
          stunting_rast2 <- crop(stunting_rast2, extent(extracted_polygons_profile_react()))
          stunting_rast <- (stunting_rast2 - stunting_rast1)
      }
      stunting_rast
  })
  

# Output: Stunting Map (stunting_map_polys) -------------------------------
  output$stunting_map_polys <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
        
      leaflet() %>%
          addTiles() %>%
          addRasterImage(stunting_rast_polys_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), fillOpacity = 0, color = "black")
  })
  

# Output: Stunting Avg. Text (stunting_avg_oneyear_polys) -----------------
  output$stunting_avg_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
        
      avg <- raster::cellStats(stunting_rast_polys_react(), "mean")
      paste("The average stunting in the selected year was ", avg, ".", sep = '')
  })

  
# Output: Stunting Sum Text (stunting_sum_oneyear_polys) ------------------
  output$stunting_sum_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
        
      sum <- raster::cellStats(stunting_rast_polys_react(), "sum")
      paste("The total stunting in the selected year was ", sum, ".", sep = '')
  })  
  

# Output: Stunting Avg. Change Text (stunting_avg_change_polys) -----------
  output$stunting_avg_change_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
    
      base <- "./gis_files/stunting/IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_"
      base2 <- "_PREVALENCE_Y2018M02D28.TIF"
      pattern <- paste(base, input$stunting_change_year1_polys, base2, sep = '')
      print(pattern)
      pattern2 <- paste(base, input$stunting_change_year2_polys, base2, sep = '')
      print(pattern2)
      stunting_rast1 <- raster(pattern)
      stunting_rast2 <- raster(pattern2)
      stunting_rast1 <- crop(stunting_rast1, extent(extracted_polygons_profile_react()))
      sum_year1 <- raster::cellStats(stunting_rast1, 'sum')
      stunting_rast2 <- crop(stunting_rast2, extent(extracted_polygons_profile_react()))
      sum_year2 <- raster::cellStats(stunting_rast2, 'sum')
      diff <- (sum_year2 - sum_year1)
      up_down <- ifelse(diff < 0, 'decreased', 'increased')
      paste("The total change in stunting between the selected years is ", diff, ". \n", "Stunting has ", up_down, '.', sep = '')
  })    


# Output: Child 5 Raster Clipped to Suitable Polygons (child5_rast_polys_react) --------
  child5_rast_polys_react <- reactive({
      if (input$child5_calc_choice_polys == "Select one year"){
          base <- "./gis_files/child5/IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_"
          base2 <- "_Y2017M09D25.TIF"
          pattern <- paste(base, input$child5_year_polys, base2, sep = '')
          print(pattern)
          rast <- raster(pattern)
          rast <- crop(rast, extent(extracted_polygons_profile_react()))
      } else if (input$child5_calc_choice_polys == "Calculate change over time") {
          base <- "./gis_files/child5/IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_"
          base2 <- "_Y2017M09D25.TIF"
          pattern <- paste(base, input$child5_change_year1_polys, base2, sep = '')
          print(pattern)
          pattern2 <- paste(base, input$child5_change_year2_polys, base2, sep = '')
          print(pattern2)
          rast1 <- raster(pattern)
          rast2 <- raster(pattern2)
          rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
          rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
          rast <- (rast2 - rast1)
      }
      rast
  })


# Output: Child 5 Map (child5_map_polys) ----------------------------------
  output$child5_map_polys <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(child5_rast_polys_react()) %>%
        addPolygons(data = extracted_polygons_profile_react(), fillOpacity = 0, color = "black")
  })
  

# Output: Child 5 Avg. Text (child5_avg_oneyear_polys) --------------------
  output$child5_avg_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      avg <- raster::cellStats(child5_rast_polys_react(), "mean")
      paste("The mean number of children under 5 in the selected year was ", avg, ".", sep = '')
  })
  

# Output: Child 5 Sum Text (child5_sum_oneyear_polys) ---------------------
  output$child5_sum_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      sum <- raster::cellStats(child5_rast_polys_react(), "sum")
      paste("The mean number of children under 5 in the selected year was ", sum, ".", sep = '')
  })  
  

# Output: Child 5 Avg. Change Text (child5_avg_change_polys) --------------
  output$child5_avg_change_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      base <- "./gis_files/child5/IHME_AFRICA_U5M_1998_2017_MEAN_UNDER5_"
      base2 <- "_Y2017M09D25.TIF"
      pattern <- paste(base, input$child5_change_year1_polys, base2, sep = '')
      print(pattern)
      pattern2 <- paste(base, input$child5_change_year2_polys, base2, sep = '')
      print(pattern2)
      rast1 <- raster(pattern)
      rast2 <- raster(pattern2)
      rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
      sum_year1 <- raster::cellStats(rast1, 'sum')
      rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
      sum_year2 <- raster::cellStats(rast2, 'sum')
      diff <- (sum_year2 - sum_year1)
      up_down <- ifelse(diff < 0, 'decreased', 'increased')
      paste("The total change in children under 5 between the selected years is ", diff, ". \n", "The number of children under 5 has ", up_down, '.', sep = '')
  })    
  

# Reactive: F. Ed. Raster Clipped to Suitable Polygons  (fed15_ras --------
  fed15_rast_polys_react <- reactive({
      if (input$fed15_calc_choice_polys == "Select one year"){
          base <- "./gis_files/education_female_15_49_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN_"
          base2 <- "_Y2018M02D28.TIF"
          pattern <- paste(base, input$fed15_year_polys, base2, sep = '')
          print(pattern)
          rast <- raster(pattern)
          rast <- crop(rast, extent(extracted_polygons_profile_react()))
      } else if (input$fed15_calc_choice_polys == "Calculate change over time") {
          base <- "./gis_files/education_female_15_49_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN_"
          base2 <- "_Y2018M02D28.TIF"
          pattern <- paste(base, input$fed15_change_year1_polys, base2, sep = '')
          print(pattern)
          pattern2 <- paste(base, input$fed15_change_year2_polys, base2, sep = '')
          print(pattern2)
          rast1 <- raster(pattern)
          rast2 <- raster(pattern2)
          rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
          rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
          rast <- (rast2 - rast1)
      }
      rast
  })
  

# Output: F. Ed. Suitable Polygons Map (fed15_map_polys) ------------------
  output$fed15_map_polys <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(fed15_rast_polys_react()) %>%
        addPolygons(data = extracted_polygons_profile_react(), fillOpacity = 0, color = "black")
  })
  
  
# Output: F. Ed. Avg. Text (fed15_avg_oneyear_polys) ----------------------
  output$fed15_avg_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      avg <- raster::cellStats(fed15_rast_polys_react(), "mean")
      paste("The mean years of education for females aged 15 - 49 in the selected year was ", avg, ".", sep = '')
  })
  

# Output: F. Ed. Sum Text (fed_15_sum_oneyear_polys) ----------------------
  output$fed15_sum_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      sum <- raster::cellStats(fed15_rast_polys_react(), "sum")
      paste("The mean years of education for females aged 15 - 49 in the selected year was ", sum, ".", sep = '')
  })  
  

# Output: F. Ed. Avg. Change Text (fed15_avg_change_polys) ----------------
  output$fed15_avg_change_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      base <- "./gis_files/education_female_15_49_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_15_49_MEAN_"
      base2 <- "_Y2018M02D28.TIF"
      pattern <- paste(base, input$fed15_change_year1_polys, base2, sep = '')
      print(pattern)
      pattern2 <- paste(base, input$fed15_change_year2_polys, base2, sep = '')
      print(pattern2)
      rast1 <- raster(pattern)
      rast2 <- raster(pattern2)
      rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
      sum_year1 <- raster::cellStats(rast1, 'sum')
      rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
      sum_year2 <- raster::cellStats(rast2, 'sum')
      diff <- (sum_year2 - sum_year1)
      up_down <- ifelse(diff < 0, 'decreased', 'increased')
      paste("The total change in the mean years of education for females aged 15 - 49 between the selected years is ", diff, ". \n", "The mean years of education has ", up_down, '.', sep = '')
  })   
  
  

# Reactive: F. Ed. Clipped to Suitable Polygons (fed20_rast_polys_ --------
  fed20_rast_polys_react <- reactive({
      if (input$fed20_calc_choice_polys == "Select one year"){
          base <- "./gis_files/education_female_20_24_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_20_24_MEAN_"
          base2 <- "_Y2018M02D28.TIF"
          pattern <- paste(base, input$fed20_year_polys, base2, sep = '')
          print(pattern)
          rast <- raster(pattern)
          rast <- crop(rast, extent(extracted_polygons_profile_react()))
      } else if (input$fed20_calc_choice_polys == "Calculate change over time") {
          base <- "./gis_files/education_female_20_24_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_20_24_MEAN_"
          base2 <- "_Y2018M02D28.TIF"
          pattern <- paste(base, input$fed20_change_year1_polys, base2, sep = '')
          print(pattern)
          pattern2 <- paste(base, input$fed20_change_year2_polys, base2, sep = '')
          print(pattern2)
          rast1 <- raster(pattern)
          rast2 <- raster(pattern2)
          rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
          rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
          rast <- (rast2 - rast1)
      }
      return(rast)
  })
  

# Output: F. Ed. Suitable Polygons Map (fed20_map_polys) ------------------
  output$fed20_map_polys <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(fed20_rast_polys_react()) %>%
        addPolygons(data = extracted_polygons_profile_react(), fillOpacity = 0, color = "black")
  })
  
  
# Output: F. Ed. Avg. Change Text (fed20_avg_oneyear_polys) ---------------
  output$fed20_avg_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      avg <- raster::cellStats(fed20_rast_polys_react(), "mean")
      paste("The mean years of education for females aged 20 - 24 in the selected year was ", avg, ".", sep = '')
  })
  

# Output: F. Ed. Sum Text (fed20_sum_oneyear_polys) -----------------------
  output$fed20_sum_oneyear_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      sum <- raster::cellStats(fed20_rast_polys_react(), "sum")
      paste("The mean years of education for females aged 20 - 24 in the selected year was ", sum, ".", sep = '')
  })  
  

# Output: F. Ed. Avg. Change Text (fed20_avg_change_polys) ----------------
  output$fed20_avg_change_polys <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      base <- "./gis_files/education_female_20_24_mean/IHME_AFRICA_EDU_2000_2015_YEARS_FEMALE_20_24_MEAN_"
      base2 <- "_Y2018M02D28.TIF"
      pattern <- paste(base, input$fed20_change_year1_polys, base2, sep = '')
      print(pattern)
      pattern2 <- paste(base, input$fed20_change_year2_polys, base2, sep = '')
      print(pattern2)
      rast1 <- raster(pattern)
      rast2 <- raster(pattern2)
      rast1 <- crop(rast1, extent(extracted_polygons_profile_react()))
      sum_year1 <- raster::cellStats(rast1, 'sum')
      rast2 <- crop(rast2, extent(extracted_polygons_profile_react()))
      sum_year2 <- raster::cellStats(rast2, 'sum')
      diff <- (sum_year2 - sum_year1)
      up_down <- ifelse(diff < 0, 'decreased', 'increased')
      paste("The total change in the mean years of education for females aged 20 - 24 between the selected years is ", diff, ". \n", "The mean years of education has ", up_down, '.', sep = '')
  })   
  

# Reactive: Population Raster Clipped to Suitable Polygon (pop_rast_react) --------
  pop_rast_react <- reactive({
      print('calculating est ben')
      base1 <- "./pop_rasters/population_"
      base2 <- "_2018-10-01.tif"
      pattern <- paste(base1, pop_countries_named[input$suitability_country], base2, sep = '')
      rast <- raster::raster(pattern)
      rast <- crop(rast, extent(extracted_polygons_profile_react()))
      return(rast)
  })
  
  
# Output: Est Ben in Suitable Polygon Text (extpolys_est_ben_text) -----------
  output$extpolys_est_ben_text <- renderText({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
    
      shiny::validate(
        need(input$suitability_country %in% pop_countries, paste("No Population Data Available for Selected Country."))
      )
        
      print('in est ben text')
      ben <- raster::cellStats(pop_rast_react(), "sum")
      print(ben)
      paste("The estimated number of total beneficiaries within the extracted polygon is ", ben, ".", sep = '')
  })
  

# Output: Est Ben in Suitable Polygon Map (extpolys_est_ben_map) --------------
  output$extpolys_est_ben_map <- renderLeaflet({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      shiny::validate(
        need(input$suitability_country %in% pop_countries, paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      print('in est ben map')

      leaflet() %>%
          addTiles() %>%
          addRasterImage(pop_rast_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), fillOpacity = 0, color = "black")
  })


# Reactive: Rainfall Raster clipped to Suitable Polygon (poly_extract_rainfall_react) --------
  poly_extract_rainfall_react <- reactive({
      crop(precip_mean_tif, extent(extracted_polygons_profile_react()))
  })


# Reactive: NDVI Raster clipped to Suitable Polygon (poly_extract_ndvi_react) --------
  poly_extract_ndvi_react <- reactive({
      crop(ndvi_mean_tif, extent(extracted_polygons_profile_react()))
  })


# Reactive: Min Temp Raster clipped to Suitable Polygon (poly_extract_mintemp_react) --------
  poly_extract_mintemp_react <- reactive({
      crop(mintemp_mean_tif, extent(extracted_polygons_profile_react()))
  })
  

# Reactive: Max Temp Raster clipped to Suitable Polygon (poly_extract_maxtemp_react) --------
  poly_extract_maxtemp_react <- reactive({
      crop(maxtemp_mean_tif, extent(extracted_polygons_profile_react()))
  })
  

# Output: Rainfall Map (poly_extract_map_rain) ----------------------------
  output$poly_extract_map_rain <- renderLeaflet({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      leaflet() %>%
          addTiles() %>%
          addRasterImage(poly_extract_rainfall_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), color = "#444444", weight = 4, fillOpacity = 0)
  })
    

# Output: NDVI Map (poly_extract_map_ndvi) --------------------------------
  output$poly_extract_map_ndvi <- renderLeaflet({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      leaflet() %>%
          addTiles() %>%
          addRasterImage(poly_extract_ndvi_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), color = "#444444", weight = 4, fillOpacity = 0)
  })

    
# Output: Minimum Temperature Map (poly_extract_map_mintemp) --------------
  output$poly_extract_map_mintemp <- renderLeaflet({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      leaflet() %>%
          addTiles() %>%
          addRasterImage(poly_extract_mintemp_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), color = "#444444", weight = 4, fillOpacity = 0)
  })
    

# Output: Maximum Temperature Map (poly_extract_map_maxtemp) ---------------
  output$poly_extract_map_maxtemp <- renderLeaflet({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      leaflet() %>%
          addTiles() %>%
          addRasterImage(poly_extract_maxtemp_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), color = "#444444", weight = 4, fillOpacity = 0)
  })


# Output: Rainfall Histogram (poly_extract_hist_rain) ---------------------
  output$poly_extract_hist_rain <- renderMetricsgraphics({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      xyz <- rasterToPoints(poly_extract_rainfall_react())
      yo <- as.data.frame(xyz)
      mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Rainfall", y_label = "Frequency")
  })


# Output: NDVI Histogram (poly_extract_hist_ndvi) -------------------------
  output$poly_extract_hist_ndvi <- renderMetricsgraphics({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      xyz <- rasterToPoints(poly_extract_ndvi_react())
      yo <- as.data.frame(xyz)
      mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="NDVI", y_label = "Frequency")
  })
    

# Output: Minimum Temperature Histogram (poly_extract_hist_mintemp) -------
  output$poly_extract_hist_mintemp <- renderMetricsgraphics({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      xyz <- rasterToPoints(poly_extract_mintemp_react())
      yo <- as.data.frame(xyz)
      mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Minimum Temperature", y_label = "Frequency")
  })
    

# Output: Maximum Temperature Histogram (poly_extract_hist_maxtemp) -------
  output$poly_extract_hist_maxtemp <- renderMetricsgraphics({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      xyz <- rasterToPoints(poly_extract_maxtemp_react())
      yo <- as.data.frame(xyz)
      mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Minimum Temperature", y_label = "Frequency")
  })


# Output: Rainfall Table (poly_extract_table_rain) ------------------------
  output$poly_extract_table_rain <- renderTable({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      raster_stats <- matrix(c(cellStats(poly_extract_rainfall_react(), "mean"), cellStats(poly_extract_rainfall_react(), "min"), cellStats(poly_extract_rainfall_react(), "max"), cellStats(poly_extract_rainfall_react(), "sd"), cellStats(poly_extract_rainfall_react(), "skew")),
                              ncol=1,
                              byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Rainfall Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
  })


# Output: NDVI Table (poly_extract_table_ndvi) ----------------------------
  output$poly_extract_table_ndvi <- renderTable({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      raster_stats <- matrix(c(cellStats(poly_extract_ndvi_react(), "mean"), cellStats(poly_extract_ndvi_react(), "min"), cellStats(poly_extract_ndvi_react(), "max"), cellStats(poly_extract_ndvi_react(), "sd"), cellStats(poly_extract_ndvi_react(), "skew")),
                               ncol=1,
                               byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("NDVI Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
  })


# Output: Minimum Temperature Table (poly_extract_table_mintemp) ----------
  output$poly_extract_table_mintemp <- renderTable({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )

      raster_stats <- matrix(c(cellStats(poly_extract_mintemp_react(), "mean"), cellStats(poly_extract_mintemp_react(), "min"), cellStats(poly_extract_mintemp_react(), "max"), cellStats(poly_extract_mintemp_react(), "sd"), cellStats(poly_extract_mintemp_react(), "skew")),
                              ncol=1,
                              byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Min Temp. Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
  })


# Output: Maximum Temperature Table (poly_extract_table_maxtemp) ----------
  output$poly_extract_table_maxtemp <- renderTable({
      shiny::validate(
        need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      
      raster_stats <- matrix(c(cellStats(poly_extract_maxtemp_react(), "mean"), cellStats(poly_extract_maxtemp_react(), "min"), cellStats(poly_extract_maxtemp_react(), "max"), cellStats(poly_extract_maxtemp_react(), "sd"), cellStats(poly_extract_maxtemp_react(), "skew")),
                              ncol=1,
                              byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Max Temp. Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
  })
  

# Reactive: Land Cover Raster Cropped to Suitable Polygon (lc_rast_react) --------
  lc_rast_react <- reactive({
      crop(landcover, extent(extracted_polygons_profile_react()))
  })
    

# Output: Land Cover Map (poly_extract_lc_map) -----------------------------------
  output$poly_extract_lc_map <- renderLeaflet({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
    
      leaflet() %>%
          addTiles() %>%
          addRasterImage(lc_rast_react()) %>%
          addPolygons(data = extracted_polygons_profile_react(), color = "#444444", weight = 4, fillOpacity = 0)
  })
    

# Reactive: Land Cover Table (lc_table_react) -----------------------------
  lc_table_react <- reactive({
      points <- rasterToPoints(lc_rast_react())
      points_df <- as.data.frame(points)
      table <- as.data.frame(table(points_df$Land.cover.class.defined.in.LCCS))
      print(table)
      table$Var1 <- as.numeric(table$Var1)
      table <- subset(table, Var1 > 0)
      table$Var1 <- as.character(table$Var1)
      table$class <- NA
      a <- 1
      for (i in names(land_covers_named)) {
          table$class[table$Var1 == i] <- names(land_covers_named2)[a]
          a <- a + 1
      }
      print(table)
      return(table)
  })
    
   

# Output: Land Cover Pie Chart (poly_extract_lc_pie) ----------------------
  output$poly_extract_lc_pie <- renderPlotly({
      shiny::validate(
          need(r$my_color == "red", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
      )
      points <- rasterToPoints(lc_rast_react())
      points_df <- as.data.frame(points)
      table <- as.data.frame(table(points_df$Land.cover.class.defined.in.LCCS))
      print(table)
      table$Var1 <- as.character(table$Var1)
      table$Var1 <- as.integer(table$Var1)
      table <- subset(table, Var1 > 0)
      table$Var1 <- as.character(table$Var1)
      table$class <- NA
      a <- 1
      for (i in names(land_covers_named)) {
          table$class[table$Var1 == i] <- names(land_covers_named2)[a]
          a <- a + 1
      }
      print(table)
      
      p <- plot_ly(table, labels = ~class, values = ~Freq, type = 'pie') %>%
        layout(title = ' ',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      p
  })
    
  
  
  
  
  
    
  #####End Land Cover Stuff#####

  # extract_poly_stats_df_react <- reactive({
  #   
  #   shp_dis <- as(final_suit_polygons_react(), "SpatialPolygonsDataFrame")
  #   
  #   shp_dis <- disaggregate(shp_dis)
  #   
  #   print(shp_dis)
  #   
  #   new_df <- as.data.frame(1:dim(shp_dis)[1])
  #   
  #   stunting_rast <- raster("./gis_files/stunting/IHME_AFRICA_CGF_2000_2015_STUNTING_MEAN_2015_PREVALENCE_Y2018M02D28.TIF")
  #   
  #   stunting <- raster::extract(stunting_rast, shp_dis)
  #   
  #   print(stunting)
  #   
  #   new_df$stunting <- stunting
  #   
  #   # colnames(new_df) <- c("ID", "Stunting")
  #   
  #   return(new_df)
  # 
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  # ND-Gain Here ------------------------------------------------------------
  
  
  observe({
      check <- gain$Name
      if (input$country %in% check) {
            updateSelectInput(session, 'gain_country', selected = input$country)
      }
  })
  
  
  
  
  output$nd_gain_chart <- renderHighchart({
    
    #Delta 
    subsetted1 <- subset(gain, Name == input$gain_country)
    trans1 <- as.data.frame(t(subsetted1))
    new_df1 <- as.data.frame(trans1[3:dim(subsetted1)[2],])
    new_df1$year <- c(1995:2017)
    colnames(new_df1) <- c("Value", "Year")
    new_df1$Value <- as.numeric(as.character(new_df1$Value))
    
    #One Year
    subsetted <- subset(gain_delta, Name == input$gain_country)
    trans <- as.data.frame(t(subsetted))
    new_df <- as.data.frame(trans[3:dim(subsetted)[2],])
    new_df$year <- c(1995:2017)
    colnames(new_df) <- c("Value", "Year")
    new_df$Value <- as.numeric(as.character(new_df$Value))
    
    if(input$gain_choices == "Individual Year") {
      
      hc <- highchart() %>% 
        hc_xAxis(categories = new_df1$Year) %>% 
        hc_add_series(name = "Gain", data = new_df1$Value)
      
      hc
      
    } else if (input$gain_choices == "Change Over Time") {
      
      hc <- highchart() %>% 
        hc_xAxis(categories = new_df$Year) %>% 
        hc_add_series(name = "Gain Delta", data = new_df$Value)
      
      hc
      
    } else if (input$gain_choices == "Both") {
      
      hc <- highchart() %>% 
        hc_xAxis(categories = new_df$Year) %>% 
        hc_add_series(name = "Gain Delta", data = new_df$Value) %>%
        hc_add_series(name = "Gain", data = new_df1$Value)
      
      hc
      
    }
    
    
    
    
  })
  
  
  
  
  # End ND-Gain Here --------------------------------------------------------
  
  
  
  

  
  
  

# OLD NEW STUFF -----------------------------------------------------------

  # Second Tab Panel --------------------------------------------------------

    reactive_raster <- reactive({
        #get country boundaries from postgres
        pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
        clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                       geom = "the_geom",
                                       clause = (pattern))

        if (input$raster_choice == 'Rainfall') {
            rainfall_raster_con <- raster_con(input$con_rainfall[1], input$con_rainfall[2], precip_mean_tif, clipped)
        } else if (input$raster_choice == 'NDVI') {
            ndvi_raster_con <- raster_con(input$con_ndvi[1], input$con_ndvi[2], ndvi_mean_tif, clipped)
        } else if (input$raster_choice == 'Minimum Temperature') {
            mintemp_raster_con <- raster_con(input$con_mintemp[1], input$con_mintemp[2], mintemp_mean_tif, clipped)
        } else if (input$raster_choice == 'Maximum Temperature') {
            maxtemp_raster_con <- raster_con(input$con_maxtemp[1], input$con_maxtemp[2], maxtemp_mean_tif, clipped)
        }
    })


    legend_name_react <- reactive ({
        if (input$raster_choice == 'Rainfall') {
            legend_title <- "Rainfall Suitability"
        } else if (input$raster_choice == 'NDVI') {
            legend_title <- "NDVI Suitability"
        } else if (input$raster_choice == 'Minimum Temperature') {
            legend_title <- "Minimum Temperature Suitability"
        } else if (input$raster_choice == 'Maximum Temperature') {
            legend_title <- "Maximum Temperature Suitability"
        }
    })


    raster_calc_react <- reactive({
        pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
        clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                       geom = "the_geom",
                                       clause = (pattern))

        ndvi_raster_con <- raster_con(input$overlay_ndvi[1], input$overlay_ndvi[2], ndvi_mean_tif, clipped)

        rainfall_raster_con <- raster_con(input$overlay_rainfall[1], input$overlay_rainfall[2], precip_mean_tif, clipped, ndvi_raster_con)

        mintemp_raster_con <- raster_con(input$overlay_mintemp[1], input$overlay_mintemp[2], mintemp_mean_tif, clipped, ndvi_raster_con)

        maxtemp_raster_con <- raster_con(input$overlay_maxtemp[1], input$overlay_maxtemp[2], maxtemp_mean_tif, clipped, ndvi_raster_con)

        # You might needa come back later and re-con the final added raster so that only pixel values of 4 are shown in the final raster
        # Or add a legend so that ppl know whether the pixel is 1, 2, 3 or 4
        final_raster <- (rainfall_raster_con + ndvi_raster_con + mintemp_raster_con + maxtemp_raster_con)
    })


    raster_choose_react <- reactive({

          # There's an error in here somewere where the mintemp raster shows up with values already filtered out
          pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
          clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                         geom = "the_geom",
                                         clause = (pattern))

          ndvi_raster_con <- raster_con(input$choose_ndvi[1], input$choose_ndvi[2], ndvi_mean_tif, clipped)

          rainfall_raster_con <- raster_con(input$choose_rainfall[1], input$choose_rainfall[2], precip_mean_tif, clipped, ndvi_raster_con)

          mintemp_raster_con <- raster_con(input$choose_mintemp[1], input$choose_mintemp[2], mintemp_mean_tif, clipped, ndvi_raster_con)

          maxtemp_raster_con <- raster_con(input$choose_maxtemp[1], input$choose_maxtemp[2], maxtemp_mean_tif, clipped, ndvi_raster_con)

          # You might needa come back later and re-con the final added raster so that only pixel values of 4 are shown in the final raster
          # Or add a legend so that ppl know whether the pixel is 1, 2, 3 or 4
          # final_raster <- (rainfall_raster_con + ndvi_raster_con + mintemp_raster_con + maxtemp_raster_con)

          final_raster <- raster()

          if ("Rainfall" %in% input$choose_rasters) {
            if (hasValues(final_raster) == TRUE) {
              final_raster <- final_raster + rainfall_raster_con
            } else {
              final_raster <- rainfall_raster_con
            }
          }

          if ("NDVI" %in% input$choose_rasters) {
              final_raster <- final_raster + ndvi_raster_con
              if (hasValues(final_raster) == TRUE) {
                final_raster <- final_raster + ndvi_raster_con
              } else {
                final_raster <- ndvi_raster_con
              }
          }

          if ("Minimum Temperature" %in% input$choose_rasters) {
              final_raster <- final_raster + mintemp_raster_con
              if (hasValues(final_raster) == TRUE) {
                final_raster <- final_raster + mintemp_raster_con
              } else {
                final_raster <- mintemp_raster_con
              }
          }

          if ("Maximum Temperature" %in% input$choose_rasters) {
              final_raster <- final_raster + maxtemp_raster_con
              if (hasValues(final_raster) == TRUE) {
                final_raster <- final_raster + maxtemp_raster_con
              } else {
                final_raster <- maxtemp_raster_con
              }
          }

          final_raster

    })





    output$mymap <- renderLeaflet({

        if (input$one_or_overlay == 'Map one Raster') {

              pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
              country_boundaries <- rpostgis::pgGetGeom(con, "ifad_regions",
                                                        geom = "the_geom",
                                                        clause = (pattern))

              pal <- colorNumeric(c("#2dc937"), values(reactive_raster()),
                                  na.color = "transparent")

              leaflet() %>%
                addTiles() %>%
                addDrawToolbar(targetGroup = "drawnPoly",
                               rectangleOptions = F,
                               polylineOptions = F,
                               markerOptions = F,
                               editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                               circleOptions=F,
                               polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions( fillColor="red", clickable = TRUE))) %>%
                addStyleEditor() %>%
                addPolygons(data = country_boundaries, color = "black", fillOpacity = 0, weight = 2) %>%
                addRasterImage(reactive_raster(), colors = pal, opacity = 0.8)

        } else if (input$one_or_overlay == 'Choose Rasters to Overlay') {

          pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
          country_boundaries <- rpostgis::pgGetGeom(con, "ifad_regions",
                                                    geom = "the_geom",
                                                    clause = (pattern))

          pal <- colorNumeric(c("#2dc937"), values(reactive_raster()),
                              na.color = "transparent")

          leaflet() %>%
            addTiles() %>%
            addDrawToolbar(targetGroup = "drawnPoly",
                           rectangleOptions = F,
                           polylineOptions = F,
                           markerOptions = F,
                           editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                           circleOptions = F,
                           polygonOptions = drawPolygonOptions(showArea=TRUE, repeatMode = F, shapeOptions=drawShapeOptions( fillColor="red", clickable = TRUE))) %>%
            addStyleEditor() %>%
            addPolygons(data = country_boundaries, color = "black", fillOpacity = 0, weight = 2) %>%
            addRasterImage(raster_choose_react(), colors = pal, opacity = 0.8)

        } else {

              pal <- colorNumeric(c("#2dc937"), values(reactive_raster()),
                                  na.color = "transparent")

              leaflet() %>%
                addTiles() %>%
                addDrawToolbar(targetGroup = "drawnPoly",
                               rectangleOptions = F,
                               polylineOptions = F,
                               markerOptions = F,
                               editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                               circleOptions=F,
                               polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE))) %>%
                addStyleEditor() %>%
                addRasterImage(raster_calc_react(), colors = pal, opacity = 0.8) %>%
                addLegend(pal = pal, values = values(raster_calc_react()),
                          title = "Overall Suitaility")

        }

    })


    # PDB/DRAW/Convert Drawn Polygons to SpatialPolygons -------------------------------
    latlongs <- reactiveValues()   #temporary to hold coords
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
    value <- reactiveValues()
    value$drawnPoly <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))


    # PDB/DRAW/Observe: Draw New Features ----------------------------------------------
    observeEvent(input$mymap_draw_new_feature, {

        coor <- unlist(input$mymap_draw_new_feature$geometry$coordinates)
        Longitude <- coor[seq(1,length(coor), 2)]
        Latitude <- coor[seq(2,length(coor), 2)]
        isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
        poly <- Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
        polys <- Polygons(list(poly), ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
        spPolys <- SpatialPolygons(list(polys))
        value$drawnPoly <- rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys,
                                                                          data=data.frame(notes=NA, row.names=
                                                                                            row.names(spPolys))))
        observeEvent(input$mymap_draw_stop, {
          leafletProxy('mymap') %>%
            removeDrawToolbar(clearFeatures=TRUE) %>%
            removeShape('temp') %>% clearGroup('drawnPoly') %>%
            addPolygons(data=value$drawnPoly, popup="New Project Boundary", group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>%
            addDrawToolbar(targetGroup = "drawnPoly",
                           rectangleOptions = F,
                           polylineOptions = F,
                           markerOptions = F,
                           editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                           circleOptions=F,
                           polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))

        })

        latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df

    })

    # PDB/DRAW/Observe: Delete Drawn Features ------------------------------------------
    observeEvent(input$mymap_draw_edited_features, {
        f <- input$mymap_draw_edited_features
        coordy <- lapply(f$features, function(x){unlist(x$geometry$coordinates)})
        Longitudes <- lapply(coordy, function(coor){coor[seq(1,length(coor), 2)]})
        Latitudes <- lapply(coordy, function(coor){coor[seq(2,length(coor), 2)]})
        polys <- list()
        for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
          list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
        )}
        spPolys <- SpatialPolygons(polys)
        SPDF <- SpatialPolygonsDataFrame(spPolys,
                                         data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
        value$drawnPoly <- value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
        value$drawnPoly <- rbind(value$drawnPoly, SPDF)
    })



    # PDB/DRAW/Observe: Edit Drawn Features --------------------------------------------
    observeEvent(input$mymap_draw_deleted_features, {
        f <- input$mymap_draw_deleted_features
        ids <- lapply(f$features, function(x){unlist(x$properties$layerId)})
        value$drawnPoly <- value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,]
    })


    # PDB/DRAW/Download: Drawn Polygons to Shapefile ----------------------------------------
    output$downloadData<-downloadHandler(
        filename = 'shpExport.zip',

        content = function(file) {
          if (length(Sys.glob("shpExport.*"))>0){
            file.remove(Sys.glob("shpExport.*"))
          }

          proj4string(value$drawnPoly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

          writeOGR(value$drawnPoly, dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")

          zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))

          file.copy("shpExport.zip", file)

          if (length(Sys.glob("shpExport.*"))>0){
            file.remove(Sys.glob("shpExport.*"))

          }
        }
    )


    draw_final_polygons <- reactive({
        value$drawnPoly
    })




    # PDB/DRAW/Reactive Rasters ---------------------------------------------------
    drawraster_rainfall_react <- reactive({
        crop(precip_mean_tif, extent(value$drawnPoly))
    })

    drawraster_ndvi_react <- reactive({
        crop(ndvi_mean_tif, extent(value$drawnPoly))
    })

    drawraster_mintemp_react <- reactive({
        crop(mintemp_mean_tif, extent(value$drawnPoly))
    })

    drawraster_maxtemp_react <- reactive({
        crop(maxtemp_mean_tif, extent(value$drawnPoly))
    })


    draw_country_clip_react <- reactive({
        pattern <- paste("WHERE adm0_name = '", input$drawing_country, "';", sep = '')
        clipped <- rpostgis::pgGetGeom(con, "ifad_regions",
                                       geom = "the_geom",
                                       clause = (pattern))
    })


    observe({
        cropped <- crop(precip_mean_tif, extent(draw_country_clip_react()))
        min <- raster::cellStats(cropped, 'min')
        max <- raster::cellStats(cropped, 'max')
        updateSliderInput(session, 'con_rainfall', min = min, max = max, value = c(min, max))
        updateSliderInput(session, 'overlay_rainfall', min = min, max = max, value = c(min, max))
    })

    observe({
        cropped <- crop(ndvi_mean_tif, extent(draw_country_clip_react()))
        min <- raster::cellStats(cropped, 'min')
        max <- raster::cellStats(cropped, 'max')
        updateSliderInput(session, 'con_ndvi', min = min, max = max, value = c(min, max))
        updateSliderInput(session, 'overlay_ndvi', min = min, max = max, value = c(min, max))
    })

    observe({
      cropped <- crop(mintemp_mean_tif, extent(draw_country_clip_react()))
      min <- raster::cellStats(cropped, 'min')
      max <- raster::cellStats(cropped, 'max')
      updateSliderInput(session, 'con_mintemp', min = min, max = max, value = c(min, max))
      updateSliderInput(session, 'overlay_mintemp', min = min, max = max, value = c(min, max))
    })

    observe({
      cropped <- crop(maxtemp_mean_tif, extent(draw_country_clip_react()))
      min <- raster::cellStats(cropped, 'min')
      max <- raster::cellStats(cropped, 'max')
      updateSliderInput(session, 'con_maxtemp', min = min, max = max, value = c(min, max))
      updateSliderInput(session, 'overlay_maxtemp', min = min, max = max, value = c(min, max))
    })



    est_beneficiaries_draw_react <- reactive({
          directory_pattern <- paste("./pop_rasters/population_", pop_countries[input$drawing_country], "_2018-10-01.tif", sep = '')
          pop_rast <- raster(directory_pattern)
          pop_rast <- crop(pop_rast, extent(value$drawnPoly))
          ben_sum <- raster::cellStats(pop_rast, 'sum')
          ben_sum
    })


    output$est_beneficiaries <- renderText({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for an estimated number of beneficiaries."))
        )

        # validate(
        #   need(input$drawing_country %in% pop_countries, paste("Choose a country in Africa for an estimated number of beneficiaries."))
        # )

        paste(as.character(floor(est_beneficiaries_draw_react())), 'people', sep = ' ')
    })


    # PDB/DRAW/Raster Maps --------------------------------------------------------
    output$rasterextract_map_rain <- renderLeaflet({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )

        leaflet() %>%
          addTiles() %>%
          addRasterImage(drawraster_rainfall_react()) %>%
          addPolygons(data = value$drawnPoly, color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$rasterextract_map_ndvi <- renderLeaflet({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a map of the NDVI raster within the boundaries."))
        )

        leaflet() %>%
          addTiles() %>%
          addRasterImage(drawraster_ndvi_react()) %>%
          addPolygons(data = value$drawnPoly, color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$rasterextract_map_mintemp <- renderLeaflet({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a map of the Minimum Temeprature raster within the boundaries."))

        )

        leaflet() %>%
          addTiles() %>%
          addRasterImage(drawraster_mintemp_react()) %>%
          addPolygons(data = value$drawnPoly, color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$rasterextract_map_maxtemp <- renderLeaflet({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a map of the Maximum Temperature raster within the boundaries."))
        )

        leaflet() %>%
          addTiles() %>%
          addRasterImage(drawraster_maxtemp_react()) %>%
          addPolygons(data = value$drawnPoly, color = "#444444", weight = 4, fillOpacity = 0)
    })


    # PDB/DRAW/Raster Histograms --------------------------------------------------
    output$rasterextract_hist_rain <- renderMetricsgraphics({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a histogram of the Rainfall raster within the boundaries."))
        )

        xyz <- rasterToPoints(drawraster_rainfall_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Rainfall", y_label = "Frequency")
    })

    output$rasterextract_hist_ndvi <- renderMetricsgraphics({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a histogram of the NDVI raster within the boundaries."))
        )

        xyz <- rasterToPoints(drawraster_ndvi_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="NDVI", y_label = "Frequency")
    })

    output$rasterextract_hist_maxtemp <- renderMetricsgraphics({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a histogram of the Minimum Temperature raster within the boundaries."))
        )

        xyz <- rasterToPoints(drawraster_maxtemp_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Maximum Temperature", y_label = "Frequency")
    })

    output$rasterextract_hist_mintemp <- renderMetricsgraphics({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a histogram of the Maximum Temperature raster within the boundaries.!"))
        )

        xyz <- rasterToPoints(drawraster_mintemp_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Minimum Temperature", y_label = "Frequency")
    })




    output$draw_rainfall_table <- renderTable({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a table of Rainfall raster statistics."))
        )

        raster_stats <- matrix(c(cellStats(drawraster_rainfall_react(), "mean"), cellStats(drawraster_rainfall_react(), "min"), cellStats(drawraster_rainfall_react(), "max"), cellStats(drawraster_rainfall_react(), "sd"), cellStats(drawraster_rainfall_react(), "skew")),
                               ncol=1,
                               byrow=TRUE)
        rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        colnames(raster_stats) <- c("Rainfall Raster Statistics")
        raster_stats <- as.data.frame(raster_stats)
        raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        raster_stats
    })

    output$draw_ndvi_table <- renderTable({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a table of NDVI raster statistics."))
        )

        raster_stats <- matrix(c(cellStats(drawraster_ndvi_react(), "mean"), cellStats(drawraster_ndvi_react(), "min"), cellStats(drawraster_ndvi_react(), "max"), cellStats(drawraster_ndvi_react(), "sd"), cellStats(drawraster_ndvi_react(), "skew")),
                               ncol=1,
                               byrow=TRUE)
        rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        colnames(raster_stats) <- c("NDVI Raster Statistics")
        raster_stats <- as.data.frame(raster_stats)
        raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        raster_stats
    })

    output$draw_mintemp_table <- renderTable({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a table of NDVI raster statistics."))
        )

        raster_stats <- matrix(c(cellStats(drawraster_mintemp_react(), "mean"), cellStats(drawraster_mintemp_react(), "min"), cellStats(drawraster_mintemp_react(), "max"), cellStats(drawraster_mintemp_react(), "sd"), cellStats(drawraster_mintemp_react(), "skew")),
                               ncol=1,
                               byrow=TRUE)
        rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        colnames(raster_stats) <- c("Min Temp. Raster Statistics")
        raster_stats <- as.data.frame(raster_stats)
        raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        raster_stats
    })



    output$draw_maxtemp_table <- renderTable({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a table of NDVI raster statistics."))
        )

        raster_stats <- matrix(c(cellStats(drawraster_maxtemp_react(), "mean"), cellStats(drawraster_maxtemp_react(), "min"), cellStats(drawraster_maxtemp_react(), "max"), cellStats(drawraster_maxtemp_react(), "sd"), cellStats(drawraster_maxtemp_react(), "skew")),
                               ncol=1,
                               byrow=TRUE)
        rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        colnames(raster_stats) <- c("Max Temp. Raster Statistics")
        raster_stats <- as.data.frame(raster_stats)
        raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
        raster_stats
    })




    observe({
        if (input$compareone_country != "") {
            ongoing_pattern <- paste("WHERE display_name = '", input$compareone_country, "';", sep = '')
            project_choices <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                                   geom = "the_geom",
                                                   clause = (ongoing_pattern))
            updateSelectInput(session, "compareone_project", choices = c("None" = "", project_choices$name))
        }
    })


    output$compare_project_map <- renderLeaflet({
        validate(
          need(dim(value$drawnPoly@data)[1] != 0, paste("Draw a project boundary on the map for a project comparison map."))
        )

        validate(
          need(input$compareone_project != "", paste("Draw a project boundary on the map for a project comparison map."))
        )

        #get ongoing project boundaries from postgres
        ongoing_pattern <- paste("WHERE name = '", input$compareone_project, "';", sep = '')

        project_choices <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                                geom = "the_geom",
                                                clause = (ongoing_pattern))

        leaflet() %>%
          addTiles() %>%
          addPolygons(data = project_choices, fillColor = "#157f1f", color = "#444444", weight = 2,
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~paste("<b>Project Name:</b>", project_choices$name, "<br/>",
                                     "<b>Status:</b>", project_choices$status, "<br/>",
                                     "<b>Sector:</b>", project_choices$sector_name, "<br/>",
                                     "<b>Start Date:</b>", project_choices$start_date, "<br/>",
                                     "<b>End Date:</b>", project_choices$end_date, "<br/>",
                                     "<b>Direct Benficiaries:</b>", as.character(project_choices$direct_ben), "<br/>")) %>%
          addPolygons(data = value$drawnPoly, popup="New Project Boundary", fillColor = "#5c6784",
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      color = "#444444", weight = 2)
    })


    compare_project_zonal_react <- reactive({
        ongoing_pattern <- paste("WHERE name = '", input$compareone_project, "';", sep = '')

        project_choices <- rpostgis::pgGetGeom(con, "view_ifad_project_polygons_ongoing",
                                               geom = "the_geom",
                                               clause = (ongoing_pattern))

        Mean_Rainfall <- extract(x = precip_mean_tif, y = project_choices, fun = mean)
        Mean_NDVI <- extract(x = ndvi_mean_tif, y = project_choices, fun = mean)
        Mean_Min_Temp <- extract(x = mintemp_mean_tif, y = project_choices, fun = mean)
        Mean_Max_Temp <- extract(x = maxtemp_mean_tif, y = project_choices, fun = mean)

        mean_rain <- data.frame(Mean_Rainfall)
        mean_ndvi <- data.frame(Mean_NDVI)
        mean_min <- data.frame(Mean_Min_Temp)
        mean_max <- data.frame(Mean_Max_Temp)

        mean_rain$Project_Name <- project_choices$name
        mean_ndvi$Project_Name <- project_choices$name
        mean_min$Project_Name <- project_choices$name
        mean_max$Project_Name <- project_choices$name

        stats <- merge(mean_rain, mean_ndvi, by="Project_Name")
        stats <- merge(stats, mean_min, by="Project_Name")
        stats <- merge(stats, mean_max, by="Project_Name")

        stats <- setNames(stats, c("Project Name", "Mean Rainfall", "Mean NDVI", "Mean Minimum Temperature", "Mean Maximum Temperature"))

        stats

    })

    output$compare_project_table <- renderFormattable({
        validate(
          need(input$compareone_project != "", paste("Draw a project boundary on the map for a project comparison map."))
        )

        formattable(compare_project_zonal_react(),
                    align = c("l", rep("r", NCOL(compare_project_zonal_react()) - 1)),
                    list(`Project Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                         `Mean Rainfall` = color_bar("#A6ABBD"),
                         `Mean NDVI` = color_bar("#A6ABBD"),
                         `Mean Minimum Temperature` = color_bar("#A6ABBD"),
                         `Mean Maximum Temperature` = color_bar("#A6ABBD")))
    })



  # Select ADM Boundaries ------------------------------------------------------------------

  # PDB/ADM SELECT/ADM Level Update Observers ----------------------------------------------
    observe({

        if (input$admselect_adm0 != '') {

            ongoing_pattern <- paste("WHERE adm0_name = '", input$admselect_adm0, "';", sep = '')

            new_adm1_input <- rpostgis::pgGetGeom(con, "ifad_gaul_level_1_2017",
                                            geom = "the_geom",
                                            clause = (ongoing_pattern))

            updateSelectInput(session, "admselect_adm1", choices = c("All ADM1" = "", new_adm1_input$adm1_name))

        } else {

            updateSelectInput(session, "admselect_adm1", choices = c("All ADM1" = ""))

        }

    })


    observe({

        if (input$admselect_adm1 != ''){

            ongoing_pattern <- paste("WHERE adm1_name = '", input$admselect_adm1, "';", sep = '')

            new_adm2_input <- rpostgis::pgGetGeom(con, "ifad_gaul_level_2_2017",
                                                  geom = "the_geom",
                                                  clause = (ongoing_pattern))

            updateSelectInput(session, "admselect_adm2", choices = c("All ADM2" = "", new_adm2_input$adm2_name))

        } else {

            updateSelectInput(session, "admselect_adm2", choices = c("All ADM2" = ""))

        }

    })


  # PDB/ADM SELECT/Map Reactive -------------------------------------------------
    admselect_shp_react <- reactive({
      if (input$admselect_adm0 == "") {#if adm0 has no input, map the whole world

        #this does nothing? country_boundaries isn't even read in yo
        map_data <- NULL

      } else {

          if (input$admselect_adm0 != "" & input$admselect_adm1 == "") {

            ongoing_pattern <- paste("WHERE adm0_name = '", input$admselect_adm0, "';", sep = '')

            map_data <- rpostgis::pgGetGeom(con, "ifad_gaul_level_1_2017",
                                                  geom = "the_geom",
                                                  clause = (ongoing_pattern))

          } else {

              if (input$admselect_adm1 != "" & input$admselect_adm2 == "") {



                ongoing_pattern <- paste("WHERE adm1_name = '", input$admselect_adm1, "';", sep = '')

                map_data <- rpostgis::pgGetGeom(con, "ifad_gaul_level_2_2017",
                                                      geom = "the_geom",
                                                      clause = (ongoing_pattern))

              } else {

                    ongoing_pattern <- paste("WHERE adm2_name = '", input$admselect_adm2, "';", sep = '')

                    map_data <- rpostgis::pgGetGeom(con, "ifad_gaul_level_2_2017",
                                                    geom = "the_geom",
                                                    clause = (ongoing_pattern))

              }
           }
        }
    })


  # PDB/ADM SELECT/Map ----------------------------------------------------------
    output$admselect_map <- renderLeaflet({

      if (input$admselect_adm0 == "") {

          label <- NULL

      } else {

          if (input$admselect_adm0 != "" & input$admselect_adm1 == "") {

              ongoing_pattern <- paste("WHERE adm0_name = '", input$admselect_adm0, "';", sep = '')

              map_data <- rpostgis::pgGetGeom(con, "ifad_gaul_level_1_2017",
                                              geom = "the_geom",
                                              clause = (ongoing_pattern))

              label <- map_data$adm1_name

          } else {

              if (input$admselect_adm1 != "" & input$admselect_adm2 == "") {

                ongoing_pattern <- paste("WHERE adm1_name = '", input$admselect_adm1, "';", sep = '')

                map_data <- rpostgis::pgGetGeom(con, "ifad_gaul_level_2_2017",
                                                geom = "the_geom",
                                                clause = (ongoing_pattern))

                label <- map_data$adm2_name

            } else{

                  label <- NULL

            }

         }

      }



      if (input$admselect_adm0 == "") {

          leaflet() %>%
              addTiles(group = "OSM (default)") %>%
              addTiles('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', group = "Terrain") %>%
              addTiles("https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}{r}.png", group = "Street") %>%
              addLayersControl(
                  baseGroups = c("OSM (default)", "Terrain", "Street"),
                  options = layersControlOptions(collapsed = TRUE))

      } else {#Fix this map it real wonky

          leaflet() %>%
              addTiles(group = "OSM (default)") %>%
              addTiles('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', group = "Terrain") %>%
              addTiles("https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}{r}.png", group = "Street") %>%
              addLayersControl(
                  baseGroups = c("OSM (default)", "Terrain", "Street"),
                  options = layersControlOptions(collapsed = TRUE)) %>%
              addPolygons(data = admselect_shp_react(), color = "#003870", fillOpacity = 0, weight = 4, label = label)

      }

    })



    # PDB/ADM SELECT/Raster Maps ----------------------------------------------
    admraster_rainfall_react <- reactive({
        crop(precip_mean_tif, extent(admselect_shp_react()))
    })

    admraster_ndvi_react <- reactive({
        crop(ndvi_mean_tif, extent(admselect_shp_react()))
    })

    admraster_mintemp_react <- reactive({
        crop(mintemp_mean_tif, extent(admselect_shp_react()))
    })

    admraster_maxtemp_react <- reactive({
        crop(maxtemp_mean_tif, extent(admselect_shp_react()))
    })








    est_beneficiaries_adm_react <- reactive({
      directory_pattern <- paste("./pop_rasters/population_", pop_countries[input$admselect_adm0], "_2018-10-01.tif", sep = '')
      pop_rast <- raster(directory_pattern)
      pop_rast <- crop(pop_rast, extent(admselect_shp_react()))
      ben_sum <- raster::cellStats(pop_rast, 'sum')
      ben_sum
    })


    output$est_adm_beneficiaries <- renderText({
      validate(
        need(input$admselect_adm0 != "", paste("Choose and administrative level for an estimated number of beneficiaries."))
      )

      validate(
        need(input$admselect_adm1 != "", paste("Choose and administrative level for an estimated number of beneficiaries."))
      )

      validate(
        need(input$admselect_adm0 %in% pop_countries, paste("Choose a country in Africa for an estimated number of beneficiaries."))
      )

      paste(as.character(floor(est_beneficiaries_adm_react())), 'people', sep = ' ')
    })









    # PDB/DRAW/Raster Maps --------------------------------------------------------
    output$adm_rastermap_rain <- renderLeaflet({
        validate(
            need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )

        leaflet() %>%
            addTiles() %>%
            addRasterImage(admraster_rainfall_react()) %>%
            addPolygons(data = admselect_shp_react(), color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$adm_rastermap_ndvi <- renderLeaflet({
        validate(
            need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )

        leaflet() %>%
            addTiles() %>%
            addRasterImage(admraster_ndvi_react()) %>%
            addPolygons(data = admselect_shp_react(), color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$adm_rastermap_maxtemp <- renderLeaflet({
        validate(
            need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )

        leaflet() %>%
            addTiles() %>%
            addRasterImage(admraster_mintemp_react()) %>%
            addPolygons(data = admselect_shp_react(), color = "#444444", weight = 4, fillOpacity = 0)
    })

    output$adm_rastermap_mintemp <- renderLeaflet({
        validate(
            need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )

        leaflet() %>%
            addTiles() %>%
            addRasterImage(admraster_maxtemp_react()) %>%
            addPolygons(data = admselect_shp_react(), color = "#444444", weight = 4, fillOpacity = 0)
    })



    # PDB/ADM SELECT/Raster Histograms --------------------------------------------------
    output$adm_hist_rain <- renderMetricsgraphics({
        validate(
          need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )
        xyz <- rasterToPoints(admraster_rainfall_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Rainfall", y_label = "Frequency")
    })

    output$adm_hist_ndvi <- renderMetricsgraphics({
        validate(
          need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )
        xyz <- rasterToPoints(admraster_ndvi_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="NDVI", y_label = "Frequency")

    })

    output$adm_hist_maxtemp <- renderMetricsgraphics({
        validate(
          need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )
        xyz <- rasterToPoints(admraster_maxtemp_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Maximum Temperature", y_label = "Frequency")
    })

    output$adm_hist_mintemp <- renderMetricsgraphics({
        validate(
          need(input$admselect_adm0 != "", paste("Input your project parameters and above and click Calculate Suitability for an analysis of suitable project areas."))
        )
        xyz <- rasterToPoints(admraster_mintemp_react())
        yo <- as.data.frame(xyz)
        mjs_plot(yo[,3]) %>%
          mjs_histogram() %>%
          mjs_labs(x_label="Minimum Temperature", y_label = "Frequency")
    })


    output$adm_table_rain <- renderTable({
      validate(
        need(input$admselect_adm0 != "", paste("Draw a table of the Rainfall raster statistics within the boundaries."))
      )

      raster_stats <- matrix(c(cellStats(admraster_rainfall_react(), "mean"), cellStats(admraster_rainfall_react(), "min"), cellStats(admraster_rainfall_react(), "max"), cellStats(admraster_rainfall_react(), "sd"), cellStats(admraster_rainfall_react(), "skew")),
                             ncol=1,
                             byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Rainfall Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
    })

    output$adm_table_ndvi <- renderTable({
      validate(
        need(input$admselect_adm0 != "", paste("Draw a table of the NDVI raster statistics within the boundaries."))
      )

      raster_stats <- matrix(c(cellStats(admraster_ndvi_react(), "mean"), cellStats(admraster_ndvi_react(), "min"), cellStats(admraster_ndvi_react(), "max"), cellStats(admraster_ndvi_react(), "sd"), cellStats(admraster_ndvi_react(), "skew")),
                             ncol=1,
                             byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("NDVI Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
    })

    output$adm_table_mintemp <- renderTable({
      validate(
        need(input$admselect_adm0 != "", paste("Draw a table of the Minimum Temperature raster statistics within the boundaries."))
      )

      raster_stats <- matrix(c(cellStats(admraster_mintemp_react(), "mean"), cellStats(admraster_mintemp_react(), "min"), cellStats(admraster_mintemp_react(), "max"), cellStats(admraster_mintemp_react(), "sd"), cellStats(admraster_mintemp_react(), "skew")),
                             ncol=1,
                             byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Min Temp. Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
    })

    output$adm_table_maxtemp <- renderTable({
      validate(
        need(input$admselect_adm0 != "", paste("Draw a table of the Maximum Temperature raster statistics within the boundaries."))
      )

      raster_stats <- matrix(c(cellStats(admraster_maxtemp_react(), "mean"), cellStats(admraster_maxtemp_react(), "min"), cellStats(admraster_maxtemp_react(), "max"), cellStats(admraster_maxtemp_react(), "sd"), cellStats(admraster_maxtemp_react(), "skew")),
                             ncol=1,
                             byrow=TRUE)
      rownames(raster_stats) <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      colnames(raster_stats) <- c("Max Temp. Raster Statistics")
      raster_stats <- as.data.frame(raster_stats)
      raster_stats$Variables <- c("Mean", "Minimum", "Maximum", "Standard Deviation", "Skew")
      raster_stats
    })


    # PDB/ADM SELECT/Country Specific ADM Select Map -----------------------------------------
    key <- reactive({
        input$admselect_adm0
    })


    reactive_list <- reactive({
      matched_files <- list()
      for (i in files$table_name) {
          if (length(grep(key(), i, ignore.case = TRUE)) != 0) {
              matched_files <- rlist::list.append(matched_files, i)
          }
      }
      matched_files
    })


    observe({
        updateSelectInput(session, "map_vars", choices = c("None" = "", reactive_list()))
    })


    output$country_specific_map <- renderLeaflet({

      if (input$map_vars == "") {

        leaflet() %>%
          addTiles()

      } else {

        selected_file <- subset(files, table_name == input$map_vars)

        if (selected_file$geometry_type == "MULTIPOLYGON") {

          polygons <- rpostgis::pgGetGeom(con, input$map_vars, geom = selected_file$geom_column)

          leaflet() %>%
            addTiles() %>%
            addPolygons(data = polygons)

        } else if (selected_file$geometry_type == "POINT" | selected_file$geometry_type == "MULTIPOINT") {

          points <- rpostgis::pgGetGeom(con, input$map_vars, geom = selected_file$geom_column)

          leaflet() %>%
            addTiles() %>%
            addMarkers(data = points)

        }
      }
    })



    drawreport_country <- reactive({
      input$drawing_country
    })

    output$drawproject_report <- downloadHandler(
        filename = function() {
          paste('my-report.pdf')
        },

        content = function(file) {
          src <- normalizePath('draw_report.Rmd')

          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'draw_report.Rmd', overwrite = TRUE)

          out <- render('draw_report.Rmd', pdf_document())
          file.rename(out, file)
        }
    )
  
})

