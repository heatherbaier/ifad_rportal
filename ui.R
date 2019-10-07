shinyUI(navbarPage(" ", theme = "styles.css",
                   
            tabPanel(h5("Country Context"),
                     
                  mainPanel(
                    
                    column(11, offset = 0,
                           
                           fluidRow(
                             
                               column(12,
                  
                                          selectInput("country", " ", choices = sort(unique(country_names)), width = "100%", selected = "Afghanistan")

                                )
                             
                           ),

                           tabsetPanel(
                               
# Country Context: Demographic Indicators --------------------------------------------------

                               tabPanel(h5("Demographic Indicators"), 
                                        
                                        h1(" "),
                                        
                                        column(12, style = "border: 2px solid #003870; border-radius: 5px;",
                                               
                                               h1(" "),
                                               
                                               h4("World Bank Demographic Indicators", align = "center"),
                                              
                                               radioButtons("dem_indicator", label = NULL, choices = dem_indicators, inline = TRUE),

                                               highchartOutput("dem_indicators_chart") %>% withSpinner(color="#003870"),

                                               h1(" "),
                                               
                                               hr(),
                                               
                                               tags$a(href="http://datatopics.worldbank.org/world-development-indicators/", h5("Data from the World Bank Development Indicators API. For More information about the data click here."))
                                               
                                        )
                                        
                               ),
                               
# Country Context: Agricultural Indicators -------------------------------------------------
  
                               tabPanel(h5("Agricultural Indicators"), 
                                        
                                        h1(" "),
                                        
                                        column(12, style = "border: 2px solid #003870; border-radius: 5px;",
                                               
                                               h1(" "),
                                               
                                               h4("World Bank Agricultural Indicators", align = "center"),
                                               
                                               radioButtons("ag_indicator", label = NULL, choices = ag_indicators, inline = TRUE),
                                               
                                               highchartOutput("ag_indicators_chart") %>% withSpinner(color="#003870"),
                                               
                                               h1(" "),
                                               
                                               hr(),
                                               
                                               tags$a(href="http://datatopics.worldbank.org/world-development-indicators/", h5("Data from the World Bank Development Indicators API. For More information about the data click here."))
                                               
                                        )
                                        
                               ),
                               
# Country Context: Poverty Indicators ------------------------------------------------------
  
                               tabPanel(h5("Poverty Indicators"),
                                        
                                        h1(" "),
                                        
                                        column(12, style = "border: 2px solid #003870; border-radius: 5px;",
                                               
                                               h1(" "),
                                               
                                               h4("World Bank Poverty Indicators", align = "center"),
                                               
                                               radioButtons("pov_indicator", label = NULL, choices = poverty_indicators, inline = TRUE),
                                               
                                               highchartOutput("poverty_indicators_chart") %>% withSpinner(color="#003870"),
                                               
                                               h1(" "),
                                               
                                               hr(),
                                               
                                               tags$a(href="http://datatopics.worldbank.org/world-development-indicators/", h5("Data from the World Bank Development Indicators API. For More information about the data click here."))
                                               
                                        )
                                        
                               ),
                               
# Country Context: Climate Indicators -----------------------------------------------------------------
  
                               tabPanel(h5("Climate Indicators"),
                                        
                                        h1(" "),
                                        
# Country Context: Climate Indicators - Rainfall -------------------------------------------------------
                                        
                                        h3("Mean Precipitation (MM)", align = "center"),
                                        
                                        div(leafletOutput("precip_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        div(highchartOutput("rainfall_plot") %>% withSpinner(color="#003870"), align = "center"),

                                        helpText("About the data:
                                                  Rainfall data derived from the CHIRPS (Climate Hazard Group InfraRed Precipitation with
                                                  Station data) depicts rainfall estimates from 1981- 2018 at a spatial resolution of about
                                                  5-km. Dataset merges satellite imagery with in-situ station data to create gridded rainfall
                                                  time-series for trend analysis and seasonal drought monitoring. This dataset was produced
                                                  with the methodological support provided the IFAD-WFP Climate Analysis Partnership.
                                                  Source of the dataset: Climate Hazard Group of California, Santa Barbara.", align = "justify"),

                                        h1(" "),

                                        h3("Precipitation Coefficient of Variation (MM)", align = "center"),
                                        
                                        div(leafletOutput("precip_cov_map_country") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        helpText("About the data: 
                                                  The CoV of annual precipitation is an index of climatic risk, indicating a likelihood of 
                                                  fluctuations in reservoir storage or crop yield from year to year. The higher the CoV, the 
                                                  more variable the year-to-year (i.e. inter-annual) rainfall of a locality is. The Coefficient 
                                                  Variation is simply the standard deviation divided by the average annual rainfall."),

                                        h1(" "),
                                          
                                        h3("Trend in Precipitation (MM)", align = "center"),
                                        
                                        div(leafletOutput("precip_trend_map_country") %>% withSpinner(color="#003870"), align = "center"),

                                        h1(" "),
                                        
                                        h1(" "),

                                        hr(),
                                        
                                        h1(" "),
                                        
# Country Context: Climate Indicators - NDVI -------------------------------------------------------
                                        
                                        h3("Mean NDVI", align = "center"),
                                        
                                        h1(" "),
                                        
                                        div(leafletOutput("ndvi_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        div(highchartOutput("ndvi_plot") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        h1(" "),
                                        
                                        helpText("About the data: 
                                                  Vegetation Index (NDVI) data reflect both amount and health of vegetation.
                                                  In areas of low rainfall where water is the limiting factor for vegetation growth,
                                                  seasonal NDVI is closely linked to rainfall; this relationship gets progressively
                                                  weaker as seasonal rainfall increases and factors other than water availability
                                                  assume greater importance in controlling vegetation development. The NDVI data in
                                                  use is from the NASA MODIS platforms Terra, which provide global coverage since 2001
                                                  (Terra) at about 1Km resolution with a temporal frequency of 16 day periods. Methodological
                                                  support provided the IFAD-WFP Climate Analysis Partnership Time period: 2001-2018. Source
                                                  NASA LP DAAC at the USGS EROS Centre", align = "justify"),
                                        
                                        h1(" "),

                                        h3("NDVI Coefficient of Variation", align = "center"),
                                        
                                        h1(" "),
                                        
                                        div(leafletOutput("ndvi_cov_country_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        h1(" "),
                                        
                                        helpText("About the data: 
                                                  Coefficient of variation (CoV) can be used to compare the amount of variation in different 
                                                  sets of sample data. NDVI CoV immages are generated by computing for each pixel the standard 
                                                  deviation of the set of individual NDVI values and dividing this by the mean of these values.",
                                                  align = "justify"),

                                                  h1(" "),

                                        h3("Trend in NDVI", align = "center"),
                                        
                                        h1(" "),
                                        
                                        div(leafletOutput("ndvi_trend_country_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        h1(" "),
                                        
# Country Context: Climate Indicators - Minimum Temperature ----------------------------------------------------
  
                                        h1(" "),
                                        
                                        hr(),
                                        
                                        h1(" "),
                                        
                                        h3("Mean Annual Minimum Temperature (°C)", align = "center"),
                                        
                                        div(leafletOutput("mintemp_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        div(highchartOutput("mintemp_plot") %>% withSpinner(color="#003870"), align = "center"),

                                        helpText("About the data: 
                                                  Global mean min temperature data is based on the TerraClimate
                                                  dataset of monthly climate for global terrestrial surfaces from 1958- 2017 at
                                                  a spatial resolution of about 5-km. This data set was produced with the
                                                  methodological support provided the IFAD-WFP Climate Analysis Partnership.
                                                  Mean minimum temperature (°C) can be defined as the average daily minimum
                                                  air temperature, for each month and as an annual statistic, calculated for
                                                  each of the years of interest.
                                                  Source of data: University of Idaho", align = "justify"),
                                        
                                        h1(" "),

                                        h3("Minimum Temperature Standard Deviation  (°C)", align = "center"),
                                        
                                        leafletOutput('mintemp_std_map_country'),

                                        h3("Trend in Annual Minimum Temperature (°C)", align = "center"),
                                        
                                        leafletOutput('mintemp_trend_map_country'),
                                                                                
                                        h1(" "),
                                        
# Country Context: Climate Indicators - Maximum Temperature --------------------------------------------
  
                                        h1(" "),
                                        
                                        hr(),
                                        
                                        h1(" "),
                                        
                                        h3("Mean Annual Maximum Temperature (°C)", align = "center"),
                                        
                                        div(leafletOutput("maxtemp_map") %>% withSpinner(color="#003870"), align = "center"),
                                        
                                        div(highchartOutput("maxtemp_plot") %>% withSpinner(color="#003870"), align = "center"),

                                        helpText("About the data: 
                                                  Global mean max temperature data is based on the TerraClimate
                                                  dataset of monthly climate for global terrestrial surfaces from 1958- 2017
                                                  at a spatial resolution of about 5-km. This data set was produced with the
                                                  methodological support provided the IFAD-WFP Climate Analysis Partnership.
                                                  Mean maximum temperature (°C) can be defined as the average daily maximum
                                                  air temperature, for each month and as an annual statistic, calculated for
                                                  each of the years of interest.
                                                  Source of data: University of Idaho", align = 'justify'),

                                        h3("Maximum Temperature Standard Deviation (°C)", align = "center"),
                                        
                                        leafletOutput('maxtemp_std_map_country'),
                                        
                                        h1(" "),

                                        h3("Trend in Annual Maximum Temperature (°C)", align = "center"),
                                        
                                        leafletOutput('maxtemp_trend_map_country'),

                                        h1(" "),
                                        
# Country Context: Climate Indicators - Download Options --------------------------------------------------------
  
                                        column(12, style = "border: 2px solid #003870; border-radius: 5px; background-color: #A6ABBD;",
                                               
                                               h4(tags$u("Download Options"), align = "center"),
                                               
                                               div(textOutput('download_error'), align = "center"),
                                               
                                               h5("Reports", align = "center"),
                                               
                                               div(downloadButton('downloadReport', h5("Download PDF Report")),
                                                   
                                                   downloadButton('downloadppt', h5("Download Presentation")),
                                                   
                                                   downloadButton('downloadword', h5("Download Word Document")), align = "center"),
                                               
                                               h5("Data", align = "center"),
                                               
                                               div(downloadButton('downloadclimatecsv', h5("Download Climate CSV")), 
                                                   
                                                   align = "center"),
                                               
                                               h1(" ")
                                               
                                        )
                                        
                                  )
                               
                            )
                           
                    ),
                    
# Country Context: Sidebar Project Map -----------------------------------------------------
                    column(width = 1,
                           
                           absolutePanel(top = '5px', left = '80px', right = NULL, bottom = NULL,
                                         
                                         width = '500px', height = NULL, draggable = TRUE, fixed = FALSE,
                                         
                                         cursor = "auto",
                                         
                                         selectInput('project', ' ', choices = c("All Projects" = ""), width = '450px')
                                         
                                         ),

                           leafletOutput("sidebar_map", width = "600px", height = '900px')
                           
                    )
                    
                )

            ),

            tabPanel(h5("Country Vulnerability"),

                     selectInput('gain_country', "Choose Country", choices = (gain$Name)),
                     
                     tabsetPanel(
                       
# Country Vulnerability: Gain ---------------------------------------------------
                       
                       tabPanel("Gain",
                                
                                h1(" "),
                                
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    
                                    h4("ND-GAIN Score", align = "center"),
                                    
                                    radioButtons('gain_choices', " ", choices = c("Individual Year", "Change Over Time", "Both"), selected = "Individual Year"),
                                    
                                    hr(),
                                    
                                    helpText("ND-GAIN assesses the vulnerability of a country by considering six life-supporting sectors: food, water, health, ecosystem services, 
                                             human habitat and infrastructure. Each sector is in turn represented by six indicators that represent three cross-cutting components: 
                                             the exposure of the sector to climate-related or climate-exacerbated hazards; the sensitivity of that sector to the impacts of the 
                                             hazard and the adaptive capacity of the sector to cope or adapt to these impacts."),
                                    
                                    helpText("The ND-Gain score is computed as (Readiness - Vulnerability + 1) * 50. Readiness is defined as the readiness to make 
                                             effective use of investments for adaptation actions thanks to a safe and efficient business environment. Vulnerability
                                             is defined as the propensity or predisposition of human societies to be negatively impacted by climate hazards.")
                                    
                                  ),
                                  
                                  mainPanel(
                                    
                                    highchartOutput('nd_gain_chart')
                                    
                                  )
                                  
                                )
                                
                       ),
                       
# Country Vulnerability: Indicators ---------------------------------------------------

                       tabPanel("Indicators",
                                
                                h1(" "),
                                
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    
                                    radioButtons('ndindicator_choices', " ", choices = c("Individual Year", "Change Over Time", "Both"), selected = "Individual Year")
                                    
                                  ),
                                  
                                  mainPanel(
                                    
                                    highchartOutput('nd_indicator_chart')
                                    
                                  )
                                  
                            )
                                
                       ),

# Country Vulnerability: Readiness ---------------------------------------------------

                       tabPanel("Readiness"),

# Country Vulnerability: Trends ---------------------------------------------------
                       
                       tabPanel("Trends"),

# Country Vulnerability: Vulnerability ---------------------------------------------------
                       
                       tabPanel("Vulnerability")
                       
                     )
                     
                     ),

# ADM Select Project Creation Map ---------------------------------------------------

            tabPanel(h5("Targeting"), value = 2,

                     tabsetPanel(

                       tabPanel("Project Design",

                                # column(12, offset = 0, style = "border: 2px solid #003870; background-color: #003870; color: #ffffff; padding: 5px;",
                                #
                                #        div(h3("Project Drawing Board"), align = "center")
                                #
                                # ),

                                h1(" "),

                                sidebarLayout(

                                  sidebarPanel(

                                    selectInput("drawing_country", h5("Choose Country"), choices = sort(unique(country_names)), width = "100%", selected = "Afghanistan"),

                                    selectInput('one_or_overlay', h5("Map one Raster or Calculate Multiple Rasters?"), choices = c("Map one Raster", "Overlay All Rasters", "Choose Rasters to Overlay")),

                                    conditionalPanel("input.one_or_overlay == 'Map one Raster'",

                                                     selectInput('raster_choice', h5("Choose Climate Raster"), choices = c("Rainfall", "NDVI", "Minimum Temperature", "Maximum Temperature"))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Map one Raster' & input.raster_choice == 'Rainfall'",

                                                     sliderInput('con_rainfall', h5("Select The Range of Rainfall (in MM) appropriate for your project"), min = 0,

                                                                 max = 130, value = c(0, 130))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Map one Raster' & input.raster_choice == 'NDVI'",

                                                     sliderInput('con_ndvi', h5("Select The Range of NDVI appropriate for your project"), min = -2000,

                                                                 max = 9686, value = c(-2000, 9686))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Map one Raster' & input.raster_choice == 'Minimum Temperature'",

                                                     sliderInput('con_mintemp', h5("Select The Range of Minimum Temperature appropriate for your project"), min = -4,

                                                                 max = 23, value = c(-4, 23))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Map one Raster' & input.raster_choice == 'Maximum Temperature'",

                                                     sliderInput('con_maxtemp', h5("Select The Range of Minimum Temperature appropriate for your project"), min = 7,

                                                                 max = 36, value = c(7, 36))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Overlay All Rasters'",

                                                     sliderInput('overlay_rainfall', h5("Select The Range of Rainfall (in MM) appropriate for your project"), min = 0,

                                                                 max = 130, value = c(0, 130)),

                                                     sliderInput('overlay_ndvi', h5("Select The Range of NDVI appropriate for your project"), min = -2000,

                                                                 max = 9686, value = c(-2000, 9686)),

                                                     sliderInput('overlay_mintemp', h5("Select The Range of Minimum Temperature appropriate for your project"), min = -4,

                                                                 max = 23, value = c(-4, 23)),

                                                     sliderInput('overlay_maxtemp', h5("Select The Range of Maximum Temperature appropriate for your project"), min = 7,

                                                                 max = 36, value = c(7, 36))

                                    ),

                                    conditionalPanel("input.one_or_overlay == 'Choose Rasters to Overlay'",

                                                     checkboxGroupInput('choose_rasters', " ", choices = c("Rainfall", "NDVI", "Minimum Temperature", "Maximum Temperature")),

                                                     conditionalPanel("input.choose_rasters.indexOf('Rainfall') != -1",

                                                                      sliderInput('choose_rainfall', h5("Select The Range of Rainfall (in MM) appropriate for your project"), min = 0,

                                                                                             max = 130, value = c(0, 130))

                                                                      ),

                                                     conditionalPanel("input.choose_rasters.indexOf('NDVI') != -1",

                                                                      sliderInput('choose_ndvi', h5("Select The Range of NDVI appropriate for your project"), min = -2000,

                                                                                  max = 9686, value = c(-2000, 9686))

                                                     ),

                                                     conditionalPanel("input.choose_rasters.indexOf('Minimum Temperature') != -1",

                                                                      sliderInput('choose_mintemp', h5("Select The Range of Minimum Temperature appropriate for your project"), min = -4,

                                                                                  max = 23, value = c(-4, 23))

                                                     ),

                                                     conditionalPanel("input.choose_rasters.indexOf('Maximum Temperature') != -1",

                                                                      sliderInput('choose_maxtemp', h5("Select The Range of Maximum Temperature appropriate for your project"), min = 7,

                                                                                  max = 36, value = c(7, 36))

                                                     )

                                    ),

                                    hr(),

                                    helpText("The raster con tool allows you to select the range of values in each climate raster that would be
                                              suitable for your project's sucess. Set the first circle over the minimum value (i.e. Minimum
                                              acceptable rainfall (in MM) for your project's specifiations and set the second circle over the maximum value.
                                              The raster on the map to the left will show you areas that fit your specifiations in order to identify locations
                                              that might be most suitable for your project."),

                                    hr(),

                                    h4("Estimated Number of Beneficiaries"),

                                    textOutput('est_beneficiaries')

                                    ),

                                    mainPanel(

                                          leafletOutput('mymap', height = "600px") %>% withSpinner(color="#003870")

                                    )

                                ),

# Rainfall ----------------------------------------------------------------

                                column(12,

                                       div(h3("Rainfall"), align = "center"),

                                       fluidRow(

                                           column(4,

                                                  leafletOutput("rasterextract_map_rain") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  metricsgraphicsOutput("rasterextract_hist_rain") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  # textOutput('draw_rainfall_table') %>% withSpinner(color="#003870")

                                                  tableOutput('draw_rainfall_table') %>% withSpinner(color="#003870")

                                           )

                                       ),

                                       hr(),

# NDVI --------------------------------------------------------------------

                                       div(h3("NDVI"), align = "center"),

                                       fluidRow(

                                           column(4,

                                                  leafletOutput("rasterextract_map_ndvi") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  metricsgraphicsOutput("rasterextract_hist_ndvi") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  tableOutput('draw_ndvi_table') %>% withSpinner(color="#003870")

                                           )

                                       ),

                                       h1(" "),

                                       hr(),

# Maximum Temperature -----------------------------------------------------

                                       div(h3("Maximum Temperature"), align = "center"),

                                       fluidRow(

                                           column(4,

                                                  leafletOutput("rasterextract_map_maxtemp") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  metricsgraphicsOutput("rasterextract_hist_maxtemp") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  tableOutput('draw_maxtemp_table') %>% withSpinner(color="#003870")

                                           )

                                       ),

                                       h1(" "),

                                       hr(),

# Minimum Temperature ----------------------------------------------------

                                       div(h3("Minimum Temperature"), align = "center"),

                                       fluidRow(

                                           column(4,

                                                  leafletOutput("rasterextract_map_mintemp") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  metricsgraphicsOutput("rasterextract_hist_mintemp") %>% withSpinner(color="#003870")

                                           ),

                                           column(4,

                                                  tableOutput('draw_mintemp_table') %>% withSpinner(color="#003870")

                                           )

                                       ),

                                       hr(),

                                       # column(12, offset = 0, style = "border: 2px solid #003870; background-color: #003870; color: #ffffff; padding: 5px;",
                                       #
                                       #        div(h3("Compare New Boundaries With Ongoing Projects"), align = "center")),

                                       div(h3(tags$u("Compare New Boundaries With Ongoing Projects")), align = "center"),

                                       column(1, h1(" ")),

                                       column(3,

                                              selectInput('compareone_country', "Select Project Country", choices = c("None" = "", sort(unique(country_names))), selected = "Afghanistan"),

                                              conditionalPanel('input.compareone_country',

                                                               selectInput('compareone_project', "Select Project", choices = c("None" = ""))

                                              )

                                       ),

                                       column(8,

                                              leafletOutput('compare_project_map') %>% withSpinner(color="#003870"),

                                              h1(" "),

                                              formattableOutput("compare_project_table", width = "900px", height = '500px') %>% withSpinner(color="#003870")

                                       ),

                                       hr(),

                                       column(10, offset = 1, style = "border: 2px solid #003870; border-radius: 5px; background-color: #A6ABBD;",

                                              div(h4(tags$u("Download Options")), align = "center"),

                                              div(downloadButton('downloadData', 'Download Shapefile'), downloadButton('drawproject_report', 'Download PDF'), align = "center")

                                       )

                               )

                       ),

# Tab: Upload Project Shapefile -------------------------------------------

                       # tabPanel("Upload Project Shapefile"),

# Tab: Use Current ADM Boundaries -----------------------------------------

                       tabPanel("Use Current ADM Boundaries",

                                h1(" "),

                                sidebarLayout(

                                    sidebarPanel(

                                        selectInput('admselect_adm0', h5('Select Country'), choices = c("All Countries" = "", sort(unique(country_names))), selected = "Afghanistan"),

                                        conditionalPanel("input.admselect_adm0",

                                                         selectInput('admselect_adm1', h5("Select ADM 1"), choices = c("All ADM1" = ""))

                                        ),

                                        conditionalPanel("input.admselect_adm1",

                                                         selectInput('admselect_adm2', h5("Select ADM 2"), choices = c("All ADM2" = ""))

                                        ),

                                        hr(),

                                        h4("Estimated Number of Beneficiaries"),

                                        textOutput('est_adm_beneficiaries')

                                    ),

                                    mainPanel(

                                        leafletOutput('admselect_map', height = "600px") %>% withSpinner(color="#003870")

                                        )

                                    ),

                                column(12,

                                        hr(),

                                        # h3(tags$u("Proposed Project Statistics"), align = "center"),

# ADM Select Raster Maps -------------------------------------------------------

                                        div(h3("Rainfall"), align = "center"),

                                        fluidRow(

                                            column(4,

                                                   leafletOutput("adm_rastermap_rain") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   metricsgraphicsOutput("adm_hist_rain") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   tableOutput("adm_table_rain") %>% withSpinner(color="#003870")

                                            )

                                        ),

                                        h1(" "),

                                        div(h4("Maximum Temperature"), align = "center"),

                                        fluidRow(

                                            column(4,

                                                   leafletOutput("adm_rastermap_ndvi") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   metricsgraphicsOutput("adm_hist_ndvi") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   tableOutput("adm_table_ndvi") %>% withSpinner(color="#003870")

                                            )

                                        ),

                                        h1(" "),

# ADM Select Raster Histograms ----------------------------------------------------

                                        div(h3("Maximum Temperature"), align = "center"),

                                        fluidRow(

                                            column(4,

                                                   leafletOutput("adm_rastermap_maxtemp") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   metricsgraphicsOutput("adm_hist_maxtemp") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   tableOutput("adm_table_maxtemp") %>% withSpinner(color="#003870")

                                            )

                                        ),

                                        h1(" "),

                                        div(h4("Minimum Temperature"), align = "center"),

                                        fluidRow(

                                            column(4,

                                                   leafletOutput("adm_rastermap_mintemp") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   metricsgraphicsOutput("adm_hist_mintemp") %>% withSpinner(color="#003870")

                                            ),

                                            column(4,

                                                   tableOutput("adm_table_mintemp") %>% withSpinner(color="#003870")

                                            )

                                        ),

# ADM Select Country Specific Postgres Map -------------------------------------------

                                        hr(),

                                        div(h3("Available Country Specific Data"), align = "center"),

                                        selectInput('map_vars', "Map Variables", choices = c("Select a country to find associated shapefiles" = "")),

                                        leafletOutput('country_specific_map') %>% withSpinner(color="#003870"),

                                        h1(" ")

                                    )

                              )

                        )

                  ),

                  tabPanel(h5("Suitability Modeling"),

                              h1(" "),

                              sidebarLayout(

                                    sidebarPanel(

                                      selectInput('suitability_country', h5("Step 1: Choose the country you would like to design a project in:"), choices = country_names),

                                      h5("Step 2: Set the geospatial parameters necessary for your project. For example, if you would like to area that fall within at least .2 meters of a

                                         primary road, select the Primary Roads checkbox below and set the buffer size to .2, then click add Primary Roads to add it to your drawing board."),


                                                       checkboxInput('linear_osm_water', "Waterways"),

                                                       conditionalPanel("input.linear_osm_water",

                                                                        checkboxGroupInput('linear_water_choices', "Select Appropriate Linear Water Types", choices = c("None")),

                                                                        radioButtons('linear_water_buffer_size', "Optional: Set water buffer size (in meters)", choices = c(0, .05, .1, .2, .3, .4), inline = TRUE),

                                                                        actionButton('add_linear_water', "Add Water"),

                                                                        actionButton('remove_linear_water', "Remove Water")

                                                       ),

                                                       checkboxInput('polygonal_osm_water', "Waterbodies"),

                                                       conditionalPanel("input.polygonal_osm_water",

                                                                        checkboxGroupInput('polygonal_water_choices', "Select Appropriate Water Types", choices = c("None")),

                                                                        # numericInput('polygonal_water_buffer_size', "Optional: Set water buffer size (in meters)", value = 0),

                                                                        radioButtons('polygonal_water_buffer_size', "Optional: Set water buffer size (in meters)", choices = c(0, .05, .1, .2, .3, .4), inline = TRUE),

                                                                        actionButton('add_polygonal_water', "Add Water"),

                                                                        actionButton('remove_polygonal_water', "Remove Water")

                                                       ),

                                                       checkboxInput('osm_proads', "Primary Roads"),

                                                       conditionalPanel("input.osm_proads",

                                                                        # numericInput('praods_buffer_size', "Optional: Set primary roads buffer size (in meters)", value = 0),

                                                                        radioButtons('praods_buffer_size', "Optional: Set primary roads buffer size (in meters)", choices = c(0, .05, .1, .2, .3, .4), inline = TRUE),

                                                                        actionButton('add_proads', "Add Primary Roads"),

                                                                        actionButton('remove_proads', "Remove Primary Roads")

                                                       ),

                                                       checkboxInput('osm_sroads', "Seconday Roads"),

                                                       conditionalPanel("input.osm_sroads",

                                                                        # numericInput('sraods_buffer_size', "Optional: Set secondary roads buffer size (in meters)", value = 0),

                                                                        radioButtons('sraods_buffer_size', "Optional: Set secondary roads buffer size (in meters)", choices = c(0, .05, .1, .2, .3, .4), inline = TRUE),

                                                                        actionButton('add_sroads', "Add Secondary Roads"),

                                                                        actionButton('remove_sroads', "Remove Secondary Roads")

                                                       ),

                                                       checkboxInput('osm_troads', "Tertiary Roads"),

                                                       conditionalPanel("input.osm_troads",

                                                                        # numericInput('traods_buffer_size', "Optional: Set tertiary roads buffer size (in meters)", value = 0),

                                                                        radioButtons('traods_buffer_size', "Optional: Set tertiary roads buffer size (in meters)", choices = c(0, .05, .1, .2, .3, .4), inline = TRUE),

                                                                        actionButton('add_troads', "Add Tertiary Roads"),

                                                                        actionButton('remove_troads', "Remove Tertiary Roads")

                                                       ),

                                                      checkboxInput('eco_regions', "Eco Regions"),

                                                      conditionalPanel("input.eco_regions",

                                                                       checkboxGroupInput('eco_region_choices', label = NULL, choices = c("None")),

                                                                       actionButton('add_ecoregions', "Add Eco Regions"),

                                                                       actionButton('remove_ecoregions', "Remove Eco Regions")

                                                      ),

                                                      checkboxInput('climatic_zones', "Climatic Zones"),

                                                      conditionalPanel("input.climatic_zones",

                                                                       checkboxGroupInput('climatic_zone_choices', label = NULL, choices = c("None")),

                                                                       actionButton('add_climaticzones', "Add Climatic Zones"),

                                                                       actionButton('remove_climaticzones', "Remove Climatic Zones")

                                                      ),

                                                      checkboxInput('country_spec_data', "Country Specific Data from GeoNode"),

                                                      conditionalPanel("input.country_spec_data",

                                                                       checkboxGroupInput('country_spec_data_choices', label = NULL, choices = c("No data in GeoNode for selected country")),

                                                                       radioButtons('include_cs', "Include or exclude data in model?", choices = c("Include", "Exclude"), selected = "Include"),

                                                                       actionButton('add_cs', "Add Data"),

                                                                       actionButton('remove_cs', "Remove Data")

                                                      ),

                                                       h5("Step 3: Set the climate parameters necessary for your project. For example, if you would like to design a project in areas with a maximum rainfall of 115 MM and a

                                                          Maximum Temperature of 32, select the Overlay Climate Rasters checkbox, then click on Rainfall and set the slider to 115, then click on Maximum Temperature and set

                                                          the slider to 32."),

                                                       checkboxInput('climate_overlay', "Overlay Climate Rasters"),

                                                       conditionalPanel('input.climate_overlay',

                                                                        checkboxGroupInput('suit_clim_vars', "Climate Rasters", choices = c("Rainfall", "NDVI", "Minimum Temperature", "Maximum Temperature")),

                                                                        conditionalPanel("input.suit_clim_vars.indexOf('Rainfall') != -1",

                                                                                         sliderInput('choose_rainfall', h5("Select The Range of Rainfall (in MM) appropriate for your project"), min = 0,

                                                                                                     max = 130, value = c(0, 130))

                                                                        ),

                                                                        conditionalPanel("input.suit_clim_vars.indexOf('NDVI') != -1",

                                                                                         sliderInput('choose_ndvi', h5("Select The Range of NDVI appropriate for your project"), min = -2000,

                                                                                                     max = 9686, value = c(-2000, 9686))

                                                                        ),

                                                                        conditionalPanel("input.suit_clim_vars.indexOf('Minimum Temperature') != -1",

                                                                                         sliderInput('choose_mintemp', h5("Select The Range of Minimum Temperature appropriate for your project"), min = -4,

                                                                                                     max = 23, value = c(-4, 23))

                                                                        ),

                                                                        conditionalPanel("input.suit_clim_vars.indexOf('Maximum Temperature') != -1",

                                                                                         sliderInput('choose_maxtemp', h5("Select The Range of Maximum Temperature appropriate for your project"), min = 7,

                                                                                                     max = 36, value = c(7, 36))

                                                                        ),

                                                                        actionButton('add_rasters', "Add Rasters"),

                                                                        actionButton('remove_rasters', "Remove Rasters")

                                                       ),

                                                       hr(),

                                                       h5("Step 4: Once you are finished setting the parameters of your project, click the Calculate Suitability below. If you want to choose a new country and

                                                          redesign your model, click Reset Suitability Model and input your new parameters."),

                                                       actionButton('calc_suitability', "Calculate Suitability"),

                                                       actionButton('reset_suitability', "Reset Suitability Model")

                                      #)

                                    ),

                                    mainPanel(

                                          leafletOutput('suitability_map', height = "700px")

                                    )

                              ),

                              hr(),

                              h3('Suitability Analysis', align = "center", style = "border: 2px solid #003870; background-color: #003870; border-radius: 5px; padding: 10px; height: 50px; color: #ffffff;"),

                                         sidebarLayout(

                                           sidebarPanel(

                                             h4("Extracted Polygons Suitability Map")

                                           ),

                                           mainPanel(

                                             leafletOutput('suit_polys_extract_map')

                                           )

                                         ),

                                        h1(" "),

                                        hr(),

                                        h1(" "),

                                        h4("Profiles for Individual Extracted Suitable Polygons", align = "center"),

                                         selectInput('extracted_polygon', "Choose Extracted Polygon", choices = c("None"), width = "100%"),

                                         tabsetPanel(

                                              tabPanel("Climate Profile",

                                                                                               div(h4("Rainfall"), align = "center"),

                                                                                               fluidRow(

                                                                                                   column(4,

                                                                                                          leafletOutput("poly_extract_map_rain") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          metricsgraphicsOutput("poly_extract_hist_rain") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          tableOutput("poly_extract_table_rain") %>% withSpinner(color="#003870")

                                                                                                   )

                                                                                               ),

                                                                                               h1(" "),

                                                                                               div(h4("NDVI"), align = "center"),

                                                                                               fluidRow(

                                                                                                   column(4,

                                                                                                          leafletOutput("poly_extract_map_ndvi") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          metricsgraphicsOutput("poly_extract_hist_ndvi") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          tableOutput("poly_extract_table_ndvi") %>% withSpinner(color="#003870")

                                                                                                   )

                                                                                               ),

                                                                                               h1(" "),

# ADM Select Raster Histograms ----------------------------------------------------

                                                                                               div(h4("Maximum Temperature"), align = "center"),

                                                                                               fluidRow(

                                                                                                   column(4,

                                                                                                          leafletOutput("poly_extract_map_maxtemp") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          metricsgraphicsOutput("poly_extract_hist_maxtemp") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          tableOutput("poly_extract_table_maxtemp") %>% withSpinner(color="#003870")

                                                                                                   )

                                                                                               ),

                                                                                               h1(" "),

                                                                                               div(h4("Minimum Temperature"), align = "center"),

                                                                                               fluidRow(

                                                                                                   column(4,

                                                                                                          leafletOutput("poly_extract_map_mintemp") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          metricsgraphicsOutput("poly_extract_hist_mintemp") %>% withSpinner(color="#003870")

                                                                                                   ),

                                                                                                   column(4,

                                                                                                          tableOutput("poly_extract_table_mintemp") %>% withSpinner(color="#003870")

                                                                                                   )

                                                                                               )


                                                       ),

                                              tabPanel("Social and Demographic Profile",

                                                       h3("Estimated Benificiaries", align = "center"),

                                                       sidebarLayout(

                                                         sidebarPanel(

                                                           textOutput('extpolys_est_ben_text')

                                                         ),

                                                         mainPanel(

                                                                leafletOutput('extpolys_est_ben_map')

                                                                   )

                                                       ),

                                                       h1(" "),

                                                       hr(),

                                                       h1(" "),

# Extracted Polygons Child Stunting -----------------------------------------

                                                       h3("Child Stunting", align = "center"),

                                                       sidebarLayout(

                                                         sidebarPanel(

                                                           radioButtons('stunting_calc_choice_polys', "Display data for one year or calculate change over time?", choices = c("Select one year", "Calculate change over time")),

                                                           conditionalPanel("input.stunting_calc_choice_polys == 'Select one year'",

                                                                            selectInput('stunting_year_polys', "Year", choices = c(2000:2015))

                                                           ),

                                                           conditionalPanel("input.stunting_calc_choice_polys == 'Calculate change over time'",

                                                                            selectInput('stunting_change_year1_polys', "Starting Year", choices = c(2000:2015)),

                                                                            selectInput('stunting_change_year2_polys', "End Year", choices = c(2001:2015))

                                                           ),

                                                           hr(),

                                                           h4("Stunting Statistics"),

                                                           conditionalPanel("input.stunting_calc_choice_polys == 'Select one year'",

                                                                            textOutput('stunting_avg_oneyear_polys'),

                                                                            textOutput('stunting_sum_oneyear_polys')


                                                           ),

                                                           conditionalPanel("input.stunting_calc_choice_polys == 'Calculate change over time'",

                                                                            textOutput('stunting_avg_change_polys')

                                                           )

                                                         ),

                                                         mainPanel(

                                                           leafletOutput('stunting_map_polys')

                                                         )

                                                       ),

h1(" "),

hr(),

h1(" "),

# Extracted Polygons Children Under 5 -------------------------------------

h3("Children Under 5", align = "center"),

sidebarLayout(

  sidebarPanel(

    radioButtons('child5_calc_choice_polys', "Display data for one year or calculate change over time?", choices = c("Select one year", "Calculate change over time")),

    conditionalPanel("input.child5_calc_choice_polys == 'Select one year'",

                     selectInput('child5_year_polys', "Year", choices = c(2000, 2005, 2010, 2015))

    ),

    conditionalPanel("input.child5_calc_choice_polys == 'Calculate change over time'",

                     selectInput('child5_change_year1_polys', "Starting Year", choices = c(2000, 2005, 2010, 2015)),

                     selectInput('child5_change_year2_polys', "End Year", choices = c(2005, 2010, 2015))

    ),

    hr(),

    h4("Children Under 5 Statistics"),

    conditionalPanel("input.child5_calc_choice_polys == 'Select one year'",

                     textOutput('child5_avg_oneyear_polys'),

                     textOutput('child5_sum_oneyear_polys')


    ),

    conditionalPanel("input.child5_calc_choice_polys == 'Calculate change over time'",

                     textOutput('child5_avg_change_polys')

    )

  ),

  mainPanel(

    leafletOutput('child5_map_polys')

  )

),

h1(" "),

hr(),

h1(" "),

# Extracted Polygons Female Education 15-49 -------------------------------

h3("Mean years of Education for females aged 15-49", align = "center"),

sidebarLayout(

  sidebarPanel(

    radioButtons('fed15_calc_choice_polys', "Display data for one year or calculate change over time?", choices = c("Select one year", "Calculate change over time")),

    conditionalPanel("input.fed15_calc_choice_polys == 'Select one year'",

                     selectInput('fed15_year_polys', "Year", choices = c(2000:2014))

    ),

    conditionalPanel("input.fed15_calc_choice_polys == 'Calculate change over time'",

                     selectInput('fed15_change_year1_polys', "Starting Year", choices = c(2000:2014)),

                     selectInput('fed15_change_year2_polys', "End Year", choices = c(2001:2014))

    ),

    hr(),

    h4("Mean years of Education for females aged 15-49 Statistics"),

    conditionalPanel("input.fed15_calc_choice_polys == 'Select one year'",

                     textOutput('fed15_avg_oneyear_polys'),

                     textOutput('fed15_sum_oneyear_polys')


    ),

    conditionalPanel("input.fed15_calc_choice_polys == 'Calculate change over time'",

                     textOutput('fed15_avg_change_polys')

    )

  ),

  mainPanel(

    leafletOutput('fed15_map_polys')

  )

),

h1(" "),

hr(),

h1(" "),

# Extracted Polygons Female Education 20 - 24-------------------------------

h3("Mean years of Education for females aged 20 - 24", align = "center"),

sidebarLayout(

  sidebarPanel(

    radioButtons('fed20_calc_choice_polys', "Display data for one year or calculate change over time?", choices = c("Select one year", "Calculate change over time")),

    conditionalPanel("input.fed20_calc_choice_polys == 'Select one year'",

                     selectInput('fed20_year_polys', "Year", choices = c(2000:2015))

    ),

    conditionalPanel("input.fed20_calc_choice_polys == 'Calculate change over time'",

                     selectInput('fed20_change_year1_polys', "Starting Year", choices = c(2000:2015)),

                     selectInput('fed20_change_year2_polys', "End Year", choices = c(2001:2015))

    ),

    hr(),

    h4("Mean years of Education for females aged 15-49 Statistics"),

    conditionalPanel("input.fed20_calc_choice_polys == 'Select one year'",

                     textOutput('fed20_avg_oneyear_polys'),

                     textOutput('fed20_sum_oneyear_polys')


    ),

    conditionalPanel("input.fed20_calc_choice_polys == 'Calculate change over time'",

                     textOutput('fed20_avg_change_polys')

    )

  ),

  mainPanel(

    leafletOutput('fed20_map_polys')

  )

)


     ),

tabPanel("Land Use Profile",

         fluidRow(

           column(6,

                  h4("Land Cover Map"),

                  leafletOutput('poly_extract_lc_map')

                  ),

           column(6,

                  h4("Land cover distribution within extracted polygon"),

                  plotlyOutput('poly_extract_lc_pie')

                  )

            )

         )

                                         ),

                              column(10, offset = 1, style = "border: 2px solid #003870; border-radius: 5px; background-color: #A6ABBD;",
                                     
                                     div(h4(tags$u("Download Options")), align = "center"),
                                     
                                     div(downloadButton('download_suitable_polygons', 'Download Suitable Polygons Shapefile'), align = "center")
                                     
                              )
                              
                      )
                    
              )

      )

