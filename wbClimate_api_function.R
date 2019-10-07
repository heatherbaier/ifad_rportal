library(jsonlite)
library(httr)
library(highcharter)


worldbank_climate_varquery_charts <- function(ensemble, type, var, start, end, model_percentile) {
  
  if (ensemble == 'Choose Variable') {
    base_path <- "http://climatedataapi.worldbank.org/climateweb/rest/v1/country/"
    path <- paste(base_path, type, "/", var,  "/", start, "/", end, "/", 'cpv', sep = '')
    print(path)
    request <- GET(url = path)
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE) %>% 
      data.frame()
    print(df)
    if (type == 'mavg') {
      df <- subset(df, gcm == model_percentile)
      y_list <- as.vector(df[1,3])
      hc <- highchart() %>%
        hc_xAxis(categories = c("January", "February", "March", "April", "May", "June", "July", "August",
                                'September', "October", "November", "December")) %>%
        hc_add_series(name = as.character(model_percentile), data = y_list[[1]])
      return(hc)
    } else if (type == 'annualavg') {
      hc <- highchart() %>%
        hc_xAxis(categories = df$gcm) %>%
        hc_add_series(name = as.character(model_percentile), data = df$annualData) %>%
        hc_chart(type = "column")
      return(hc)
    }
    
  } else if (ensemble == 'Ensemble') {
    base_path <- "http://climatedataapi.worldbank.org/climateweb/rest/v1/country/"
    path <- paste(base_path, type, "/ensemble/", var,  "/", start, "/", end, "/", 'cpv', sep = '')
    print(path)
    request <- GET(url = path)
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE) %>% 
      data.frame()
    print(df)
    if (type == 'mavg') {
      df <- subset(df, percentile == model_percentile)
      y_list <- as.vector(df[1,1])
      hc <- highchart() %>%
        hc_xAxis(categories = c("January", "February", "March", "April", "May", "June", "July", "August",
                                'September', "October", "November", "December")) %>%
        hc_add_series(name = as.character(model_percentile), data = y_list[[1]])
      return(hc)
    } else if (type == 'annualavg') {
      hc <- highchart() %>%
        hc_xAxis(categories = df$percentile) %>%
        hc_add_series(name = as.character(model_percentile), data = df$annualVal) %>%
        hc_chart(type = "column")
      return(hc)
      
      
    }
  }
  
}




query <- worldbank_climate_varquery_charts('Ensemble', 'annualavg', 'pr', 1920, 1939, 90)

query
