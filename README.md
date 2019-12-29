# IFAD R Portal
Data visualization and suitability modeling platform

## File Structure:

**ui.R:** All of the code for the user interfacce. Everything in here is connected to outputs in the server file 
through unique output ID's

**server.R:** All of the code that actually processes the data and creates each output.

**report.Rmd:** Creates the PDF for the climate indicators at a country level.)

**presentation.Rmd:** Creates the PowerPoint for the climate indicators at a country level. The main differnce between this and the PDF and Word Document .Rmd's is the output format and a few stylistic things to make sure the pages/slides break appropriatley.

**document.Rmd:** Creates the Word Document for the climate indicators at a country level.

**drawReport.Rmd:** Creates a very bare bones climate report for hand drawn polygons.

## Overall Design

**Country Context:** This is the orginal tab that we have been working with since the beginning. The sidebar map shows ongoing projects in the selected country, the first three tabs are World Bank Development Indicators. It now has rasters from a Google Bucket for Mean Precipitation (MM), Precipitation Coefficient of Variation (%), Trend in Precipitation (MM), Mean NDVI, NDVI Coefficient of Variation (%), Trend in NDVI, Mean Annual Minimum Temperature (°C), Standard Deviation in Minimum Temperature, Trend in Minimum Temperature, Mean Annual Maximum Temperature (°C), Standard Deviation in Maximum Temperature, Trend in Maximum Temperature (MM). It has download options for PDF, Word, Powerpoint and CSV but all of these need to be updates (except the CSV).

**Country Vulnerability:** Foundationof ND-Gain data visualizatin tab.

**Targeting:** Environment, Climate, Gender and Social Inclusion targeting tab.

**Suitability Modeling:** A solid start to a dyanic suitability modeling tool.

## Database Connections: 
 
**World Bank Development Indicators API:** I use an R package (WDI) that’s already been written to query this API.

    ag_data <- WDI(indicator = input$ag_indicator, country = country_iso2$Key, start=2000, end=2017)


**World Bank Climate API:** Temporarily removed from the portal. I wrote the API query function to query the API and also produce graphs. The R script is titled wbClimate_api_function.R

## Functions

**Raster Con:** There was no existing/reliable function to con a raster (convert it to boolean format) so I wrote a pretty simple one on my own. The function is used in the suitability modeling to identify areas that match user inputted parameters (i.e. max rainfall of 30mm).  

    raster_con <- function(min_input, max_input, raster, clip_extent, resample_raster = NULL) {
        clipped_raster <- raster::crop(raster, extent(clip_extent))
        relcass_df <- c(min_input, max_input, 1)
        reclass_maxtrix <- matrix(relcass_df,
                                  ncol = 3,
                                  byrow = TRUE)
        raster_con <- raster::reclassify(clipped_raster,
                                         reclass_maxtrix)
        raster_con[raster_con < 1] <- NA
        raster_con[raster_con > 1] <- NA

        if (is.null(resample_raster) == FALSE) {
          raster_con <- raster::resample(raster_con, resample_raster)
        }

        return(raster_con)
    }
    

