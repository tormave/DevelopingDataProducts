library(shiny)
library(dplyr)
library(ggplot2)

loadFile <- function(url, destFile, type = "CSV", ...) {
  data = NULL
  if (!file.exists(destFile))
    download.file(url, destFile, method = "curl")
  if (type == "CSV")
    data = read.csv(destFile, ...)
  else if (type == "TXT")
    data = read.table(destFile, sep = "", ...)
  
  data
}

shinyServer(function(input, output) {
  if (!exists("climate_df")) { # load data and construct data frame
    temps <- loadFile("https://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A2.txt",
                      "temps.txt",
                      "TXT",
                      skip = 4,
                      nrows = 137,
                      col.names = c("year", "temp", "temp_5"))
    temps <- temps[, c("year", "temp")]

    co2_59 = loadFile("https://raw.githubusercontent.com/datasets/co2-ppm/master/data/co2-annmean-mlo.csv",
                      "co2-annmean-mlo.csv")
    co2_59 <- co2_59[, c("Year", "Mean")]
    names(co2_59) <- c("year", "co2") # CO2 data pre-1959
    
    co2 <- loadFile("ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law2006.txt",
                    "law2006.txt",
                    "TXT",
                    skip = 182,
                    nrows = 1959,
                    header = TRUE)
    co2 <- co2[co2$YearAD >= 1880, c("YearAD", "CO2spl")]
    names(co2) <- c("year", "co2")
    co2 <- rbind(co2, co2_59) # Combine old and new for duration of temp measurements
    
    sunspots <- loadFile("http://www.sidc.be/silso/INFO/snytotcsv.php",
                        "sunspots.csv",
                        "CSV",
                        sep = ";",
                        skip = 179,
                        col.names = c("year", "sunspots", "stdev", "obs", "prov"))
    sunspots <- data.frame(year = as.integer(sunspots$year)-1, sunspots = sunspots$sunspots)
    
    volcanoes <- loadFile("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$HAZ_EVENT_ID&t=102557&s=50&d=54&dfn=volerup.txt",
                          "volcanoes.tsv",
                          "CSV",
                          sep = "\t")
    volcanoes <- volcanoes %>%
      filter(Year >= 1880 & !is.na(VEI)) %>%
      select(Year, VEI) %>%
      group_by(Year) %>%
      summarize(eruptions = sum(10 ^ VEI))
    names(volcanoes) <- c("year", "eruptions")
    
    climate_df <- merge(temps, co2, by = "year")
    climate_df <- merge(climate_df, sunspots, by = "year")
    climate_df <- merge(climate_df, volcanoes, by = "year", all.x = TRUE)
    climate_df$eruptions[is.na(climate_df$eruptions)] <- 0
  }
  
  fit_climate_df <- reactive({
    m <- "temp ~"
    m <- ifelse("co2" %in% input$predictor, paste0(m, "+ co2"), m)
    m <- ifelse("sunspots" %in% input$predictor, paste0(m, "+ sunspots"), m)
    m <- ifelse("eruptions" %in% input$predictor, paste0(m, "+ eruptions"), m)

    sub_climate_df <- climate_df[climate_df$year >= input$years[1] &
                                   climate_df$year <= input$years[2], ]
    
    if (nchar(m) > 6) { 
      sub_climate_fit <- lm (m, data = sub_climate_df)
      fit_df <- cbind(sub_climate_df, predict(sub_climate_fit, interval = "prediction")) 
    } else {
      fit_df <- sub_climate_df
    }
    fit_df
  })
  
  output$fitPlot <- renderPlot({
      df <- fit_climate_df()
      g <- ggplot(df, aes(x = year)) + 
        geom_point(aes(y = temp)) +
        xlab("Year") +
        ylab("Temperature")
        
      if (length(names(df)) != 5) {
        g <- g +
          geom_ribbon(aes(ymin = lwr, ymax = upr), 
                      fill = "blue", alpha = 0.2) + 
          geom_line(aes(y = fit), colour = "blue", size = 1)
      } 
      g
  })
})
