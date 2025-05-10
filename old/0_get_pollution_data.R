
library(jsonlite)
library(httr)
library(tidyverse)

func_pol <- function(latitude, longitude){

##Reference: https://aqicn.org/api/"
AQItoken<-"e5ccfd17137961ad8b8033c7d94d76e7eb329bb0"

##Builds base URL for AQI information and queries the API.##
BaseURL<-paste0("http://api.waqi.info/feed/geo:", latitude,";", longitude,"/?token=",AQItoken)
response<-as.character(RETRY("GET", BaseURL, encode="json",time=10))
content<-jsonlite::fromJSON(response)
AQI<-content$data$aqi

##Creates dataframes for each pollutant that is forecasted and binds them together##
AQIO3Fore<-content$data$forecast$daily$o3
AQIO3Fore$pollutant<-"o3"
AQIpm10Fore<-content$data$forecast$daily$pm10
AQIpm10Fore$pollutant<-"pm10"
AQIpm25Fore<-content$data$forecast$daily$pm25
AQIpm25Fore$pollutant<-"pm2.5"
AQIForecast<-bind_rows(AQIO3Fore,AQIpm10Fore,AQIpm25Fore)
AQIForecast$day<-as.Date(AQIForecast$day)

AQIForecast$city <- content[["data"]][["city"]][["name"]]
AQIForecast <- bind_cols(AQIForecast, data.frame(y=matrix(content[["data"]][["city"]][["geo"]], nrow=1)[,1], x=matrix(content[["data"]][["city"]][["geo"]], nrow=1)[,2]))

return(AQIForecast)

}

AQIForecast <- func_pol(latitude, longitude)

##Creates a plot of the AQI data.##
AQIPlot<-ggplot(data=AQIForecast, aes(x=day, y = max, color=pollutant))+
  geom_line()+xlab("Date")+ylab("Individual AQI by Pollutant")+
guides(color=guide_legend(title="Pollutant"))

AQIPlot





