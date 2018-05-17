#convert json to data frame
library(httr)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(plotly)

plotOnlineHours<-function(cities,date){
df<-data.frame()

for(i in 1:length(cities)){
json_file <- paste("http://159.89.172.254:8090/lhours?city=",cities[i],"&date=",date,"",sep="")
response<-GET(json_file)
cwr_raw <- content(response,"text")
df <-rbind(df,fromJSON(cwr_raw,flatten=TRUE))
}
return(df)
}

#List online hours for the days
driverTimeCity<-function(df){
hour_df<-df%>%
  group_by(city,date)%>%
  summarize(Total_hours=(sum(minutes))/60) %>%
  ungroup()
plt<-ggplot(hour_df,aes(x=date,y=Total_hours,group=city,color=city))+ geom_line()+labs(title="Total Hours spent online by captains",x ="Day", y = "Hours Online",colour= "city")
plt<-ggplotly(plt)
return(plt)
}

getPlot  <- function(cities,date){
df=plotOnlineHours(cities,date)
hour_df=driverTimeCity(df)
return(hour_df)
}

getPlot(c("Hyderabad","Bangalore"),"2018-05-10")