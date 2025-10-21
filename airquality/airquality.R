#statement:Clean and Analyze AirQuality Dataset (Handle NA, find correlation, 
#draw plots)

data("airquality")
View(airquality)
#install.packages("VIM")
#library(VIM)
#aggr(airquality, numbers = TRUE, prop = TRUE, cex.axis = 0.7)
#or
install.packages("naniar")
library(naniar)
gg_miss_var(airquality, show_pct = TRUE)
remove_na<-function(df,cols){
  for(colname in cols){
    before_rows=nrow(df)
    print(before_rows)
    mean_value=mean(df[[colname]],na.rm=TRUE)
    print(mean_value)
    #remove na
    #df<-na.omit(df)
    #replace na
    df[[colname]][is.na(df[[colname]])]<-mean_value
    after_rows=nrow(df)
    print(after_rows)
  }
  return (df)
}
clean_data<-remove_na(airquality,c("Ozone","Solar.R"))
clean_data<-as.data.frame(clean_data)
View(clean_data)

# Calculate mean Ozone by Month
avg_ozone <- aggregate(Ozone ~ Month, data = clean_data, mean)

# Create pie chart
pie(avg_ozone$Ozone,
    labels = paste("Month", avg_ozone$Month),
    main = "Average Ozone Level by Month",
    col = rainbow(length(avg_ozone$Month)))


avg_solar <- aggregate(Solar.R ~ Month, data = clean_data, mean)
pie(avg_solar$Solar.R,
    labels = paste("Month", avg_solar$Month),
    main = "Average Solar Radiation by Month",
    col = rainbow(length(avg_solar$Month)))


#barplot(avg_ozone$Ozone,
#        names.arg = avg_ozone$Month,
#        col = "skyblue",
#       main = "Average Ozone Levels by Month",
#       xlab = "Month",
#       ylab = "Average Ozone")

