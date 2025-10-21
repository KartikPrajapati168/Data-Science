#statement:Student Score Analysis — clean dataset, normalize, and visualize 
#performance.

data<-read.csv("E:/R/projects/files/student-scores.csv")
data
str(data)
numeric_data<-data[,sapply(data,is.numeric)]
numeric_data
boxplot(numeric_data)
View(numeric_data)
remove_outliers<-function(df,cols){
  for(colname in cols){
    q1=quantile(df[[colname]],0.25,na.rm=TRUE)
    q3=quantile(df[[colname]],0.75,na.rm=TRUE)
    IQR=q3-q1
    lower_bound=q1-1.5*IQR
    upper_bound=q3+1.5*IQR
    outliers=df[df[[colname]]<lower_bound | df[[colname]] > upper_bound,]
    print(outliers)
    df<-df[df[[colname]]>=lower_bound & df[[colname]]<=upper_bound,]
  }
  return (df)
}
cleaned_data<-remove_outliers(numeric_data, c("absence_days",
            "math_score","biology_score"))
#head(cleaned_data)
#View(cleaned_data)
boxplot(cleaned_data)

barplot(
  colMeans(cleaned_data),
  main = "Average Values After Outlier Removal",
  col = rainbow(length(data)),
  ylab = "Mean Value"
)
