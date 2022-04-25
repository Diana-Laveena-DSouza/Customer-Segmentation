getwd()
setwd("C:/Users/lloyd/Desktop/simplilearn/pg_program_ds&business_analytics/course/courses/Data Science with R/Capstone Projects/Retail PGP/Project 3")
#load file
data = readxl::read_xlsx('Online Retail.xlsx')
View(data)
#shape
dim(data)
#data type
str(data)
#Descriptive statistics
summary(data)
#Remove records for cancelled transactions
library(dplyr)
data_filter = filter(data, ((UnitPrice>0)& (Quantity>0)))
View(data_filter)
#1. Perform a preliminary data inspection and data cleaning.
#Check for missing data and formulate an apt strategy to treat them.
colSums(is.na(data_filter))
#in percent
(colSums(is.na(data_filter))/nrow(data_filter))*100
#Handling Missing values
data_nomiss = as.data.frame(filter(data_filter, is.na(data_filter$CustomerID)==FALSE))
View(data_nomiss)
summary(data_nomiss)
colSums(is.na(data_nomiss))
#Drop duplicate records
data_new=unique(data_nomiss)
View(data_new)
#Finding duplicated records
data_new[duplicated(data_new), ]
#Data Transformation:
#2. Perform cohort analysis (a cohort is a group of subjects that share a defining characteristic). Observe how a cohort behaves across time and compare it to other cohorts. 
library(lubridate)
# A function that will parse the date Time based cohort:  1 day of month
get_month = function(x){
  return(as.Date(paste0(format(x, format='%Y'),
                        '-',format(x, format='%m'),
                        '-','01'), format = "%Y-%m-%d"))
}
# Create transaction_date column based on month and store in TransactionMonth
TransactionMonth=ymd("2016-04-01")
for( i in sequence(nrow(data_new))){

  TransactionMonth=c(TransactionMonth, get_month(data_new['InvoiceDate'][i,1]))
}
data_new$TransactionMonth=TransactionMonth[2:length(TransactionMonth)]
# Grouping by CustomerID and select the InvoiceMonth value
grouping = summarise(group_by(data_new, CustomerID), TransactionMonth)
# Assigning a minimum InvoiceMonth value to the dataset
library(dplyr)
cohortmonth=ymd("2016-04-01")
for(i in sequence(nrow(data_new['CustomerID']))){
  data_filt= filter(data_new, CustomerID==data_new['CustomerID'][i,1])
  value = min(data_filt$TransactionMonth)
  cohortmonth=c(cohortmonth, value)
}
data_new['CohortMonth']=cohortmonth[2:length(cohortmonth)]
# printing top 5 rows
head(data_new)
#2. Calculating time offset in Month as Cohort Index
get_date_int = function(df, column){
  year = format(df[column], format='%Y')
  month = format(df[column], format='%m')
  return (c(year, month))
}
# Getting the integers for date parts from the `InvoiceDay` column
transcation_year=get_date_int(data_new, 'TransactionMonth')[1]
transaction_month= get_date_int(data_new, 'TransactionMonth')[2]
# Getting the integers for date parts from the `CohortDay` column
cohort_year=get_date_int(data_new, 'CohortMonth')[1]
cohort_month = get_date_int(data_new, 'CohortMonth')[2]
#  Get the  difference in years
year_diff = as.integer(unlist(transcation_year))-as.integer(unlist(cohort_year))
# Calculate difference in months
month_diff=as.integer(unlist(transaction_month))-as.integer(unlist(cohort_month))
#Calculate cohort index
data_new['CohertIndex']=year_diff *12 + month_diff + 1
head(data_new)
#Create a Cohort Table
grouping = summarise(group_by(data_new, CohortMonth, CohertIndex, CustomerID), max(CustomerID))
cohort_data = summarise(group_by(as.data.frame(grouping), CohortMonth, CohertIndex), n())
colnames(cohort_data)=c('CohortMonth', 'CohertIndex', 'CustomerCount')
#Pivot table for cohort
library(tidyr)
cohort_counts=cohort_data %>% spread(CohertIndex, CustomerCount)
View(cohort_counts)
#calculate retantion rate
retention_rate=data.frame(matrix(ncol=13, nrow=13))
colnames(retention_rate)=colnames(cohort_counts)[2:14]
rownames(retention_rate)=c(cohort_counts[,1])$CohortMonth
cohort_sizes=cohort_counts[,2]
for (row in seq(nrow(cohort_counts))){
    retention_rate[row,]=round((cohort_counts[row ,2:14]/cohort_sizes[row, 1]$`1`)*100, 3)
}
View(retention_rate)
#3. Build a RFM (Recency Frequency Monetary) model. Recency means the number of days since a customer made the last purchase. Frequency is the number of purchase in a given period. It could be 3 months, 6 months or 1 year. Monetary is the total amount of money a customer spent in that given period. Therefore, big spenders will be differentiated among other customers such as MVP (Minimum Viable Product) or VIP.
#4. Calculate RFM metrics.
#Calculate recency
library(dplyr)
data_new$InvoiceDate=as.Date(data_new$InvoiceDate, format="%Y-%m-%d")
data_new$Purchase=data_new$Quantity * data_new$UnitPrice
last_date = max(data_new$InvoiceDate)
Recency_data = last_date - summarise(group_by(data_new, CustomerID), max(InvoiceDate))$`max(InvoiceDate)`+1
#Calculate Moetary and Frequency
Monetary_data = summarise(group_by(data_new, CustomerID), sum(Purchase))
Frequency_data = count(data_new, CustomerID)$n
RFM_data = Monetary_data
RFM_data$Recency=Recency_data
RFM_data$Frequency=Frequency_data
#Change to difftime to numeric
RFM_data$Recency=as.numeric(RFM_data$Recency)
colnames(RFM_data)[2]='Monetary'
View(RFM_data)
#5. Build RFM Segments. Give recency, frequency, and monetary scores individually by dividing them into quartiles.
#Combine three ratings to get a RFM segment (as strings).
#Get the RFM score by adding up the three ratings.
#Analyze the RFM segments by summarizing them and comment on the findings.
rfm_segment = RFM_data
recency = max(RFM_data$Recency)-RFM_data$Recency
install.packages("timereg")
library(timereg)
rfm_segment$M = as.numeric(qcut(rfm_segment$Monetary, retbins=TRUE))
rfm_segment$R = as.numeric(qcut(recency, retbins=TRUE))
rfm_segment$F = as.numeric(qcut(rfm_segment$Frequency, labels=f_labels, retbins=TRUE))
rfm_segment$score=rfm_segment$R+rfm_segment$F+rfm_segment$M
segment=paste0(as.character(rfm_segment$R), as.character(rfm_segment$F), as.character(rfm_segment$M))
rfm_segment$segment=segment
View(rfm_segment)
#Project Task: Week 2
#Data Modeling :
#  1. Create clusters using k-means clustering algorithm.
#Prepare the data for the algorithm. If the data is asymmetrically distributed, manage the skewness with appropriate transformation. Standardize the data.
rfm= rfm_segment[, 2:4]
install.packages('GGally')
library(GGally)
ggpairs(rfm)
install.packages('moments')
library(moments)
for (i in colnames(rfm)){
  print(paste(i, skewness(rfm[, i])))
}
install.packages("MASS")
library('MASS')
y = rfm$Monetary
result  = boxcox(y~1, lambda = seq(-5,5,0.5))
mylambda = result$x[which.max(result$y)]
mylambda
rfm$Monetary = (y^mylambda-1)/mylambda
y = rfm$Recency
result  = boxcox(y~1, lambda = seq(-5,5,0.5))
mylambda = result$x[which.max(result$y)]
mylambda
rfm$Recency = (y^mylambda-1)/mylambda
y = rfm$Frequency
result  = boxcox(y~1, lambda = seq(-5,5,0.5))
mylambda = result$x[which.max(result$y)]
mylambda
rfm$Frequency = (y^mylambda-1)/mylambda
#Plotting again
ggpairs(rfm)
#Standard Scaler
for (i in colnames(rfm)){
  rfm[, i] = scale(rfm[, i], center = TRUE, scale = TRUE)  
}
View(rfm)
#Decide the optimum number of clusters to be formed.
install.packages("NbClust")
library(NbClust)
NbClust(data = rfm, diss = NULL, distance = "euclidean", min.nc = 4, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)
#Kmeans algorithm for number of clusters 4
kmeans = kmeans(rfm, centers = 4)
RFM_data$Cluster=kmeans$cluster
View(RFM_data)
