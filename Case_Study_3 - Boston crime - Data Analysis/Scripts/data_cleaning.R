
################################################################
#Data cleaning
################################################################

library(dplyr) #for: %>%
library(stringr) #for: str_sub
library(tidyverse) #for: add_column
library(Hmisc) #for: describe()
library(DescTools)

#loading the dataset-----
dataset_boston<-read.csv("data/boston-crimes-2022.csv")
boston_crimes <- dataset_boston

Desc(boston_crimes)

#checking the data
dim(boston_crimes)
head(boston_crimes)
summary(boston_crimes)
str(boston_crimes)

#removing unnecessary columns
boston_crimes<-boston_crimes %>% 
  select(!c(INCIDENT_NUMBER,OFFENSE_CODE_GROUP,YEAR,UCR_PART))

#assigning better column names
boston_crimes <- rename_with(boston_crimes, tolower)
boston_crimes <- boston_crimes %>% rename("date" = "occurred_on_date")

#renaming cols in another way
#colnames(boston_crimes)[7] = "date"
head(boston_crimes)

#---------------------------------------------------------------------------
#NULL - null/empty object
#NaN - not a number
#NA - missing value 
#""- empty string

# 1) check if there is ANY " " (or NaN, NULL ?)
# 2) convert them into NA 
# 3) check how many NA values we have (comparing to total number of rows)

# boston_crimes$district[226]
# boston_crimes$lat[226]
#is.null(boston_crimes)
#is.nan(boston_crimes) it doesn't work

boston_crimes <- replace(boston_crimes,boston_crimes == '', NA)
nNA<-colSums(is.na(boston_crimes))
nNA
#the 2nd way
#boston_crimes %>% summarise_all(~sum(is.na(.)))


NA_percentage<- nNA / nrow(boston_crimes) * 100
cat("Percent of NA values in each column [%]: ", NA_percentage)
#should I remove some records/columns ? 

#removing missing values -> I'm deleting one column, because it has huge impact on my date, rest i leave
#only for analysis of single column i will remove missing records
boston_crimes <- boston_crimes[,-4] 
#boston_crimes <- boston_crimes[complete.cases(boston_crimes), ]
#the 2nd way
#cleaned <- boston_crimes[rowSums(is.na(boston_crimes)) == 0,]
#colSums(is.na(boston_crimes))  #rechecking
#dim(boston_crimes) #rechecking the dimensions
colnames(boston_crimes)
#--------------------------------------------------------------------------------------------------
#checking the correctness of data
#--------------------------------------------------------------------------------------------------
corr_description <- as.data.frame(unique(boston_crimes$offense_description))
# I found out that there is one typo in data: 
#MURDER, NON-NEGLIGENT MANSLAUGHTER
#MURDER, NON-NEGLIGIENT MANSLAUGHTER
#the correct form is: NEGLIGENT
#how many times does incorrect form come up? 
sum(boston_crimes$offense_description == "MURDER, NON-NEGLIGIENT MANSLAUGHTER")
#replacement
boston_crimes$offense_description[boston_crimes$offense_description == "MURDER, NON-NEGLIGIENT MANSLAUGHTER"] <- "MURDER, NON-NEGLIGENT MANSLAUGHTER"
sum(boston_crimes$offense_description == "MURDER, NON-NEGLIGIENT MANSLAUGHTER")
#--------------------------------------------------------------------------------------------------
#dealing with date
#--------------------------------------------------------------------------------------------------

str(boston_crimes)
head(boston_crimes$date, 10)

# ATTEMPS
#boston_crimes$date <- as.Date(ymd_hms(boston_crimes$date)) #it doesn't work and idk why ??? okey i know.. as.Date will give you only DATE so ymd + our data are from excel, so you have to specify the origin
#boston_crimes$date <- as.Date(boston_crimes$date, origin = "1899-12-30 00:00:00") it shows only ymd without hms
#boston_crimes$date<-as.POSIXct(boston_crimes$date,format="%Y/%m/%d %H:%M:%S",origin = "1899-12-30 00:00:00")
#convert_date(boston_crimes$date, type = "Excel", fraction = TRUE)
#boston_crimes$date <- as.Date(boston_crimes$date, origin = "1899-12-30",format="%d/%m/%Y") 
#str(boston_crimes$date) #where is the inf about hour/minute/second ?

#final considerations
# column data contains full date and time (without seconds). I can extract from this column day and minutes and put it into separate column. Month,year and hour i have already split

#--------------------------------------------------------------------------------------------------
#DAYS
#--------------------------------------------------------------------------------------------------

#vector which stores days
day<-c()

for(element in boston_crimes$date){
   day <- append(day,unlist(strsplit(element, split="/"))[2])
}
day

# less efficient way below but still works
# for (element in boston_crimes$date){
#   if(substr(element,2,2) == "/"){
#     if(substr(element,4,4) == "/"){
#       day <- append(day,substr(element,3,3))
#     }
#     if(substr(element,5,5) == "/"){
#       day <- append(day,substr(element,3,4))
#     }
#   }else if(substr(element,3,3) == "/"){
#     if(substr(element,5,5) == "/"){
#       day <- append(day,substr(element,4,4))
#     }
#     if(substr(element,6,6) == "/"){
#       day <- append(day,substr(element,4,5))
#     }
#   }else{
#     print("ERROR")
#   }
# }


#test if the loop works, verification based on different values
boston_crimes$date[56783]
day[56783]

boston_crimes$date[675]
day[675]

#--------------------------------------------------------------------------------------------------
#MINUTES
#--------------------------------------------------------------------------------------------------

#vector which stores minutes
minutes<-c()

for ( item in boston_crimes$date ){
  if(str_sub(item,-2,-2) == "0"){
    minutes <- append(minutes, str_sub(item,-1,-1))
  }else minutes<- append(minutes,str_sub(item,-2,-1))
}

minutes

#--------------------------------------------------------------------------------------------------
#ADDING NEW COLUMNS TO OUR DATASET & REMOVING COLUMN DATE (NOW IT IS REDUNDANT)
#--------------------------------------------------------------------------------------------------

colnames(boston_crimes)

boston_crimes <- boston_crimes[,-5]
boston_crimes <- add_column(boston_crimes,day,.after=5)
boston_crimes <- add_column(boston_crimes,minutes,.after=8)
boston_crimes <- boston_crimes %>% relocate(district,.after=9)

colnames(boston_crimes)

#FACTORS - verifying how many/which levels we have in some factors
#we have to convert qualitative variables to factors

boston_crimes$offense_code <- as.factor(boston_crimes$offense_code)
boston_crimes$offense_description <- as.factor(boston_crimes$offense_description)
boston_crimes$shooting <- as.factor(boston_crimes$shooting)

boston_crimes$district <- as.factor(boston_crimes$district)
boston_crimes$street <- as.factor(boston_crimes$street)
boston_crimes$reporting_area <- as.factor(boston_crimes$reporting_area)

boston_crimes$month <- as.factor(boston_crimes$month)
boston_crimes$day <- as.factor(boston_crimes$day)
boston_crimes$day_of_week <- as.factor(boston_crimes$day_of_week)
boston_crimes$hour <- as.factor(boston_crimes$hour)
boston_crimes$minutes <- as.factor(boston_crimes$minutes)

length(levels(as.factor(boston_crimes$offense_description)))
levels(as.factor(boston_crimes$district))
length(levels(as.factor(boston_crimes$street)))
levels(as.factor(boston_crimes$day_of_week))

head(boston_crimes$street,20)
describe(boston_crimes)
summary(boston_crimes)
str(boston_crimes)

