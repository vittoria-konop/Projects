
################################################################
#EDA ONE-DIMENSIONAL
#################################################################

library(lubridate)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(geojsonio)
library(broom)

# windows() - plots will be shown in different window
windowsFonts(A = windowsFont("Rockwell")) 

#-------------------------------------------------------------------------
# OFFENSE DESCRIPTION
#-------------------------------------------------------------------------

#margins: BLTR,  the default is mar = c(5.1, 4.1, 4.1, 2.1).
par(mar=c(3,20,3,3))

#frequency table for one variable 
tab <- table(boston_crimes$offense_description)

#10 first offenses - frequency
sorted_tab1 <- tab[order(tab,decreasing = TRUE)] 
sorted1<-sorted_tab1[1:10]
sorted1

#20 first offenses - frequency
sorted2<-sorted_tab1[1:20]

#10 lat offences - frequency
sorted_tab3 <- tab[order(tab)] 
sorted3<-sorted_tab3[1:20]
sorted3<-sorted3[order(sorted3,decreasing = TRUE)]


barplot(sorted1,horiz=T, las=1, xlim=c(0,10000), main ="Top 10 most common crimes in Boston", col="#1D356D")
par(mar=c(3,19,3,3))
barplot(sorted2,horiz=T, las=1, xlim=c(0,10000), main ="Top 20 most common crimes in Boston", cex.names=0.8, col="#34497c")


par(mar=c(3,19,5,1.5))
barplot(sorted3,ylim=c(0,12), width=.6, horiz=T,las=1, cex.names = 0.6, xlim=c(0,12), main="20 the rarest crimes in Boston",col="#367BCA")

#-------------------------------------------------------------------------
# OFFENSE CODE
#-------------------------------------------------------------------------

par(mar=c(5,5,5,2))
tab_code <- table(boston_crimes$offense_code)
tab_code
tab_code_organised<- tab_code[order(tab_code,decreasing = TRUE)][1:20]
tab_code_organised
barplot(tab_code_organised,cex.names=0.9, las=2, main="TOP 20 most popular offense codes in Boston", ylim= c(0,10000),col="#86b0df")

windows()
par(mar=c(20,7,5,3))
code <- names(tab_code_organised)

#on balance: maybe offences are grouped somehow based on gravity or codes, so that we could decide which group comes up the most frequently 
#for example we can presume that code with first number 3 will indicate one group
#however there is no explicit information related with it (at least I don't have an access to it)

barplot(tab_code_organised,cex.names=0.7, las=2, main="TOP 20 most popular offense codes in Boston", ylim= c(0,10000), names.arg = paste(names(sorted2), "-", code), col = ifelse(substr(code,1,1) == "3", 'red','grey'))


#-------------------------------------------------------------------------
# SHOOTING 
#-------------------------------------------------------------------------

par(mar=c(3,15,4,1.5))
#width will not work without xlim !
x<-barplot(table(boston_crimes$shooting), xlim=c(0,50), width=5, ylim=c(0,80000), main="Shots fired?", names.arg=c("no", "yes"),col="#DFBC1E")
y<-table(boston_crimes$shooting)

y_dim<-c(77119,4733) # the equal space between text and bars
text(x,y_dim,labels=as.character(y))

#-------------------------------------------------------------------------
# DATE -  month, day, day of the week
#-------------------------------------------------------------------------

par(mar=c(3,5,4,1.5))
#months
barplot(table(boston_crimes$month),ylim=c(0,8000), main="Crimes spread over months",col="#e9d062")

#days
# 1- 31
day <- as.integer(boston_crimes$day)
day_table <- table(day[order(day)])
day_table
barplot(day_table,las=2, ylim=c(0,3000), main="Crimes spread over days",col="#e9d062")

#descending order based on frequency
# tab_day <- table(boston_crimes$day)
# sorted_tab_day <- tab_day[order(tab_day,decreasing = TRUE)] 
# sorted_tab_day
# barplot(sorted_tab_day,las=2, ylim=c(0,3000),main="Crimes spread over days")

#day of the week
fac_week <- factor(boston_crimes$day_of_week, levels = c("Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
x_week <- table(fac_week)
barplot( x_week, main="Crimes spread over day of the week", ylim=c(0,12000), cex.names=0.8,col="#e9d062")

#-------------------------------------------------------------------------
# NUMBER OF CRIMES IN ORDINARY DAYS AND FESTIVE DAYS (IN THE USA)
#-------------------------------------------------------------------------

#Public holidays in USA are: 
# New Year's Day (January 1)
# Memorial Day (May 25–31, floating Monday)
# Independence Day (July 4)
# Labor Day (September 1–7, floating Monday)
# Thanksgiving (November 22–28, floating Thursday)
# Christmas (December 25)

new_year <- sum(boston_crimes$month == 1 & boston_crimes$day =="1")
new_year
table_new_year <- boston_crimes[boston_crimes$month ==1 & boston_crimes$day =="1",]

memorial_day <- sum(boston_crimes$month == 5 & boston_crimes$day_of_week == "Monday")
independence_day<- sum(boston_crimes$month == 7 & boston_crimes$day =="4")
labor_day <- sum(boston_crimes$month == 9 & boston_crimes$day_of_week == "Monday")
thanksgiving <- sum(boston_crimes$month == 11 & boston_crimes$day_of_week == "Thursday")
Christmas <- sum(boston_crimes$month == 12 & boston_crimes$day == "12")

holidays_number <- c(new_year,memorial_day,independence_day,labor_day,thanksgiving,Christmas)
holidays_number
crimes_holidays <- sum(holidays_number)
crimes_ordinary<- nrow(boston_crimes)- crimes_holidays
crimes_ordinary
divided_crimes<-c(crimes_holidays,crimes_ordinary)
divided_crimes
x_crimes<-barplot(divided_crimes,names.arg=c("Holidays", "Weekdays"),ylim=c(0,80000), xlim=c(0, 5),main="Number of crimes during Holidays and ordinary days",col="#355C96")
y_dim_crimes<-c(7516,75000) # the equal space between text and bars
text(x_crimes,y_dim_crimes,labels=as.character(divided_crimes))

#WHAT HOLIDAY HAS THE HEIGHEST NUMBER OF COMMITED CRIMES ? 
holidays_names <- c("New Year's Day", "Memorial Day","Independence Day","Labor Day","Thanksgiving","Christmas")
holidays_names <- factor(holidays_names, levels = holidays_names)

holidays <- data.frame(
  holidays_number,
  holidays_names
)

holy_plot <- ggplot(holidays, aes(x=holidays_names, y=holidays_number)) + 
  geom_bar(stat = "identity", width=0.3, fill="#1D356D") + ggtitle("Crimes on holidays") +
  xlab("holidays") + ylab("frequency") + theme(plot.title=element_text(hjust=0.5))
ggplotly(holy_plot)


#####
memorial_crimes <- boston_crimes[boston_crimes$month == 5 & boston_crimes$day_of_week == "Monday",]
labour_crimes<- boston_crimes[boston_crimes$month == 9 & boston_crimes$day_of_week == "Monday",]
thanks_crimes<- boston_crimes[boston_crimes$month == 11 & boston_crimes$day_of_week == "Thursday",]


ls_1 <- sum(memorial_crimes$offense_description == "LARCENY SHOPLIFTING")
lt_1 <-sum(memorial_crimes$offense_description == "LARCENY THEFT FROM BUILDING")
lb_1 <- sum(memorial_crimes$offense_description == "LARCENY THEFT OF BICYCLE")

ls_2 <-sum(labour_crimes$offense_description == "LARCENY SHOPLIFTING")
lt_2 <-sum(labour_crimes$offense_description == "LARCENY THEFT FROM BUILDING")
lb_2 <-sum(labour_crimes$offense_description == "LARCENY SHOPLIFTING")

ls_3 <-sum(thanks_crimes$offense_description == "LARCENY SHOPLIFTING")
lt_3 <-sum(thanks_crimes$offense_description == "LARCENY THEFT FROM BUILDING")
lb_3 <-sum(thanks_crimes$offense_description == "LARCENY THEFT OF BICYCLE")


#GROUPED BAR PLOT 
#create a dataset 

holidays_grouped <- c(rep("Memorial Day",3),rep("Labor Day",3),rep("Thanksgiving",3))
larceny_type<- rep(c("LARCENY SHOPLIFTING", "LARCENY THEFT FROM BUILDING", "LARCENY THEFT OF BICYCLE"),3)
value <- c(ls_1,lt_1,lb_1,ls_2,lt_2,lb_2,ls_3,lt_3,lb_3 )
grouped_crimes <- data.frame(holidays_grouped,larceny_type,value)
grouped_crimes

ggplot(grouped_crimes,aes(fill=larceny_type,y=value,x=holidays_grouped)) + 
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Number of specific types of larceny during the festive days") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Types of holidays") + ylab("Frequency") +
  scale_fill_manual(values = c("#367BCA", "#1D356D", "#DFBC1E"),name = "Larceny", labels = c("Shoplifting", "Theft from building", "Theft of bicycle"))

#LARCENY -> only material things(the theft of personal property capable of being possessed and carried away)
#THEFT -> identity theft, theft of intellectual property, theft of services and theft of personal property - more general 
#-------------------------------------------------------------------------
# TIME - hours, minutes
#-------------------------------------------------------------------------

windows()
hour <- as.integer(boston_crimes$hour)
barplot(table(hour[order(hour)]), main ="Hours of crimes", ylim=c(0, 6000), cex.names=0.8,col="#1D356D")

minutes <-as.integer(boston_crimes$minutes)
barplot(table(minutes[order(minutes)]), main="Minutes of crimes", ylim=c(0, 16000), cex.names = 0.7,col="#1D356D")

#On balance: Dominates the multiply of 5 => often crimes are committed in minute 0,5,10,15,20 etc. 
#Maybe people tend to round numbers

#-----------------------------------------------------------------------------
# LOCALIZATION
#-----------------------------------------------------------------------------

#Divide the screen in 2 columns and 2 lines
#par(mfrow=c(2,2))

#districts
districts_NA <- boston_crimes$district
sum(is.na(districts_NA))
districts_noNA <- districts_NA[!is.na(boston_crimes$district)]
districts_noNA
sum(is.na(districts_noNA))

table(districts_noNA)
labels_district <- sort(unique(districts_noNA))
labels_district
str(levels(labels_district))

length(table(districts_noNA))
myPalette <- c("#0c152c","#433809","#867112","#9c8415","#c9a91b","#dfbc1e","#e9d062","#d2d7e2","#e8ebf0","#1d356d","#34497c","#4a5d8a","#7786a7","#a5aec5")
myPalette
#prop.table()- to calculate the value of each cell in a table as a proportion of all values
# counting % for each value
percent_districts <-round(prop.table(table(districts_noNA))*100)
percent_districts_combined <- paste(percent_districts, "%", sep = "")

par(mar= c(5,2,5,2))
pie(table(districts_noNA),main="Pie Chart of Districts",col=myPalette, labels=paste(percent_districts_combined,sep = " "),radius=1)
legend("topright",levels(labels_district), fill=myPalette )

#street
length(levels(as.factor(boston_crimes$street))) # a lot 8378
tab_street<-table(boston_crimes$street)
ordered_street<- tab_street[order(tab_street,decreasing = TRUE)]
street_data<-ordered_street[1:10]
street_data

# 10 Streets where the crime rate was the highest
par(mar=c(5,10,5,3))
y_street<-barplot(street_data, horiz = T, las=1,xlim = c(0,6000), main="10 Streets where the crime rate was the highest",col="#367BCA")
#numbers
x_street<-c(5600,street_data[2:10]+300)
text(x=x_street,y=y_street,labels=as.character(street_data))

# WHAT CRIMES DO COME UP THE MOST FREQUENTLY ON THE MOST COMMON STREETS? (APART FROM WASHINGTON ST)
street_data
gb_street <- boston_crimes[boston_crimes$street == "GIBSON ST",]
gb_tab<- table(gb_street$offense_description)
gb_sorted <- gb_tab[order(gb_tab,decreasing = TRUE)]
gb_sorted

harr_street <-boston_crimes[boston_crimes$street == "HARRISON AVE",]
harr_tab <- table(harr_street$offense_description)
harr_sorted <- harr_tab[order(harr_tab, decreasing = TRUE)]
harr_sorted

blue_street <- boston_crimes[boston_crimes$street == "BLUE HILL AVE",]
blue_tab <- table(blue_street$offense_description)
blue_sorted <- blue_tab[order(blue_tab,decreasing = TRUE)]
blue_sorted

par(mar=c(5,17,2,3))
barplot(gb_sorted[1:10],horiz = T, las=1,cex.names=0.8, main="Crimes in Gibson Street", xlim=c(0,500),col="#86b0df")
barplot(harr_sorted[1:10],horiz = T, las=1,cex.names=0.8, main="Crimes in Harrison Ave", xlim=c(0,400),col="#86b0df")
par(mar=c(5,18,2,3))
barplot(blue_sorted[1:10],horiz = T, las=1,cex.names=0.8, main="Crimes in Blue Hill Ave", xlim=c(0,300),col="#86b0df")

#-----------------------------------------------------------------------------
# MAPS
#-----------------------------------------------------------------------------
spdf<-geojson_read("Maps/Boston_Neighborhoods.geojson",what="sp")
#plot(spdf)
spdf_fortified <- tidy(spdf)
names(spdf_fortified)


data<- boston_crimes %>% filter(shooting == "1") %>% filter(!is.na(lat))
head(data)
nrow(data)

#windows()
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group),fill="#8e9ab6", alpha=0.3, color="white") +
  geom_point( data=data, aes(x=long, y=lat)) + coord_fixed() + theme_void() + ggtitle("Places where the shooting took place") + theme(plot.title = element_text(hjust = 0.5, vjust=-3, family = "A"))


# + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"))

#-----------------------------------------------------------------------------
# DISTRICTS 
#-----------------------------------------------------------------------------

#rn we have cols: lat,long, district with NA values + districts contain External values
#i have noticed (after plotting) that there are some mistakes: 5 values don't fit -> they are beyond map with lat > 42.4, lat <<< 42.3 + 2 others 
#we will remove them in order to create more correct plot 
#filter(!is.na(lat)) - will eleminate also lacks in long

#which(grepl(42.37941, boston_crimes$Lat))
#which(grepl(-71.10143, boston_crimes$Long))

data2<-boston_crimes
data2<- boston_crimes %>% filter(!is.na(lat)) %>% filter(!is.na(district)) %>% filter(district != "External") %>% filter(lat < 42.4) 
#data2 <- boston_crimes %>% filter(!is.na(lat) | !is.na(district) | district != "External" | lat < 42.4) #check out why it doesn't work
nrow(data2)
#sum(data2$district == "External")
#after plot i detected 3 values that are misplaced
data2<-data2 %>% filter( lat !=42.37941001 & long != -71.10142997)
data2<-data2 %>% filter( lat !=42.36083997 & long != -71.09598004)
data2<-data2 %>% filter( lat !=42.21440996 & long != -71.00938995)
nrow(data2)

# rn district is still a factor, because i only wiped out values, so as a level External will still appear
length(table(data2$district))
table(data2$district)

head(data2)
 p2<- ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group),fill="#F6E8C3", color="#D8B365") + 
  geom_point( data=data2, aes(x=long, y=lat,col=district)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplotly(p2) #to make plot interactive

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group),fill="#F6E8C3", color="#D8B365") + 
  geom_point( data=data2, aes(x=long, y=lat,col=district)) + 
  scale_colour_manual(values = myPalette)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
