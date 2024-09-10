
################################################################
#EDA TWO-DIMENSIONAL
#################################################################

#-------------------------------------------------------------------------
# OFFENSE DESCRIPTION & MONTHS
#-------------------------------------------------------------------------

#frequency of 4 the most common crimes showed in 12 months

tab <- table(boston_crimes$offense_description)
#if i'm ordering table i order the number of occurrences
tab_offense <- tab[order(tab,decreasing = TRUE)][1:4]
tab_offense[4]

#generate a vector with names of months, we will use it for 4 crimes plots
months_vec <- month.abb
months_vec <- factor(months_vec, levels=month.abb)
months_vec
str(months_vec)

colnames(boston_crimes)
crime1 <- boston_crimes[boston_crimes$offense_description == "INVESTIGATE PERSON",]
crime2 <- boston_crimes[boston_crimes$offense_description == "SICK ASSIST",]
crime3 <- boston_crimes[boston_crimes$offense_description == "M/V - LEAVING SCENE - PROPERTY DAMAGE",]
crime4 <- boston_crimes[boston_crimes$offense_description == "INVESTIGATE PROPERTY",]
head(crime3)

#to take numbers from table: as.numeric(), to take names: names()
table(crime1$month)
num_crimes1<-as.numeric(table(crime1$month))
num_crimes2<-as.numeric(table(crime2$month))
num_crimes3<-as.numeric(table(crime3$month))
num_crimes4<-as.numeric(table(crime4$month))

fac_month_offense <- data.frame(month = months_vec, number1=num_crimes1,number2=num_crimes2, number3=num_crimes3, number4=num_crimes4)
str(fac_month_offense)

#link which was really handy
#statology.org/r-geom_path-each-group-consists-of-only-one-observation/
windows(h=8,w=8)
ggplot( data = fac_month_offense, aes(x=month, group=1, shape=), ) + labs(x="Month",y = "Frequency") +
  geom_line(aes(y=number1, color="Investigate person")) + geom_line(aes(y=number2, color="Sick assist")) + 
  geom_line(aes(y=number3, color="Investigate property")) + geom_line(aes(y=number4, color="Leaving scene - \n property damage"))+
  ggtitle("Top 4 the most common crimes within 1 year")+
  theme(plot.title = element_text(hjust = 0.5, vjust=3))+
  theme(plot.margin = unit(c(1,1,1,1),"cm")) +
  scale_color_manual(values=c("#367BCA","#1D356D","#DFBC1E","#9c8415"),
                     breaks = c('Investigate person', 'Sick assist', 'Investigate property', 'Leaving scene - \n property damage')) +
  guides(color = guide_legend(title = "Crimes"))
  # margins work as t, r, b, l.

#-------------------------------------------------------------------------
#STREETS & SHOOTING 
#-------------------------------------------------------------------------

nrow(boston_crimes)
shot <- boston_crimes[boston_crimes$shooting == "1",]
nrow(shot1)
shot1$street

shot_tab <-table(shot$street)
shot_tab_ordered <- shot_tab[order(shot_tab,decreasing = TRUE)][1:10]
shot_tab_ordered
street_names<-factor(names(shot_tab_ordered), levels=names(shot_tab_ordered))
street_names

fac_shot_street <- data.frame(street=street_names, frequency = as.integer(shot_tab_ordered))

shot_plot <- ggplot(data=fac_shot_street, aes(y=street, x=frequency, group=1)) + geom_bar(stat = "identity") +
  ggtitle("Which streets has the most reported crimes with shooting?")+
  theme(plot.title = element_text(hjust = 0.5, vjust=3), plot.margin = unit(c(1,1,1,1),"cm")) +
  theme_minimal()
# with theme_minimal() plot.margin doesn't work :(

ggplotly(shot_plot)

#-------------------------------------------------------------------------
# WASHINGTON STREET
#-------------------------------------------------------------------------

washington_st <- boston_crimes[boston_crimes$street == "WASHINGTON ST" & boston_crimes$shooting == "1",]
nrow(washington_st)

wash_tab <- table(as.character(washington_st$offense_description))
wash_tab_ordered <- wash_tab[order(wash_tab,decreasing=TRUE)]
wash_tab_ordered
par(mar=c(5,16,7,5))
barplot(wash_tab_ordered, horiz=T, las=1,cex.names = 0.8, xlim=c(0,10), main="Types of crimes on Washington street \n with reported shootout", col="#DFBC1E")

#Where is Washngton street? 
wash_location <- boston_crimes[boston_crimes$street == "WASHINGTON ST",]
nrow(wash_location)

spdf<-geojson_read("Maps/Boston_Neighborhoods.geojson",what="sp")
spdf_fortified <- tidy(spdf)
names(spdf_fortified)
data_wash<- boston_crimes %>% filter(!is.na(lat)) %>% filter(street == "WASHINGTON ST")
head(data_wash)
nrow(data_wash)

#windows()
ggplot() +
  geom_polygon(data=spdf_fortified, aes( x = long, y = lat, group = group),fill="#8e9ab6", alpha=0.3, color="white") +
  geom_point( data=data_wash, aes(x=long, y=lat)) + coord_fixed() + theme_void() + ggtitle("Location of Washington streets") + theme(plot.title = element_text(hjust = 0.5, vjust=-3))

# Due to various municipal annexations with the city of Boston, the name Washington Street 
# now exists 6 or more times within the jurisdiction(s) of the City of Boston.
