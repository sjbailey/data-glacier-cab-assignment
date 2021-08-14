library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)

c.and.t <- merge(Cab_Data, Transaction_ID, by=Transaction.ID, all=TRUE)
c.and.t <- c.and.t[,1:8]
CabCusTrans <- merge(c.and.t, Customer_ID, by="Customer.ID", all=TRUE)

# Dividing the data set according to the two companies for convenience
CD_pink <- data.frame(CabCusTrans[CabCusTrans$Company == "Pink Cab",], Company = rep("Pink Cab"))
CD_yellow <- data.frame(CabCusTrans[CabCusTrans$Company == "Yellow Cab",], Company = rep("Yellow Cab"))

# Convert dates from the Excel serial format (no times are included so no need to worry about that)
Cab_Data[,"Date.of.Travel"] <- as.Date(Cab_Data[,"Date.of.Travel"], origin = "1899-12-30")
CabCusTrans[,"Date.of.Travel"] <- as.Date(CabCusTrans[,"Date.of.Travel"], origin = "1899-12-30")

# Identify and remove outliers and missing values
Cab_Data <- na.omit(Cab_Data)

# Visualise the continuous variables looking for differences according to the cab company
boxplot(data.frame( Overall = CabCusTrans$KM.Travelled, "Yellow Cab" = ifelse(CabCusTrans$Company == "Yellow Cab", CabCusTrans$KM.Travelled, NA), "Pink Cab" = ifelse(CabCusTrans$Company == "Pink Cab", CabCusTrans$KM.Travelled, NA)), ylab="Distance of cab ride (km)")
boxplot(data.frame( Overall = CabCusTrans$Price.Charged, "Yellow Cab" = ifelse(CabCusTrans$Company == "Yellow Cab", CabCusTrans$Price.Charged, NA), "Pink Cab" = ifelse(CabCusTrans$Company == "Pink Cab", CabCusTrans$Price.Charged, NA)), ylab="Price of trip (US$)")
boxplot(data.frame( Overall = CabCusTrans$Cost.of.Trip, "Yellow Cab" = ifelse(CabCusTrans$Company == "Yellow Cab", CabCusTrans$Cost.of.Trip, NA), "Pink Cab" = ifelse(CabCusTrans$Company == "Pink Cab", CabCusTrans$Cost.of.Trip, NA)), ylab="Cost to the company of trip (US$)")

# Calculate the profits earned from each trip
CabCusTrans$Profit <- CabCusTrans$Price.Charged - CabCusTrans$Cost.of.Trip

# Grouping cities by the region of the United States where they are located
west <- City$City[grep(" WA| MT| OR| ID| WY| CO| UT| NV| CA| AZ| NM", City$City)]
midwest <-  City$City[grep(" ND| MN| WI| MI| OH| SD| IA| IL| IN| NE| KS| MO", City$City)]
northeast <- City$City[grep(" ME| NH| VT| NY| CT| RI| NJ| PA", City$City)]
south <- City$City[grep(" OK| AR| KY| TN| MS| AL| LA| TX| WV| VA| NC| DE| MD| DC| NC| SC| GA| FL", City$City)]

CabCusTrans <- CabCusTrans %>%
  mutate(Region=case_when(
    City %in% west ~ "West",
    City %in% midwest ~ "Midwest",
    City %in% northeast ~ "Northeast",
    City %in% south ~ "South"
  ))

# Counting the number of trips per day over the 35-month period

cabDays <- as.data.table(CabCusTrans)
trips_per_day <- cabDays[, .N, by = Date.of.Travel]
  
# Aggregating the total distance travelled by cab passengers per day
pink_dist_per_day <- aggregate(CD_pink$KM.Travelled, by=list(Day=CD_pink$Date.of.Travel), FUN=sum)
yellow_dist_per_day <- aggregate(CD_yellow$KM.Travelled, by=list(Day=CD_yellow$Date.of.Travel), FUN=sum)

total_dist <- rbind(data.frame(pink_dist_per_day, Company = rep("Pink Cab")), data.frame(yellow_dist_per_day, Company = rep("Yellow Cab")))

# Bar chart showing the proportion of total trip distance over the 35-month period by region
ggplot(total_dist, aes(x = Day, y = x, fill = Company)) +#
  geom_bar(stat = "identity") +#
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +#
  labs(y = "Distance (km)")
  
# How do the different cities/regions compare in the gross profit and average profit per trip? How do the two companies compare in this regard?
# Prepare the data
gprof.city.pink <- data.frame(aggregate(CabCusTrans[CabCusTrans$Company == "Pink Cab", 12], by = list(City = CabCusTrans[CabCusTrans$Company == "Pink Cab", 5]), FUN = sum), Company = rep("Pink Cab"))
gprof.city.ylw <- data.frame(aggregate(CabCusTrans[CabCusTrans$Company == "Yellow Cab", 12], by = list(City = CabCusTrans[CabCusTrans$Company == "Yellow Cab", 5]), FUN = sum), Company = rep("Yellow Cab"))
gprof.city <- rbind(gprof.city.pink, gprof.city.ylw)
gprof.city <- gprof.city[order(gprof.city$City),]
City$Users <- as.numeric(gsub(',', '', City$Users))
CityS <- City[-16,]
CityS <- CityS[order(CityS$City),]

# Create the bar charts of gross profit by city and by region
ggplot(gprof.city, aes(x = City, y = x/1000000, fill = Company)) +#
  geom_bar(stat = "identity") +#
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +#
  labs(y = "Profit (millions of US$)")

# Create the bar charts of average profit per person by city and by region
ggplot(gprof.city, aes(x = City, y = x/rep(CityS$Users, each = 2), fill = Company)) +#
  geom_bar(stat = "identity") +#
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +#
  labs(y = "Minimum average profit per trip (US$)")

# Do seasons play a role in the frequency of cab usage?

spring <- c(seq(as.Date("2016-03-19"), as.Date("2016-06-19"), by = "day"),
            seq(as.Date("2017-03-20"), as.Date("2017-06-19"), by = "day"),
            seq(as.Date("2018-03-20"), as.Date("2018-06-20"), by = "day"))

summer <- c(seq(as.Date("2016-06-20"), as.Date("2016-09-21"), by = "day"),
            seq(as.Date("2017-06-20"), as.Date("2017-09-21"), by = "day"),
            seq(as.Date("2018-06-21"), as.Date("2018-09-21"), by = "day"))

autumn <- c(seq(as.Date("2016-09-22"), as.Date("2016-12-20"), by = "day"),
            seq(as.Date("2017-09-22"), as.Date("2017-12-20"), by = "day"),
            seq(as.Date("2018-09-22"), as.Date("2018-12-20"), by = "day"))

winter <- c(seq(as.Date("2016-01-01"), as.Date("2016-03-18"), by = "day"),
            seq(as.Date("2016-12-21"), as.Date("2017-03-19"), by = "day"),
            seq(as.Date("2017-12-21"), as.Date("2018-03-19"), by = "day"),
            seq(as.Date("2018-12-21"), as.Date("2018-12-31"), by = "day"))

Cab_Data <- Cab_Data %>%
  mutate(Season=case_when(
    Date.of.Travel %in% spring ~ "Spring",
    Date.of.Travel %in% summer ~ "Summer",
    Date.of.Travel %in% autumn ~ "Autumn",
    Date.of.Travel %in% winter ~ "Winter"
  ))

dist_per_szn <- aggregate(Cab_Data$KM.Travelled, by=list(Season=Cab_Data$Season), FUN=sum)

ggplot(data=dist_per_day, aes(x=Day, y=x, group=1)) +#
  geom_line() +#
  geom_point() +#
  labs(y = "Cumulative distance travelled (km)")

ggplot(dist_per_szn, aes(x = Season, y=x) ) +#
  geom_bar(stat = "identity") +#
  labs(y= "Distance travelled (km)")

# Is there a relationship between the age of a customer and their level of cab use?
# Calculate the number of trips taken by customers of every age in the data

custAge.pink <- cbind(as.data.table(CD_pink)[, .N, by = Age], Company = rep("Pink Cab"))
custAge.ylw <- cbind(as.data.table(CD_yellow)[, .N, by=Age], Company = rep("Yellow Cab"))
custAge <- rbind(custAge.pink, custAge.ylw)

ggplot(custAge, aes(x = Age, y = N, fill = Company)) +#
  geom_bar(stat = "identity") +#
  labs(y = "Number of users")

age_count <- as.data.table(Cutomer_ID)[, .N, by = A]