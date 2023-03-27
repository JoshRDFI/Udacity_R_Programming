# Summary:
# In this project, 3 questions will be explored on a merge 
# of the 3 data sets.
# The questions are:
# 1) 1) What are the most popular bike rental days in each city?
# 2) What is the average daily trip time in each city? (start and end 
#    times of rentals)
# 3) In NYC and Chicago, which gender rents more often?

# ---------------
# Additional learning resources used: 
# "R Programming A-Z™: R For Data Science With Real Exercises!" 
# by Kirill Eremenko on Udemy
# and
# Lubridate package and usage found on
# https://www.statology.org/r-day-of-week/
# and of course multiple pages on https://stackoverflow.com

# ---------------
# Project Start: 
# Import the CSV files
chi <- read.csv("chicago.csv")
nyc <- read.csv("new-york-city.csv")
wsh <- read.csv("washington.csv")

# ---------------
# Verify csv files imported; check structures
# Chicago
head(chi, n=5)
str(chi)
summary(chi)

# New York City
head(nyc, n=5)
str(nyc)
summary(nyc)

# Washington DC
head(wsh, n=5)
str(wsh)
summary(wsh)

# ---------------
# Libraries needed for visualizations
library(ggplot2)
library(lubridate)
library(dplyr)

# ---------------
# Data manipulations and data frame modifications:
# Add Gender and Birth.Year columns to Washington
wsh$Gender <- "Unknown"
wsh$Birth.Year <- NA

# Add City column to all data frames
nyc$City <- 'New York City'
wsh$City <- 'Washington'
chi$City <- 'Chicago'

# Create a function for concatenation
concatenation <- function(city1, city2) {
  return(rbind(city1, city2))
}

# Concatenate all three data frames into one called "Combined"
combined <- concatenation(nyc, wsh)
combined <- concatenation(combined, chi)

# Add column with the day of the week extracted from the Start.Time column
combined$Weekday <- wday(combined$Start.Time, label=TRUE, abbr=FALSE)
combined$Trips <- 1 # Add column for totals

# Fix date columns so they are date-time format instead of character
combined$Start.Time <- ymd_hms(combined$Start.Time)
combined$End.Time <- ymd_hms(combined$End.Time)

summary(combined)

# ---------------
# Questions:
# 1) What are the most popular bike rental days in each city?

# visualization
ggplot(data=combined, aes(x=Weekday, fill=City)) +
geom_bar(position = "dodge", color="black") +
  ggtitle("Day of the week with most rentals") +
  labs(y = "Total Rentals", x = "Day of the week") +
  scale_fill_manual("legend", values = c("Chicago" = "red", "New York City" = "darkgreen", "Washington" = "lightblue"))

# Summary
combined %>% 
  group_by(City, Weekday) %>% 
  summarize_at("Trips", sum, na.rm=TRUE) %>%
  print(n=21)

# Insights from visualization
# Chicago has the most rentals on Tuesday, but it's fairly even each day, with 
# the lowest rentals on Sunday.
# New York has the most rentals on Wednesday, with the lowest on Saturday.
# Washington DC has the most rentals also on Wednesday, with the lowest on Sunday, 
# followed closely by Monday.


# ----------------
# 2) What is the average daily trip time in each city? (start and end 
#    times of rentals)

# Visualization
ggplot(data = combined, aes(City, Trip.Duration)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean", fill = "darkgreen") + 
  ggtitle("What is the average travel time per city?") +
  labs(y = "Average Trip Time (in Seconds)", x = "City") +
  coord_flip()

# Summary
combined %>% 
  group_by(City) %>% 
  summarize_at("Trip.Duration", list(mean=mean), na.rm=TRUE)

# Insights from visualization
# Washington DC has the highest average trip time, while New York and Chicago 
# have very similar trip times.



# ----------------
# 3) In NYC and Chicago, which gender rents more often?
# Combine NYC and Chicago data into new data frame
genderdf <- concatenation(chi,nyc)
ggplot(data = genderdf, aes(x = Gender, fill = City)) +
  geom_bar(position = 'dodge') +
  ggtitle('Gender totals of rentals') +
  scale_x_discrete(labels = c('Unknown', 'Female', 'Male')) +
  labs(y = 'Number of rentals', x = 'Gender') +
  scale_fill_manual("legend", values = c("Chicago" = "red", "New York City" = "darkgreen"))

# Summary
# Total of each Gender in New York City and Chicago
genderdf %>%
  group_by(City) %>%
  count(Gender)
  
# Insights from visualization
# New York City has the most male and female riders, while Chicago has the highest 
# number of undeclared gender riders.


# ----------------
print("End of Code")


