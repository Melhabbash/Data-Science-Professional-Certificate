# Climate Change Exercises: Questions 1-7
# 
# Background
# The planet's surface temperature is increasing as greenhouse gas emissions 
# increase, and this global warming and carbon cycle disruption is wreaking havoc 
# on natural systems. Living systems that depend on current temperature, weather, 
# currents and carbon balance are jeopardized, and human society will be forced to 
# contend with widespread economic, social, political and environmental damage as 
# the temperature continues to rise. In these exercises, we examine the relationship 
# between global temperature changes, greenhouse gases and human carbon emissions 
# using time series of actual atmospheric and ice core measurements from the 
# National Oceanic and Atmospheric Administration (NOAA) and Carbon Dioxide 
# Information Analysis Center (CDIAC).

# Libraries and Data Import
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

str(temp_carbon)
head(temp_carbon)
tail(temp_carbon)
dim(temp_carbon)
# 
# IMPORTANT: These exercises use dslabs datasets that were added in a July 2019 
# update. Make sure your package is up to date with the command install.
# packages("dslabs").
# 
# Question 1
# 0.0/3.0 points (graded)
# Load the temp_carbon dataset from dslabs, which contains annual global 
# temperature anomalies (difference from 20th century mean temperature in 
# degrees Celsius), temperature anomalies over the land and ocean, and global 
# carbon emissions (in metric tons). Note that the date ranges differ for 
# temperature and carbon emissions.
# 
# Which of these code blocks return the latest year for which carbon emissions 
# are reported?

# FALSE
temp_carbon %>%
  .$year %>%
  max()
###############
# True
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()
###############
# False
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)
###############
# True
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()
###############
# True
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()
###############
# False
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)
##################################

# Question 2
# 0.0/3.0 points (graded)
# Inspect the difference in carbon emissions in temp_carbon from the first 
# available year to the last available year.
# 
# What is the first year for which carbon emissions (carbon_emissions) data are 
# available?

# line plot of annual global, land and ocean temperature anomalies since 1880
data(temp_carbon)
colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")
# line plot of anthropogenic carbon emissions over 250+ years
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") +
  ggtitle("Annual global carbon emmissions, 1751-2014")

minn<-temp_carbon%>%
  filter(year==1751)
minnn<-minn$carbon_emissions
minnn

maxx<-temp_carbon%>%
  filter(year==2014)
maxxx<-maxx$carbon_emissions
maxxx

maxxx/minnn

#####################

# Question 3
# 0.0/3.0 points (graded)
# Inspect the difference in temperature in temp_carbon from the first available
# year to the last available year.
# 
# What is the first year for which global temperature anomaly (temp_anomaly) 
# data are available?
minn<-temp_carbon%>%
  filter(!is.na(temp_anomaly))%>%
  select(year)
min(minn) 

# What is the last year for which global temperature anomaly data are available?
maxx<-temp_carbon%>%
  filter(!is.na(temp_anomaly))%>%
  select(year)
max(maxx) 

# How many degrees Celsius has temperature increased over the date range? 
# Compare the temperatures in the most recent year versus the oldest year.

minn<-temp_carbon%>%
  filter(!is.na(temp_anomaly))%>%
  select(year)
year_min<-min(minn) 

mi<-temp_carbon%>%
  filter(year==year_min)%>%
  select(temp_anomaly)
mi



maxx<-temp_carbon%>%
  filter(!is.na(temp_anomaly))%>%
  select(year)
year_max<-max(maxx) 
year_max

ma<-temp_carbon%>%
  filter(year==year_max)%>%
  select(temp_anomaly)
ma

ma-mi

##########################################
# 
# Question 4
# 1 point possible (graded)
# Create a time series line plot of the temperature anomaly. Only include years
# where temperatures are reported. Save this plot to the object p.
# 
# Which command adds a blue horizontal line indicating the 20th century mean 
# temperature?

data(temp_carbon)
#view(temp_carbon)

# line plot of annual global, land and ocean temperature anomalies since 1880
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")

p<-temp_carbon %>%
  select(Year = year, Global = temp_anomaly) %>%
  gather(Region, Temp_anomaly, Global) %>%
  ggplot(aes(Year, Temp_anomaly, col = Region)) +
  geom_line(size = 1) +
  # geom_hline(aes(yintercept = 0), col = colorblind_palette[8], lty = 2) +
  # geom_label(aes(x = 2005, y = -.08), col = colorblind_palette[8],label = "20th century mean", size = 4) +
  # ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  scale_color_manual(values = colorblind_palette) 
  # ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
p

#######
p <- p + geom_vline(aes(xintercept = 0), col = "blue")   #(False)
p
#######
p <- p + geom_hline(aes(y = 0), col = "blue")   #(False)
p
#######
p <- p + geom_hline(aes(yintercept = 0, col = blue))#(False)
p
#######
p <- p + geom_hline(aes(yintercept = 0), col = "blue") #(True)
p

############################################

# Question 5
# 1 point possible (graded)
# Continue working with p, the plot created in the previous question.
# 
# Change the y-axis label to be "Temperature anomaly (degrees C)". Add a title,
# "Temperature anomaly relative to 20th century mean, 1880-2018". Also add a 
# text layer to the plot: the x-coordinate should be 2000, the y-coordinate 
# should be 0.05, the text should be "20th century mean", and the text color 
# should be blue.
# 
# Which of the following code blocks is correct?

p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))
# (false)


p + ylim("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
# (false)


p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))
# (false)


p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
# (True)

p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
# (false)

###########################

# Question 6
# 0.0/3.0 points (graded)
# Use the plot created in the last two exercises to answer the following 
# questions. Answers within 5 years of the correct answer will be accepted.
# 
# When was the earliest year with a temperature above the 20th century mean?
# 1938

# When was the last year with an average temperature below the 20th century mean?
#   1977

# In what year did the temperature anomaly exceed 0.5 degrees Celsius for the 
# first time?
#   1995
############################
# 
# Question 7
# 0.0/1.5 points (graded)
# Add layers to the previous plot to include line graphs of the temperature 
# anomaly in the ocean (ocean_anomaly) and on land (land_anomaly). Assign 
# different colors to the lines. Compare the global temperature anomaly to the 
# land temperature anomaly and ocean temperature anomaly.
# 
# Which region has the largest 2018 temperature anomaly relative to the 20th 
# century mean?

data(temp_carbon)
#view(temp_carbon)

# line plot of annual global, land and ocean temperature anomalies since 1880
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")

p<-temp_carbon %>%
  select(Year = year, Global = temp_anomaly, Land = land_anomaly, Ocean = ocean_anomaly) %>%
  gather(Region, Temp_anomaly, Global:Ocean) %>%
  ggplot(aes(Year, Temp_anomaly, col = Region)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0), col = colorblind_palette[8], lty = 2) +
  geom_label(aes(x = 2005, y = -.08), col = colorblind_palette[8],label = "20th century mean", size = 4) +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  scale_color_manual(values = colorblind_palette) 
# ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")
p
#########################################


# Climate Change Exercises: Questions 8-12
# 
# Assessment continues
# The climate change exercises continue on this page. Use the libraries and 
# datasets below.

# Libraries and Data Import
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Question 8
# 0.0/1.5 points (graded)
# A major determinant of Earth's temperature is the greenhouse effect. Many 
# gases trap heat and reflect it towards the surface, preventing heat from 
# escaping the atmosphere. The greenhouse effect is vital in keeping Earth at 
# a warm enough temperature to sustain liquid water and life; however, changes 
# in greenhouse gas levels can alter the temperature balance of the planet.
# 
# The greenhouse_gases data frame from dslabs contains concentrations of the 
# three most significant greenhouse gases: carbon dioxide (CO2, abbreviated in 
# the data as co2), methane (CH4, ch4 in the data), and nitrous oxide 
# (N2O, n2o in the data). Measurements are provided every 20 years for the past 
# 2000 years.
# 
# Complete the code outline below to make a line plot of concentration on the 
# y-axis by year on the x-axis. Facet by gas, aligning the plots vertically so 
# as to ease comparisons along the year axis. Add a vertical line with an 
# x-intercept at the year 1850, noting the unofficial start of the industrial 
# revolution and widespread fossil fuel consumption. Note that the units for 
# ch4 and n2o are ppb while the units for co2 are ppm.

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
##################

# Question 9
# 0.0/5.0 points (graded)
# Interpret the plot of greenhouse gases over time from the previous question. 
# You will use each answer exactly once ch4, co2, n2o, all, none).
# 
# Which gas was stable at approximately 275 ppm/ppb until around 1850?
# co2

# Which gas more than doubled in concentration since 1850?
# ch4

# Which gas decreased in concentration since 1850?
# none

# Which gas had the smallest magnitude change since 1850?
# n2o

# Which gas increased exponentially in concentration after 1850?
# all

####################################################

# Question 10
# 0.0/3.0 points (graded)
# While many aspects of climate are independent of human influence, and co2 
# levels can change without human intervention, climate models cannot 
# reconstruct current conditions without incorporating the effect of manmade 
# carbon emissions. These emissions consist of greenhouse gases and are mainly 
# the result of burning fossil fuels such as oil, coal and natural gas.
# 
# Make a time series line plot of carbon emissions (carbon_emissions) from the 
# temp_carbon dataset. The y-axis is metric tons of carbon emitted per year.
# 
# Which of the following are true about the trend of carbon emissions?
#   Check all correct answers.

# line plot of anthropogenic carbon emissions over 250+ years
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") +
  geom_vline(aes(xintercept = 1850))+
  geom_vline(aes(xintercept = 1960))+
  geom_vline(aes(xintercept = 1970))+
  geom_vline(aes(xintercept = 1979))+
  geom_vline(aes(xintercept = 2014))+
  ggtitle("Annual global carbon emmissions, 1751-2014")

# Carbon emissions were essentially zero before 1850 and have increased 
# exponentially since then. (True)
# Carbon emissions are reaching a stable level. (False)
# Carbon emissions have increased every year on record. (False)
# Carbon emissions in 2014 were about 4 times as large as 1960 emissions.(True)
# Carbon emissions have doubled since the late 1970s.(True)
# Carbon emissions change with the same trend as atmospheric greenhouse gas levels 
# (co2, ch4, n2o) (True)

################################
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Question 11
# 0.0/2.0 points (graded)
# We saw how greenhouse gases have changed over the course of human history, 
# but how has  CO2  (co2 in the data) varied over a longer time scale? The 
# historic_co2 data frame in dslabs contains direct measurements of atmospheric 
# co2 from Mauna Loa since 1959 as well as indirect measurements of atmospheric 
# co2 from ice cores dating back 800,000 years.
# 
# Make a line plot of co2 concentration over time (year), coloring by the 
# measurement source (source). Save this plot as co2_time for later use.
# 
# Which of the following are true about co2_time, the time series of co2 over 
# the last 800,000 years?
#   Check all correct answers.

co2_time<-historic_co2 %>%
  # filter(year>-200000)%>%
  ggplot(aes(year, co2, col=source)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") 
co2_time

# Modern co2 levels are higher than at any point in the last 800,000 years.(True)
# There are natural cycles of co2 increase and decrease lasting 50,000-100,000 
# years per cycle.(True)
# In most cases, it appears to take longer for co2 levels to decrease than to 
# increase. (True)
# co2 concentration has been at least 200 ppm for the last 800,000 years. (false)

###########################
# Question 12
# 0.0/4.0 points (graded)
# One way to differentiate natural co2 oscillations from today's manmade co2 
# spike is by examining the rate of change of co2. The planet is affected not 
# only by the absolute concentration of co2 but also by its rate of change. 
# When the rate of change is slow, living and nonliving systems have time to 
# adapt to new temperature and gas levels, but when the rate of change is fast, 
# abrupt differences can overwhelm natural systems. How does the pace of natural
# co2 change differ from the current rate of change?
# 
# Use the co2_time plot saved above. Change the limits as directed to 
# investigate the rate of change in co2 over various periods with spikes in co2 
# concentration.
# 
# Change the x-axis limits to -800,000 and -775,000. About how many years did 
# it take for co2 to rise from 200 ppmv to its peak near 275 ppmv?

co2_time<-historic_co2 %>%
  # filter(year>-800000 && year<-775000)%>%
  ggplot(aes(year, co2, col=source)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") 
co2_time+coord_cartesian(xlim = c(-800000,-775000))

# 10000
 


# Change the x-axis limits to -375,000 and -330,000. About how many years did 
# it take for co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv?

co2_time<-historic_co2 %>%
  # filter(year>-800000 && year<-775000)%>%
  ggplot(aes(year, co2, col=source)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") 
co2_time+coord_cartesian(xlim = c(-375000,-330000))

# 25000

# Change the x-axis limits to -140,000 and -120,000. About how many years did 
# it take for co2 to rise from 200 ppmv to its peak near 280 ppmv?

co2_time<-historic_co2 %>%
  # filter(year>-800000 && year<-775000)%>%
  ggplot(aes(year, co2, col=source)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") 
co2_time+coord_cartesian(xlim = c(-140000,-120000))

# 5000

# 
# Change the x-axis limits to -3000 and 2018 to investigate modern changes in 
# co2. About how many years did it take for co2 to rise from its stable level 
# around 275 ppmv to the current level of over 400 ppmv?

co2_time<-historic_co2 %>%
  # filter(year>-800000 && year<-775000)%>%
  ggplot(aes(year, co2, col=source)) +
  geom_line() +
  xlab("Year") +
  ylab("Carbon emmissions (metric tons)") 
co2_time+coord_cartesian(xlim = c(-3000,2018))

# 250
