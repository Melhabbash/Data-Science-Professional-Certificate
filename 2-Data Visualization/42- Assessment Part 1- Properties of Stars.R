# Background
# Astronomy is one of the oldest data-driven sciences. In the late 1800s, the 
# director of the Harvard College Observatory hired women to analyze astronomical 
# data, which at the time was done using photographic glass plates. These women 
# became known as the "Harvard Computers". They computed the position and luminosity 
# of various astronomical objects such as stars and galaxies.(If you are interested,
# you can learn more about the Harvard Computers External link). Today, astronomy is
# even more of a data-driven science, with an inordinate amount of data being 
# produced by modern instruments every day.
# 
# In the following exercises we will analyze some actual astronomical data to 
# inspect properties of stars, their absolute magnitude (which relates to a star's 
# luminosity, or brightness), temperature and type (spectral class).

# Libraries and Options

library(tidyverse)
library(dslabs)
data(stars)

# options(digits = 3)   # report 3 significant digits
# IMPORTANT: These exercises use dslabs datasets that were added in a July 2019 
# update. Make sure your package is up to date by reinstalling the package with 
# the command install.packages("dslabs").

# Question 1
# 0.0/2.0 points (graded)
# Load the stars data frame from dslabs. This contains the name, absolute 
# magnitude, temperature in degrees Kelvin, and spectral class of selected stars. 
# Absolute magnitude (shortened in these problems to simply "magnitude") is a 
# function of star luminosity, where negative values of magnitude have higher 
# luminosity.
# 
# What is the mean magnitude?
str(stars)
x<-stars$magnitude
mean(x)
sd(x)
##########################
# Question 2
# 1 point possible (graded)
# Make a density plot of the magnitude.
# 
# How many peaks are there in the data?

p<-density(stars$magnitude)
plot(p)
#########################

# Question 3
# 1 point possible (graded)
# Examine the distribution of star temperature.
# 
# Which of these statements best characterizes the temperature distribution?
str(stars)
x<- hist(stars$temp)

# The majority of stars have a high temperature.(False)
# 
# The majority of stars have a low temperature.(True)
# 
# The temperature distribution is normal.(False)
# 
# There are equal numbers of stars across the temperature range. (False)

################################

# Question 4
# 1 point possible (graded)
# Make a scatter plot of the data with temperature on the x-axis and magnitude on
# the y-axis and examine the relationship between the variables. Recall that 
# lower magnitude means a more luminous (brighter) star.
# 
# When considering the plot of magnitude vs. temperature, most stars follow 
# a _______________ trend. These are called main sequence stars.
# 
# Fill in the blank:
stars %>% ggplot() +
  geom_point(aes(x = temp, y = magnitude))

# decreasing linear (False)
# 
# increasing linear (False)
# 
# decreasing exponential (True)
# 
# increasing exponential (False)

####################################

# Question 5
# 0.0/2.0 points (graded)
# For various reasons, scientists do not always follow straight conventions when 
# making plots, and astronomers usually transform values of star luminosity and 
# temperature before plotting. Flip the y-axis so that lower values of magnitude 
# are at the top of the axis (recall that more luminous stars have lower 
# magnitude) using scale_y_reverse(). Take the log base 10 of temperature and 
# then also flip the x-axis.
# 
# Fill in the blanks in the statements below to describe the resulting plot:
#   
#   The brighest, highest temperature stars are in the ______________ corner 
# of the plot.

# lower left (False)
# 
# lower right (False)
# 
# upper left (True)
# 
# upper right (False)

# For main sequence stars, hotter stars have __________ luminosity.
# 
# higher (True)
# 
# lower (False)

stars %>% ggplot() +
  geom_point(aes(x =log10(temp) , y = magnitude))+ 
  scale_y_reverse()+
  
  scale_x_reverse()
#######################################

# Question 6
# 0.0/1.0 point (graded)
# The trends you see allow scientists to learn about the evolution and lifetime 
# of stars. The primary group of stars to which most stars belong we will call 
# the main sequence stars (discussed in question 4). Most stars belong to this 
# main sequence, however some of the more rare stars are classified as "old" 
# and "evolved" stars. These stars tend to be hotter stars, but also have low 
# luminosity, and are known as white dwarfs.
# 
# How many white dwarfs are there in our sample?

4

stars %>% ggplot() +
  geom_point(aes(x =log10(temp) , y = magnitude))+ 
  scale_y_reverse()+
  scale_x_reverse()

stars %>% ggplot() +
  geom_point(aes(x = temp, y = magnitude))
########################

# Question 7
# 1 point possible (graded)
# Consider stars which are not part of the Main Group but are not old/evolved 
# (white dwarf) stars. These stars must also be unique in certain ways and are 
# known as giants. Use the plot from Question 5 to estimate the average 
# temperature of a giant.
# 
# Which of these temperatures is closest to the average temperature of a giant?

# 5000K (True)
# 
# 10000K (False)
# 
# 15000K (False)
# 
# 20000K (False)

#############################
# Question 8
# 0.0/3.0 points (graded)
# We can now identify whether specific stars are main sequence stars, red giants
# or white dwarfs. Add text labels to the plot to answer these questions. You 
# may wish to plot only a selection of the labels, repel the labels, or zoom in 
# on the plot in RStudio so you can locate specific stars.
# 
# Fill in the blanks in the statements below:
#   
#   The least lumninous star in the sample with a surface temperature over 
# 5000K is _________.
# 
# Antares (false)
# 
# Castor  (false)
# 
# Mirfak  (false)
# 
# Polaris (false)
# 
# van Maanen's Star (True)
# 


# The two stars with lowest temperature and highest luminosity are known as 
# supergiants. The two supergiants in this dataset are ____________.
# 
# Rigel and Deneb (false)
# 
# *SiriusB and van Maanen's Star (false)
# 
# Alnitak and Alnitam (false)
# 
# Betelgeuse and Antares  (True)
# 
# Wolf359 and G51-I5 (false)


# The Sun is a ______________.
# main sequence star (True)
# 
# giant (false)
# 
# white dwarf (false)

stars %>% ggplot() +
  geom_point(aes(x =log10(temp) , y = magnitude))+ 
  scale_y_reverse()+
  scale_x_reverse()+
  geom_text(aes(log10(temp) ,magnitude, label = star))



####################################
# 
# Question 9
# 0.0/3.0 points (graded)
# Remove the text labels and color the points by star type. This classification 
# describes the properties of the star's spectrum, the amount of light produced 
# at various wavelengths.
# 
# Which star type has the lowest temperature?
# M

# Which star type has the highest temperature?
# O  

# The Sun is classified as a G-type star. Is the most luminous G-type star in 
# this dataset also the hottest?
#   NO
  
  
stars %>% ggplot() +
  geom_point(aes(x =log10(temp) , y = magnitude, col=type), size=3)+ 
  scale_y_reverse()+
  scale_x_reverse()
  
