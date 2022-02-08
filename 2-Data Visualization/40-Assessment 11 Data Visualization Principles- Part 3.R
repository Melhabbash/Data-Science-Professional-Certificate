# Tile plot - measles and smallpox
# The sample code given creates a tile plot showing the rate of measles cases 
# per population. We are going to modify the tile plot to look at smallpox cases 
# instead.
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
head(us_contagious_diseases)
##       disease   state year weeks_reporting count population
## 1 Hepatitis A Alabama 1966              50   321    3345787
## 2 Hepatitis A Alabama 1967              49   291    3364130
## 3 Hepatitis A Alabama 1968              52   314    3386068
## 4 Hepatitis A Alabama 1969              49   380    3412450
## 5 Hepatitis A Alabama 1970              51   413    3444165
## 6 Hepatitis A Alabama 1971              51   378    3481798
the_disease = "Measles"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
# Modify the tile plot to show the rate of smallpox cases instead of measles 
# cases.
# Exclude years in which cases were reported in fewer than 10 weeks from the plot.

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
head(us_contagious_diseases)
##       disease   state year weeks_reporting count population
## 1 Hepatitis A Alabama 1966              50   321    3345787
## 2 Hepatitis A Alabama 1967              49   291    3364130
## 3 Hepatitis A Alabama 1968              52   314    3386068
## 4 Hepatitis A Alabama 1969              49   380    3412450
## 5 Hepatitis A Alabama 1970              51   413    3444165
## 6 Hepatitis A Alabama 1971              51   378    3481798
the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting<10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
##############################

# Time series plot - measles and smallpox
# The sample code given creates a time series plot showing the rate of measles 
# cases per population by state. We are going to again modify this plot to look 
# at smallpox cases instead.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Measles"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
str(dat)
## 'data.frame':    3724 obs. of  7 variables:
##  $ disease        : Factor w/ 7 levels "Hepatitis A",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ state          : Factor w/ 51 levels "Mississippi",..: 9 9 9 9 9 9 9 9 9 9 ...
##   ..- attr(*, "scores")= num [1:51(1d)] 9.27 NA 24.15 9.37 19.16 ...
##   .. ..- attr(*, "dimnames")=List of 1
##   .. .. ..$ : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
##  $ year           : num  1928 1929 1930 1931 1932 ...
##  $ weeks_reporting: int  52 49 52 49 41 51 52 49 40 49 ...
##  $ count          : num  8843 2959 4156 8934 270 ...
##  $ population     : num  2589923 2619131 2646248 2670818 2693027 ...
##  $ rate           : num  34.1 11.3 15.7 33.5 1 ...
avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

# Modify the sample code for the time series plot to plot data for smallpox 
# instead of for measles.
# Once again, restrict the plot to years in which cases were reported in at 
# least 10 weeks.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting<10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))
str(dat)
## 'data.frame':    1014 obs. of  7 variables:
##  $ disease        : Factor w/ 7 levels "Hepatitis A",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ state          : Factor w/ 51 levels "Rhode Island",..: 17 17 17 17 17 17 17 17 17 17 ...
##   ..- attr(*, "scores")= num [1:51(1d)] 0.382 NA 2.011 0.805 0.924 ...
##   .. ..- attr(*, "dimnames")=List of 1
##   .. .. ..$ : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
##  $ year           : num  1928 1929 1930 1931 1932 ...
##  $ weeks_reporting: int  51 52 52 52 52 52 52 52 51 52 ...
##  $ count          : num  341 378 192 295 467 82 23 42 12 54 ...
##  $ population     : num  2589923 2619131 2646248 2670818 2693027 ...
##  $ rate           : num  1.317 1.443 0.726 1.105 1.734 ...
avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

########################
# Time series plot - all diseases in California
# Now we are going to look at the rates of all diseases in one state. Again, 
# you will be modifying the sample code to produce the desired plot.
# For the state of California, make a time series plot showing rates for all 
# diseases.
# Include only years with 10 or more weeks reporting.
# Use a different color for each disease.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California" & !weeks_reporting<10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color=disease)) + 
  geom_line()
##############################
# 
# Time series plot - all diseases in the United States
# Now we are going to make a time series plot for the rates of all diseases in the United States. For this exercise, we have provided less sample code - you can take a look at the previous exercise to get you started.
# Compute the US rate by using summarize to sum over states.
# The US rate for each disease will be the total number of cases divided by the total population.
# Remember to convert to cases per 10,000.
# You will need to filter for !is.na(population) to get all the data.
# Plot each disease in a different color.
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate=sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color=disease)) + geom_line()
