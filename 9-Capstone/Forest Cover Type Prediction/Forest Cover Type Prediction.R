
################## Install Basic Package required -- 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ellipse)) install.packages("ellipse", repos = "http://cran.us.r-project.org")

################## Install Additional Package  used in code
#### Used formattable and kableExtra Package to formate Table

# import libraries
library(tidyverse)   # general
library(caret)       # model
library(dplyr)
library(dslabs)
library(ggcorrplot)  # correlation matrix plot
library(psych)
library(MASS)


library(data.table)
library(kableExtra)
library(formattable)
library(DescTools)   # descriptive statistics
library(ggthemes)
library(lubridate) ## used to deal with timestamp
library(knitr)
library(rmarkdown)

library(rpart)
library(party)

# import data set
data <- read.csv("train.csv",stringsAsFactors=T)


## 1. Introduction

### 1.1 Investigation the dataset
dim(data) 

glimpse(data)


#table 1
name<-c(
  names(data[1]),
  names(data[2]),
  names(data[3]),
  names(data[4]),
  names(data[5]),
  names(data[6]),
  names(data[7]),
  names(data[8]),
  names(data[9]),
  names(data[10]),
  names(data[11])
  
)

description<-c(
  "A primary key (unique value for each record,row )",
  "Elevation in meters",
  "Aspect in degrees azimuth",
  "Slope in degrees",
  "Horz Dist to nearest surface water features",
  "Vert Dist to nearest surface water features",
  "Horz Dist to nearest roadway",
  "Hillshade index at 9am, summer solstice",
  "Hillshade index at noon, summer solstice",
  "Hillshade index at 3pm, summer solstice",
  "Horz Dist to nearest wildfire ignition points"
  
)
table_1 <- data.frame(Name=name,Describtion=description)
table_1%>%
	kable(
		align = 'l', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
		
       	kable_styling(full_width = FALSE, position = "left", latex_options = "hold_position")

#table 2
name<-c(
  names(data[12]),
  names(data[13]),
  names(data[14]),
  names(data[15]),
  names(data[16]),
  names(data[17]),
  names(data[18]),
  names(data[19]),
  names(data[20]),
  names(data[21]),
  names(data[22]),
  names(data[23]),
  names(data[24]),
  names(data[25]),
  names(data[26]),
  names(data[27]),
  names(data[28]),
  names(data[29]),
  names(data[30]),
  names(data[31]),
  names(data[32]),
  names(data[33]),
  names(data[34]),
  names(data[35]),
  names(data[36]),
  names(data[37]),
  names(data[38]),
  names(data[39]),
  names(data[40]),
  names(data[41]),
  names(data[42]),
  names(data[43]),
  names(data[44]),
  names(data[45]),
  names(data[46]),
  names(data[47]),
  names(data[48]),
  names(data[49]),
  names(data[50]),
  names(data[51]),
  names(data[52]),
  names(data[53]),
  names(data[54]),
  names(data[55]),
  names(data[56])
)


description<-c(
  "Rawah Wilderness Area",
  "Neota Wilderness Area",
  "Comanche Peak Wilderness Area",
  "Cache la Poudre Wilderness Area",
  "Cathedral family - Rock outcrop complex, extremely stony",
  "Vanet - Ratake families complex, very stony",
  "Haploborolis - Rock outcrop complex, rubbly",
  "Ratake family - Rock outcrop complex, rubbly",
  "Vanet family - Rock outcrop complex complex, rubbly",
  "Vanet - Wetmore families - Rock outcrop complex, stony",
  "Gothic family",
  "Supervisor - Limber families complex",
  "Troutville family, very stony",
  "Bullwark - Catamount families - Rock outcrop complex, rubbly",
  "Bullwark - Catamount families - Rock land complex, rubbly",
  "Legault family - Rock land complex, stony",
  "Catamount family -Rock land- Bullwark family complex, rubbly",
  "Pachic Argiborolis - Aquolis complex",
  "Unspecified in the USFS Soil and ELU Survey",
  "Cryaquolis - Cryoborolis complex",
  "Gateview family - Cryaquolis complex",
  "Rogert family, very stony",
  "Typic Cryaquolis - Borohemists complex",
  "Typic Cryaquepts - Typic Cryaquolls complex",
  "Typic Cryaquolls - Leighcan family, till substratum complex",
  "Leighcan family, till substratum, extremely bouldery",
  "Leighcan family, till substratum - Typic Cryaquolls complex",
  "Leighcan family, extremely stony",
  "Leighcan family, warm, extremely stony",
  "Granile - Catamount families complex, very stony",
  "Leighcan family, warm - Rock outcrop complex, extremely stony",
  "Leighcan family - Rock outcrop complex, extremely stony",
  "Como - Legault families complex, extremely stony",
  "Como family -Rock land- Legault family complex, extremely stony",
  "Leighcan - Catamount families complex, extremely stony",
  "Catamount family - Rock outcrop - Leighcan family complex, extremely stony",
  "Leighcan - Catamount families - Rock outcrop complex, extremely stony",
  "Cryorthents - Rock land complex, extremely stony",
  "Cryumbrepts - Rock outcrop - Cryaquepts complex",
  "Bross family - Rock land - Cryumbrepts complex, extremely stony",
  "Rock outcrop - Cryumbrepts - Cryorthents complex, extremely stony",
  "Leighcan - Moran families - Cryaquolls complex, extremely stony",
  "Moran family -Cryorthents- Leighcan family complex,extremely stony",
  "Moran family -Cryorthents- Rock land complex, extremely stony",
  "7 types, integers 1 to 7- Forest Cover Type designation"
)
table_2 <- data.frame(Name=name,Describtion=description)
table_2%>%
	kable(
		align = 'l', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
		
       	kable_styling(full_width = FALSE, position = "left", latex_options = "hold_position")


#table 3 
table_3<-unclass(summary(data[,2:4]))
table_3%>%
	kable(col.names = names(data)[2:4],
        align = 'c', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
		    column_spec(3, color =  "#41729F", bold = T) %>%
       	kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")


#table 4 
table_4<-unclass(summary(data[,5:6]))
table_4%>%
	kable(table_3,col.names = names(data)[5:6],
		    align = 'c', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
       	kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")


#table 5 
table_5<-unclass(summary(data[,8:10]))
table_5%>%
	kable(col.names = names(data)[8:10],
		    align = 'c', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
        column_spec(3, color =  "#41729F", bold = T) %>%
		kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

#table 6  
table_6<-unclass(summary(data[,c(7,11)]))

table_6 %>%
	kable(col.names = names(data)[c(7,11)],
	      align = 'c', booktabs = T,format = "latex", linesep = "") %>%
        column_spec(1, color =  "#41729F", bold = T) %>%
        column_spec(2, color =  "#41729F", bold = T) %>%
        kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

#table 7
table_7 <- data.frame(
  Var = c(names(data[12]), 
          names(data[13]),
          names(data[14]),
          names(data[15])
  ),
  Rec = c(
    length(which(data$Wilderness_Area1==1)),
    length(which(data$Wilderness_Area2==1)),
    length(which(data$Wilderness_Area3==1)),
    length(which(data$Wilderness_Area4==1))
  )
)

table_7 %>% 
  kable(col.names = c("Wilderness area type", "Number of records"), 
      align = 'c', booktabs = T,format = "latex", linesep = "") %>%
      column_spec(1, color =  "#41729F", bold = T) %>%
      column_spec(2, color =  "#41729F", bold = T) %>%
      kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

#table 8,
table_8 <- data.frame(
                            c(
                              names(data[16]), 
                              names(data[17]),
                              names(data[18]),
                              names(data[19]),
                              names(data[20]),
                              names(data[21]),
                              names(data[22]),
                              names(data[23]),
                              names(data[24]),
                              names(data[25]),
                              names(data[26]),
                              names(data[27]),
                              names(data[28]),
                              names(data[29]),
                              names(data[30]),
                              names(data[31]),
                              names(data[32]),
                              names(data[33]),
                              names(data[34]),
                              names(data[35]),
                              names(data[36]),
                              names(data[37]),
                              names(data[38]),
                              names(data[39]),
                              names(data[40]),
                              names(data[41]),
                              names(data[42]),
                              names(data[43]),
                              names(data[44]),
                              names(data[45]),
                              names(data[46]),
                              names(data[47]),
                              names(data[48]),
                              names(data[49]),
                              names(data[50]),
                              names(data[51]),
                              names(data[52]),
                              names(data[53]),
                              names(data[54]),
                              names(data[55])
                              ),
                      c(
                        length(which(data$Soil_Type1==1)),
                        length(which(data$Soil_Type2==1)),
                        length(which(data$Soil_Type3==1)),
                        length(which(data$Soil_Type4==1)),
                        length(which(data$Soil_Type5==1)),
                        length(which(data$Soil_Type6==1)),
                        length(which(data$Soil_Type7==1)),
                        length(which(data$Soil_Type8==1)),
                        length(which(data$Soil_Type9==1)),
                        length(which(data$Soil_Type10==1)),
                        length(which(data$Soil_Type11==1)),
                        length(which(data$Soil_Type12==1)),
                        length(which(data$Soil_Type13==1)),
                        length(which(data$Soil_Type14==1)),
                        length(which(data$Soil_Type15==1)),
                        length(which(data$Soil_Type16==1)),
                        length(which(data$Soil_Type17==1)),
                        length(which(data$Soil_Type18==1)),
                        length(which(data$Soil_Type19==1)),
                        length(which(data$Soil_Type20==1)),
                        length(which(data$Soil_Type21==1)),
                        length(which(data$Soil_Type22==1)),
                        length(which(data$Soil_Type23==1)),
                        length(which(data$Soil_Type24==1)),
                        length(which(data$Soil_Type25==1)),
                        length(which(data$Soil_Type26==1)),
                        length(which(data$Soil_Type27==1)),
                        length(which(data$Soil_Type28==1)),
                        length(which(data$Soil_Type29==1)),
                        length(which(data$Soil_Type30==1)),
                        length(which(data$Soil_Type31==1)),
                        length(which(data$Soil_Type32==1)),
                        length(which(data$Soil_Type33==1)),
                        length(which(data$Soil_Type34==1)),
                        length(which(data$Soil_Type35==1)),
                        length(which(data$Soil_Type36==1)),
                        length(which(data$Soil_Type37==1)),
                        length(which(data$Soil_Type38==1)),
                        length(which(data$Soil_Type39==1)),
                        length(which(data$Soil_Type40==1))
                      )
                      )

table_8 %>% 
  kable(col.names = c("Soil type", "Number of records"), 
      align = 'c', booktabs = T,format = "latex", linesep = "") %>%
      column_spec(1, color =  "#41729F", bold = T) %>%
      column_spec(2, color =  "#41729F", bold = T) %>%
      kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

### 1.2 Wrangling and cleaning data

#copy clean data
Clean_Data<-data



#1- "Horizontal_Distance_To_Hydrology", " Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways", 
#"Hillshade_9am", "Hillshade_3pm", and "Horizontal_Distance_To_Fire_Point"  have negative values, but the distance should be only a positive value.\

#negative values to positive values 
index_1<-which(Clean_Data$Horizontal_Distance_To_Hydrology<0)
Clean_Data$Horizontal_Distance_To_Hydrology[index_1]<-Clean_Data$Horizontal_Distance_To_Hydrology[index_1]*-1

index_2<-which(Clean_Data$Vertical_Distance_To_Hydrology<0)
Clean_Data$Vertical_Distance_To_Hydrology[index_2]<-Clean_Data$Vertical_Distance_To_Hydrology[index_2]*-1

index_3<-which(Clean_Data$Horizontal_Distance_To_Roadways<0)
Clean_Data$Horizontal_Distance_To_Roadways[index_3]<-Clean_Data$Horizontal_Distance_To_Roadways[index_3]*-1

index_4<-which(Clean_Data$Hillshade_9am<0)
Clean_Data$Hillshade_9am[index_4]<-Clean_Data$Hillshade_9am[index_4]*-1

index_5<-which(data$Hillshade_3pm<0)
Clean_Data$Hillshade_3pm[index_5]<-Clean_Data$Hillshade_3pm[index_5]*-1

index_6<-which(Clean_Data$Horizontal_Distance_To_Fire_Points<0)
Clean_Data$Horizontal_Distance_To_Fire_Points[index_6]<-Clean_Data$Horizontal_Distance_To_Fire_Points[index_6]*-1


#2- "Soil_Type7", and "Soil_Type15" are with only 0 value. This means that we can drop these two columns.

#drop variables with single value -0 value-, echo=FALSE
Clean_Data<-subset(Clean_Data, select = -c(Soil_Type7,Soil_Type15 ))

#drop single record
level_5<-which(Clean_Data$Cover_Type==5)
Clean_Data<-Clean_Data[-level_5,]
Clean_Data<-droplevels(Clean_Data)
Clean_Data$Cover_Type<-as.factor(Clean_Data$Cover_Type)

#3- "Cover_Type" has 
 length(level_5) #value for level 5, this means that this single value will appear in the training data set or will appear in validation-data-set, 
 #and this will cause a problem in the "train" or in the "predict" function. the solution for this problem is to drop level 5 in Cover_Type, so the levels will be:
 levels(Clean_Data$Cover_Type)

### 1.3 Statistic summary and visualizing data after wrangling and cleaning 
#Elevation

#Figure3 
hist(Clean_Data$Elevation, freq=TRUE, col="#41729F", border="white", 
     xlab="Elevation", ylab="Count")

#Statistic summary of Elevation
samm_Elevation<-summary(Clean_Data$Elevation)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Elevation[1]
samm_Elevation[2]
samm_Elevation[3]
samm_Elevation[4]
samm_Elevation[5]
samm_Elevation[6]

#Aspect

#Figure4 
hist(Clean_Data$Aspect, freq=TRUE, col="#41729F", border="white", 
     xlab="Aspect", ylab="Count")

#Statistic summary of Aspect
samm_Aspect<-summary(Clean_Data$Aspect)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Aspect[1]
samm_Aspect[2]
samm_Aspect[3]
samm_Aspect[4]
samm_Aspect[5]
samm_Aspect[6]


#Slope
#Figure5
hist(Clean_Data$Slope, freq=TRUE, col="#41729F", border="white", 
     xlab="Slope", ylab="Count")

#Statistic summary of Slope
samm_Slope<-summary(Clean_Data$Slope)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Slope[1]
samm_Slope[2]
samm_Slope[3]
samm_Slope[4]
samm_Slope[5]
samm_Slope[6]

#Horizontal Distance To Hydrology
# Figure6 
hist(Clean_Data$Horizontal_Distance_To_Hydrology, freq=TRUE, col="#41729F", border="white", 
     xlab="Horizontal Distance To Hydrology", ylab="Count")

#Statistic summary of Horizontal Distance To Hydrology
samm_Horizontal_Distance_To_Hydrology<-summary(Clean_Data$Horizontal_Distance_To_Hydrology)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Horizontal_Distance_To_Hydrology[1]
samm_Horizontal_Distance_To_Hydrology[2]
samm_Horizontal_Distance_To_Hydrology[3]
samm_Horizontal_Distance_To_Hydrology[4]
samm_Horizontal_Distance_To_Hydrology[5]
samm_Horizontal_Distance_To_Hydrology[6]
 

#Vertical Distance To Hydrology
# Figure7
hist(Clean_Data$Vertical_Distance_To_Hydrology, freq=TRUE, col="#41729F", border="white", 
     xlab="Vertical Distance To Hydrology", ylab="Count")

#Statistic summary of Vertical Distance To Hydrology
samm_Vertical_Distance_To_Hydrology<-summary(Clean_Data$Vertical_Distance_To_Hydrology)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Vertical_Distance_To_Hydrology[1]
samm_Vertical_Distance_To_Hydrology[2]
samm_Vertical_Distance_To_Hydrology[3]
samm_Vertical_Distance_To_Hydrology[4]
samm_Vertical_Distance_To_Hydrology[5]
samm_Vertical_Distance_To_Hydrology[6]


#Hillshade 9am
#Figure8 
hist(Clean_Data$Hillshade_9am, freq=TRUE, col="#41729F", border="white", 
     xlab="Hillshade 9am", ylab="Count")

#Statistic summary of Hillshade 9am
samm_Hillshade_9am<-summary(Clean_Data$Hillshade_9am)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Hillshade_9am[1]
samm_Hillshade_9am[2]
samm_Hillshade_9am[3]
samm_Hillshade_9am[4]
samm_Hillshade_9am[5]
samm_Hillshade_9am[6]


#Hillshade Noon
#Figure9 
hist(Clean_Data$Hillshade_Noon, freq=TRUE, col="#41729F", border="white", 
     xlab="Hillshade Noon", ylab="Count")

#Statistic summary of Hillshade Noon
samm_Hillshade_Noon<-summary(Clean_Data$Hillshade_Noon)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Hillshade_Noon[1]
samm_Hillshade_Noon[2]
samm_Hillshade_Noon[3]
samm_Hillshade_Noon[4]
samm_Hillshade_Noon[5]
samm_Hillshade_Noon[6]


#Hillshade 3pm
#Figure10
hist(Clean_Data$Hillshade_3pm, freq=TRUE, col="#41729F", border="white", 
     xlab="Hillshade 3pm", ylab="Count")

#Statistic summary of Hillshade 3pm
samm_Hillshade_3pm<-summary(Clean_Data$Hillshade_3pm)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Hillshade_3pm[1]
samm_Hillshade_3pm[2]
samm_Hillshade_3pm[3]
samm_Hillshade_3pm[4]
samm_Hillshade_3pm[5]
samm_Hillshade_3pm[6]


#Horizontal Distance To Roadways
#Figure11 
hist(Clean_Data$Horizontal_Distance_To_Roadways, freq=TRUE, col="#41729F", border="white", 
     xlab="Horizontal Distance To Roadways", ylab="Count")

#Statistic summary of Horizontal Distance To Roadways
samm_Horizontal_Distance_To_Roadways<-summary(Clean_Data$Horizontal_Distance_To_Roadways)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Horizontal_Distance_To_Roadways[1]
samm_Horizontal_Distance_To_Roadways[2]
samm_Horizontal_Distance_To_Roadways[3]
samm_Horizontal_Distance_To_Roadways[4]
samm_Horizontal_Distance_To_Roadways[5]
samm_Horizontal_Distance_To_Roadways[6]


#Horizontal Distance To Fire Points
#Figure12 
hist(Clean_Data$Horizontal_Distance_To_Fire_Points, freq=TRUE, col="#41729F", border="white", 
     xlab="Horizontal Distance To Fire Points", ylab="Count")

#statistic summary of Horizontal Distance To Fire Points
samm_Horizontal_Distance_To_Fire_Points<-summary(Clean_Data$Horizontal_Distance_To_Fire_Points)

# Min.      | $Q_1$     | Median   | Mean    | $Q_3$     | Max
#-----------|-----------|------------|---------|-----------|---------
samm_Horizontal_Distance_To_Fire_Points[1]
samm_Horizontal_Distance_To_Fire_Points[2]
samm_Horizontal_Distance_To_Fire_Points[3]
samm_Horizontal_Distance_To_Fire_Points[4]
samm_Horizontal_Distance_To_Fire_Points[5]
samm_Horizontal_Distance_To_Fire_Points[6]


### 1.4 Split data into training data set and validation data set

#split data into training dataset and validation dataset
Clean_Data$Cover_Type<-as.factor(Clean_Data$Cover_Type)
validation_index <- createDataPartition(Clean_Data$Cover_Type, p=0.20, list=FALSE)
x<-dim(data)
validation_percentage<-round(length(validation_index)/x[1],3)*100
validation_set <- Clean_Data[validation_index,]
training_set <- Clean_Data[-validation_index,]
dim_training_set<-dim(training_set) 
dim_validation_set<-dim(validation_set)
100-validation_percentage
validation_percentage
dim_training_set[1]
dim_training_set[2]
dim_validation_set[1]
dim_validation_set[2] 


levels(training_set$Cover_Type)
#summarize the class distribution
percentage <- prop.table(table(training_set$Cover_Type)) * 100 
cbind(freq=table(training_set$Cover_Type), percentage=round(percentage, 2))

## 2. Algorithms

#train control
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

### 2.1 Algorithm 1 (linear algorithm): Linear discriminant analysis (L.D.A.)

# lda algorithm
gc()
memory.limit(9999999999)
gc()
set.seed(7,sample.kind = "Rejection")
fit.lda <- train(Cover_Type~., data=training_set, method="lda", metric=metric, trControl=control)
predect_lda<-predict(fit.lda, validation_set)
lda_conf_mat<-confusionMatrix(predect_lda, validation_set$Cover_Type)


### 2.2 Algorithm 2 ( nonlinear algorithm): Classification And Regression Tree (C.A.R.T)

## 3. Results
### 3.1 Result of (L.D.A.) model
lda_conf_mat$overall["Accuracy"] 

#lda confusion matrix
lda_conf_mat

### 3.2 Result of (C.A.R.T) model

#cart algorithm
gc()
memory.limit(9999999999)
gc()
set.seed(7,sample.kind = "Rejection")
fit.cart <- train(Cover_Type~., data=training_set, method="rpart", metric=metric, trControl=control, tuneGrid = data.frame(cp = seq(0, 0.05, len = 20)))
ggplot(fit.cart, highlight = TRUE)+ ggtitle("Accuracy for each number of selected complexity  predictors")
ggsave("32.jpeg", width = 10, height = 8)

### 3.2.1 The optimal complexity parameter "C.P."

#complexity parameter, echo=FALSE}
opt_cp <- fit.cart$bestTune %>% as.numeric()
paste0("Optimal cp parameter = ", toString(round(opt_cp, 2)))


### 3.2.2 Redefine the model using the train_data and optimal cp

#Redefine cart
gc()
memory.limit(9999999999)
gc()
model_rpart<-train(Cover_Type~., data=training_set, method="rpart", metric=metric, trControl=control, tuneGrid = data.frame(cp = opt_cp))
predect_cart<-predict(model_rpart, validation_set)
cart_conf_mat<-confusionMatrix(predect_cart, validation_set$Cover_Type)

#cart confusion matrix
cart_conf_mat

### 3.3. Summarize accuracy of models

#accuracy of models lda and cart
results <- resamples(list(lda=fit.lda, cart=model_rpart))
summary(results)

### 3.4. Compare accuracy and inter-rater reliability of models

#Comparing accuracy and inter-rater
dotplot(results)

### 3.5. Estimating skill of C.A.R.T. on the validation dataset

#Estimating skill of C.A.R.T. on the validation dataset
gc()
memory.limit(9999999999)
gc()
predictions <- predict(model_rpart, validation_set)
cart_conf_mat_validation<-confusionMatrix(predictions, validation_set$Cover_Type)
cart_conf_mat_validation
cart_conf_mat_validation$overall["Accuracy"]

## 4. Conclusion 

cart_conf_mat$overall["Accuracy"]
lda_conf_mat$overall["Accuracy"]
cart_conf_mat$overall["Kappa"]
lda_conf_mat$overall["Kappa"]



