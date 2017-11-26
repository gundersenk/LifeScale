#LifeScale Analytics R Coding CHallenge

#### 2.1 - Read and prepare the data for analysis ----

#set working directory
setwd("../../LifeScale_Coding_Challenge/")


#load appropriate packages
library(tidyverse)
library(corrplot)
library(ggplot2)


#read in dataset saved in local folder
#when being read in, I am converting all "?"s in the dataset to NAs
dat <- read.csv("./Data/Soybean_Large.csv", na.strings = c("?"))

#take a look at the dataset
head(dat) #no columns names
dim(dat) #306, 36

#check for column names
colnames(dat)
#insert column names based on soybean_large_names.txt file from:
#https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/
colnames(dat) <- c("disease", "date", "plant.stand", "precip", "temp", "hail", "crop.hist",
                   "area.damaged", "severity", "seed.tmt", "germination", "plant.growth", "leaves",
                   "leafspots.halo", "leafspots.marg", "leafspot.size", "leaf.shred", "leaf.malf", 
                   "leaf.mild", "stem", "lodging", "stem.cankers", "canker.lesion", "fruiting.bodies",
                   "external.decay", "mycelium", "int.discolor", "sclerotia", "fruit.pods", "fruit.spots",
                   "seed", "mold.growth", "seed.discolor", "seed.size", "shriveling", "roots")
#check to make sure columns had the correct name                 
colnames(dat)


#data should be ready for analysis at this point




#### 2.2 - Summarize the disease classes by rating attribute frequency ----
#check column names
colnames(dat)

#checking the structure of the dataset
str(dat)

#group dataset by disease
groupings <-group_by(dat, disease)
summarise(groupings)

#summarize all data by disease across all variables 
help(summarise_all)
dat2 <- dat %>% group_by(disease) %>% summarise_all(funs(mean, median))
#view summarized means and medians
dat2 #the output from this command does not show the entire dataset, thus I
      #looked at dat2 in the global environment 

#arranging the data table I made above by abiotic factors I think might play
  #a role in disease prevalence
dattemp <- arrange(dat2, temp_mean)
dattemp

datprecip <- arrange(dat2, precip_mean)
datprecip

datch <- arrange(dat2, crop.hist_mean)
datch

datdate <- arrange(dat2, date_mean) #note to self, temperature and date are probably correlated
datdate



#### 2.3 - Summarize 5 selected diseases having strong rating correlations ----

#diseases I will summarize:
  #Alternovial leaf spot, n = 40
  #brown spot, n = 40
  #frog eye leaf spot, n = 40
  #phyllosticta rot, n = 40
  #brown stem rot, n = 20



#### 2.4 - Generate a few relevant graphics illustrating these summaries ----

#### 2.5 - Develop a preliminary predictive model from the ratings of one disease class ----