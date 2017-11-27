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
str(dat) #looking at the structure of the data. variables need to be converted from integers to factors

#convert all variables from integers to factors
datf <- dat %>% mutate_if(is.integer,as.factor)
str(datf)

#check for column names
colnames(datf)
#insert column names based on soybean_large_names.txt file from:
#https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/
#name column names for original dataset (it will be used later)
colnames(dat) <- c("disease", "date", "plant.stand", "precip", "temp", "hail", "crop.hist",
                    "area.damaged", "severity", "seed.tmt", "germination", "plant.growth", "leaves",
                    "leafspots.halo", "leafspots.marg", "leafspot.size", "leaf.shred", "leaf.malf", 
                    "leaf.mild", "stem", "lodging", "stem.cankers", "canker.lesion", "fruiting.bodies",
                    "external.decay", "mycelium", "int.discolor", "sclerotia", "fruit.pods", "fruit.spots",
                    "seed", "mold.growth", "seed.discolor", "seed.size", "shriveling", "roots")
#name column names for new dataset with variables as factors
colnames(datf) <- c("disease", "date", "plant.stand", "precip", "temp", "hail", "crop.hist",
                   "area.damaged", "severity", "seed.tmt", "germination", "plant.growth", "leaves",
                   "leafspots.halo", "leafspots.marg", "leafspot.size", "leaf.shred", "leaf.malf", 
                   "leaf.mild", "stem", "lodging", "stem.cankers", "canker.lesion", "fruiting.bodies",
                   "external.decay", "mycelium", "int.discolor", "sclerotia", "fruit.pods", "fruit.spots",
                   "seed", "mold.growth", "seed.discolor", "seed.size", "shriveling", "roots")
#check to make sure columns had the correct name                 
colnames(datf)

#checking to see how many observations have been recorded for each disease.
table(datf$disease)


#data should be ready for analysis at this point




#### 2.2 - Summarize the disease classes by rating attribute frequency ----
#check column names
colnames(datf)

#checking frequencies of categorical variables across all diseases
datfreq <- datf %>% group_by(disease) %>% lapply(table)
datfreq #some overall contributors to disease in soybeans
  #precipitation, most observations occur when there is higher than normal precipitation
  #maybe temperature..
  #crop history, most observaitons occur when the crop is at least the same as it was the year before
  #area damaged: low areas in particular
  
  #common symptoms across the board 
    #plant growth: 1/3 of observations have abnormal plant growth
    #leaves: most ~90% have abnormal leaves
    #stem: look for stem abnormalities
  



#group dataset by disease
groupings <-group_by(datf, disease)
summarise(groupings)


#calculate means by disease across all variables using the original dataset that does not have all variables as factors.
#I realize that I am doing this with categorical variables, but I think it might be an easy way of quickly assessing some 
#of the predictor variables for each disease without having to make frequency tables for all 19 diseases. From this, only 
#predictor variables with very strong correlations (little variation) should stand out.
dat2 <- dat%>% group_by(disease) %>% summarise_all(funs(mean, median))
#view summarized means and medians 
dat2 



#arranging the data table I made above by abiotic factors I think might play
  #a role in disease prevalence
dattemp <- arrange(dat2, temp_mean)
dattemp #looks like rhizoctonia-root-rot occurs at lower than normal temps, 
  #diaporthe-pod-&-stem-blight seems to occur at higher than normal temps

datprecip <- arrange(dat2, precip_mean)
datprecip #charcoal rot and brown stem rot seemt o occur at lower than normal precipitation,
  #alternarialeaf-spot, brown spot, anthracnose, diaporthe-stem-canker, downy-mildew, purple-seed-stain, rhizoctonia-root-rot
  #seem to occur at higher than normal precipitation

datdate <- arrange(dat2, date_mean) #note to self, temperature and date are probably correlated
datdate #herbicide injury and rhizoctonia-root-rot occur in April-May, ~half of the diseases occur in August

#arranging the data table I made by other factors that I think may play 
  #a role in disease prevalence
datch <- arrange(dat2, crop.hist_mean)
datch #diaporthe-pod-&-stem-blight, cyst-nematode, brown spot, and brown stem rot all seem to occur when
  #soybeans been planted at least 2 years in a row in the same field

dathail <- arrange(dat2, hail_mean) #I'm looking at hail for physical damage to the plants, which could allow 
  #fungi or bacteria to enter inner tissues of the plant
dathail

datad <- arrange(dat2, area.damaged_mean) #Low areas in the field probably have more moisture and thus are mikely to be 
  #more prone to fungal diseases
datad # phytophthora-rot, rhizoctonia-root-rot, downy-mildew, purple-seed-stain seem to occur most often in low areas of the field

#### 2.3 - Summarize 5 selected diseases having strong rating correlations ----

#diseases I will summarize:
  #alternarialeaf-spot, n = 40
  #brown-spot, n = 40
  #downy-mildew, n = 10
  #rhizoctonia-root-rot, n = 10
  #charcoal-rot, n = 10


#create a data.frame for each of the 5 selected diseases using the filter() command
levels(datf$disease)


###data.frame for alternarialeaf-spot
altls <- filter(datf, disease == "alternarialeaf-spot")
altls

#generate frequency table of all predictor variables for alternarial leaf spot
altlsfreq <- altls %>% lapply(table)
altlsfreq #interesting patterns observed (non normal):
  #date: Aug-October
  #precip: occurs most often under higher than normal precipitation
  #temp: normal to higher than normal
  #severity: does not seem to be very severe for the most part, could be potentially severe
  #seed treatment: most often when seed treatments are not present or if a fungicide is used, which is interesting because alternarial leaf spot is a fungus (maybe resistance is occuring)
  #leaves: all abnormal. Probably a telltale sign of this disease
  #halo: no halos present
  #leafspots.marg: w-s-marg
  #leafspots size: greater than 1/8
  #all other variables seem normal


  #remove first column using the select function. 
  altls1 <- select(altls, date:roots)
  altls1 #data should now be ready for corrplot to look at correlations between variables. 
          #This will be used down the road when developing a predictive model for this particular disease
  caltls <- cor(altls1)
  corrplot(caltls, method = 'ellipse', type = "upper")

corr(altls)
  

  
###data.frame for brown-spot
brsp <- filter(datf, disease == "brown-spot")
brsp

#generate frequency table of all predictor variables for alternarial leaf spot
brspfreq <- brsp %>% lapply(table)
brspfreq #interesting patterns observed (non normal):
  #date: occurs mostly in may and june
  #plant stand: half of observations have plant stands lower than normal
  #precipitation: occurs most often under higher than normal precipitation
  #crop history: disease seems to be more prevalent in farms that have planted the same crop >= 2 years
  #area damaged: whole field typically
  #severity: potentially severe with a few observations of severe
  #seed treatment: most observations of brown spot occur when seed treatment is absent
  #leaves: abnormal
  #leafspots-halo: halos present, but are not yellow
  #leafspots marg: w-s-marg
  #leafspot size: greater than 1/8
  #leaf shred: present in half of the cases
  #stem: roughly 1/3 of the stems are abnormal
  #stem cankers: 1/4 of stem cankers occur above the second node
  #canker lesion: brown and tan
  #fruiting bodies: 1/3 of fruiting bodies are abnormal
  #all other predictor variables look normal



###data.frame for downy-mildew
dm <- filter(datf, disease == "downy-mildew")
dm

#generate frequency table of all predictor variables for alternarial leaf spot
dmfreq <- dm %>% lapply(table)
dmfreq #interesting patterns observed (non normal) (also, this is a relatively small dataset, so patterns may be tricky to find):
  #date: seems like it can occur at any time during the growing season
  #plant stand: 6/10 are lower than normal
  #precipitation: higher than normal precipitation
  #hail: occurred in 4/10 instances
  #crop history:disease seems to be more prevalent in farms that have planted the same crop >= 2 years
  #area damaged: seems to be localized where it is found and causing damage
  #severity: potentially severe
  #seed treatment: most cases do not have a seed treatment
  #germination: <=89% gemination
  #leaves: abnormal
  #leafspots: yellow halos mostly, sometimes halos present but not yellow
  #leafspots marg: w-s-marg
  #leafspot size: greater than 1/8
  #leaf-mild: lower surface
  #seed: abnormal 
  #mold growth: present
  



###data.frame for rhizoctonia-root-rot
rrr <- filter(datf, disease == "rhizoctonia-root-rot")
rrr

#generate frequency table of all predictor variables for alternarial leaf spot
rrrfreq <- rrr %>% lapply(table)
rrrfreq #interesting patterns observed (non normal) (also, this is a relatively small dataset, so patterns may be tricky to find):
  #date: ocurs early in the season (Arpil-June mostly)
  #plant stand: lower than normal
  #precipitation: higher than normal
  #temp: lower than normal
  #area damaged: low areas
  #severity: mostly severe
  #seed treatment: most do not have a seed treatment
  #gemination: <=89% germination, 6/10 have less than 80% germ.
  #plant growth: abnormal
  #stem: abnormal
  #lodging: yes
  #stem cankers: below the soil 
  #canker lesion: brown
  #external decay: present (there should be two categories for present: firm & dry and watery, however, the two categories do not exist, therefore, I used absent, present)
  #fruit pods: distorted



###data.frame for charcoal-rot
cr <- filter(datf, disease =="charcoal-rot")
cr

#generate frequency table of all predictor variables for alternarial leaf spot
crfreq <- cr %>% lapply(table)
crfreq #interesting patterns observed (non normal) (also, this is a relatively small dataset, so patterns may be tricky to find):
  #date: mid to late season
  #precipitation: lower than normal in all cases
  #temp: normal to greater than normal
  #area damaged: upper areas or whole field
  #severity: potentially severe
  #plant growth: abnormal
  #leaves: abnormal
  #stem: abnormal
  #canker lesion: tan
  #int discolor: black
  #sclerotia: present




#### 2.4 - Generate a few relevant graphics illustrating these summaries ----

#### 2.5 - Develop a preliminary predictive model from the ratings of one disease class ----




