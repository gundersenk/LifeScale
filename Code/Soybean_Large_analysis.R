#LifeScale Analytics R Coding CHallenge

#### 2.1 - Read and prepare the data for analysis ----

#set working directory
setwd("../Knute/Desktop/Knute/LifeScale_Coding_Challenge/Data/")


#load appropriate packages
library(tidyverse)
library(corrplot)
library(ggplot2)
library(cowplot)


#read in dataset saved in local folder
#when being read in, I am converting all "?"s in the dataset to NAs
dat <- read.csv("./Soybean_Large.csv", na.strings = c("?"))



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


#calculate means and standard deviation by disease across all variables using the original dataset that does not have all variables as factors.
#I realize that I am doing this with categorical variables, but I think it might be an easy way of quickly assessing some 
#of the predictor variables for each disease without having to make frequency tables for all 19 diseases. From this, only 
#predictor variables with very strong correlations (little variation) should stand out.
dat2 <- dat%>% group_by(disease) %>% summarise_all(funs(mean, sd))
#view summarized means and standard deviations 
dat2 



#arranging the data table I made above by abiotic factors I think might play
  #a role in disease prevalence
dattemp <- arrange(dat2, temp_mean)
dattemp #looks like rhizoctonia-root-rot occurs at lower than normal temps, 
  #diaporthe-pod-&-stem-blight seems to occur at higher than normal temps

datprecip <- arrange(dat2, precip_mean)
datprecip #charcoal rot and brown stem rot seem to occur at lower than normal precipitation,
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

#generate frequency table of all predictor variables for brown spot
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

#generate frequency table of all predictor variables for downy mildew
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

#generate frequency table of all predictor variables for rhizoctonia root rot
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

#generate frequency table of all predictor variables for charcoal rot
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

#setting themes for the figures
mytheme <- theme_bw() + 
  theme(panel.border=element_rect(color="black"), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position=c(0.1,0.97), legend.justification=c(0,1), 
        legend.key.size=unit(2, "cm"),
        legend.background=element_rect(color="black"), 
        legend.key=element_rect(colour="black"),
        axis.text.x = element_text(angle=0, hjust = 0.5),
        axis.title.x = element_text(),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

mytheme2 <- theme_bw() + 
  theme(panel.border=element_rect(color="black"), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        axis.text.x = element_text(angle=0, hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



  ####graphics of summaries for alternarial leaf spot----
#row 1
#date
altdate <- ggplot(altls ,aes(x=date)) +
  geom_bar(fill="skyblue") +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop = FALSE, "Date", breaks = c("0","1","2","3","4","5","6"), labels=c("Apr","May","Jun","Jul","Aug","Sept","Oct")) +
  mytheme
altdate

#precipitation
altprec <- ggplot(altls ,aes(x=precip)) +
 geom_bar(fill="skyblue") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop=FALSE, "Precipitation", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
  mytheme2
altprec

#temperature
alttemp <- ggplot(altls ,aes(x=temp)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Temperature", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
alttemp

#row 2
#severity
altseverity <- ggplot(altls ,aes(x=severity)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Severity", breaks = c("0","1","2"), labels=c("minor","pot severe","severe")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme
altseverity

#seed treatment
altseedtrt <- ggplot(altls ,aes(x=seed.tmt)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Seed treatment", breaks = c("0","1","2"), labels=c("none","fungicide","other")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
altseedtrt

#leaves
altleaves <- ggplot(altls ,aes(x=leaves)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leaves", breaks = c("0","1"), labels=c("normal","abnormal")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
altleaves

#row 3
#leafspots halo
althalo <- ggplot(altls ,aes(x=leafspots.halo)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots halo", breaks = c("0","1", "2"), labels=c("absent","yellow", "not yellow")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme
althalo

#leafspots marg
altmarg <- ggplot(altls ,aes(x=leafspots.marg)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots-marg", breaks = c("0","1", "2"), labels=c("w-s-marg","no-w-s-marg", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
altmarg

#leafspot size
altlsize <- ggplot(altls ,aes(x=leafspot.size)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspot size", breaks = c("0","1", "2"), labels=c("< 1/8","> 1/8", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
altlsize

#final 3x3 graphic for alternarial leaf spot
altsumfreq <- plot_grid(altdate, altprec, alttemp, 
                        altseverity, altseedtrt, altleaves,
                        althalo, altmarg, altlsize,
                       labels = c("", "", "", 
                                  "", "", "", 
                                  "", "", ""),
                       ncol = 3, nrow = 3) 
altsumfreq

title <- ggdraw() + draw_label("Alternarial leaf spot", fontface='bold')
plot_grid(title, altsumfreq, ncol=1, rel_heights=c(.05, 1))






  ####graphics of summaries for brown spot----

#Row 1
#date
brspdate <- ggplot(brsp ,aes(x=date)) +
  geom_bar(fill="skyblue") +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop = FALSE, "Date", breaks = c("0","1","2","3","4","5","6"), labels=c("Apr","May","Jun","Jul","Aug","Sept","Oct")) +
  mytheme
brspdate

#precipitation
brspprec <- ggplot(brsp ,aes(x=precip)) +
  geom_bar(fill="skyblue") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop=FALSE, "Precipitation", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
  mytheme2
brspprec

#crop history
brspcphist <- ggplot(brsp ,aes(x=crop.hist)) +
  geom_bar(fill="skyblue") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop=FALSE, "Crop history", breaks = c("0","1","2", "3"), 
                   labels=c("diff lst yr","same lst yr","same lst 2 yrs", "same lst sev yrs")) +
  mytheme2
brspcphist

#Row 2
#area damaged
brspad <- ggplot(brsp ,aes(x=area.damaged)) +
  geom_bar(fill="skyblue") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) +
  scale_x_discrete(drop=FALSE, "Area damaged", breaks = c("0","1","2", "3"), 
                   labels=c("scattered","lower fld","upper fld", "whole fld")) +
mytheme
brspad

#severity
brspsev <- ggplot(brsp ,aes(x=severity)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Severity", breaks = c("0","1","2"), labels=c("minor","pot severe","severe")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + 
  mytheme2
brspsev

#leaves
brspleaves <- ggplot(brsp ,aes(x=leaves)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leaves", breaks = c("0","1"), labels=c("normal","abnormal")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
brspleaves


#Row 3
#leafspots halo
brsphalo <- ggplot(brsp ,aes(x=leafspots.halo)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots halo", breaks = c("0","1", "2"), labels=c("absent","yellow", "not yellow")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme
brsphalo

#leafspots marg
brspmarg <- ggplot(brsp ,aes(x=leafspots.marg)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots-marg", breaks = c("0","1", "2"), labels=c("w-s-marg","no-w-s-marg", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
brspmarg

#leafspot size
brspsize <- ggplot(brsp ,aes(x=leafspot.size)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspot size", breaks = c("0","1", "2"), labels=c("< 1/8","> 1/8", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
brspsize

#Row 4
#stem
brspstem <- ggplot(brsp ,aes(x=stem)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Stem", breaks = c("0","1"), labels=c("normal","abnormal")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme
brspstem

#stem cankers
brspsc <- ggplot(brsp ,aes(x=stem.cankers)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=TRUE, "Stem cankers", breaks = c("0","1", "2", "3"), labels=c("absent","below soil", "above soil", "above 2nd node")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
brspsc

#canker lesion
brspcl <- ggplot(brsp ,aes(x=canker.lesion)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Canker lesion", breaks = c("0","1", "2", "3"), labels=c("absent","below soil", "above soil", "above 2nd node")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 41)) + mytheme2
brspcl


#final 3x4 graphic for brown spot
brspsumfreq <- plot_grid(brspdate, brspprec, brspcphist, 
                         brspad, brspsev, brspleaves,
                         brsphalo, brspmarg, brspsize,
                         brspstem, brspsc, brspcl,
                        labels = c("", "", "", 
                                   "", "", "", 
                                   "", "", ""),
                        ncol = 3, nrow = 4) 
brspsumfreq

brsptitle <- ggdraw() + draw_label("Brown spot", fontface='bold')
plot_grid(brsptitle, brspsumfreq, ncol=1, rel_heights=c(.05, 1))


  ####graphics of summaries for downy mildew ----
#Row 1

#precipitation
dmprec <- ggplot(dm ,aes(x=precip)) +
  geom_bar(fill="skyblue") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
  scale_x_discrete(drop=FALSE, "Precipitation", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
  mytheme
dmprec

  #seed treatment
dmseedtrt <- ggplot(dm ,aes(x=seed.tmt)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Seed treatment", breaks = c("0","1","2"), labels=c("none","fungicide","other")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmseedtrt

#leaves
dmleaves <- ggplot(dm ,aes(x=leaves)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leaves", breaks = c("0","1"), labels=c("normal","abnormal")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmleaves


#Row 2
#leafspots halo
dmhalo <- ggplot(dm ,aes(x=leafspots.halo)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots halo", breaks = c("0","1", "2"), labels=c("absent","yellow", "not yellow")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme
dmhalo

#leafspots margin
dmmarg <- ggplot(dm ,aes(x=leafspots.marg)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspots-marg", breaks = c("0","1", "2"), labels=c("w-s-marg","no-w-s-marg", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmmarg

#leafspot size
dmsize <- ggplot(dm ,aes(x=leafspot.size)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leafspot size", breaks = c("0","1", "2"), labels=c("< 1/8","> 1/8", "dna")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmsize


#Row 3
#mold growth
dmmgrowth <- ggplot(dm ,aes(x=mold.growth)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Mold growth", breaks = c("0","1"), labels=c("absent","present")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme
dmmgrowth
 
#leaf mildew
dmlmild <- ggplot(dm ,aes(x=leaf.mild)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Leaf mildew", breaks = c("0","1", "2"), labels=c("absent","upper surf", "lower surf")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmlmild

#seed
dmseed <- ggplot(dm ,aes(x=seed)) +
  geom_bar(fill="skyblue") + 
  scale_x_discrete(drop=FALSE, "Seed", breaks = c("0","1"), labels=c("normal","abnormal")) +
  scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + mytheme2
dmseed


#final figure for downy mildew
dmsumfreq <- plot_grid(dmprec, dmseedtrt, dmleaves, 
                         dmhalo, dmmarg, dmsize,
                         dmmgrowth, dmlmild, dmseed,
                         labels = c("", "", "", 
                                    "", "", "", 
                                    "", "", ""),
                         ncol = 3, nrow = 3) 
dmsumfreq

dmtitle <- ggdraw() + draw_label("Downy mildew", fontface='bold')
plot_grid(dmtitle, dmsumfreq, ncol=1, rel_heights=c(.05, 1))

  ####graphics of summaries for rhizoctonia-root-rot ----
#row 1  
  #date
  rrrdate <- ggplot(rrr, aes(x=date)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Date", breaks = c("0","1","2","3","4","5","6"), labels=c("Apr","May","Jun","Jul","Aug","Sept","Oct")) +
    mytheme
  rrrdate


  #precipitation
  rrrprec <- ggplot(rrr, aes(x=precip)) +
    geom_bar(fill="skyblue") + 
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop=FALSE, "Precipitation", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
    mytheme2
  rrrprec
  
  #temperature
  rrrtemp <- ggplot(rrr, aes(x = temp)) +
    geom_bar(fill="skyblue") + 
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop=FALSE, "Temperature", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
    mytheme2
  rrrtemp
  
#row 2
  #severity
   rrrsev <- ggplot(rrr ,aes(x=severity)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Severity", breaks = c("0","1","2"), labels=c("minor","pot severe","severe")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme
  rrrsev

  #seed treatment
  rrrseedtrt <- ggplot(rrr ,aes(x=seed.tmt)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Seed treatment", breaks = c("0","1","2"), labels=c("none","fungicide","other")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme2
  rrrseedtrt
  
  #plant growth
  rrrptgrowth <- ggplot(rrr ,aes(x=plant.growth)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Plant growth", breaks = c("0","1"), labels=c("normal","abnormal")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme2 
  rrrptgrowth
  
#Row 3
  #stem
  rrrstem <- ggplot(rrr ,aes(x=stem)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Stem", breaks = c("0","1"), labels=c("normal","abnormal")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme
  rrrstem
  
  #lodging
  rrrlodging <- ggplot(rrr ,aes(x=lodging)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Lodging", breaks = c("0","1"), labels=c("yes","no")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme2
  rrrlodging  
  
  #stem cankers
  rrrsc <- ggplot(rrr ,aes(x=stem.cankers)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Stem cankers", breaks = c("0","1", "2", "3"), labels=c("absent","below soil", "above soil", "above 2nd node")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme2
  rrrsc
  
  
  #Row 4
  rrrcl <- ggplot(rrr ,aes(x=canker.lesion)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "Canker lesion", breaks = c("0","1", "2", "3"), labels=c("dna","brown", "dk brown/black", "tan")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme
  rrrcl
  
  rrred <- ggplot(rrr ,aes(x=external.decay)) +
    geom_bar(fill="skyblue") + 
    scale_x_discrete(drop=FALSE, "External decay", breaks = c("0","1"), labels=c("absent", "present")) +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) + 
    mytheme2
  rrred
  

  
  #final 3x4 graphic for rhizoctonia-root-rot
  rrrsumfreq <- plot_grid(rrrdate, rrrprec, rrrtemp, 
                           rrrsev, rrrseedtrt, rrrptgrowth,
                           rrrstem, rrrlodging, rrrsc,
                           rrrcl, rrred,
                           labels = c("", "", "", 
                                      "", "", "", 
                                      "", ""),
                           ncol = 3, nrow = 4) 
  rrrsumfreq
  
  rrrtitle <- ggdraw() + draw_label("Rhizoctonia root rot", fontface='bold')
  plot_grid(rrrtitle, rrrsumfreq, ncol=1, rel_heights=c(.05, 1))
  
  ####graphics of summarire for charcoal rot----
#row 1 
  #precipitation
  crprecip <- ggplot(cr, aes(x=precip)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Precipitation", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
    mytheme
  crprecip
  
  #temp
  crtemp <- ggplot(cr, aes(x=temp)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Temp", breaks = c("0","1","2"), labels=c("< norm","norm","> norm")) +
    mytheme2
  crtemp
  
  #area damaged
  crad <- ggplot(cr, aes(x=area.damaged)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Area damaged", breaks = c("0","1","2","3"), 
                     labels=c("scattered","lower fld","upper fld", "whole fld")) +
   mytheme2
  crad
  
  
#Row 2
  #plant growth
  crplgrowth <- ggplot(cr, aes(x=plant.growth)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Plant growth", breaks = c("0","1"), labels=c("normal","abnormal")) +
    mytheme
  crplgrowth
  
  #leaves
  crleaves <- ggplot(cr, aes(x=leaves)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Leaves", breaks = c("0","1"), labels=c("normal","abnormal")) +
    mytheme2
  crleaves
  
  #stem
  crstem <- ggplot(cr, aes(x=stem)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Stem", breaks = c("0","1"), labels=c("normal","abnormal")) +
    mytheme2
  crstem
  
#Row 3
  #canker lesion
  crcl <- ggplot(cr, aes(x=canker.lesion)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = TRUE, "Canker lesion", breaks = c("0","1","2","3"), 
                     labels=c("dna","brown","drk brown/black","tan")) +
    mytheme
  crcl
  
  #int discolor
  crintd <- ggplot(cr, aes(x=int.discolor)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Int discolor", breaks = c("0","1","2"), 
                     labels=c("none","brown","black")) +
    mytheme2
  crintd
  
  #scleritia
  crscl  <- ggplot(cr, aes(x=sclerotia)) +
    geom_bar(fill="skyblue") +
    scale_y_continuous(expand = c(0,0),limits = c(0, 11)) +
    scale_x_discrete(drop = FALSE, "Sclerotia", breaks = c("0","1"), 
                     labels=c("absent","present")) +
    mytheme2
  crscl
  
  
  #final 3x3 graphic for charcoal rot
  crsumfreq <- plot_grid(crprecip, crtemp, crad, 
                         crplgrowth, crleaves, crstem ,
                          crcl, crintd, crscl,
                          
                          labels = c("", "", "", 
                                     "", "", "", 
                                     "", "", ""),
                          ncol = 3, nrow = 3) 
  crsumfreq
  
  crtitle <- ggdraw() + draw_label("Charcoal rot", fontface='bold')
  plot_grid(crtitle, crsumfreq, ncol=1, rel_heights=c(.05, 1))
  
  
  
  
  
#### 2.5 - Develop a preliminary predictive model from the ratings of one disease class ----

  #checking the structure of my dataset. note that disease is a factor comprised of names for variables. 
  #we need to change this to binary to develop a model
  str(datf)
  levels(datf$disease)

  #creating a binomial vector at the end of the dataframe for logistic regression 
  #(0 = no alternarial leaf spot, 1 = alternarial leaf spot)
  datf$bindisease <- as.numeric(datf$disease == "alternarialeaf-spot")
  datf

  datf$bindisease <-as.factor(datf$bindisease)  
  is.factor(datf$bindisease)

  str(datf$bindisease)
  

  #omit rows with NAs
  nonadatf <- datf[complete.cases(datf), ]
  dim(nonadatf) #checking dimensions
  str(nonadatf) #checking structure
  
  
  #best fit model to predict alternarial leaf spot
  library(multcomp)
  library(glm2)
  
  contrasts(datf$disease)
  contrasts(nonadatf$bindisease)
  
  #logistic regression
  model <- glm(nonadatf$bindisease ~ nonadatf$severity, family = binomial(link = 'logit'), data = nonadatf)
  summary(model)
  plot(model)
  
  
  #Stepwise AIC - forward and reverse progression to select final model
  min.pred <- glm(nonadatf$bindisease ~ 1, family = binomial(link='logit'))
  full.pred = stepAIC(min.pred, direction='forward', scope=(~ nonadatf$date +
                                                           nonadatf$precip + 
                                                           nonadatf$temp + 
                                                           nonadatf$severity + 
                                                           nonadatf$seed.tmt + 
                                                           nonadatf$leaves +
                                                           nonadatf$leafspots.halo + 
                                                           nonadatf$leafspots.marg + 
                                                           nonadatf$leafspot.size))
  
  full.pred$anova
  
  #model selected by AIC
  best.fit <- glm(nonadatf$bindisease ~ nonadatf$date + nonadatf$leafspot.size + 
                    nonadatf$precip + nonadatf$leafspots.halo + nonadatf$temp, family = binomial(link = 'logit'))
  summary(best.fit)

  #It looks like the logistic regression is not working because we have perfect separation of some variables.
  #I think the best way to progress is by using a Bayesian logistic regression 
  
  #attempt at a Bayesian logistic regression
  
  library(MCMCpack)
  
  #check the dimensions of the dataset
  dim(nonadatf) #265 rows
  
  #create a dataframe for the bayesian regression using all 265 rows with no replacement (max size of the dataframe)
  brdat <- nonadatf[sample(1:nrow(nonadatf), 265, replace = FALSE),]
  
  help(MCMCregress)
  
  #attempt at the regression. unfortunately, it did not work out because I don't remember how I get my prior means and prior precisions
  #also, I am running into another error that I don't currently know how to fix.
  breg <- MCMCregress(bindisease ~ temp, b0 = 0, B0 = 0, data = brdat)
  
  
  
  
  
  
  
  
  
  
  
