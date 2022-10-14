## soil water content: Considilation & Cleaning (Miriam)
#----------------------------'

# goal is to create a file which has the following: 
  #1 extracting the dates
  #2 output file should ultimately be ordered by: 
  # ID, Genotype, Species, Treatment, Order withing block, mPot (kg), dry weight of soil (kg), mH2O (kg), Date, 
  # PWW, WA, BB, TW, POWW, PD.WT, midday.WT, WU, tgt_swc, p_swc, po_swc

  
  # variable names:PWW - pre water weight, WA - water added, TW - target water content (weight of plant+ the amount of water it should have post watering on that day),
  # POWW - post water weight, PD.WT - predawn pot weights, midday.WT - midday pot weights, p_swc - pre-water soil water content, po_swc - post-water soil water content
  
  #when clearing data: what do I have to think of?
    # what where the empty pots used for? 
    # how to handle the addition of bamboo shoots? 
    # --> maybe use bamboo shoots calculation sheet to incorporate?
    # could possibly adjust mpot by the amount of bamboo shoots beginning with the days that an additional bamboo shoot is added. There is an extra Excel sheet showing how many bamboo shoots where added on which day for each species
  #the question is: why do we have to calculate water usage? I see that if calculating it, we have to substract the evaporation (and therefore use the empty pods weight) but I don't see how this is related to the SMC? Is there something else we might want to calculate with this information?


# calculate p_swc and po_swc in a next step
  #p_swc = (pre-water weight/weight at field capacity) *100
  #po_swc = (post-water weight/weight at field capacity) * 100
# plot the p_swc and po_swc in a figure where y axis represents swc and x axis the time (date). Compare the p-swc and po_swc of one genotype (plot all the data points) for drought and control
# in Mastercode of Nico, there is already something written, so maybe that is a way to go? 
  
#---- here starts the code-----------------------------'

library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)

#Set working directory into the Input folder
setwd("~/Documents/Master_Thesis/Master_Thesis/data")

#read in watering data
wu<-(read_csv("WU_for_R.csv", #warnings are OK? --> why is that? what does a parsing issue mean #can check if it looks okay with the view(wu) function
              col_types = cols(.default = col_double(),
                               ID = col_character(), Genotype = col_character(),
                               Species = col_character(), Treatment = col_character())))[1:284,]

#removing text and notes in incorrect columns, or #VALUE which is output by excel if formulas are missing input values (this happened if, for example, controls aren't watered one day and treatments are, but the watering formulas in the spreadsheet were still copied for all plants)

metawu<-wu[1:4]
#View(wu)
f<-names(wu)
b<-data.frame(matrix(ncol = 0,nrow = 284))

for (i in f) {
  a<-str_replace_all(wu[[i]],"[:alpha:]",NA_character_)
  b<-cbind.data.frame(b,a)
  names(b)[length(names(b))]<- i }
cleanwu1<-b[-c(1,2,3,4)]
wateringdata<-cbind(metawu,lapply(cleanwu1,as.numeric)) ## the following part was orriginally in Nicos Code, but those dates/columns are not in the WU_for_R csv.file and an error occured. So I excluded them for now: 
# %>%select(!`11/15_cntrl.drained.wt`)%>%select(!contains("11/15_BB"))

#View(wateringdata)

# in the following, the waterusage was calculated. but why?
# this is nicos code, but I haven't understood yet, what he was doing here: 


##Calculating Water Use## Water use is the amount of water consumed by the plant since the last watering (Kg).
##For dates without a post water weight, just put the water added value as post water weight.
##Otherwise, subtract the pre water weight on day x from the post water weight on day x-1 

#Water Use Notes:
##Empty pots for control and treatment were each averaged per each day of watering in excel. With days where empty pot values weren't usable (calculations were wrong or people forgot to water.)
#the closest three days of the same watering volume were averaged and used (if control ep's are messed up, but should have been watered to 
# 80%, and then three days passed before the next watering, I found the most recent day where control empty pots had that condition and use the average of the eps from that day)
#If three days aren't available, then I used two or one day, as much as there was. I then subtracted the average ep water use per day per treatment from each plant.
#The sheet with corresponding edits is named wateruse.minus.ep.

# Some empty pot water use values are higher than the plants for that day, leading to negative water use values for the plants. Also, some plants had 
#  negative water use values before factoring in the empty pots. Since this is impossible, we will likely have to pull them from the water use dataset
# however, the negative water use values for the plants happened because the post water weight on watering day x-1 was somehow lower than the pre
# water weight on day x, meaning that the water use for the time between X-1 and X is negative. The difference between post and pre is more than a bamboo shoot, and 
# is not always accounted for by additional control waterings (see 11/2 and 12/2). However, it is interesting that all these negatives seem to be in the control plants.


#pulling in the control watering column with varied volumes on 12/6 and 12/13. Also missing 12/7 post water weight (Niko fixed this 4/12 so that 12/7 is now in WU_for_R.

vols126<-read_csv("controlwateradded126.csv")
vols1213<-read_csv("controlwateradded1213.csv")

# since the code below was already in hashtags, it might be irrelevant? what was it doing here? 

# code to copy and paste columns from google sheets. Had to make sure samples were in the same order.
#d<-readClipboard() 
#r<-d[2:285] #trimming white space
#rre<-str_replace_all(r,"[:alpha:]","")
#rre<-str_replace_all(rre,"`","")
#rre[rre==""]<-0
#post127<-data.frame("12/7 post water weight" = as.numeric(rre)) 
#write_csv(post1217,"post127.csv")

# I don't understand how Nico was able to mutate because I in the watering data there is no such thing like 10/26 or 10/28, so how did he do that? 
# I will probably have to use the rename(), the colnames() function to solve that or is there a better way to do so? and maybe while doing this, I could add an additional column with the dates? or can I reorder it first by Id and then rename all the dates (this might be way more complicated?)

#wateringdata_dates <- wateringdata %>% 
 # colnames(wateringdata)(5)= "mpot_1"
     # "dw" = "Dry weight of soil (kg)",
      # "mw" = "m`H20")

# other way:
#my_dataframe <- my_dataframe %>% 
 # rename("dw" = `Dry weight of soil (kg)`)

  #       "pages" = "c2",
   #      "name" = "c3")

# --> but how do I handle spaces in the names of the columns? 
# there are several ways: look here https://www.codingprof.com/5-easy-ways-to-replace-blanks-in-column-names-in-r-examples/ 

#wateringdata_1 <- wateringdata 
 # clean_names(wateringdata_1)
# wateringdata_1

# error that occurs here is, that it can't find the function clean_names although it is from tidyverse

# this is how nico did it. maybe that is any help further down? 
#wdlong<-wateruse[1:274,]%>%
  #pivot_longer(colnames(wateruse[9:ncol(wateruse)]),
               #names_to = c("Date", ".value"),
               #names_pattern = "(.+)_(.+)")%>%

#This code calculates water use for empty pots, but without taking into account bamboo shoots or supplemental watering, since empty pots didn't receive any. Mostly just copied from the below code chunk which includes bamboo shoots and eps, so look there for notes about confusing code
wateruseEP<-wateringdata%>%
  mutate_at(colnames(wateringdata[5:ncol(wateringdata)]), as.numeric)%>%
  mutate("10/26_WU" = wateringdata$`10/26_WA`)%>%
  mutate("10/28_WU" = wateringdata$`10/28_WA`)%>%
  mutate("10/30_WU" = wateringdata$`10/30_WA`)%>%
 # here the problem starts: the error occuring in 'mutate: Problem while computing, 11/02, or 11/04 and following must be size 284 or 1 not 0
   mutate("11/02_WU" = wateringdata$`10/30_TW`-wateringdata$`11/02_PWW`)%>%  #On this day, drought empty pots were watered, but the plants weren't. I chose to still subtract the empty pots for drought treatment (from Niko)
  
  mutate("11/04_WU" = wateringdata$`11/02_POWW`-wateringdata$`11/04_PWW`)%>% 
  mutate("11/06_WU" = wateringdata$`11/04_POWW`-wateringdata$`11/06_PWW`)%>%
  mutate("11/09_WU" = wateringdata$`11/06_POWW`-wateringdata$`11/09_PWW`)%>%
  mutate("11/11_WU" = wateringdata$`11/09_POWW`-wateringdata$`11/11_PWW`)%>%
  mutate("11/13_WU" = ifelse(wateringdata$Treatment == "Control",wateringdata$`11/11_POWW`-wateringdata$`11/13_PWW`,
                             wateringdata$`11/11_PWW`-wateringdata$`11/13_PWW`))%>% #since drought treatment pots weren't watered, the pre water weight is relevant here.
  mutate("11/16_WU" = ifelse(wateringdata$Treatment == "Drought", as.numeric(wateringdata$`11/13_PWW`)-as.numeric(wateringdata$`11/16_PWW`), NA_integer_))%>%
  mutate("11/18_WU" = ifelse(wateringdata$Treatment == "Drought", wateringdata$`11/16_POWW`-wateringdata$`11/18_PWW`,
                             wateringdata$`11/16_PWW`-wateringdata$`11/18_PWW`))%>%
  mutate("11/20_WU" =  wateringdata$`11/18_POWW`-wateringdata$`11/20_PWW`)%>%
  mutate("11/23_WU" =  wateringdata$`11/20_POWW`-wateringdata$`11/23_PWW`)%>%
  mutate("11/25_WU" =  wateringdata$`11/23_POWW`-wateringdata$`11/25_PWW`)%>%
  mutate("11/27_WU" =  wateringdata$`11/25_POWW`-wateringdata$`11/27_PWW`)%>%
  mutate("11/30_WU" =  ifelse(wateringdata$Treatment == "Drought", wateringdata$`11/27_POWW`-wateringdata$`11/30_PWW`,
                              (wateringdata$`11/27_POWW`-wateringdata$`11/30_PWW`)))%>%
  mutate("12/02_WU" =  wateringdata$`11/30_POWW`-wateringdata$`12/02_PWW`)%>%
  mutate("12/04_WU" =  wateringdata$`12/02_POWW`-wateringdata$`12/04_PWW`)%>%
  mutate("12/07_WU" =  wateringdata$`12/04_POWW`-wateringdata$`12/07_PWW`)%>%
  mutate("12/09_WU" =  wateringdata$`12/07_POWW`-wateringdata$`12/09_PWW`)%>%
  mutate("12/11_WU" =  ifelse(wateringdata$Treatment == "Drought",
                              ifelse(is.na(wateringdata$`12/09_POWW`) == TRUE, wateringdata$`12/09_POWW`-wateringdata$`12/11_PWW`,
                                     (wateringdata$`12/09_POWW`-wateringdata$`12/11_PWW`)),
                              ifelse(is.na(wateringdata$`12/09_POWW`) == TRUE, (wateringdata$`12/09_POWW`-wateringdata$`12/11_PWW`),
                                     (wateringdata$`12/09_POWW`-wateringdata$`12/11_PWW`))))%>%
  mutate("12/14_WU" =  ifelse(wateringdata$Treatment == "Drought", (wateringdata$`12/11_POWW`-wateringdata$`12/14_PWW`)+vols1213$X12.13.water.added,
                              (wateringdata$`12/11_POWW`-wateringdata$`12/14_PWW`)+vols1213$X12.13.water.added))%>%
  mutate("12/16_WU" =  wateringdata$`12/14_POWW`-wateringdata$`12/16_PWW`)%>%
  mutate("12/17_WU" =  wateringdata$`12/16_POWW`-wateringdata$`12/17_PWW`)


emptypots<-wateruseEP%>%
  slice_tail(n=10)%>%select(!contains(c("BB","Genotype","Species","Order Within Block")))%>%
  mutate("11/16_WU" = ifelse(Treatment == "Control", (`11/06_WU`+`11/09_WU`+`11/11_WU`)/3,`11/16_WU`))%>% #filling in water use for control pots in case that is necessary, even though controls were not watered on this interval. I averaged 3 days where the target weights were the same (80%)
  mutate("12/02_WU" = (`11/25_WU`+`11/27_WU`)/2)%>% #for some reason there is not data for empty pots on these days, so I averaged the two other watering intervals with the same target water contents and used them.
  mutate("12/04_WU" = (`11/25_WU`+`11/27_WU`)/2)%>% #for some reason there is not data for empty pots on these days, so I averaged the two other watering intervals with the same target water contents and used them.
  select(contains(c("WU","Treatment")))
emptypots[emptypots<0]<-NA

emptypotsAVG<-emptypots%>%
  group_by(Treatment)%>%
  summarise(across(everything(),~ mean(., na.rm = TRUE)))
#View(emptypotsAVG)

#

#View(wateringdata)

#look at how the SWC was calculated. how does this make sense? 
#where are the dates coming from/ hoe did nico manage to convert them from the excel spreadsheet into actually usable data points? 
#from my understanding right now, the water use sheet is not too bad. I just wouldn't use the water usage to calculate the smc, I would rather use mpot and adjust it to the bamboo shoots and devide the treatment by the field capacity. in an extra spread sheet. However, I would have to come up with an formula, that uses the bamboo shoot spreadsheet and incorporates it to the mpot: saying, if there are two bamboo shoots, add amount x to mpod_adjusted and if three, add amount x*2 to mpod etc. Coulnd't that be a great possibility? 
# but I first would have to adjust the entire sheet that it is ordered by the right dates. and how do I order the bamboo-shoot csv file to make that clear? 