library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)


####Water Potential Consolidation and Cleaning-------------------------------------------------------------------------------------------

#NOTES: 12/1 had duplicate wps and leaves for some measurements. I picked the lowest wps in every case,
    #unless a previous note specified otherwise. When leaves were labeled L1 or L2, I picked the corresponding 
    #leaf to the lowest wp.

###Reading in water potential data
setwd("./Input") #move into the Input folder in vitisdrought
wp<-read_csv("WP_for_R.csv")
wp<-wp[-(275:284), ] #Removing empty pots
meta<-wp[1:5] #saving string metadata before removing non-numerical data(notes) from data columns

###cleaning words from columns of numerical data in the wp spreadsheet###

f<-names(wp)
b<-data.frame(matrix(ncol = 0,nrow = 274))

for (i in f) {
  a<-str_replace_all(wp[[i]],"[:alpha:]",NA_character_)
  b<-cbind.data.frame(b,a)
  names(b)[length(names(b))]<- i 
  
}
cleanwp<-cbind(meta,b[,6:ncol(b)])
#View(cleanwp)

####Combining daily leaf water potential values (lwp), predawns (pd),
    #and stem water potentials (SWP) into a longform dataset###

#Making leaf water potentials long form
Leafwaterpotentials<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".lwp")&!contains(".area"))
Leafwaterpotentials_Gathered<-Leafwaterpotentials%>%gather("Date","LWP",`11.9.lwp`:ncol(Leafwaterpotentials))%>%
  select("ID","Genotype","Treatment","Date","LWP")
Leafwaterpotentials_Gathered$Date<-gsub(".Harvest.lwp","", Leafwaterpotentials_Gathered$Date)
Leafwaterpotentials_Gathered$Date<-gsub(".lwp","", Leafwaterpotentials_Gathered$Date)

Leafwaterpotentials.area<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".lwp")&contains(".area"))
Leafwaterpotentials.area_Gathered<-Leafwaterpotentials.area%>%gather("Date","LWP.area",`11.9.lwp.area`:ncol(Leafwaterpotentials.area))%>%
  select("ID","Genotype","Treatment","Date","LWP.area")
Leafwaterpotentials.area_Gathered$Date<-gsub(".Harvest.lwp.area","", Leafwaterpotentials.area_Gathered$Date)
Leafwaterpotentials.area_Gathered$Date<-gsub(".lwp.area","", Leafwaterpotentials.area_Gathered$Date)

#Making predawns long form
pdlwp<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".pd")&!contains(".area"))
pdlwp_Gathered<-pdlwp%>%gather("Date","PD",`11.11.pd`:ncol(pdlwp))%>%
  select("ID","Genotype","Treatment","Date","PD")
pdlwp_Gathered$Date<-gsub(".als.pd","", pdlwp_Gathered$Date)
pdlwp_Gathered$Date<-gsub(".pd","", pdlwp_Gathered$Date)

pdlwp.area<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".pd")&contains(".area"))
pdlwp.area_Gathered<-pdlwp.area%>%gather("Date","PD.area",`12.11.pd.area`:ncol(pdlwp.area))%>%
  select("ID","Genotype","Treatment","Date","PD.area")
pdlwp.area_Gathered$Date<-gsub(".pd.area","", pdlwp.area_Gathered$Date)

#Making stem water potentials long form
swp<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".swp")&!contains(".area"))
swp_Gathered<-swp%>%gather("Date","SWP",`12.11.swp`:ncol(swp))%>%
  select("ID","Genotype","Treatment","Date","SWP")
swp_Gathered$Date<-gsub(".swp","", swp_Gathered$Date)

swp.area<-cleanwp%>%
  select("ID","Genotype","Treatment",contains(".swp")&contains(".area"))
swp.area_Gathered<-swp.area%>%gather("Date","SWP.area",`12.11.swp.area`:ncol(swp.area))%>%
  select("ID","Genotype","Treatment","Date","SWP.area")
swp.area_Gathered$Date<-gsub(".swp.area","", swp.area_Gathered$Date)

#Joining water potentials into a single dataframe | Note: Each day has a row for every plant, even if there were no measurements for that plant on that day. Potentially fix this? 
waterpotentials_gathered<-full_join(Leafwaterpotentials_Gathered,swp_Gathered)%>%
  full_join(pdlwp_Gathered)%>%
  full_join(Leafwaterpotentials.area_Gathered)%>%
  full_join(swp.area_Gathered)%>%
  full_join(pdlwp.area_Gathered)

#View(waterpotentials_gathered)


###Licor 6800 Point Measurements Consolidation and Cleaning-----------------------------------------------------------------------------
#|Note: This code takes a folder of .xsls type licor point measurement files and compiles them by date,
        #and the user defined constants "accession" and "rep"

#When running this function, select the folder containing all of the licor excel files.

licor6800.point_measurement.compiler<-function(){
  filenames <- list.files("Raw 6800 Point measurements", pattern="*.xlsx", full.names=TRUE) #pick the folder with 6800 xlsx files. If unsure replace "Raw 6800 Point measurements" with chose.dir() and manually pick the folder 
  for (filename in filenames) { #cycle through each file and apply the following code to each file 
    meas<-read_xlsx(filename,skip = 14)[-1,] #pulls the relevant measurements from the main tab of the spreadsheet
    id<-(read_xlsx(filename,sheet = "Remarks"))[c(1:6),] #pull the console and head id from the second tab of the spreadsheet
    meas.id<-meas%>%select("date",6,7,"A","Ci","CO2_s","gsw","Pa","Qin","RHcham","Tleaf")%>% # selecting the columns you care about. 6 & 7 are user defined constants.
      mutate("6800_licor_id" = paste0(id[1,2],"|",id[4,2]))%>% #save console and head id as a column
      rename("gsw_6800" = gsw)%>%
      rename("Genotype" = accession)%>%
      mutate("ID" = paste0(Genotype,".",str_sub(rep, start = 2)))%>%
      mutate("Treatment" = ifelse(as.numeric(str_sub(rep, start = 2))>5,"Control","Drought"))%>%
      mutate("date" = str_split_fixed(meas$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 5))%>%
      mutate("Time" = str_split_fixed(meas$date, "[:blank:](?=\\d)", 2)[,2])%>%
      rename("Date" = date)
    meas.id$Date <- sub("(?<=^..)", "\\.", meas.id$Date, perl=TRUE)
    complete6800<-if(exists('complete6800') == TRUE){rbind(complete6800,meas.id)} else{meas.id} #appending this individual formatted file to the compiled file
  }
  return(complete6800)
}
compiled6800<-licor6800.point_measurement.compiler()
#View(compiled6800)

###Cleaning the licor 6800 point measurement data based on stomatal conductance values###

#Cleaning point measurements by removing negative gsw values

compiled6800$gsw_6800<-as.numeric(compiled6800$gsw_6800) #Making gsw numeric

compiled6800 <- add_column(compiled6800, Code_Date = paste(compiled6800$ID,compiled6800$Date,sep = "_"), .after = "Date")
unique_CodeDate <- unique(compiled6800$Code_Date)

list <- NULL

for(i in 1:length(unique_CodeDate)){
  rows_niko <- which(compiled6800$Code_Date == unique_CodeDate[i])
  repetion <- seq(from = 1, to = length(rows_niko), by = 1)
  list[[i]] <- add_column(compiled6800[rows_niko,], Rep = repetion, .after = "Time")
}

dd_wreps <- do.call(bind_rows, list)

dd_wreps_noneg<-dd_wreps%>%filter(gsw_6800 >0)%>%
  arrange(Date,Time)
dd_wreps_noneg$ID<-gsub(".10",".10.",dd_wreps_noneg$ID)
#View(dd_wreps_noneg)


#Removing outliers using 3sigma outlier removal on IDs with 3+ reps#
#NOTE: No outliers found using this method


#putting rep numbers on scans to find IDs with 3+ reps
extrarep<-dd_wreps_noneg%>%filter(Rep == 3)%>%select(Code_Date)
#View(extrarep)
list <- NULL

for(i in 1:length(extrarep$Code_Date)){
  rows_niko <- which(dd_wreps_noneg$Code_Date == extrarep$Code_Date[i]) #this pulls a unique id on a unique day
  lbound<-mean(dd_wreps_noneg[rows_niko,]$gsw_6800)-3*sd(dd_wreps_noneg[rows_niko,]$gsw_6800)
  hbound<-mean(dd_wreps_noneg[rows_niko,]$gsw_6800)+3*sd(dd_wreps_noneg[rows_niko,]$gsw_6800)
  list[[i]]<-dd_wreps_noneg[rows_niko,]%>%filter(gsw_6800<lbound | gsw_6800>hbound)
  #repetion <- seq(from = 1, to = length(rows_niko), by = 1) #this gives it a number 1-x
  #list[[i]] <- add_column(gsw_6800_chris[rows_niko,], Rep = repetion, .after = "Time") #this adds it to a new column in the position of the unique ids
}
outliers <- do.call(bind_rows, list)
#View(outliers) No outliers

###Save as csv and manually remove mistakes in scanning 
#Followed this protocol: remove obvious scanning mistakes by hand if scan 4+ was more similar to the following plant's gsw values, or if scan 1 was more similar to the previous.

#setwd(choose.dir())
#write.csv(dd_wreps_noneg%>%arrange(Date, Time),"gsw_6800_pre-clean.csv")

#Read in licor 6800 point measurements with obvious outliers removed 
pointsclean<-read.csv("gsw_6800_cleaned.csv")
#View(pointsclean)

#look at histograms of gsw for each treatment of each genotype on each day
pointsclean<-add_column(pointsclean, Geno_Date_Treat = paste(pointsclean$Genotype,pointsclean$Treatment,pointsclean$Date,sep = "_"), .after = "Date")
pointsclean$Geno_Date_Treat
codedates<-unique(pointsclean$Geno_Date_Treat)

for (i in codedates) {
  a<-pointsclean%>%filter(Geno_Date_Treat == i)%>%
    ggplot()+
    geom_histogram(aes(x = gsw_6800,fill = Treatment), bins = 3)+
    ggtitle(i)
  print(a)
}


#View(de)

#### Porometer Consolidation and Cleaning-------------------------------------------------------------------------------
#4.6.21 excel chopped the zeros off the numeric sample names containing.10, so I had to go in and fix those.
#11/11: barcodes won't scan on porometer T52.6 scan 1 and 2 is T52.6; scan 3 and 4 is 9018.8 DONE
#11/12:Rescanned 9018.8 on the porometer DONE
#11/23: b42-24.6 was scanned as last two haines.7 for porometer, ut12.10 is ut12.1 scan 3-4, 1149.8 is scan 5-6,
#T48.2 scan 3-4 is Haines2.2, CC12.8 scan 3-4 is 9031.7, 588155.01.10 scan 3-4 is b42-24.3,
#v37-96.3 scan 3-4 is 588155.01.1 DONE
#11/30: Porometer - the last two T52.7 scans are the good ones DONE

#Code compiling porometer excel files long form
porometer.compiler.long<-function(){
  filenames <- list.files("Raw Porometer Files", pattern="*.csv", full.names=TRUE)
  for (x in filenames) {
    meas<-read_csv(x,skip = 1)[-1,]
    meas$`Sample ID`<-str_remove_all(meas$`Sample ID`,"URL:")
    colnames(meas)[7]<- "ID"
    meas$`ID`<-gsub("10.","10",meas$`ID`)
    meas.id<-meas%>%select("Date","Time","ID","gsw","Fs", "Fm'", "Qamb", "Tleaf", "ETR")%>% # select the columns you care about
      rename("gsw_porometer" = gsw)%>%
      mutate("Genotype" = gsub("(.*)\\..*","\\1",`ID`))%>%
      mutate("Treatment" = ifelse(as.numeric(sub('.*\\.', '', `ID`))>5,"Control","Drought"))%>%
      mutate("Date" = str_sub(Date,start = 1, end = 5))
    meas.id$Date<-gsub("/",".",meas.id$Date)
    meas.id$Date<-gsub("12.2.","12.02",meas.id$Date)
    meas.id$Date<-gsub("11.6.","11.06",meas.id$Date)
    completep<-if(exists('completep') == TRUE){rbind(completep,meas.id)} else{meas.id}
  }
  return(completep)
}
dd1<-porometer.compiler.long()


#View(dd1)

###Cleaning porometer measurments according to gsw parameters.

#Notes: Protocol was, remove any negative scans, remove outliers >3 standard deviations from the mean, remove obvious scanning mistakes by hand if
# scan 4+ was more similar to the following plant's gsw values. units for 6800 and porometer (mol m‑2 s‑1)


###Adding number of scans per sample ID (thanks nico!)###


dd1$gsw_porometer<-as.numeric(dd1$gsw_porometer) #Making gsw numeric

dd1 <- add_column(dd1, Code_Date = paste(dd1$ID,dd1$Date,sep = "_"), .after = "Date")
unique_CodeDate <- unique(dd1$Code_Date)

list <- NULL

for(i in 1:length(unique_CodeDate)){
  rows_niko <- which(dd1$Code_Date == unique_CodeDate[i])
  repetion <- seq(from = 1, to = length(rows_niko), by = 1)
  list[[i]] <- add_column(dd1[rows_niko,], Rep = repetion, .after = "Time")
}

dd1_wreps <- do.call(bind_rows, list)

dd1_wreps_noneg<-dd1_wreps%>%filter(gsw_porometer >0)%>%
  arrange(Date,Time)
dd1_wreps_noneg$ID<-gsub(".10",".10.",dd1_wreps_noneg$ID)
#View(dd1_wreps_noneg)


###Removing porometer stomatal conductance outliers on IDs with 4+ reps ###
#NOTE: No outliers found using this method

extrarep<-dd1_wreps_noneg%>%filter(Rep == 4)%>%select(Code_Date)
#View(extrarep)
list <- NULL

for(i in 1:length(extrarep$Code_Date)){
  rows_niko <- which(dd1_wreps_noneg$Code_Date == extrarep$Code_Date[i]) #this pulls a unique id on a unique day
  lbound<-mean(dd1_wreps_noneg[rows_niko,]$gsw_porometer)-3*sd(dd1_wreps_noneg[rows_niko,]$gsw_porometer)
  hbound<-mean(dd1_wreps_noneg[rows_niko,]$gsw_porometer)+3*sd(dd1_wreps_noneg[rows_niko,]$gsw_porometer)
  list[[i]]<-dd1_wreps_noneg[rows_niko,]%>%filter(gsw_porometer<lbound | gsw_porometer>hbound)
  #repetion <- seq(from = 1, to = length(rows_niko), by = 1) #this gives it a number 1-x
  #list[[i]] <- add_column(gsw_porometer_chris[rows_niko,], Rep = repetion, .after = "Time") #this adds it to a new column in the position of the unique ids
}
outliers <- do.call(bind_rows, list)
#View(outliers) No outliers

###Save as csv and manually remove obvious mistakes in scanning. Followed the cleaning protocol in the spreadsheet.

#setwd(choose.dir())

#write.csv(dd1_wreps_noneg%>%arrange(Date, Time),"gsw_porometer_pre-clean.csv")

#Read in cleaned porometer gsw
gswclean<-read.csv("gsw_porometer_cleaned.csv")
#View(gswclean)

#look at histograms of  porometer gsw for each treatment of each genotype on each day
gswclean<-add_column(gswclean, Geno_Date_Treat = paste(gswclean$Genotype,gswclean$Treatment,gswclean$Date,sep = "_"), .after = "Date")
gswclean$Geno_Date_Treat
codedates<-unique(gswclean$Geno_Date_Treat)

 for (i in codedates) {
  a<-gswclean%>%filter(Geno_Date_Treat == i)%>%ggplot()+
    geom_histogram(aes(x = gsw_porometer,fill = Treatment), binwidth = 0.1)+
    ggtitle(i)
  print(a)
}


####Licor 6800 CO2 Curves Consolidation and Cleaning-----------------------------------------------------

licor6800.curve.compiler.long<-function(){
  filenames <- list.files("Raw 6800 CO2 Curves/all_6800_CO2_curves", pattern="*.xlsx", full.names=TRUE) #pick the folder with 6800 xlsx files
  for (filename in filenames) {
    meas<-read_xlsx(filename,skip = 14)[-1,]
    id<-(read_xlsx(filename,sheet = "Remarks"))[c(1:6),] #pull the console and head id
    meas.id<-meas%>%
      select(!contains(c("time","hhmmss","TIME")))%>%
      #select("date",6,7,"A","Ci","CO2_s","gsw","Pa","Qin","RHcham","Tleaf")%>% # select the columns you care about
      mutate("6800_licor_id" = paste0(id[1,2],"|",id[4,2]))%>% #save console and head id as a column
      rename("Genotype" = accession)%>%
      mutate("ID" = paste0(Genotype,".",str_sub(rep, start = 2)))%>%
      mutate("Treatment" = ifelse(as.numeric(str_sub(rep, start = 2))>5,"Control","Drought"))%>%
      mutate("date" = str_split_fixed(meas$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 5))%>%
      mutate("Time" = str_split_fixed(meas$date, "[:blank:](?=\\d)", 2)[,2])%>%
      rename("Date" = date)
    meas.id$Date <- sub("(?<=^..)", "\\.", meas.id$Date, perl=TRUE)
    meas.id<-meas.id%>%
      mutate("curveid" = paste0(Date,"_",ID))
    complete6800<-if(exists('complete6800') == TRUE){bind_rows(complete6800,meas.id)} else{meas.id}
  }
  return(complete6800)
}
dd<-licor6800.curve.compiler.long()
View(dd)

#write.csv(dd,"Consolidated_Curves.csv")


####Water Use Consolidation and Cleaning------------------------------------------------------------------------------

##NEW SWC & WU CODE 5/18/21 - in progress
#Variable names: PWW - pre water weight, WA - water added, TW - target water content (weight of plant+ the amount of water it should have post watering on that day),
# POWW - post water weight, PD.WT - predawn pot weights, midday.WT - midday pot weights

#To Do:
#Make column of bamboo shoots by subtracting first the water, then the known weights of the pot, plant, soil, and one bamboo shoot. 
  #the remaining weight is bamboo shoots. Divide this value by the avg bamboo shoot weight and round to 1's place.
#Adapt formula for SWC in excel sheet to r, keeping in mind that sometimes pre water weights exceeded post-water weights due to 
  #decreasing target water content between two weeks, or overwatering mistakes, or supplemental watering of controls.
#The POWW.notes column is getting deleted due to the text removal code. ignore that column in that part of the code.

#NOTE: water supplements for control that were not scheduled waterings (but are recorded in the GHDD Raw Data spreadsheet)
       #may not be in wateringdata as of 5.19.21. Check when you start Water Use code


#Set working directory if need be
#setwd(choose.dir())

#read in watering data
wu<-(read_csv("WU_for_R.csv", #warnings are OK
              col_types = cols(.default = col_double(),
                               ID = col_character(), Genotype = col_character(),
                               Species = col_character(), Treatment = col_character())))[1:284,]


#removing text and notes in incorrect columns, or #VALUE which is output by excel if formulas are missing input values
   #(this happens if, for example, controls aren't watered one day and treatments are, but I still copied the watering
   #formulas for all plants)

metawu<-wu[1:4]
#View(wu)
f<-names(wu)
b<-data.frame(matrix(ncol = 0,nrow = 284))

for (i in f) {
  a<-str_replace_all(wu[[i]],"[:alpha:]",NA_character_)
  b<-cbind.data.frame(b,a)
  names(b)[length(names(b))]<- i }
cleanwu1<-b[-c(1,2,3,4)]
wateringdata<-cbind(metawu,lapply(cleanwu1,as.numeric))
#cleanwu<-wu
#View(wateringdata)

#making watering data long form and accounting for differences in target water content for treatment vs control. 
 #Target water content (% soil saturation to field capacity) for each day of watering can be found in the GHDD raw data spreadsheet, 
 #in a note in the target water content column for each watering day

wdlong<-wateringdata%>%
  pivot_longer(colnames(wateringdata[9:ncol(wateringdata)]),
             names_to = c("Date", ".value"),
             names_pattern = "(.+)_(.+)")%>%
  mutate(tgt_swc = case_when(
    Date %in% c("10/23","10/26","10/28","10/30","11/02") ~ 0.8,
    Date %in% c("11/04","11/06","11/09","11/11","11/13","11/16") & Treatment == "Control" ~ 0.8,
    Date %in% c("11/18","11/20","11/23","11/25","11/27","11/30",
                "12/02","12/04","12/07","12/09","12/11","12/13",
                "12/14","12/15","12/16","12/17") & Treatment == "Control" ~ 0.9,
    Date %in% c("11/04","11/06","11/09") & Treatment == "Drought" ~ 0.5,
    Date %in% c("11/11") & Treatment == "Drought" ~ 0.4,
    Date %in% c("11/13","11/16","11/18","11/20","11/23","11/25","11/27","11/30","12/02") & Treatment == "Drought" ~ 0.3,
    Date %in% c("12/04","12/07") & Treatment == "Drought" ~ 0.2,
    Date %in% c("12/09","12/11","12/13","12/14","12/15","12/16","12/17") & Treatment == "Drought" ~ 0.3))
#View(wdlong)
#Attempting to add a column with bamboo shoots to watering data. Current problem is that the weight remaining after removing all soil, pot and water weights does not evenly divide into the bamboo shoot weight.
    #This is due to the off-weighted scale difference which was fixed 11/06. 
    #Also there are negative values after removing soil pot and water sometimes, also needing explanation



View(wdlong)            
View(wateringdata)
  
#Bamboo.Shoots = 0.0325kg

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

# code to copy and paste columns from google sheets. Had to make sure samples were in the same order.
#d<-readClipboard() 
#r<-d[2:285] #trimming white space
#rre<-str_replace_all(r,"[:alpha:]","")
#rre<-str_replace_all(rre,"`","")
#rre[rre==""]<-0
#post127<-data.frame("12/7 post water weight" = as.numeric(rre)) 
#write_csv(post1217,"post127.csv")


###THIS CODE IS OUTDATED AS OF 7/20/21, cleanwu NEEDS TO BE wateringdata AND VARIABLE TERMS NEED TO BE CHANGED TO wateringdata VARIABLES
#View(cleanwu) #Cleanwu was generated at the beginning of the water use chunk
#Addressing each period of water consumption before considering empty pots
wateruse_cleaned<-cleanwu%>%
  mutate_at(colnames(cleanwu[5:ncol(cleanwu)]), as.numeric)%>%
  #approx columns are from before we started taking post water weights, so if we went over in our watering, there is no record of that.
  mutate("10/23-10/26 WU (approx)" = cleanwu$`10/26 Water added for 80%SW`)%>%
  mutate("10/26-10/28 WU (approx)" = cleanwu$`10/28 Water added for 80%SW`)%>%
  mutate("10/28-10/30 WU (approx)" = cleanwu$`10/30 Water added for 80%SW`)%>%
  mutate("10/30-11/2 WU (approx)" = (cleanwu$`11/2 Water added for 80%SW`))%>%
  mutate("11/2-11/4 WU" = cleanwu$`11/2 post water weight`-cleanwu$`11/4 pre water`)%>%
  mutate("11/4-11/6 WU" = cleanwu$`11/4 post water`-cleanwu$`11/6 pre water`)%>%
  mutate("11/6-11/9 WU" = cleanwu$`11/6 post water`-cleanwu$`11/9 pre water`)%>%
  mutate("11/9-11/11 WU" = cleanwu$`11/9 post water`-cleanwu$`11/11 pre water weight`)%>%
  mutate("11/11-11/13 WU" = cleanwu$`11/11 post water weight`-cleanwu$`11/13 pre water weight`)%>%
  #on 11/15 there was a (likely? I can't remember and havent found anything in notes) hydration to saturation of the controls to prep for ALS, so they
  #couldnt be compared to the between-waterings water useage of the drought treatment. 11/15 has the control drained weight column which isnt very helpful.
  #Additionally, since treatments were not watered on 11/13, the pre water weight on 11/13 was used instead of the post water weight
  mutate("11/13-11/16 WU" = ifelse(cleanwu$Treatment == "Drought", cleanwu$`11/13 pre water weight`-cleanwu$`11/16 pre water weight`, NA_character_))%>%
  #Since controls were not watered on 11/16, the pre water weights of the control plants were used to calculate 11/18 WU instead of the post-water weights.
  #Additionally, many plants were missing 11/18 since they went to ALS.
  mutate("11/16-11/18 WU" = ifelse(cleanwu$Treatment == "Drought", cleanwu$`11/16 post water weight`-cleanwu$`11/18 pre water weight`,
                                   cleanwu$`11/16 pre water weight`-cleanwu$`11/18 pre water weight`))%>%
  mutate("11/18-11/20 WU" =  cleanwu$`11/18 post water`-cleanwu$`11/20 pre water weight`)%>%
  mutate("11/20-11/23 WU" =  cleanwu$`11/20 post water weight`-cleanwu$`11/23 pre water weight`)%>%
  mutate("11/23-11/25 WU" =  cleanwu$`11/23 post water weight`-cleanwu$`11/25 pre water weight`)%>%
  mutate("11/25-11/27 WU" =  cleanwu$`11/25 post water weight`-cleanwu$`11/27 pre water weight`)%>%
  #On 11/29 control plants received 600ml of water. So I just added this as .6kg of water at the end of the calculation for controls
  mutate("11/27-11/30 WU" =  ifelse(cleanwu$Treatment == "Drought", cleanwu$`11/27 post water weight`-cleanwu$`11/30 pre water weight`,
                                    (cleanwu$`11/27 post water weight`-cleanwu$`11/30 pre water weight`)+0.6))%>%
  mutate("11/30-12/2 WU" =  cleanwu$`11/30 post water weight`-cleanwu$`12/2 pre water weight`)%>%
  mutate("12/2-12/4 WU" =  cleanwu$`12/2 post water weight`-cleanwu$`12/4 pre water weight`)%>%
  #controls were watered twice this week in addition. Once consistently 500ml and once varying amounts, recorded in a column.
  mutate("12/4-12/7 WU" =  ifelse(cleanwu$Treatment == "Drought", cleanwu$`12/4 post water weight`-cleanwu$`12/7 pre water weight`,
                                  (cleanwu$`12/4 post water weight`-cleanwu$`12/7 pre water weight`)+0.5+(vols126$`X12.6.control.water.added`)*.001))%>%
  #controls were watered an additional 500ml. Target weights were off this week.
  mutate("12/7-12/9 WU" =  ifelse(cleanwu$Treatment == "Drought", cleanwu$`12/7 post water weight`-cleanwu$`12/9 pre water weight`,
                                  (cleanwu$`12/7 post water weight`-cleanwu$`12/9 pre water weight`)+0.5))%>%
  #controls were watered an additional 500ml. Also, on 12/9 we fixed the target weights, and rewatered treatments to target. So those that got rewatered have a seperate post water weight
  mutate("12/9-12/11 WU" =  ifelse(cleanwu$Treatment == "Drought",
                                   ifelse(is.na(cleanwu$`12/9 rewater post water weight`) == TRUE, cleanwu$`12/9 post water weight`-cleanwu$`12/11 pre-water weight`,
                                          (cleanwu$`12/9 rewater post water weight`-cleanwu$`12/11 pre-water weight`)),
                                   ifelse(is.na(cleanwu$`12/9 rewater post water weight`) == TRUE, (cleanwu$`12/9 post water weight`-cleanwu$`12/11 pre-water weight`)+0.5,
                                          (cleanwu$`12/9 rewater post water weight`-cleanwu$`12/11 pre-water weight`)+0.5)))%>%
  #Another day of varied bonus watering for the control (and sometimes treatment? think they were accidents)
  mutate("12/11-12/14 WU" =  ifelse(cleanwu$Treatment == "Drought", (cleanwu$`12/11 post-water weight`-cleanwu$`12/14 pre-water weight`)+vols1213$X12.13.water.added,
                                    (cleanwu$`12/11 post-water weight`-cleanwu$`12/14 pre-water weight`)+vols1213$X12.13.water.added))%>%
  mutate("12/14-12/16 WU" =  cleanwu$`12/14 post water weight`-cleanwu$`12/16 pre-water weight`)%>%
  mutate("12/16-12/17 WU" =  cleanwu$`12/16 post-water weight`-cleanwu$`12/17 pre water weight`)%>%
  select(1,2,3,4,5,6,7,8,contains(c("WU")))


#View(wateruse_cleaned)

#write.csv(wateruse_cleaned,"wateruse.preep.csv")
##Addressing Empty Pots:
emptypots<-wateruse_cleaned%>%filter(is.na(Genotype))

#write_csv(emptypots,"emptypots.csv")

wuminusep<-read_csv("wateruse.minus.ep.csv")
#View(wuminusep)
WU_Gathered<-wuminusep%>%
  mutate("Treatment" = ifelse(as.numeric(str_split_fixed(wuminusep$`ID`, "\\.(?=\\d)", 2)[,2])>5,"Control","Drought"))%>%
  gather("Date","WU",`10/26`:ncol(wuminusep))%>%
  select("ID","Genotype","Species","Treatment","Date","WU")
#View(WU_Gathered)


ghdd_data_pre_harvest<-full_join(ghdd_data_long_form%>%
                                   mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d")),
                                 WU_Gathered%>%select(!Species))%>%
  mutate(Date = format(as.Date(Date, "%m/%d"),"%m/%d"))
ghdd_data_pre_harvest<-modify_at(ghdd_data_pre_harvest,.at = colnames(ghdd_data_pre_harvest%>%select(!c("ID","Time","Genotype","Treatment","Date","6800_licor_id"))),~as.numeric(as.character(.)))
#View(ghdd_data_pre_harvest)
#write_csv(ghdd_data_pre_harvest,"GHDD_Data_Pre_Harvest-2020.csv")

###Soil Water Content Consolidation and Cleaning---------------------------------------------------------------------------------
#Needs to be redone in R


####Harvest Data Consolidation and Cleaning----------------------------------------------------------------------------------------
#setwd(choose.dir())
roots<-(read_csv("roots.csv"))[1:378,] #chopping the white space off by removing rows, since trim_ws isnt working
hweights<-(read_csv("hweights.csv"))[1:274,]
cbiomass<-(read_csv("cbiomass.csv"))[1:378,]

s<-full_join(roots,hweights, by = "ID",copy = TRUE)
harvest<-(full_join(s,cbiomass, by = "ID",copy = TRUE))%>%filter(genotype.x != "S")
#View(harvest)


######################################################################################################################
#############################################################Graphing#################################################

###Water Potential Graphs-----------------------------------------------------

##Leaf Water Potential##

LWP_plot<-waterpotentials_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
  filter(!is.na(LWP))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "11/09" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/10" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/11" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/16" = "11/16-17"))%>%
  mutate(Date= recode(Date, "11/17" = "11/16-17"))%>%
  mutate(Date= recode(Date, "12/01" = "12/01-02"))%>%
  mutate(Date= recode(Date, "12/02" = "12/01-02"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(LWP = as.numeric(LWP))
#View(LWP_plot)
genos<-unique(LWP_plot$Genotype)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")

for (i in genos) {
  graph<-LWP_plot%>%
    filter(Genotype == i)%>%
    filter(-LWP < -3 & -LWP >-15)%>%
    ggplot(aes(x = Date, Y = LWP, fill = Treatment))+
    geom_boxplot(aes(y =-LWP))+
    ylim(-20,-4)+
    theme_classic()+
    scale_x_discrete(limits = order1)+
    ggtitle(i)
  print(graph)
  
}

##Predawn Water Potential## 

#NOTES: 11/18 was ALS

PD_plot<-waterpotentials_gathered%>% #NA introduced by coercion ok
  filter(!is.na(PD))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "11/11" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/12" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/13" = "11/11-13"))%>%
  mutate(Date= recode(Date, "12/01" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/02" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/03" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(PD = as.numeric(PD))
#View(PD_plot) 
genos<-unique(PD_plot$Genotype)

for (i in genos) {
  graph<-PD_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = PD, fill = Treatment))+
    geom_boxplot(aes(y =-PD))+
    ylim(-20,-1)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
}

##Stem Water Potential##
SWP_plot<-waterpotentials_gathered%>%
  filter(!is.na(SWP))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(SWP = as.numeric(SWP))


genos<-unique(SWP_plot$Genotype)

for (i in genos) {
  graph<-SWP_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
    geom_boxplot(aes(y =-SWP))+
    ylim(-20,-5)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
}


###Porometer Graphs----------------------------------------------------------------

#Notes : we have more than two reads per plant right now. they are all equivalently weighted. What does this do to the stats?
#also I have to figure out on what bounds to bin sample days.

GSW_plot<-gswclean%>%
  filter(!is.na(gsw_porometer))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(as.character(Date),"%m.%d"),"%m/%d"))

#mutate(Date= recode(Date, "10/26" = "10/26-27"))%>%
#mutate(Date= recode(Date, "10/27" = "10/26-27"))%>%
#mutate(Date= recode(Date, "11/16" = "11/16-17"))%>%
#mutate(Date= recode(Date, "11/17" = "11/16-17"))%>%
#mutate(Date= recode(Date, "11/23" = "11/23-24"))%>%
#mutate(Date= recode(Date, "11/24" = "11/23-24"))%>%
#mutate(Date= recode(Date, "11/30" = "11/30-12/2"))%>%
#mutate(Date= recode(Date, "12/02" = "11/30-12/2"))



#View(GSW_plot%>%filter(Genotype == "Vru42"))
genos<-unique(GSW_plot$Genotype)


for (i in genos) {
  graph<-GSW_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = gsw_porometer, fill = Treatment))+
    geom_boxplot(aes(y =gsw_porometer))+
    ylim(0,1)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
}


###Licor 6800 Point Measurement Graphs-------------------------------------------------

#Looking at A variation between control and drought

A_plot<-pointsclean%>%
  filter(!is.na(A))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "10/26" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/27" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/28" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/29" = "10/26-29"))%>%
  mutate(Date= recode(Date, "11/12" = "11/12-13"))%>%
  mutate(Date= recode(Date, "11/13" = "11/12-13"))%>%
  mutate(Date= recode(Date, "12/06" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/07" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/08" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/09" = "12/06-09"))



#View(A_plot%>%filter(Genotype == "Vru42"))
genos<-unique(A_plot$Genotype)


for (i in genos) {
  graph<-A_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = A, fill = Treatment))+
    geom_boxplot(aes(y =A))+
    #ylim(0,1)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
}





####Water Use Graphs---------------------------------------------------  
#NEEDS TO BE UPDATED ONCE CLEANING CODE IS FINISHED - 7/21/21
## Look into broken code for numeric IDs

WU_plot<-GHDD_Preharvest%>%
  filter(!is.na(WU))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(Date, "%m/%d"),"%m/%d"))
#View(WU_plot)
genos<-unique(WU_plot$Genotype)

for (i in genos) {
  graph<-WU_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = WU, fill = Treatment))+
    geom_boxplot(aes(y =WU))+
    theme_classic()+
    #ylim(-.5,4)+
    #scale_x_discrete(limits = order3)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    ggtitle(i)
  print(graph)
  
}

####SWC Graphs----------------------------------------------------------------------------

#NEEDS TO BE REFORMATTED ONCE SWC CALCULATIONS ARE REDONE IN R - 7/21/21

##DailySWC##
#CURRENTLY EDITED TO FIND WHY THERE ARE SWC IRREGULARITIES 
SWC_plot<-GHDD_Preharvest%>%
  filter(!is.na(SWC))%>%
  filter(!(Date == "11/16" & Treatment == "Control"))%>% #Removing erroneous control weights on 11/13
  filter(Genotype== "255189.01")%>%
  #filter(Treatment == "Control")%>%
  #one input in the raw data sheet had a comma not a decimal place leading to a huge swc. I can go in and fix
  filter(SWC <500)%>%
  #filter(Date == "11/18")%>%
  select(SWC,Date,Genotype,ID,Treatment)%>%
  arrange(Date,SWC)%>%
  distinct()
View(SWC_plot)
genos<-unique(SWC_plot$Genotype)
genos<-genos[!is.na(genos)]

for (i in genos) {
  
  error.df <- SWC_plot %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(SWC, na.rm = TRUE),
      len = mean(SWC),
      Treatment = Treatment,
      Date = Date)
  
  graph<-SWC_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(y =SWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    stat_summary(fun="mean",geom="line",size = 1)+
    #stat_smooth(geom="line",size = 1.5)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-sd,
                      ymax = len+sd),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  #linetype = "dotted",
                  data = distinct(error.df))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    ggtitle(i)
  print(graph)
  
} ##Warnings are ok

