library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)


###Licor 6800 Point Measurements Consolidation and Cleaning-----------------------------------------------------------------------------
#|Note: This code takes a folder of .xsls type licor point measurement files and compiles them by date,
#and the user defined constants "accession" and "rep"

#When running this function, select the folder containing all of the licor excel files. IT IS CURRENTLY SELECTED AS LONG AS WORKING DIRECTORY IS THE Input FOLDER
choose.dir()
setwd("~/Documents/GitHub/vitisdrought/input")


licor6800.point_measurement.compiler<-function(){
  filenames <- list.files("Raw 6800 Point measurements", pattern="*.xlsx", full.names=TRUE) #pick the folder with 6800 xlsx files. If unsure replace "Raw 6800 Point measurements" with chose.dir() and manually pick the folder 
  for (filename in filenames) { #cycle through each file and apply the following code to each file 
    meas<-read_xlsx(filename,skip = 14)[-1,] #pulls the relevant measurements from the main tab of the spreadsheet
    id<-(read_xlsx(filename,sheet = "Remarks"))[c(1:6),] #pull the console and head id from the second tab of the spreadsheet
    meas.id<-meas%>%select("date",6,7,"A","Ci","E", "CO2_s","gsw","Pa","Qin","RHcham","Tleaf", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm'")%>% # selecting the columns you care about. 6 & 7 are user defined constants.
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
compiled6800<-licor6800.point_measurement.compiler() #running the function
View(compiled6800)

###Cleaning the licor 6800 point measurement data based on stomatal conductance values###

#Cleaning point measurements by removing negative gsw values

compiled6800$gsw_6800<-as.numeric(compiled6800$gsw_6800) #Making gsw numeric

compiled6800 <- add_column(compiled6800, Code_Date = paste(compiled6800$ID,compiled6800$Date,sep = "_"), .after = "Date") #Attaching reps to each scan for each plant (i.e. if Vru42.1 was scanned 3 times on 12/10, each row of those three will have 1,2,or 3 in the rep column. this starts over then the plant changes)
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
write.csv(dd_wreps_noneg%>%arrange(Date, Time),"gsw_6800_pre-clean.csv")

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