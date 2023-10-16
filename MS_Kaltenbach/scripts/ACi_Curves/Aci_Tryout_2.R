library(tidyverse)
library(stringr)
library(stringi)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(writexl)
library(utils)
library(base)

setwd("~/Documents/Master_Thesis/ACi-Curves/all-excel-files/modified/CSV")

# modify & combine individual csv files -----------------------------------
filenames <- list.files(pattern="*.csv", full.names=TRUE)

  for (filename in filenames) {
    list<-read_csv(filename, skip = 13)[-1:-2,]
    
    list$A <- as.numeric(list$A)
    list$Ca <- as.numeric(list$Ca)
    list$Ci <- as.numeric(list$Ci)
    list$E <- as.numeric(list$E)
    list$CO2_s <- as.numeric(list$CO2_s)
    list$CO2_r <- as.numeric(list$CO2_r)
    list$Qin <- as.numeric(list$Qin)
    list$gsw <- as.numeric(list$ gsw)
    list$gtc <- as.numeric(list$gtc)
    list$Pa <- as.numeric(list$Pa)
    list$RHcham <- as.numeric(list$RHcham)
    list$Tleaf <- as.numeric(list$Tleaf)
    list$Adark <- as.numeric(list$Adark)

    subset_csv<-  list %>%
      select( "date","A","Ca", "Ci","E", "CO2_s", "CO2_r", "Qin", "gsw","gtc", "Pa","Qin","RHcham","Tleaf",
              "PhiPS2", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm") %>%
      mutate("date" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 6))
    
   include_name <- cbind(filename, subset_csv)
   
  completeA6800<- if(exists('completeA6800') == TRUE) rbind(completeA6800, include_name) else(include_name)
  }

# Edit & Add Column Names -------------------------------------------------
#extract file name data as columns for graphing
completeA6800$filename<- gsub(".csv", "", completeA6800$filename)
completeA6800[c('Date','Genotype', 'VineNumber')] <- str_split_fixed(completeA6800$filename, '_', 3)
completeA6800[c('Month', 'Date')] <- str_split_fixed(completeA6800$date, '-', 2)

# assign treatment
DF <- completeA6800[grep("1|2|3|4|5|6|7|8|9|10", completeA6800$VineNumber),]

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

names <- read.csv("data/2020_GHDD_Harvest_Data.csv")

names <- names %>% 
  select("ID", "trt", "ID.Order", "genotype", "species")%>% 
  rename(Treatment = trt, 
         Species= species, 
         Genotype = genotype) %>% 
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

names <- separate(names, col = ID, into = c("Genotype", "VineNumber"), sep = "\\.")

licor_name <- DF %>% 
  select("VineNumber", "filename" , "date" , "Date", "Genotype", "Month") 


merged_df <- merge(names, licor_name, by = c("VineNumber", "Genotype"), all.x = TRUE)
merged_df <- merged_df %>% 
  select("VineNumber", "Treatment", "Genotype")


DF  <- merge(DF, merged_df, by = c("VineNumber", "Genotype"), all.x = TRUE)



# Discard Negative gsw and Ci -------------------------------------------------
dd_wreps <- do.call(bind_rows, DF)
Nonegs<-dd_wreps%>%filter(gsw >0 & Ci>0) %>%
  distinct() 



# Establish &Remove Outliers Using Quartiles  ---------------------------------
CiQ1 <- quantile(Nonegs$Ci, probs=.25, na.rm = TRUE) #"type 7" default
CiQ3 <- quantile(Nonegs$Ci, probs=.75, na.rm = TRUE)
AQ1 <- quantile(Nonegs$A, probs=.25, na.rm = TRUE)
AQ3 <- quantile(Nonegs$A, probs=.75, na.rm = TRUE)
IQRCi <- IQR(Nonegs$Ci, na.rm=TRUE, type = 7)
IQRA <- IQR(Nonegs$A, na.rm=TRUE)

no_outliers <- subset(Nonegs, Nonegs$Ci > (CiQ1 - 1.5*IQRCi) 
                      & Nonegs$Ci< (CiQ3 + 1.5*IQRCi) 
                      & Nonegs$A > (AQ1 - 1.5*IQRA) 
                      & Nonegs$A < (AQ3 + 1.5*IQRA))

no_outliers$MonthGenotype <- paste(no_outliers$Genotype, no_outliers$Month,  sep="_")
no_outliers$LeafID <- paste(no_outliers$MonthGenotype, no_outliers$VineNumber,no_outliers$Treatment,sep="_")
no_outliers$Genotype_Treatment <- paste(no_outliers$Genotype, no_outliers$Treatment, sep="_")
no_outliers$LeafNumber <- paste(no_outliers$MonthGenotype, no_outliers$VineNumber,sep="_")

# Sub-setting Data For Graphing -------------------------------------------

November <- no_outliers[which(no_outliers$Month == "11"),]
December <- no_outliers[which(no_outliers$Month == "12"),]

November$NGenotype_Treatment <- paste(November$Genotype, November$Treatment, sep="_")
December$DGenotype_Treatment <- paste(December$Genotype, December$Treatment, sep="_")

# create a vector of LeafID values to keep
# keep_ids <- c("9018_11_1_Drought", "b40-14_12_1_Drought", "b42-34_11_1_Drought", "NY1_11_1_Drought",
#               "T48_12_1_Drought", "TXNM0821_12_1_Drought", "V60-96_12_1_Drought", "9018_12_10_Control",
#               "b40-14_12_10_Control", "b42-34_12_10_Control", "T48_12_10_Control", "b40-14_11_2_Drought",
#               "b42-34_11_2_Drought", "V60-96_12_2_Drought", "Vru42_12_2_Drought", "Vru42_12_3_Drought",
#               "b42-34_12_4_Drought", "T48_12_4_Drought", "T52_12_4_Drought", "TXNM0821_12_4_Drought",
#               "Vru42_12_4_Drought", "9018_11_5_Drought", "b42-34_12_5_Drought", "T52_12_5_Drought",
#               "TXNM0821_12_5_Control", "TXNM0821_11_5_Control", "b42-34_11_6_Control", "NY1_12_6_Control",
#               "T48_12_6_Control", "TXNM0821_12_6_Control", "V60-96_11_6_Control", "V60-96_12_6_Control",
#               "Vru42_12_6_Control", "9018_12_7_Control", "b40-14_11_7_Control", "b42-34_12_7_Control",
#               "T48_12_7_Control", "T52_12_7_Control", "TXNM0821_12_7_Control", "Vru42_12_7_Control",
#               "9018_12_8_Control", "b42-34_12_8_Control", "NY1_12_8_Control", "T48_12_8_Control",
#               "T52_11_8_Control", "TXNM0821_12_8_Drought", "Vru42_12_8_Control", "b40-14_12_9_Control",
#               "b42-34_12_9_Control", "NY1_12_9_Control", "NY1_11_9_Control", "T48_12_9_Control",
#               "T52_12_9_Control", "TXNM0821_11_9_Control", "TXNM0821_12_9_Control", "V60-96_11_9_Control",
#               "V60-96_12_9_Control", "Vru42_12_9_Control")


# # subset no_outliers based on keep_ids
# no_outliers <- no_outliers %>% filter(LeafID %in% keep_ids)


remove_ids <- c( "9018_12_2", "9018_12_3", "9018_12_4", "b40-14_11_2", 
                  "b40-14_12_10", "b40-14_12_2" , "b40-14_12_7", "b40-14_12_8", 
                  "b42-34_11_2", "b42-34_12_3", "NY1_12_6", "NY1_12_7", "T48_12_5", 
                  "T52_11_4", "T52_11_5", "T52_12_3", "TXNM0821_11_2", "V60-96_12_5") 
                  
  
remove_ids <- c("b40-14_12_1_Drought", "b40-14_11_10_Control", "T52_12_2_Drought", "b40-14_2_5_Drought") 



remove_ids <- c("b40-14_12_1_Drought", "b40-14_11_10_Control", "T52_12_2_Drought", "b40-14_2_5_Drought", "b40-14_12_3_Drought", "T48_12_1_Drought", "T52_12_1_Drought", "TXNM0821_12_1_Drought", "b42-34_12_5_Drought", "T48_12_3_Drought", "TXNM0821_11_3_Drought", "TXNM0821_12_1_Drought", "V60-96_12_1_Drought"   ) 


# subset no_outliers based on keep_ids
no_outliers <- no_outliers %>% filter(!LeafID %in% remove_ids)


# -------------------------------------------------------------------------



# Facet Plot --------------------------------------------------------------

individal_curves_facet <- ggplot(no_outliers, 
                                 aes(x = Ci, y = A, color = Treatment, group = LeafID)) +
  facet_wrap(~ LeafID) +
  geom_point() + 
  geom_line() +
  theme(legend.position="none")+
  theme_bw(base_size = 14)+
  scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
individal_curves_facet


#ggsave(paste0("fig_output/facet", ".png"))
#ggsave(paste0("fig_output/facet", ".pdf"))



# new ---------------------------------------------------------------------

# Create a list to store individual ggplots
individual_plots <- list()

# Loop through unique LeafIDs
for (leaf_id in unique(no_outliers$LeafID)) {
  # Subset the data for the current LeafID
  subset_data <- subset(no_outliers, LeafID == leaf_id)
  
  # Create the ggplot for the current LeafID
  individual_plot <- ggplot(subset_data, aes(x = Ci, y = A, color = Treatment)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 14) +
    labs(title = paste("Leaf ID:", leaf_id))+
    scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
  
  # Store the plot in the list
  individual_plots[[as.character(leaf_id)]] <- individual_plot
}

# Print or save the individual plots
for (i in 1:length(individual_plots)) {
  print(individual_plots[[i]])
}


# Average ggplots ---------------------------------------------------------

ggplot(data=no_outliers, aes(x=Ci, y=A, col= as.factor(Genotype_Treatment)))+
  geom_point()+
  geom_smooth()+
  labs(title = "ACi combined Control v.s. Drought by Species")

genotype_order<-unique(no_outliers$Genotype)
for (i in genotype_order) {
ACiPlot<- no_outliers%>%
    filter(Genotype== i)%>%
    ggplot(aes(x = Ci, y = A, color = LeafID))+
    geom_point()+
    geom_line()+
  ggtitle(paste("ACi-Curves of", i))
  print(ACiPlot) 
  
  #ggsave(paste0("fig_output/ACi",i, ".png"))
  #ggsave(paste0("fig_output/ACi",i, ".pdf"))
} 

# Fitting ACi Curves ------------------------------------------------------

library(plantecophys)
library(devtools)

# create individual tibbles for extraction --------------------------------

tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) #use names to find names of tibbles
#summary(tibblegroup)
df1 <- names(tibblegroup)


#f <- fitaci(no_outliers, fitmethod="bilinear")


for (fitfile in tibblegroup) {

fit_fitfile <- fitaci(fitfile, varnames=list(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin"))
#plot(fit_fitfile, addlegend = TRUE)
#fit_fitfile$Photosyn(600)
#fit_fitfile$Ci(0) #compensation point
coef(fit_fitfile)
} 


# find AMax ---------------------------------------------------------------

Amax <- 
  no_outliers %>%
  group_by(LeafID) %>%
  summarise(max(A))

View(Amax)
#write_xlsx(Amax, "Amax.xlsx")



# New Creation ------------------------------------------------------------

tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) # use names to find names of tibbles
# summary(tibblegroup)
df1 <- names(tibblegroup)

output_df <- data.frame(LeafID = character(),
                        Photosyn_600 = numeric(),
                        Ci_0 = numeric(),
                        Vcmax = numeric(),
                        Jmax = numeric(),
                        Rd = numeric(),
                        stringsAsFactors = FALSE)  # Initialize an empty data frame to store the output

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    plot(fit_fitfile, addlegend = TRUE, , main = fitfile$LeafID[1])
    
    output <- data.frame(LeafID = fitfile$LeafID[1],  # Use only the first value of LeafID
                         Photosyn_600 = fit_fitfile$Photosyn(600),
                         Ci_0 = fit_fitfile$Ci(0),
                         Vcmax = coef(fit_fitfile)["Vcmax"],
                         Jmax = coef(fit_fitfile)["Jmax"],
                         Rd = coef(fit_fitfile)["Rd"],
                         stringsAsFactors = FALSE)  # Create a data frame for the current LeafID
    
    output_df <- rbind(output_df, output)  # Append the output to the main data frame
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$LeafID, "\n")
    message(e)
  })
}

# Remove the first empty row from the data frame
output_df <- output_df[-1, ]

9018_12_1_Drought
b40-14_11_10_Control
b40-14_12_1_Drought.
NY1_12_4_Drought
NY1_12_9_Control
T48_12_3_Drought
T52_12_1_Drought
T52_12_5_Drought
TXNM0821_11_3_Drought
TXNM0821_12_9_Control
V60-96_11_2_Drought

b42-34_12_5_Drought!!


  
  
  remove_ids <- c( "9018_12_2", "9018_12_3", "9018_12_4", "b40-14_11_2", 
                   "b40-14_12_10", "b40-14_12_2" , "b40-14_12_7", "b40-14_12_8", 
                   "b42-34_11_2", "b42-34_12_3", "NY1_12_6", "NY1_12_7", "T48_12_5", 
                   "T52_11_4", "T52_11_5", "T52_12_3", "TXNM0821_11_2", "V60-96_12_5") 


  

# new species & Treatment -------------------------------------------------




# new average -------------------------------------------------------------

# Split the data by Genotype and treatment
tibblegroup <- split(no_outliers, list(no_outliers$Genotype, no_outliers$Treatment))
names(tibblegroup) # use names to find names of tibbles

output_df <- data.frame(Genotype = character(),
                        Treatment = character(),
                        Photosyn_600 = numeric(),
                        Ci_0 = numeric(),
                        Vcmax = numeric(),
                        Jmax = numeric(),
                        Rd = numeric(),
                        stringsAsFactors = FALSE)  # Initialize an empty data frame to store the output

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    output <- data.frame(Genotype = fitfile$Genotype[1],  # Use only the first value of Genotype
                         Treatment = fitfile$Treatment[1],  # Use only the first value of Treatment
                         Photosyn_600 = fit_fitfile$Photosyn(600),
                         Ci_0 = fit_fitfile$Ci(0),
                         Vcmax = coef(fit_fitfile)["Vcmax"],
                         Jmax = coef(fit_fitfile)["Jmax"],
                         Rd = coef(fit_fitfile)["Rd"],
                         stringsAsFactors = FALSE)  # Create a data frame for the current Genotype and Treatment
    
    output_df <- rbind(output_df, output)  # Append the output to the main data frame
    
    plot(fit_fitfile, addlegend = TRUE, main = paste(fitfile$Genotype[1], "-", fitfile$Treatment[1]))
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$Genotype[1], "-", fitfile$Treatment[1], "\n")
    message(e)
  })
}

# Remove the first empty row from the data frame
output_df <- output_df[-1, ]

# Calculate the average values for each combination of Genotype and Treatment
average_df <- aggregate(. ~ Genotype + Treatment, data = output_df, FUN = mean)



# new species & Treatment -------------------------------------------------


# Split the data by Genotype and treatment
tibblegroup <- split(no_outliers, list(no_outliers$Genotype, no_outliers$Treatment))
names(tibblegroup) # use names to find names of tibbles

output_df <- data.frame(Genotype = character(),
                        Treatment = character(),
                        Photosyn_600 = numeric(),
                        Ci_0 = numeric(),
                        Vcmax = numeric(),
                        Jmax = numeric(),
                        Rd = numeric(),
                        stringsAsFactors = FALSE)  # Initialize an empty data frame to store the output

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    output <- data.frame(Genotype = fitfile$Genotype[1],  # Use only the first value of Genotype
                         Treatment = fitfile$Treatment[1],  # Use only the first value of Treatment
                         Photosyn_600 = fit_fitfile$Photosyn(600),
                         Ci_0 = fit_fitfile$Ci(0),
                         Vcmax = coef(fit_fitfile)["Vcmax"],
                         Jmax = coef(fit_fitfile)["Jmax"],
                         Rd = coef(fit_fitfile)["Rd"],
                         stringsAsFactors = FALSE)  # Create a data frame for the current Genotype and Treatment
    
    output_df <- rbind(output_df, output)  # Append the output to the main data frame
    
    plot(fit_fitfile, addlegend = TRUE, main = paste(fitfile$Genotype[1], "-", fitfile$Treatment[1]))
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$Genotype[1], "-", fitfile$Treatment[1], "\n")
    message(e)
  })
}

# Remove the first empty row from the data frame
output_df <- output_df[-1, ]

# Calculate the average values for each combination of Genotype and Treatment
average_df <- aggregate(. ~ Genotype + Treatment, data = output_df, FUN = mean)


# new species & Treatment & november -------------------------------------------------

# new average -------------------------------------------------------------

# Split the data by Genotype and treatment
tibblegroup <- split(November, list(November$Genotype, November$Treatment))
names(tibblegroup) # use names to find names of tibbles

output_df <- data.frame(Genotype = character(),
                        Treatment = character(),
                        Photosyn_600 = numeric(),
                        Ci_0 = numeric(),
                        Vcmax = numeric(),
                        Jmax = numeric(),
                        Rd = numeric(),
                        stringsAsFactors = FALSE)  # Initialize an empty data frame to store the output

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    output <- data.frame(Genotype = fitfile$Genotype[1],  # Use only the first value of Genotype
                         Treatment = fitfile$Treatment[1],  # Use only the first value of Treatment
                         Photosyn_600 = fit_fitfile$Photosyn(600),
                         Ci_0 = fit_fitfile$Ci(0),
                         Vcmax = coef(fit_fitfile)["Vcmax"],
                         Jmax = coef(fit_fitfile)["Jmax"],
                         Rd = coef(fit_fitfile)["Rd"],
                         stringsAsFactors = FALSE)  # Create a data frame for the current Genotype and Treatment
    
    output_df <- rbind(output_df, output)  # Append the output to the main data frame
    
    plot(fit_fitfile, addlegend = TRUE, main = paste(fitfile$Genotype[1], "-", fitfile$Treatment[1]))
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$Genotype[1], "-", fitfile$Treatment[1], "\n")
    message(e)
  })
}

# Remove the first empty row from the data frame
output_df <- output_df[-1, ]

# Calculate the average values for each combination of Genotype and Treatment
average_df <- aggregate(. ~ Genotype + Treatment, data = output_df, FUN = mean)


# new species & Treatment & December -------------------------------------------------

# new average -------------------------------------------------------------

# Split the data by Genotype and treatment
tibblegroup <- split(December, list(December$Genotype, December$Treatment))
names(tibblegroup) # use names to find names of tibbles

output_df <- data.frame(Genotype = character(),
                        Treatment = character(),
                        Photosyn_600 = numeric(),
                        Ci_0 = numeric(),
                        Vcmax = numeric(),
                        Jmax = numeric(),
                        Rd = numeric(),
                        stringsAsFactors = FALSE)  # Initialize an empty data frame to store the output

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    output <- data.frame(Genotype = fitfile$Genotype[1],  # Use only the first value of Genotype
                         Treatment = fitfile$Treatment[1],  # Use only the first value of Treatment
                         Photosyn_600 = fit_fitfile$Photosyn(600),
                         Ci_0 = fit_fitfile$Ci(0),
                         Vcmax = coef(fit_fitfile)["Vcmax"],
                         Jmax = coef(fit_fitfile)["Jmax"],
                         Rd = coef(fit_fitfile)["Rd"],
                         stringsAsFactors = FALSE)  # Create a data frame for the current Genotype and Treatment
    
    output_df <- rbind(output_df, output)  # Append the output to the main data frame
    
    plot(fit_fitfile, addlegend = TRUE, main = paste(fitfile$Genotype[1], "-", fitfile$Treatment[1]))
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$Genotype[1], "-", fitfile$Treatment[1], "\n")
    message(e)
  })
}

# Remove the first empty row from the data frame
output_df <- output_df[-1, ]

# Calculate the average values for each combination of Genotype and Treatment
average_df <- aggregate(. ~ Genotype + Treatment, data = output_df, FUN = mean)






  
  