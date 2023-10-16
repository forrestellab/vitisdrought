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

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach/data/ACI/copy/used")


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
completeA6800[c('Genotype', 'VineNumber')] <- str_split_fixed(completeA6800$Genotype, '\\.', 2)
completeA6800[c('Month', 'Date')] <- str_split_fixed(completeA6800$date, '-', 2)

# assign treatment
DF <- completeA6800[grep("1|2|3|4|5|6|7|8|9|10", completeA6800$VineNumber),]

DF$Genotype <- ifelse(DF$Genotype == "B42-34", "b42-34", DF$Genotype)


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
  select("VineNumber", "Treatment", "Genotype")%>%
  distinct() 



DF  <- merge(DF, merged_df, by = c("VineNumber", "Genotype"), all.x = TRUE)
DF  <- DF %>%
  distinct() 

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

colnames(no_outliers)

# 
# remove_ids <- c("b40-14_12_1_Drought", "b40-14_11_10_Control", "T52_12_2_Drought", "b40-14_2_5_Drought", "b40-14_12_3_Drought", "T48_12_1_Drought", "T52_12_1_Drought", "TXNM0821_12_1_Drought", "b42-34_12_5_Drought", "T48_12_3_Drought", "TXNM0821_11_3_Drought", "TXNM0821_12_1_Drought", "V60-96_12_1_Drought"   ) 
# 
# 
# # once I spoke with Mina about
# remove_ids <- c( "9018_12_2_Drought", "9018_122_3_Drought", "9018_12_4_Drought", 
#                  "b40-14_11_2_Drought", 
#                  "b40-14_12_10_Control", "b40-14_12_2_Drought" , "b40-14_12_7_Control", "b40-14_12_8_Control",
#                  "NY1_12_6_Control", "NY1_12_7_Control", "T48_12_5_Drought", 
#                  "T52_11_4_Drought", "T52_11_5_Drought", "T52_12_3_Drought", "TXNM0821_11_2_Drought", 
#                  "V60-96_12_5_Drought",
#                  "NY1_12_3_Drought", "T48_11_3_Drought")
# 
# #Minas plus the ones that couldnt be fitted 
# remove_ids <- c( "9018_12_2_Drought", "9018_12_3_Drought", "9018_12_4_Drought", "b40-14_11_2_Drought", 
#                  "b40-14_12_10_Control", "b40-14_12_2_Drought" , "b40-14_12_7_Control", "b40-14_12_8_Control", 
#                  "b42-34_11_2_Drought", "b42-34_12_3_Drought", "NY1_12_6_Control", "NY1_12_7_Control",
#                  "T48_12_5_Drought", 
#                  "T52_11_4_Drought", "T52_11_5_Drought", "T52_12_3_Drought", "TXNM0821_11_2_Drought", 
#                  "V60-96_12_5_Drought",
#                  "NY1_12_3_Drought", "T48_11_3_Drought", 
#                  
#                  "b40-14_12_1_Drought", "NY1_12_9_Control", "V60-96_11_2_Drought", "T52_12_5_Drought",
#                  "TXNM0821_12_9_Control",
#                  "TXNM0821_12_1_Drought", "NY1_12_1_Drought" )
# 
# remove_ids <- c( "9018_12_2_Drought", "9018_12_3_Drought", "9018_12_4_Drought", "b40-14_11_2_Drought", 
#                  "b40-14_12_10_Control", "b40-14_12_2_Drought" , "b40-14_12_7_Control", "b40-14_12_8_Control", 
#                  "b42-34_11_2_Drought", "b42-34_12_3_Drought", "NY1_12_6_Control", "NY1_12_7_Control",
#                  "T48_12_5_Drought", 
#                  "T52_11_4_Drought", "T52_11_5_Drought", "T52_12_3_Drought", "TXNM0821_11_2_Drought", 
#                  "V60-96_12_5_Drought",
#                  "NY1_12_3_Drought", "T48_11_3_Drought", 
#                  
#                  "b40-14_12_1_Drought", "V60-96_11_2_Drought", "T52_12_5_Drought")


investigate_ids <- c( "9018_12_1_Drought", 
                      "b40-14_11_10_Control",
                      "b40-14_12_1_Drought",
                      "NY1_12_4_Drought",
                      "NY1_12_9_Control", 
                      "T48_12_3_Drought",
                      "T52_12_5_Drought", 
                      "TXNM0821_11_3_Drought", 
                      "TXNM0821_12_9_Control", 
                      "V60-96_11_2_Drought")

# # subset no_outliers based on keep_ids
# no_outliers <- no_outliers %>% filter(!LeafID %in% remove_ids)


# all facet ---------------------------------------------------------------

individal_curves_facet <- ggplot(no_outliers, 
                                 aes(x = Ci, y = A, color = Treatment, group = LeafID)) +
  facet_wrap(~ LeafID) +
  geom_point() + 
  geom_line() +
  theme(legend.position="none")+
  theme_bw(base_size = 14)+
  scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
individal_curves_facet



# fit formula -------------------------------------------------------------


tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) # use names to find names of tibbles
# summary(tibblegroup)
df1 <- names(tibblegroup)

for (fitfile in tibblegroup) {
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    plot(fit_fitfile, addlegend = TRUE)
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
  }, error = function(e) {
    cat("Error occurred for fitfile:", fitfile$LeafID, "\n")
    message(e)
  })
}


# with title --------------------------------------------------------------

tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) # use names to find names of tibbles
# summary(tibblegroup)
df1 <- names(tibblegroup)

for (LeafID in df1) {
  fitfile <- tibblegroup[[LeafID]]  # Access the tibble using the LeafID as index
  
  tryCatch({
    fit_fitfile <- fitaci(fitfile, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    # Set the title of the plot using the LeafID
    plot_title <- paste("Leaf ID:", LeafID)
    plot(fit_fitfile, addlegend = TRUE, main = plot_title)
    
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
  }, error = function(e) {
    cat("Error occurred for fitfile:", LeafID, "\n")
    message(e)
  })
}

# color them --------------------------------------------------------------
investigate <- no_outliers %>% filter(LeafID %in% investigate_ids)

individual_plots <- list()

# Loop through unique LeafIDs
for (leaf_id in unique(investigate$LeafID)) {
  # Subset the data for the current LeafID
  subset_data <- subset(investigate, LeafID == leaf_id)
  
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




# new ---------------------------------------------------------------------

library(ggplot2)

# Assuming 'investigate_ids' is a vector of LeafIDs you want to investigate

# Assuming 'no_outliers' is your dataset without outliers
investigate <- no_outliers %>% filter(LeafID %in% investigate_ids)

individual_plots <- list()

# Loop through unique LeafIDs
for (leaf_id in unique(investigate$LeafID)) {
  # Subset the data for the current LeafID
  subset_data <- subset(investigate, LeafID == leaf_id)
  
  # Create the ggplot for the current LeafID
  individual_plot <- ggplot(subset_data, aes(x = Ci, y = A, color = Treatment)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 14) +
    labs(title = paste("Leaf ID:", leaf_id)) +
    scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
  
  # Store the plot in the list
  individual_plots[[as.character(leaf_id)]] <- individual_plot
  
  # Save the plot with LeafID in the file name
  ggsave(filename = paste("plot_", leaf_id, ".png", sep = ""),
         plot = individual_plot,
         width = 6, height = 4, dpi = 300)
}




# group_by ----------------------------------------------------------------


library(tidyverse)  # Make sure to load the necessary libraries

# Combine "Genotype" and "Treatment" to create a combined grouping variable
no_outliers1 <- no_outliers %>%
  mutate(Genotype_Treatment_Month = paste(Genotype, Treatment, Month, sep = "_"))

# Group the data by the combined grouping variable
tibblegroup <- split(no_outliers1, no_outliers1$Genotype_Treatment_Month)

# Iterate through each group
result_list <- list()  # To store the results for each group

for (group_name in names(tibblegroup)) {
  group_data <- tibblegroup[[group_name]]
  
  tryCatch({
    fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    plot_title <- group_name
    plot(fit_fitfile, addlegend = TRUE) 
    title(main = plot_title)
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
    

    # Store the result in the result_list
    result_list[[group_name]] <- fit_fitfile
    
  }, error = function(e) {
    cat("Error occurred for group:", group_name, "\n")
    message(e)
  })
  
}


colnames(no_outliers1) 
unique(no_outliers1$Ca)

# average over Ca ---------------------------------------------------------
library(tidyverse)
no_outliers1 <- no_outliers %>%
  mutate(Genotype_Treatment= paste(Genotype, Treatment, sep = "_"))

# Create a new grouping variable for Ca ranges
no_outliers1 <- no_outliers1 %>%
  mutate(Ca_Range = cut(Ca, breaks = c(0, 53, 83, 103, 153, 203, 253, 403, 503, 603, 703, 803, 903, 1000, 1203, 1403, Inf)))

# Combine "Genotype", "Treatment", "Month", and "Ca_Range" to create a combined grouping variable
no_outliers1 <- no_outliers1 %>%
  mutate(Genotype_Treatment_Ca = paste(Genotype, Treatment, Ca_Range, sep = "_"))

# Extract the necessary columns "Genotype", "Treatment", "Month", "Ca_Range", "Tleaf", and "Qin"
columns_to_extract <- c("Genotype", "Treatment", "Month", "Ca_Range", "A", "Ci", "Tleaf", "Qin")
grouped_data <- no_outliers1 %>%
  select(all_of(columns_to_extract)) %>%
  distinct()  # Keep only unique rows
  
  # Group the data by the combined grouping variable
  grouped_data <- grouped_data %>%
  group_by(Genotype, Treatment, Ca_Range) %>%
  summarise(mean_Ci = mean(Ci), se_Ci = sd(Ci) / sqrt(n()),
            mean_A = mean(A), se_A = sd(A) / sqrt(n()))

# Print the grouped data
print(grouped_data)



# fit_Aci -----------------------------------------------------------------

grouped_data <- grouped_data %>%
  mutate(Genotype_Treatment = paste(Genotype, Treatment, sep = "_"))

# Group the data by the combined grouping variable
tibblegroup <- split(grouped_data, grouped_data$Genotype_Treatment)

# Iterate through each group
result_list <- list()  # To store the results for each group

for (group_name in names(tibblegroup)) {
  group_data <- tibblegroup[[group_name]]
  
  tryCatch({
    fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "mean_A", Ci = "mean_Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    plot_title <- group_name
    plot(fit_fitfile, addlegend = TRUE) 
    title(main = plot_title)
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
    
    
    # Store the result in the result_list
    result_list[[group_name]] <- fit_fitfile
    
  }, error = function(e) {
    cat("Error occurred for group:", group_name, "\n")
    message(e)
  })
  
}



# investigate -------------------------------------------------------------
library(ggplot2)

individual_plots <- list()

# Get unique combinations of Genotype, Treatment, and Month
unique_combinations <- unique(grouped_data[, c("Genotype", "Treatment")])

# Loop through unique combinations
for (i in 1:nrow(unique_combinations)) {
  genotype <- unique_combinations$Genotype[i]
  treatment <- unique_combinations$Treatment[i]
  
  # Subset the data for the current combination
  subset_data <- filter(grouped_data, Genotype == genotype & Treatment == treatment)
  
  # Create the ggplot for the current combination
  individual_plot <- ggplot(subset_data, aes(x = mean_Ci, y = mean_A, color = Treatment)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 14) +
    geom_errorbar(aes(ymin = mean_A - se_A, ymax = mean_A + se_A), width = 0.2) + 
    labs(title = paste(genotype, treatment)) +
    scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
  
  # Store the plot in the list
  individual_plots[[paste(genotype, treatment, sep = "_")]] <- individual_plot
}

# Print or save the individual plots
for (plot_name in names(individual_plots)) {
  print(individual_plots[[plot_name]])
}


# group by treatment & month  ---------------------------------------------



library(tidyverse)  # Make sure to load the necessary libraries

# Combine "Genotype" and "Treatment" to create a combined grouping variable
no_outliers1 <- no_outliers %>%
  mutate(Genotype_Treatment_Month = paste(Genotype, Treatment, Month, sep = "_"))

# Group the data by the combined grouping variable
tibblegroup <- split(no_outliers1, no_outliers1$Genotype_Treatment_Month)

# Iterate through each group
result_list <- list()  # To store the results for each group

for (group_name in names(tibblegroup)) {
  group_data <- tibblegroup[[group_name]]
  
  tryCatch({
    fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "A", Ci = "Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    plot_title <- group_name
    plot(fit_fitfile, addlegend = TRUE) 
    title(main = plot_title)
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
    
    
    # Store the result in the result_list
    result_list[[group_name]] <- fit_fitfile
    
  }, error = function(e) {
    cat("Error occurred for group:", group_name, "\n")
    message(e)
  })
  
}


colnames(no_outliers1) 
unique(no_outliers1$Ca)

# average over Ca ---------------------------------------------------------
library(tidyverse)
no_outliers1 <- no_outliers %>%
  mutate(Genotype_Treatment= paste(Genotype, Treatment, sep = "_"))

# Create a new grouping variable for Ca ranges
no_outliers1 <- no_outliers1 %>%
  mutate(Ca_Range = cut(Ca, breaks = c(0, 53, 83, 103, 153, 203, 253, 403, 503, 603, 703, 803, 903, 1000, 1203, 1403, Inf)))

# Combine "Genotype", "Treatment", "Month", and "Ca_Range" to create a combined grouping variable
no_outliers1 <- no_outliers1 %>%
  mutate(Genotype_Treatment_Ca = paste(Genotype, Treatment, Ca_Range, sep = "_"))

# Extract the necessary columns "Genotype", "Treatment", "Month", "Ca_Range", "Tleaf", and "Qin"
columns_to_extract <- c("Genotype", "Treatment", "Month", "Ca_Range", "A", "Ci", "Tleaf", "Qin")
grouped_data <- no_outliers1 %>%
  select(all_of(columns_to_extract)) %>%
  distinct()  # Keep only unique rows

# Group the data by the combined grouping variable
grouped_data <- grouped_data %>%
  group_by(Genotype, Treatment, Ca_Range) %>%
  summarise(mean_Ci = mean(Ci), se_Ci = sd(Ci) / sqrt(n()),
            mean_A = mean(A), se_A = sd(A) / sqrt(n()))

# Print the grouped data
print(grouped_data)



# fit_Aci -----------------------------------------------------------------

grouped_data <- grouped_data %>%
  mutate(Genotype_Treatment = paste(Genotype, Treatment, sep = "_"))

# Group the data by the combined grouping variable
tibblegroup <- split(grouped_data, grouped_data$Genotype_Treatment)

# Iterate through each group
result_list <- list()  # To store the results for each group

for (group_name in names(tibblegroup)) {
  group_data <- tibblegroup[[group_name]]
  
  tryCatch({
    fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "mean_A", Ci = "mean_Ci", Tleaf = "Tleaf", PPFD = "Qin"))
    
    plot_title <- group_name
    plot(fit_fitfile, addlegend = TRUE) 
    title(main = plot_title)
    fit_fitfile$Photosyn(600)
    fit_fitfile$Ci(0) # compensation point
    coef(fit_fitfile)
    
    
    # Store the result in the result_list
    result_list[[group_name]] <- fit_fitfile
    
  }, error = function(e) {
    cat("Error occurred for group:", group_name, "\n")
    message(e)
  })
  
}



# investigate -------------------------------------------------------------
library(ggplot2)

individual_plots <- list()

# Get unique combinations of Genotype, Treatment, and Month
unique_combinations <- unique(grouped_data[, c("Treatment", )])

# Loop through unique combinations
for (i in 1:nrow(unique_combinations)) {
  genotype <- unique_combinations$Genotype[i]
  treatment <- unique_combinations$Treatment[i]
  
  # Subset the data for the current combination
  subset_data <- filter(grouped_data, Genotype == genotype & Treatment == treatment)
  
  # Create the ggplot for the current combination
  individual_plot <- ggplot(subset_data, aes(x = mean_Ci, y = mean_A, color = Treatment)) +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 14) +
    geom_errorbar(aes(ymin = mean_A - se_A, ymax = mean_A + se_A), width = 0.2) + 
    labs(title = paste(genotype, treatment)) +
    scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
  
  # Store the plot in the list
  individual_plots[[paste(genotype, treatment, sep = "_")]] <- individual_plot
}

# Print or save the individual plots
for (plot_name in names(individual_plots)) {
  print(individual_plots[[plot_name]])
}



# group by month & treatment ----------------------------------------------
    
    no_outliers1 <- no_outliers %>%
      mutate(Month_Treatment = paste(Month, Treatment, sep = "_"))
    
    # Create a new grouping variable for Ca ranges
    no_outliers1 <- no_outliers1 %>%
      mutate(Ca_Range = cut(Ca, breaks = c(0, 53, 83, 103, 
                                           153, 203, 253, 403, 503, 603, 703, 803, 903, 1000, 1203, 1403, Inf)))
    
    # Combine "Month", "Treatment", and "Ca_Range" to create a combined grouping variable
    no_outliers1 <- no_outliers1 %>%
      mutate(Month_Treatment_Ca = paste(Month, Treatment, Ca_Range, sep = "_"))
    
    # Extract the necessary columns "Month", "Treatment", "Ca_Range", "A", "Ci", "Tleaf", and "Qin"
    columns_to_extract <- c("Month", "Treatment", "Ca_Range", "A", "Ci", "Tleaf", "Qin")
    grouped_data <- no_outliers1 %>%
      select(all_of(columns_to_extract)) %>%
      distinct()  # Keep only unique rows
    
    # Group the data by the combined grouping variable
    grouped_data <- grouped_data %>%
      group_by(Month, Treatment, Ca_Range) %>%
      summarise(mean_Ci = mean(Ci), se_Ci = sd(Ci) / sqrt(n()),
                mean_A = mean(A), se_A = sd(A) / sqrt(n()),
                across(c(Tleaf, Qin), ~ first(.))) %>%
      ungroup()  
    # Print the grouped data
    print(grouped_data)

  # fit_Aci -----------------------------------------------------------------
      
      grouped_data <- grouped_data %>%
        mutate(Month_Treatment = paste(Month, Treatment, sep = "_"))
      
      # Group the data by the combined grouping variable
      tibblegroup <- split(grouped_data, grouped_data$Month_Treatment)
      
      # Iterate through each group
      result_list <- list()  # To store the results for each group
      
      for (group_name in names(tibblegroup)) {
        group_data <- tibblegroup[[group_name]]
        
        tryCatch({
          fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "mean_A", 
                                                            Ci = "mean_Ci", Tleaf = "Tleaf", PPFD = "Qin"))
          
          plot_title <- group_name
          plot(fit_fitfile, addlegend = TRUE) 
          title(main = plot_title)
          fit_fitfile$Photosyn(600)
          fit_fitfile$Ci(0) # compensation point
          coef(fit_fitfile)
          
          # Store the result in the result_list
          result_list[[group_name]] <- fit_fitfile
          
        }, error = function(e) {
          cat("Error occurred for group:", group_name, "\n")
          message(e)
        })
        
      }


  # plot monthly  -----------------------------------------------------------
  
  individual_plots <- list()
  
  # Get unique combinations of Treatment and Month
  unique_combinations <- unique(grouped_data[, c("Treatment", "Month")])
  
  # Loop through unique combinations
  for (i in 1:nrow(unique_combinations)) {
    treatment <- unique_combinations$Treatment[i]
    month <- unique_combinations$Month[i]
    
    # Subset the data for the current combination
    subset_data <- filter(grouped_data, Treatment == treatment & Month == month)
    
    # Create the ggplot for the current combination
    individual_plot <- ggplot(subset_data, aes(x = mean_Ci, y = mean_A, color = Treatment)) +
      geom_point() +
      geom_line() +
      theme_bw(base_size = 14) +
      geom_errorbar(aes(ymin = mean_A - se_A, ymax = mean_A + se_A), width = 0.2) + 
      labs(title = paste(treatment, month)) +
      scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
    
    # Store the plot in the list
    individual_plots[[paste(treatment, month, sep = "_")]] <- individual_plot
  }
  
  # Print or save the individual plots
  for (plot_name in names(individual_plots)) {
    print(individual_plots[[plot_name]])
  }
  

  
  

# group by treatment ------------------------------------------------------

  no_outliers <- no_outliers %>%
    mutate(Ca_Range = cut(Ca, breaks = c(0, 53, 83, 103, 
                                         153, 203, 253, 403, 503, 603, 703, 803, 903, 1000, 1203, 1403, Inf)))
  
  # Combine "Month", "Treatment", and "Ca_Range" to create a combined grouping variable
  no_outliers1 <- no_outliers %>%
    mutate(Treatment_Ca = paste(Treatment, Ca_Range, sep = "_"))
  
  # Extract the necessary columns "Month", "Treatment", "Ca_Range", "A", "Ci", "Tleaf", and "Qin"
  columns_to_extract <- c( "Treatment", "Ca_Range", "A", "Ci", "Tleaf", "Qin")
  grouped_data <- no_outliers1 %>%
    select(all_of(columns_to_extract)) %>%
    distinct()  # Keep only unique rows
  
  # Group the data by the combined grouping variable
  grouped_data <- grouped_data %>%
    group_by(Treatment, Ca_Range) %>%
    summarise(mean_Ci = mean(Ci), se_Ci = sd(Ci) / sqrt(n()),
              mean_A = mean(A), se_A = sd(A) / sqrt(n()),
              across(c(Tleaf, Qin), ~ first(.))) %>%
    ungroup()  
  # Print the grouped data
  print(grouped_data)
  
  # fit_Aci -----------------------------------------------------------------

  
  # Group the data by the combined grouping variable
  tibblegroup <- split(grouped_data, grouped_data$Treatment)
  
  # Iterate through each group
  result_list <- list()  # To store the results for each group
  
  for (group_name in names(tibblegroup)) {
    group_data <- tibblegroup[[group_name]]
    
    tryCatch({
      fit_fitfile <- fitaci(group_data, varnames = list(ALEAF = "mean_A", 
                                                        Ci = "mean_Ci", Tleaf = "Tleaf", PPFD = "Qin"))
      
      plot_title <- group_name
      plot(fit_fitfile, addlegend = TRUE) 
      title(main = plot_title)
      fit_fitfile$Photosyn(600)
      fit_fitfile$Ci(0) # compensation point
      coef(fit_fitfile)
      
      # Store the result in the result_list
      result_list[[group_name]] <- fit_fitfile
      
    }, error = function(e) {
      cat("Error occurred for group:", group_name, "\n")
      message(e)
    })
    
  }
  

# print check -------------------------------------------------------------

  
  individual_plots <- list()
  
  # Get unique combinations of Treatment and Month
  unique_combinations <- unique(grouped_data[, c("Treatment")])
  
  # Loop through unique combinations
  for (i in 1:nrow(unique_combinations)) {
    treatment <- unique_combinations$Treatment[i]
    
    # Subset the data for the current combination
    subset_data <- filter(grouped_data, Treatment == treatment)
    
    # Create the ggplot for the current combination
    individual_plot <- ggplot(subset_data, aes(x = mean_Ci, y = mean_A, color = Treatment)) +
      geom_point() +
      geom_line() +
      theme_bw(base_size = 14) +
      geom_errorbar(aes(ymin = mean_A - se_A, ymax = mean_A + se_A), width = 0.2) + 
      labs(title = paste(treatment)) +
      scale_color_manual(values = c("Control" = "red", "Drought" = "green"))
    
    # Store the plot in the list
    individual_plots[[paste(treatment, sep = "_")]] <- individual_plot
  }
  
  # Print or save the individual plots
  for (plot_name in names(individual_plots)) {
    print(individual_plots[[plot_name]])
  }
  
  
  # Assuming your data frame is named 'no_outliers'
  subset_1_to_5 <- no_outliers[no_outliers$VineNumber %in% 1:5 & no_outliers$Treatment != "Drought", ]
  subset_6_to_10 <- no_outliers[no_outliers$VineNumber %in% 6:10 & no_outliers$Treatment != "Control", ]
  
  # Display the resulting subsets
  print(subset_1_to_5)
  print(subset_6_to_10)
  
  