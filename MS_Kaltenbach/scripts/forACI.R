library(tidyr)

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
# Subset the data frame for VineNumbers 1-5 and check if all are classified as "drought"
subset_1_5 <- subset(vine_treatment, VineNumber %in% 1:5)
all(subset_1_5$Treatment == "drought")

# Subset the data frame for VineNumbers 6-10 and check if all are classified as "control"
subset_6_10 <- subset(vine_treatment, VineNumber %in% 6:10)
all(subset_6_10$Treatment == "control")


# Subset data frame to rows with VineNumber 1-5
df1 <- subset(vine_treatment, VineNumber %in% 1:5, select=c(VineNumber, Treatment))

# Check unique Treatment values for each VineNumber
unique(df1)

# Subset data frame to rows with VineNumber 6-10
df2 <- subset(vine_treatment, VineNumber %in% 6:10, select=c(VineNumber, Treatment))

# Check unique Treatment values for each VineNumber
unique(df2)

# Find rows with VineNumber 1-5 that are not classified as "drought"
wrong_drought <- which(vine_treatment$VineNumber %in% 1:5 & vine_treatment$Treatment != "Drought")

# Find rows with VineNumber 6-10 that are not classified as "control"
wrong_control <- which(vine_treatment$VineNumber %in% 6:10 & vine_treatment$Treatment != "Control")




# Check the number of rows with VineNumber 1-5
nrow(vine_treatment[vine_treatment$VineNumber %in% 1:5, ])
# Check the number of rows with VineNumber 6-10
nrow(vine_treatment[vine_treatment$VineNumber %in% 6:10, ])

