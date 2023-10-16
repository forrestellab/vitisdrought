library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(broom)
library(lubridate)
library(gtExtras)
library(gt)

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# read file for entire data set & modify data 
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                    "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
             mutate( species_geno = paste(Species, Genotype, sep = "_"))) 

swclong <- swclong[, c("Genotype", "Species", "Treatment", "WU", "SWC", "Date", "species_geno")]

# Convert the date column to a Date object
swclong$Date <- as.Date(swclong$Date, format = "%m/%d")
swclong$Month <- month(swclong$Date)
swclong$Day <- day(swclong$Date)
swclong$MonthDay <- paste0(swclong$Month, "-", swclong$Day)
unique_dates <- unique(swclong$MonthDay)

# Plot SWC across ALL Species ---------------------------------------------

# Calculate quartiles for each Treatment
swc_quartiles <- swclong %>% 
  group_by(Treatment) %>% 
  summarize(q1 = quantile(SWC, 0.25, na.rm = TRUE),
            q3 = quantile(SWC, 0.75, na.rm = TRUE))

# Define upper and lower limits for each Treatment
swc_limits <- swc_quartiles %>% 
  mutate(IQR = q3 - q1,
         lower_limit = q1 - 1.5*IQR,
         upper_limit = q3 + 1.5*IQR)

# Filter out outliers by Treatment

swc_filtered <- swclong %>% 
  left_join(swc_limits, by = "Treatment") %>% 
  filter(SWC >= lower_limit & SWC <= upper_limit)

# Plot SWC without outliers by Treatment -------------------------------------

grouped_data_swc_species <- swc_filtered  %>% group_by(Date, Treatment, species_geno)

summary_data_swc_species <- grouped_data_swc_species%>% 
  summarize(mean_value_swc = mean(SWC),
            sem_swc = sd(SWC) / sqrt(n()))


# Calculate the week numbers starting from 0
summary_data_swc$Week <- as.numeric(difftime(summary_data_swc$Date, min(summary_data_swc$Date), units = "weeks"))

SWC_all_plot <- ggplot(summary_data_swc, aes(x = Week, y = mean_value_swc, 
                                             color = Treatment)) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_value_swc - sem_swc, 
                    ymax = mean_value_swc + sem_swc), width = 0.2) +
  labs(x = "", y = "Pot Water Content") +
  theme_bw() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, max(summary_data_swc$Week), by = 1)) +
  xlab(label = "Week") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(name = "Pot Water Content", labels = scales::percent, 
                     limits = c(0.14, 0.92), breaks = seq(0, 1, by = 0.1)) +
  theme(legend.position = "bottom")

print(SWC_all_plot)




# SWC by species ----------------------------------------------------------
date_1 <- as.Date("11/1", format = "%m/%d")
date_2 <- as.Date("11/13", format = "%m/%d")
date_3 <- as.Date("12/18", format = "%m/%d")

# Convert the dates to week numbers
week_1 <- as.numeric(difftime(date_1, min(summary_data_swc_species$Date), units = "weeks"))
week_2 <- as.numeric(difftime(date_2, min(summary_data_swc_species$Date), units = "weeks"))
week_3 <- as.numeric(difftime(date_3, min(summary_data_swc_species$Date), units = "weeks"))

treatment_start_date <- as.Date("10/30", format = "%m/%d")


summary_data_swc_species$Week <- as.numeric(difftime(summary_data_swc_species$Date, min(summary_data_swc_species$Date), units = "weeks"))

treatment_start_week <- as.numeric(difftime(treatment_start_date, min(summary_data_swc_species$Date), units = "weeks"))

SWC_species_plot <- ggplot(summary_data_swc_species, aes(x = Week, y = mean_value_swc, color = Treatment)) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(aes(group = Treatment), linewidth= 0.3) +
  geom_errorbar(aes(ymin = mean_value_swc - sem_swc, ymax = mean_value_swc + sem_swc), width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(summary_data_swc_species$Week), by = 1)) +
  geom_vline(xintercept = c(week_1, week_2, week_3), linetype = "dashed", color = "darkgrey") +
  xlab("Week") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(
    name = "Pot Water Content", 
    limits = c(0.1, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0)  # Set hjust to 0 for left alignment
  ) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  facet_wrap(~ species_geno, scales = "free", ncol = 2,
             labeller = labeller(species_geno = c("acerifolia_9018" = "a) V. acerifolia 9018", "aestivalis_T52" = "b) V. aestivalis T52", "arizonica_b40-14" = "c) V. arizonica b40-14", "cinerea_b42-34" = "d) V. cinerea b42-34", "mustangensis_T48" = "e) V. mustangensis T48","riparia_NY1" = "f) V. riparia NY1", "riparia_TXNM0821" = "g) hybrid TXNM0821", "rupestris_Vru42" = "h) V. rupestris Vru42" , "vulpina_V60-96" = "i) V. vulpina V60-96"  )))

print(SWC_species_plot)



# with measurementdates ---------------------------------------------------



# Convert the date strings to Date objects
date_1 <- as.Date("10/30", format = "%m/%d")
date_2 <- as.Date("11/13", format = "%m/%d")
date_3 <- as.Date("12/16", format = "%m/%d")

# Convert the dates to week numbers
week_1 <- as.numeric(difftime(date_1, min(summary_data_swc_species$Date), units = "weeks"))
week_2 <- as.numeric(difftime(date_2, min(summary_data_swc_species$Date), units = "weeks"))
week_3 <- as.numeric(difftime(date_3, min(summary_data_swc_species$Date), units = "weeks"))



SWC_species_plot <- ggplot(summary_data_swc_species, aes(x = Week, y = mean_value_swc, color = Treatment)) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(aes(group = Treatment), linewidth= 0.3) +
  geom_errorbar(aes(ymin = mean_value_swc - sem_swc, ymax = mean_value_swc + sem_swc), width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(summary_data_swc_species$Week), by = 1)) +
  #geom_vline(xintercept = treatment_start_week, linetype = "dotted", color = "darkred") +
  geom_vline(xintercept = week_1, linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = week_2, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = week_3, linetype = "dashed", color = "darkgrey") +
  xlab("Week") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(
    name = "Pot Water Content", 
    limits = c(0.1, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0)  # Set hjust to 0 for left alignment
  ) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  facet_wrap(~ species_geno, scales = "free", ncol = 2,
             labeller = labeller(species_geno = c("acerifolia_9018" = "a) V. acerifolia 9018", "aestivalis_T52" = "b) V. aestivalis T52", "arizonica_b40-14" = "c) V. arizonica b40-14", "cinerea_b42-34" = "d) V. cinerea b42-34", "mustangensis_T48" = "e) V. mustangensis T48","riparia_NY1" = "f) V. riparia NY1", "riparia_TXNM0821" = "g) hybrid TXNM0821", "rupestris_Vru42" = "h) V. rupestris Vru42" , "vulpina_V60-96" = "i) V. vulpina V60-96"  )))

print(SWC_species_plot)



# with days ---------------------------------------------------------------

summary_data_swc_species$Day <- as.numeric(difftime(summary_data_swc_species$Date, min(summary_data_swc_species$Date) , units = "days"))


# Convert the date strings to Date objects
date_1 <- as.Date("10/30", format = "%m/%d")
date_2 <- as.Date("11/13", format = "%m/%d")
date_3 <- as.Date("12/16", format = "%m/%d")

# Convert the dates to week numbers
day_1 <- as.numeric(difftime(date_1, min(summary_data_swc_species$Date), units = "days"))
day_2 <- as.numeric(difftime(date_2, min(summary_data_swc_species$Date), units = "days"))
day_3 <- as.numeric(difftime(date_3, min(summary_data_swc_species$Date), units = "days"))



SWC_species_plot <- ggplot(summary_data_swc_species, aes(x = Day, y = mean_value_swc, color = Treatment)) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(aes(group = Treatment), linewidth= 0.3) +
  geom_errorbar(aes(ymin = mean_value_swc - sem_swc, ymax = mean_value_swc + sem_swc), width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, max(summary_data_swc_species$Day), by = 7)) +
  #geom_vline(xintercept = treatment_start_week, linetype = "dotted", color = "darkred") +
  geom_vline(xintercept = day_1, linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = day_2, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = day_3, linetype = "dashed", color = "darkgrey") +
  xlab("Day") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(
    name = "Pot Water Content", 
    limits = c(0.1, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0)  # Set hjust to 0 for left alignment
  ) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  facet_wrap(~ species_geno, scales = "free", ncol = 2,
             labeller = labeller(species_geno = c("acerifolia_9018" = "a) V. acerifolia 9018", "aestivalis_T52" = "b) V. aestivalis T52", "arizonica_b40-14" = "c) V. arizonica b40-14", "cinerea_b42-34" = "d) V. cinerea b42-34", "mustangensis_T48" = "e) V. mustangensis T48","riparia_NY1" = "f) V. riparia NY1", "riparia_TXNM0821" = "g) hybrid TXNM0821", "rupestris_Vru42" = "h) V. rupestris Vru42" , "vulpina_V60-96" = "i) V. vulpina V60-96"  )))

print(SWC_species_plot)
