library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(knitr)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


A_data <-  read.csv("data_output/Licor/summary_data_6800_A_sig.csv")
gsw_data <- read.csv("data_output/Licor/summary_data_6800_gsw_sig.csv")

gswclean_me <- read.csv("data/gsw_6800_cleaned.csv")
pre_gswclean <- read.csv("data/gsw_6800_pre-clean.csv")

gswclean_me <- subset(gswclean_me, 
                      select = c("Date", "Genotype", "rep", "A", "Ci", "gsw_6800", "Treatment")) %>%
                    filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                           "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))
pre_gswclean <- subset(pre_gswclean, 
                       select = c("Date", "Genotype", "rep", "A", "Ci", "E", 
                                  "gsw_6800", "Treatment")) %>%
                  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

# assuming your data frames are called 'gswclean_me' and 'pre_gswclean'
merged_df_licor <- merge(gswclean_me, pre_gswclean[, c("Date", "Genotype", "rep", "A", "Ci", "E", "gsw_6800", "Treatment")], 
                   by = c("Date", "Genotype", "rep", "A", "Ci", "gsw_6800", "Treatment"), 
                   all.y = TRUE)

merged_df_licor <- merged_df_licor %>% 
      mutate(new_date = ifelse(Date %in% c("10.26", "10.27", "10.28", "10.29"), "10.28",
                               ifelse(Date %in% c("11.12", "11.13"), "11.13",
                                      ifelse(Date %in% c("12.06", "12.07", 
                                                         "12.08", "12.09"), "12.08", Date))))
merged_df_licor <- merged_df_licor%>% 
      select(-Date) %>% 
      rename(Date = new_date) %>%
      rename(gsw = gsw_6800)

merged_df_licor$Date <- as.character(merged_df_licor$Date)
merged_df_licor$Date <- gsub("\\.\\d{4}", "", merged_df_licor$Date)
merged_df_licor$Date <- sprintf("%02d-%02d", 
                                       as.numeric(substring(merged_df_licor$Date, 1, 2)), 
                                       as.numeric(substring(merged_df_licor$Date, 4, 5)))


# Calculate WUE -----------------------------------------------------------

merged_df_licor <- merged_df_licor %>%
                              mutate(intrWUE_AE = A / gsw,
                                     instWUE_AGs= A / E)

# Table Creation:  --------------------------------------------------------
    grouped_merged_df_licor <- merged_df_licor %>% 
      group_by(Date, Treatment, Genotype)
    
    summary_merged_df_licor <- grouped_merged_df_licor %>% 
      summarize(mean_value_gsw = mean(gsw),
                sem_gsw = sd(gsw) / sqrt(n()),
                mean_value_A = mean(A),
                sem_A = sd(A) / sqrt(n()),
                mean_value_E = mean(E),
                sem_E = sd(E) / sqrt(n()),
                mean_value_intrWUE_AE = mean(intrWUE_AE),
                sem_intrWUE_AE = sd(intrWUE_AE) / sqrt(n()),
                mean_value_instWUE_AGs = mean(instWUE_AGs),
                sem_instWUE_AGs = sd(instWUE_AGs) / sqrt(n()),
                )
    
    for (variable in c("gsw", "A", "E", "intrWUE_AE", "instWUE_AGs")) {
      t_test_results <- data.frame(Date = character(),
                                   Genotype = character(),
                                   p_value = numeric(),
                                   stringsAsFactors = FALSE)
      for (day in unique(grouped_merged_df_licor$Date)) {
        for (genotype in unique(grouped_merged_df_licor$Genotype)) {
          subset_data <- subset(grouped_merged_df_licor, 
                                Date == day & Genotype == genotype, 
                                select = c(variable, "Treatment"))
          if (all(table(subset_data$Treatment) >= 2)) {
            t_test <- t.test(get(variable) ~ Treatment, data = subset_data)
            t_test_results <- rbind(t_test_results,
                                    data.frame(Date = day,
                                               Genotype = genotype,
                                               p_value= t_test$p.value,
                                               stringsAsFactors = FALSE))
          }
        }
      }
      colnames(t_test_results)[ncol(t_test_results)] <- paste0("p_value_", variable)
      summary_merged_df_licor <- left_join(summary_merged_df_licor, 
                                           t_test_results, by = c("Date", "Genotype"))
    }
    
    summary_merged_df_licor <- summary_merged_df_licor %>%
      select(Date, Treatment, Genotype,
             mean_value_gsw, sem_gsw, p_value_gsw,
             mean_value_A, sem_A, p_value_A,
             mean_value_E, sem_E, p_value_E, 
             mean_value_intrWUE_AE, sem_intrWUE_AE, p_value_intrWUE_AE,
             mean_value_instWUE_AGs, sem_instWUE_AGs, p_value_instWUE_AGs
             ) %>% ungroup()

write.csv(summary_merged_df_licor, file = "data_output/Licor/summary_merged_df_licor.csv", row.names = FALSE)
    

    print(summary_merged_df_licor) %>% 
      gt() %>% 
      gt_theme_nytimes()  %>% 
      columns_width(width = "fit-content") %>%
      tab_header(title ="Summary of Gsw, A, E and WUE by Date, Treatment, and Genotype")  %>% 
      gtsave(filename = "summary_merged_df_licor.html", path = "fig_output/Licor/")
    

