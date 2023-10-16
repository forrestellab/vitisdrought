library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")
# Licor Data --------------------------------------------------------------


  # Load & Modify Data ---------------------------------------------------------------

gswclean_6800 <- read.csv("data/gsw_6800_cleaned.csv")
gswclean_6800 <- (gswclean_6800 %>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                    "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

# Use the mutate() function to create a new column called 'new_date'
# Use the select() function to drop the original 'Date' column
# and rename the 'new_date' column as 'Date'
    gswclean_6800 <- gswclean_6800 %>% 
      mutate(new_date = ifelse(Date %in% c("10.26", "10.27", "10.28", "10.29"), "10.28",
                               ifelse(Date %in% c("11.12", "11.13"), "11.13",
                                      ifelse(Date %in% c("12.06", "12.07", 
                                                         "12.08", "12.09"), "12.08", Date))))
    gswclean_6800 <- gswclean_6800 %>% 
      select(-Date) %>% 
      rename(Date = new_date) %>%
      rename(gsw = gsw_6800)

    gswclean_6800$Date <- as.character(gswclean_6800$Date)
    gswclean_6800$Date <- gsub("\\.\\d{4}", "", gswclean_6800$Date)
    gswclean_6800$Date <- sprintf("%02d-%02d", 
                                       as.numeric(substring(gswclean_6800$Date, 1, 2)), 
                                       as.numeric(substring(gswclean_6800$Date, 4, 5)))
    
    gswclean_6800 <- gswclean_6800[c("Date", "Genotype", "gsw", "Treatment", "A", "Ci")]
    
# Group the data by Date, Treatment, and Species
grouped_data_6800 <- gswclean_6800 %>% group_by(Date, Treatment, Genotype)

# Calculate the mean and standard error of the Mean (SEM) for each group
summary_data_6800_gsw <- grouped_data_6800 %>% summarize(mean_value_6800 = mean(gsw),
                                           sem_6800 = sd(gsw) / sqrt(n()))

  # Plot Licor Data----------------------------------------------------------------
licor_plot <-    ggplot(summary_data_6800_gsw, aes(x = Date, y = mean_value_6800, 
                                  color = Treatment, group = Treatment)) +
                geom_point(size = 3) +
                scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
                geom_errorbar(aes(ymin = mean_value_6800 - sem_6800, 
                                  ymax = mean_value_6800 + sem_6800), width = 0.2) +
                facet_wrap(~ Genotype) +
                labs(x = "Date", y = "gsw_6800") +
                theme_bw()

print(licor_plot)

# Save plot
#ggsave("fig_output/Licor/licor_plot.png", licor_plot)
#ggsave("fig_output/Licor/licor_plot.pdf", licor_plot)


# Significance ------------------------------------------------------------

# Create an empty data frame to store the results
t_test_results_gsw <- data.frame(Date = character(),
                             Genotype = character(),
                             p_value = numeric(),
                             stringsAsFactors = FALSE)

for (day in unique(grouped_data_6800$Date)) {
  for (genotype in unique(grouped_data_6800$Genotype)) {
    # Subset the data for the current day and genotype, and only include the A column
    subset_data_gsw <- subset(grouped_data_6800, Date == day & Genotype == genotype, select = c("gsw", "Treatment"))
    
    # Check if there are at least two observations for each treatment in the subsetted data
    if (all(table(subset_data_gsw$Treatment) >= 2)) {
      # Perform the two-sample t-test
      t_test_gsw <- t.test(gsw ~ Treatment, data = subset_data_gsw)
      
      # Append the results to the data frame
      t_test_results_gsw <- rbind(t_test_results_gsw,
                                data.frame(Date = day,
                                           Genotype = genotype,
                                           p_value = t_test_gsw$p.value,
                                           stringsAsFactors = FALSE))
    }
  }
}

# Print the results
print(t_test_results_gsw)

# Save the data as a CSV file in the specified directory
      # write.csv(t_test_results_gsw, 
      #file = "data_output/Licor/t_test_results_gsw.csv", row.names = FALSE)


# Plot Licor Gsw Significance ---------------------------------------------

summary_data_6800_gsw_sig <- left_join(summary_data_6800_gsw, 
                                     t_test_results_gsw, by = c("Date", "Genotype"))

      # write.csv(summary_data_6800_gsw_sig, 
                # file = "data_output/Licor/summary_data_6800_gsw_sig.csv", row.names = FALSE)

licor_plot_gsw_sig  <- ggplot(summary_data_6800_gsw_sig, aes(x = Date, y = mean_value_6800, 
                                                         color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_6800 - sem_6800, 
                    ymax = mean_value_6800 + sem_6800), width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "gsw_6800") +
  theme_bw() +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "ns"),
                x = Date, y = 0.45),
            size = 4,  color = "black")

print(licor_plot_gsw_sig)

# Save plot
#ggsave("fig_output/Licor/licor_plot_gsw_sig.png", licor_plot_gsw_sig)
#ggsave("fig_output/Licor/licor_plot_gsw_sig.pdf", licor_plot_gsw_sig)




# Porometer Data ----------------------------------------------------------

    gswclean_porometer <- read.csv("data/gsw_porometer_cleaned.csv")
    gswclean_porometer <- (gswclean_porometer  %>%
                            filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                                   "T48", "NY1", "TXNM0821", "Vru42", 
                                                   "V60-96" )))

# Use the mutate() function to create a new column called 'new_date'
# that contains the renamed dates
    gswclean_porometer <- gswclean_porometer %>% 
                      mutate(new_date = ifelse(Date %in% c("10.26", "10.27"), "10.27",
                                        ifelse(Date %in% c("11.16", "11.17"), "11.17",
                                        ifelse(Date %in% c("11.23", "11.24"), "11.24",
                                               Date))))

    gswclean_porometer <- gswclean_porometer %>% 
      select(-Date) %>% 
      rename(Date = new_date)%>%
      rename(gsw = gsw_porometer)
    # Convert the Date column to a character string
    gswclean_porometer$Date <- as.character(gswclean_porometer$Date)
    # Remove the year from the Date column
    gswclean_porometer$Date <- gsub("\\.\\d{4}", "", gswclean_porometer$Date)
    # Format the Date column to have leading zeros for month and day
    gswclean_porometer$Date <- sprintf("%02d-%02d", 
                                       as.numeric(substring(gswclean_porometer$Date, 1, 2)), 
                                       as.numeric(substring(gswclean_porometer$Date, 4, 5)))


  # Calculate SEM -----------------------------------------------------------
     grouped_data_porometer <- gswclean_porometer %>% group_by(Date, Treatment, Genotype)
    
      summary_data_porometer <- grouped_data_porometer%>% 
      summarize(mean_value_porometer = mean(gsw),
      sem_porometer = sd(gsw) / sqrt(n()))
    
    # Plot Porometer Data----------------------------------------------------------------
porometer_plot <-    ggplot(summary_data_porometer, aes(x = Date, y = mean_value_porometer, 
                                  color = Treatment, group = Treatment)) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
      geom_errorbar(aes(ymin = mean_value_porometer - sem_porometer, 
                        ymax = mean_value_porometer + sem_porometer), width = 0.2) +
      facet_wrap(~ Genotype) +
      labs(x = "Date", y = "gsw_porometer") +
      theme_bw()
  
     
print(porometer_plot)

# Save plot
ggsave("fig_output/Licor/porometer_plot.png", porometer_plot)
ggsave("fig_output/Licor/porometer_plot.pdf", porometer_plot)


# Combine Licor & Porometer Data ------------------------------------------
# Subset and rename columns for gswclean_6800_new
      
      #!!!WATCH OUT that the data 

gswclean_6800_sub <- gswclean_6800[c("Date", "Genotype", "gsw", "Treatment")]

# Subset and rename columns for gswclean_porometer_new
gswclean_porometer_sub <- gswclean_porometer[c("Date", "Genotype", "gsw", "Treatment")]

# Merge dataframes
gsw_combined <- merge(gswclean_6800_sub, gswclean_porometer_sub, by = c("Date", "Genotype", "gsw", "Treatment"), all.y = TRUE)

    # Calculate SEM -----------------------------------------------------------
    grouped_data_combined <- gsw_combined %>% group_by(Date, Treatment, Genotype)
    
    summary_data_combined <- grouped_data_combined%>% 
      summarize(mean_value_combined = mean(gsw),
                sem_combined = sd(gsw) / sqrt(n()))
    
      # Plot Combined Data----------------------------------------------------------------
      ggplot(summary_data_combined, aes(x = Date, y = mean_value_combined, 
                                         color = Treatment, group = Treatment)) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
        geom_errorbar(aes(ymin = mean_value_combined - sem_combined, 
                          ymax = mean_value_combined + sem_combined), width = 0.2) +
        facet_wrap(~ Genotype) +
        labs(x = "Date", y = "gsw_combined") +
        theme_bw()
      
      
      ggplot(summary_data_combined, aes(x = Date, y = mean_value_combined, 
                                        color = Genotype, form = Treatment)) +
        geom_point(size = 3) +
        #scale_color_manual(values = c("Control" = "blue", "Drought" = "orange"))+
        geom_errorbar(aes(ymin = mean_value_combined - sem_combined, 
                          ymax = mean_value_combined + sem_combined), width = 0.2) +
        labs(x = "Date", y = "gsw_combined") +
        theme_bw()
      
      
      
      