library(dplyr)
library(lubridate)

# Licor Data --------------------------------------------------------------


# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Load & Modify Data ---------------------------------------------------------------

leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

leaf<- leaf %>%
  select(Genotype, Species)

gswclean_6800 <- read.csv("data/gsw_6800_cleaned.csv")
gswclean_6800 <- (gswclean_6800 %>%
                    filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                           "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

unique(gswclean_6800$Genotype)

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

gswclean_6800_A <- gswclean_6800[c("Date", "Genotype", "gsw", "Treatment", "A", "Ci")]

# Group the data by Date, Treatment, and Species
grouped_data_6800_A <- gswclean_6800_A %>% group_by(Date, Treatment, Genotype)

# Calculate the mean and standard error of the Mean (SEM) for each group
summary_data_6800_A <- grouped_data_6800_A %>% summarize(mean_value_6800_A = mean(A),
                                                     sem_6800_A = sd(A) / sqrt(n()))

# Plot Licor Data  ----------------------------------------------------------------
licor_plot_A  <- ggplot(summary_data_6800_A, aes(x = Date, y = mean_value_6800_A, 
                                        color = Treatment, group = Treatment)) +
                    geom_point(size = 3) +
                    scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
                    geom_errorbar(aes(ymin = mean_value_6800_A - sem_6800_A, 
                                      ymax = mean_value_6800_A + sem_6800_A), width = 0.2) +
                    facet_wrap(~ Genotype) +
                    labs(x = "Date", y = "A_6800") +
                    theme_bw() 

print(licor_plot_A)

# Save plot
ggsave("fig_output/Licor/licor_plot_A.png", licor_plot_A)
ggsave("fig_output/Licor/licor_plot_A.pdf", licor_plot_A)


# Significance ------------------------------------------------------------

# Create an empty data frame to store the results
t_test_results_A <- data.frame(Date = character(),
                             Genotype = character(),
                             p_value = numeric(),
                             stringsAsFactors = FALSE)

# Loop through each unique combination of Date and Genotype

for (day in unique(grouped_data_6800_A$Date)) {
  for (genotype in unique(grouped_data_6800_A$Genotype)) {
    # Subset the data for the current day and genotype, and only include the A column
    subset_data_A <- subset(grouped_data_6800_A, Date == day & Genotype == genotype, select = c("A", "Treatment"))
    
    # Check if there are at least two observations for each treatment in the subsetted data
    if (all(table(subset_data_A$Treatment) >= 2)) {
      # Perform the two-sample t-test
      t_test_A <- t.test(A ~ Treatment, data = subset_data_A)
      
      # Append the results to the data frame
      t_test_results_A <- rbind(t_test_results_A,
                                data.frame(Date = day,
                                           Genotype = genotype,
                                           p_value = t_test_A$p.value,
                                           stringsAsFactors = FALSE))
    }
  }
}


# Print the results
print(t_test_results_A)

# Save the data as a CSV file in the specified directory
# write.csv(t_test_results_A, file = "data_output/Licor/t_test_results_A.csv", row.names = FALSE)


# Plot Licor Data Significance ----------------------------------------------------------------
summary_data_6800_A_sig <- left_join(summary_data_6800_A, 
                                     t_test_results_A, by = c("Date", "Genotype"))

# Save the data as a CSV file in the specified directory
  # write.csv(summary_data_6800_A_sig, 
  # file = "data_output/Licor/summary_data_6800_A_sig.csv", row.names = FALSE)


summary_data_6800_A_sig2 <- left_join(summary_data_6800_A_sig, 
                                      leaf, by = c( "Genotype"))

licor_plot_A_sig  <- ggplot(summary_data_6800_A_sig2, aes(x = Date, y = mean_value_6800_A, 
                                                         color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_6800_A - sem_6800_A, 
                    ymax = mean_value_6800_A + sem_6800_A), width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "A_6800") +
  theme_bw() +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "ns"),
                x = Date, y = 22), 
            size = 4,  color = "black")
  #ylim(-5, 25)

print(licor_plot_A_sig)

unique(summary_data_6800_A_sig$Date)

# Save plot
ggsave("fig_output/Licor/licor_plot_A_sig.png", licor_plot_A_sig)
ggsave("fig_output/Licor/licor_plot_A_sig.pdf", licor_plot_A_sig)



# Last Day ----------------------------------------------------------------

# Filter data for Date "12-08"
filtered_data <- subset(summary_data_6800_A_sig, Date == "12-08")

# Create the plot
licor_plot_A_sig <- ggplot(filtered_data, aes(x = paste(Species, Genotype), y = mean_value_6800_A, 
                                              color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_6800_A - sem_6800_A, 
                    ymax = mean_value_6800_A + sem_6800_A), width = 0.2) +
  labs(x = "", y = "Net Assimilation Rate") +
  theme_bw() +
  geom_text(data = filtered_data,
            aes(label = ifelse(p_value < 0.05, "*", "ns"),
                x = Genotype, y = 22), 
            size = 4, color = "black")

print(licor_plot_A_sig)



summary_data_6800_A_sig2 <- left_join(summary_data_6800_A_sig, 
                                      leaf, by = c( "Genotype"))
# Filter data for Date "12-08"
filtered_data <- subset(summary_data_6800_A_sig2, Date == "12-08")%>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

licor_plot_A_sig  <- ggplot(filtered_data, aes(x = paste(Species, Genotype), y = mean_value_6800_A, 
                                                          color = Treatment, group = Treatment)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_6800_A - sem_6800_A, 
                    ymax = mean_value_6800_A + sem_6800_A), width = 0.2) +
  labs(x = "Date", y = "Net assimilation rate (μmol CO2 m−2 s−1)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Net assimilation rate final date (12/08)") +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "ns"),
                x = paste(Species, Genotype), y = 15), 
            size = 4,  color = "black")
#ylim(-5, 25)

print(licor_plot_A_sig)



# Filter data for Date "12-08"
filtered_data <- subset(summary_data_6800_A_sig2, Date == "11-13")%>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

licor_plot_A_sig  <- ggplot(filtered_data, aes(x = paste(Species, Genotype), y = mean_value_6800_A, 
                                                          color = Treatment, group = Treatment)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
geom_errorbar(aes(ymin = mean_value_6800_A - sem_6800_A, 
                  ymax = mean_value_6800_A + sem_6800_A), width = 0.2) +
  labs(x = "Date", y = "Net assimilation rate (μmol CO2 m−2 s−1)") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Net assimilation rate (11/13)") +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "ns"),
                x = paste(Species, Genotype), y = 21), 
            size = 4,  color = "black")
#ylim(-5, 25)

print(licor_plot_A_sig)
