
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import data
cumWU <- read.csv("data/Subset/cumWu.csv")
cumWU <- (cumWU%>%
                  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))
cumWU$Species[cumWU $Genotype == "TXNM0821"] <- "hybrid"


colnames(cumWU)


# Plot cumWU without outliers by Treatment -------------------------------------

summary_data_cumWU <- cumWU %>%
  group_by(Genotype, Treatment, Species) %>%
  summarize(mean_value_cumWU = mean(sumwu),
            sem_cumWU = sd(sumwu) / sqrt(n()))

cumWU_all_plot <- ggplot(summary_data_cumWU, aes(x = paste(Species, Genotype), y = mean_value_cumWU, 
                                                 color = Treatment)) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_value_cumWU - sem_cumWU, 
                    ymax = mean_value_cumWU + sem_cumWU), width = 0.2) +
  labs(x = "Species - Genotype", y = "cumWU") +
  theme_bw() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(name = "Total Waterus per plant (L)",
                     limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative Water Use by Species and Treatment") +

print(cumWU_all_plot)

# Try ---------------------------------------------------------------------
summary_data_cumWU <- summary_data_cumWU %>%
  group_by(Species) %>%
  mutate(relative_water_saving = (mean_value_cumWU[Treatment == "Control"] - mean_value_cumWU) / mean_value_cumWU[Treatment == "Control"] * 100)

cumWU_all_plot <- ggplot(summary_data_cumWU, aes(x = paste(Species, Genotype), y = mean_value_cumWU, 
                                                 color = Treatment)) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_value_cumWU - sem_cumWU, 
                    ymax = mean_value_cumWU + sem_cumWU), width = 0.2) +
  labs(x = "Species - Genotype", y = "cumWU") +
  theme_bw() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(name = "Total Waterus per plant (L)",
                     limits = c(0, 18), breaks = seq(0, 18, by = 5)) +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative Water Use by Species and Treatment") +
  geom_text(data = subset(summary_data_cumWU, Treatment == "Drought"), 
            aes(label = paste0(round(relative_water_saving), "%"), y = mean_value_cumWU + sem_cumWU), 
            vjust = -0.5, size = 3, color = "black")

print(cumWU_all_plot)



# Only percentage ---------------------------------------------------------

summary_data_cumWU <- summary_data_cumWU %>%
  group_by(Species) %>%
  mutate(relative_water_saving = (mean_value_cumWU[Treatment == "Control"] - mean_value_cumWU) / mean_value_cumWU[Treatment == "Control"] * 100)

# Generate cumWU_all_plot with p-value labels
cumWU_all_plot <- ggplot(summary_data_cumWU, aes(x = paste(Species, Genotype), y = mean_value_cumWU, 
                                                 color = Treatment)) +
  geom_point(size = 3, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_value_cumWU - sem_cumWU, 
                    ymax = mean_value_cumWU + sem_cumWU), width = 0.2) +
  labs(x = "Species - Genotype", y = "cumWU") +
  theme_bw() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(name = "cumWU",
                     limits = c(0, 18), breaks = seq(0, 18, by = 5)) +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative Water Use by Species and Treatment") +
  geom_text(data = subset(summary_data_cumWU, Treatment == "Drought"), aes(label = ifelse(p_value < 0.05, "*", "ns"), 
                                                                           y = mean_value_cumWU + sem_cumWU), 
            vjust = -0.5, size = 3, color = "black")+
  geom_text(data = subset(summary_data_cumWU, Treatment == "Drought"), 
            aes(label = paste0(round(relative_water_saving), "%"), y = mean_value_cumWU + sem_cumWU), 
            vjust = -0.5, size = 3, color = "black")


print(cumWU_all_plot)


# Cumulative Water USe ----------------------------------------------------

t_test_results <- cumWU %>%
  group_by(Species) %>%
  summarize(p_value = t.test(sumwu[Treatment == "Drought"], sumwu[Treatment == "Control"])$p.value)

# Merge t-test results with summary_data_cumWU
summary_data_cumWU <- summary_data_cumWU %>%
  left_join(t_test_results, by = "Species")

summary_data_cumWU <- summary_data_cumWU %>%
  group_by(Species) %>%
  mutate(relative_water_saving = (mean_value_cumWU[Treatment == "Control"] - mean_value_cumWU) / mean_value_cumWU[Treatment == "Control"] * 100)

write.csv(summary_data_cumWU, file = "data_output/biomass/summary_data_cumWU.csv", row.names = FALSE)


# Generate cumWU_all_plot with p-value and relative water saving labels
cumWU_all_plot <- ggplot(summary_data_cumWU, aes(x = paste(Species, Genotype), y = mean_value_cumWU, 
                                                 color = Treatment)) +
  geom_point(size = 4, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean_value_cumWU - sem_cumWU, 
                    ymax = mean_value_cumWU + sem_cumWU), width = 0.2) +
  labs(x = "") +
  theme_bw() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12)) +

  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(name = "Total Wateruse per plant (L)",
                     limits = c(0, 18), breaks = seq(0, 18, by = 2)) +
  theme(legend.position = "bottom") +
  ggtitle("Cumulative Wateruse by Species and Treatment") +
  geom_text(data = subset(summary_data_cumWU, Treatment == "Drought"), 
            aes(label = ifelse(p_value < 0.05, "*", "ns"), y = mean_value_cumWU + sem_cumWU), 
            vjust = -0.5, size = 4, color = "black") +
  geom_text(data = subset(summary_data_cumWU, Treatment == "Drought"), 
            aes(label = paste0(round(relative_water_saving), "%"), y = mean_value_cumWU + sem_cumWU), 
            vjust = -3, size = 4, color = "black")

print(cumWU_all_plot)


