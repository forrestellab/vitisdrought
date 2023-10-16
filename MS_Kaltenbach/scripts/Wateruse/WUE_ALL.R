library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(knitr)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

colnames(WUE)


WUE <- read.csv("data_output/Licor/summary_merged_df_licor.csv")

intrWUE_AE_plot <-ggplot(WUE, aes(x = Date, y = mean_value_intrWUE_AE, 
                                color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_errorbar(aes(ymin = mean_value_intrWUE_AE - sem_intrWUE_AE, 
                    ymax = mean_value_intrWUE_AE + sem_intrWUE_AE), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "intr WUE (A/E)") +
  theme_bw()

print(intrWUE_AE_plot)

# Save plot
ggsave("fig_output/WUE/intrWUE_AE_plot.png", intrWUE_AE_plot)
ggsave("fig_output/WUE/intrWUE_AE_plot.pdf", intrWUE_AE_plot)


intrWUE_AE_plot_sig  <- ggplot(WUE, aes(x = Date, 
                                               y = mean_value_intrWUE_AE, 
                                              color = Treatment, 
                                              group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_intrWUE_AE - sem_intrWUE_AE, 
                    ymax = mean_value_intrWUE_AE + sem_intrWUE_AE), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "intr WUE (A/E)") +
  theme_bw() +
  geom_text(aes(label = ifelse(p_value_intrWUE_AE < 0.05, "*", "ns"),
                x = Date, y = 300),
            size = 4,  color = "black")

print(intrWUE_AE_plot_sig)

# Save plot
ggsave("fig_output/WUE/intrWUE_AE_plot_sig.png", intrWUE_AE_plot_sig)
ggsave("fig_output/WUE/intrWUE_AE_plot_sig.pdf", intrWUE_AE_plot_sig)


instWUE_AGs_plot <-ggplot(WUE, aes(x = Date, y = mean_value_instWUE_AGs, 
                                  color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_errorbar(aes(ymin = mean_value_instWUE_AGs - sem_instWUE_AGs, 
                    ymax = mean_value_instWUE_AGs + sem_instWUE_AGs), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "instWUE (A/Gs)") +
  theme_bw()

print(instWUE_AGs_plot)

# Save plot
ggsave("fig_output/WUE/instWUE_AGs_plot.png", instWUE_AGs_plot)
ggsave("fig_output/WUE/instWUE_AGs_plot.pdf", instWUE_AGs_plot)


instWUE_AGs_plot_sig  <- ggplot(WUE, aes(x = Date, 
                                        y = mean_value_instWUE_AGs, 
                                        color = Treatment, 
                                        group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_instWUE_AGs - sem_instWUE_AGs, 
                    ymax = mean_value_instWUE_AGs + sem_instWUE_AGs), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "instWUE (A/Gs)") +
  theme_bw() +
  geom_text(aes(label = ifelse(p_value_instWUE_AGs < 0.05, "*", "ns"),
                x = Date, y = 15000),
            size = 4,  color = "black")

print(instWUE_AGs_plot_sig)


# Save plot
ggsave("fig_output/WUE/instWUE_AGs_plot_sig.png", instWUE_AGs_plot_sig)
ggsave("fig_output/WUE/instWUE_AGs_plot_sig.pdf", instWUE_AGs_plot_sig)

E_plot <-ggplot(WUE, aes(x = Date, y = mean_value_E, 
                                   color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_errorbar(aes(ymin = mean_value_E - sem_E, 
                    ymax = mean_value_E + sem_E), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "Transpiration E") +
  theme_bw()

print(E_plot)

# Save plot
ggsave("fig_output/Licor/E_plot.png", E_plot)
ggsave("fig_output/Licor/E_plot.pdf", E_plot)


E_plot_sig  <- ggplot(WUE, aes(x = Date, 
                                         y = mean_value_E, 
                                         color = Treatment, 
                                         group = Treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_errorbar(aes(ymin = mean_value_E - sem_E, 
                    ymax = mean_value_E + sem_E), 
                width = 0.2) +
  facet_wrap(~ Genotype) +
  labs(x = "Date", y = "Transpiration E") +
  theme_bw() +
  geom_text(aes(label = ifelse(p_value_E < 0.05, "*", "ns"),
                x = Date, y = 0.008),
            size = 4,  color = "black")

print(E_plot_sig)

# Save plot
ggsave("fig_output/Licor/E_plot_sig.png", E_plot_sig)
ggsave("fig_output/Licor/E_plot_sig.pdf", E_plot_sig)

