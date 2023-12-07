
library(lubridate)
library(tidyverse)
library(stringr)
library(readxl)
library(hrbrthemes)
library(viridis)

# # Set Working Directory --------------------------------------------------------------

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

WP <- read.csv("data_output/WP/WP_combined.csv")


WP$LWP <- WP$LWP * -0.1
WP$PD <- WP$PD * -0.1
WP$Date <- as.Date(WP$Date_group, format = "%m/%d")
WP$Day <- as.numeric(difftime(WP$Date, min(WP$Date) - 2, units = "days"))
symnum.args <- list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))



species_order <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")
WP <- WP %>%
  mutate(species_geno = factor(species_geno, levels = species_order))


WP_LWP <- WP %>%
  select(Genotype, Species, Treatment, LWP, Day, species_geno) %>%
  filter(!is.na(LWP), Day != 2) %>%
  group_by(Day, Treatment, species_geno) %>%
  summarize(mean_LWP = mean(LWP),
            sem_LWP = sd(LWP) / sqrt(n()))

  
WP_PD <- WP %>%
  select(Genotype, Species, Treatment, PD, Day, species_geno)%>%
  filter(!is.na(PD))%>% 
  group_by(Day, Treatment, species_geno)%>% 
  summarize(mean_PD = mean(PD),
            sem_PD = sd(PD) / sqrt(n()))

# LWP --------------------------------------------------------------------

LWP_species_plot <- ggplot(WP_LWP, aes(x = Day, y = mean_LWP, color = Treatment)) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(aes(group = Treatment), linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_LWP - sem_LWP, ymax = mean_LWP + sem_LWP), width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 14, 19, 34, 49), labels = c("0", "14", "19", "34", "49")) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(
    name = bquote(paste(psi[MD], phantom(M), "(MPa", ")"))
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0)
  ) +
  facet_wrap(~ species_geno, ncol=5, scales = "free_x",
             labeller = labeller(species_geno = c("9018" = "a) V. acerifolia 9018", "T52" = "b) V. aestivalis T52", "b40-14" = "c) V. arizonica b40-14", "b42-34" = "d) V. cinerea b42-34", "T48" = "e) V. mustangensis T48","NY1" = "f) V. riparia NY1", "TXNM0821" = "g) hybrid TXNM0821", "Vru42" = "h) V. rupestris Vru42" , "V60-96" = "i) V. vulpina V60-96"  )))

print(LWP_species_plot)


# PD ----------------------------------------------------------------------



PD_species_plot <- ggplot(WP_PD, aes(x = Day, y = mean_PD, color = Treatment)) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_line(aes(group = Treatment), linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_PD - sem_PD, ymax = mean_PD + sem_PD), width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 14, 19, 34, 49), labels = c("0", "14", "19", "34", "49")) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  scale_y_continuous(
    name = bquote(paste(psi[PD], phantom(M), "(MPa", ")"))
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0)
  ) +
  facet_wrap(~ species_geno, ncol=5, scales = "free_x",
             labeller = labeller(species_geno = c("9018" = "a) V. acerifolia 9018", "T52" = "b) V. aestivalis T52", "b40-14" = "c) V. arizonica b40-14", "b42-34" = "d) V. cinerea b42-34", "T48" = "e) V. mustangensis T48","NY1" = "f) V. riparia NY1", "TXNM0821" = "g) hybrid TXNM0821", "Vru42" = "h) V. rupestris Vru42" , "V60-96" = "i) V. vulpina V60-96"  )))

print(PD_species_plot)
