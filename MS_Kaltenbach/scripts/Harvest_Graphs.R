
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Harvest Graphs####

harvest<-read_csv("data/2020_GHDD_Harvest_Data.csv")



View(harvest)