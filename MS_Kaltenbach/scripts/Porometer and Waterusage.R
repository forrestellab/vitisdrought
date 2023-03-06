# Combining Porometer Data and WaterUsage


# read file for subset 
swclong<- read.csv("data/Subset/sub_swc.csv")

swclong1<-swclong%>%
  select((c(ID, Genotype, Species, Treatment, Date, WU)))



# Subset File
gswclean <-read.csv("data/Subset/sub_gswc.csv")

# Clean Data --------------------------------------------------------------

gswclean<-gswclean%>%
  filter(!is.na(gsw_porometer))%>%
  filter(!(gsw_porometer>1))%>%
  filter(!is.na(species_geno))%>%
  mutate(Date = format(as.Date(as.character(Date),"%m.%d"),"%m/%d"))

# Average Data on Plant Level per day (combine reps) --------------------------------------------------------------

gswclean<-gswclean%>%
  group_by(Code_Date, ID, Date, Treatment, Genotype) %>%
  summarise_at(vars(gsw_porometer), mean)



# ifelse for Date ---------------------------------------------------------

gswclean$Date <- ifelse(gswclean$Date=="10/26"|gswclean$Date=="10/27","10/26-27",
                        ifelse(gswclean$Date=="11/16"|gswclean$Date=="11/17","11/16-17",
                               ifelse(gswclean$Date=="11/02"|gswclean$Date=="11/03", "11/02-03",
                                      ifelse(gswclean$Date=="11/23"|gswclean$Date=="11/24", "11/23-24",
                                             ifelse(gswclean$Date=="11/30"|gswclean$Date=="12/2","11/30-12/2",
                                                    gswclean$Date)))))


df <- merge(gswclean, swclong1, by= c("Genotype", "Treatment", "ID"))

# Combine SWC Plot --------------------------------------------------------

accessions<-unique(df$Genotype)
for (i in accessions) {
  
  error.gs <- df %>%
    filter(Genotype == i)%>%
    group_by(Date.x , Treatment) %>%
    summarise(
      sd = sd(gsw_porometer, na.rm = TRUE),
      se = (sd(gsw_porometer)/sqrt(length(gsw_porometer))),
      len = mean(gsw_porometer),
      Treatment = Treatment,
      Date = Date.y)
  
  
    
    GSW_plot<-df%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date.x, y = gsw_porometer, color = Treatment, group = Treatment)) +
    geom_point()+
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = Date.x, y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  #linetype = "solid",
                  data = distinct(error.gs))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    #scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
     ylab("Gs (mmol m−2 s−1)")+
    xlab(label = "Date")
    
  
  #print(GSW_plot)
 
   
  
##Water Use Graphs------------------------------------------------

error.wu <- df %>%
  filter(Genotype == i)%>%
  group_by(Date.y , Treatment) %>%
  summarise(
    sd = sd(WU, na.rm = TRUE),
    se = (sd(WU)/sqrt(length(WU))),
    len = mean(WU),
    Treatment = Treatment,
    Date = Date.y)


wuplot<-df%>%
  filter(Genotype == i)%>%
  ggplot(aes(y =WU, x = as.Date(Date.y ,"%m/%d"), color = Treatment))+
  geom_point()+
  stat_summary(fun="mean",geom="line",size = 1)+
  
  geom_errorbar(aes(x = as.Date(Date.y ,"%m/%d"), y = len,
                    ymin = len-se,
                    ymax = len+se),
                color = "black",
                position=position_dodge(width=0.5),
                size = .3,
                #linetype = "solid",
                data = distinct(error.wu))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  xlab(label = "Date")+
  scale_y_continuous(name = "WU (L)", limits=c(0.0, 2.1), breaks = seq(0.0, 2.1, by = 0.2))+ 
  theme(legend.position="none")+
  xlab(label = "Date") 

  #print(wuplot)
  
  
  
  ## Merge Porometer and WU Plot --------------------------------------------
  
  plot1 <-ggarrange(GSW_plot, wuplot, ncol=1, nrow=2)  #merge the 3 plots from above together
  
  combined <- annotate_figure(plot1, top = text_grob((paste("Porometer & WU  of", i)), color = "black", face = "italic", size = 11)) 
  
  print(combined)

  
  # Save Plot ---------------------------------------------------------------
  
  #path to save all files: 
  #ggsave(paste0("fig_output/Porometer/Porometer",i, ".png"))
  #ggsave(paste0("fig_output/Porometer/Porometer",i,".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset/GS_WU/GS_WU",i, ".png"))
  ggsave(paste0("fig_output/Subset/GS_WU/GS_WU",i, ".pdf"))
  
}


