###Water Use Graphs------------------------------------------------

# read file
swclong<-read.csv("data/WaterUse_SWC_Clean.csv")

accessions<-unique(swclong$Genotype)

for (i in accessions) {
  
  error.df <- swclong %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WU, na.rm = TRUE),
      se = (sd(WU)/sqrt(length(WU))), #   #how to calculate standard error? (sd(WU)/sqrt(length(WU)))
      len = mean(WU),
      Treatment = Treatment,
      Date = Date)
  
    
  wuplot<-swclong%>%
    filter(Genotype == i)%>%
    ggplot(aes(y =WU, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+
    stat_summary(fun="mean",geom="line",size = 1)+
    #stat_smooth(geom="line",size = 1.5)+
    
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  #linetype = "dotted",
                  data = distinct(error.df))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    ggtitle(i) # how to add "Time Series of Water Use by Treatment" and genotype? 
  print(wuplot)
}
