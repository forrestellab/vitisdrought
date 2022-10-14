accessions<-unique(swclong$Genotype) # unique() used to eliminate or delete the duplicate values/ rows present

for (i in accessions) {
  
  error.df <- swclong2 %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WC, na.rm = TRUE),
      se = (sd(WC)/sqrt(length(WC))), #   #how to calculate standard error? (sd(PSWC)/sqrt(length(PSWC)))
      len = mean(WC), #what does this part stand for?
      Treatment = options,
      Date = Date)
  
  
  PSWCplot<-swclong2%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =WC, x = as.Date(Date,"%m/%d"), color = options))+
    geom_point(aes(shape = options), size = 2.5)+ #use point instead of "line", how can I plot the mean and error
    # how to plot regression curve? and mean sqrt?
    stat_summary(fun="mean",geom="line",size = 1)+ # or just points: stat_summary(fun="mean",geom="point",size = 2, shape = 3)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, #is standard error here correct written?
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  linetype = "dotted",
                  data = distinct(error.df))+
    theme_classic()+
    scale_color_manual(values=c("red", "blue", "red", "blue"))+
    scale_shape_manual(values=c(1,1,4,4))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+  #is there a way to name it day x rather than the actual date?
    xlab(label = "Date")+
    ggtitle(paste("Time Series of Soil Water Content by Treatment", i)) 
  print(PSWCplot)
  #ggsave(paste0("fig_output/WC/WC",i, ".pdf"))
  #ggsave(paste0("fig_output/WC/WC",i, ".png"))
  
}