# -------  Plotting WP
genosWP<-unique(joined_WP_long$species_geno)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")

for (i in genosWP) {

    new_plot<-joined_WP_long%>%
    filter(species_geno == i)%>%
      ggplot(aes(x = Date, Y = WP, fill = Treatment))+
      geom_boxplot(aes(y =-WP))+
      #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
      theme_classic()+
      #xlab(c("11/9-11","11/16-17","12/01-02","12/11-18"))+
      #xlab(NULL)+
      theme(legend.position="none") +
      facet_grid(facets = WPType ~ .)+
      ggtitle(i)
    print(new_plot)

}

for (i in genosWP) {
  
  # Plotting WP
  LWP_plot<-joined_WP_long%>%
    filter(species_geno == i) %>%
    filter(WPType == "LWP" ) %>% 
    ggplot(aes(x = Date, Y = WP, fill = Treatment))+ 
    geom_boxplot(aes(y =-WP))+
    theme_classic()+
    #scale_x_discrete(limits = order1)+
    #scale_y_continuous(name = "WP (in bar)", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    xlab(NULL)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position="none")+
    ggtitle(i)
  #print(LWP_plot)
  
  #library(foreach)
  #foreach(i = genosLWP, j = genosSWP, k =genosPD) %do% {
  
  for (i in genosLWP, j in genosSWP, k in genosPD ) {
    
    # Plotting LWP
    LWP_plot<-LWP_data%>%
      filter(species_geno == i)%>%
      ggplot(aes(x = Date, Y = is.na(LWP), fill = Treatment))+ #does the is.na function help here?
      geom_boxplot(aes(y =-LWP))+
      theme_classic()+
      scale_x_discrete(limits = order1)+
      scale_y_continuous(name = "WP (in bar)", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
      xlab(NULL)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="none")
    #print(LWP_plot)
    
    
    
    ##--- Plotting Stem Water Potential##
    
    
    
    SWP_plot<-SWP_data%>%
      filter(species_geno == j)%>%
      ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
      geom_boxplot(aes(y =-SWP))+
      scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
      theme_classic()+
      xlab(NULL)+
      theme(legend.position="none")
    #print(SWP_plot)
    
    
    ##Plotting Predawn Water Potential## 
    
    
    PD_plot<-PD_data%>%
      filter(species_geno == k)%>%
      ggplot(aes(x = Date, Y = PD, fill = Treatment))+
      geom_boxplot(aes(y =-PD))+
      theme_classic()+
      scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
      xlab(NULL)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="none")
    
    #print(PD_plot)
    
    ###--Merging the PSWC, POSWC and WU plots 
    
    plot1 <-ggarrange(LWP_plot, NULL, SWP_plot, NULL, PD_plot, ncol=5, nrow=1, widths =  c(3, 0.00001, 3, 0.00001, 3), common.legend = TRUE, legend="bottom",align = "v", labels = "LWP", "SWP", "PD") #merge the 3 plots from above together
    
    
    combined <- annotate_figure(plot1, top = text_grob((paste("SWC & WU  of", i)), color = "black", face = "italic", size = 11)) #add title as genotype
    
    print(combined)
    #path to save all files:
    #ggsave(paste0("fig_output/WP/PD/PD",i, ".pdf"))
    #ggsave(paste0("fig_output/WP/PD/PD",i, ".png"))
    
    #path to save subset files: 
    #ggsave(paste0("fig_output/WP/Subset/PD/PD",i, ".png"))
    #ggsave(paste0("fig_output/WP/Subset/PD/PD",i, ".pdf"))
    
    
  }
  
  