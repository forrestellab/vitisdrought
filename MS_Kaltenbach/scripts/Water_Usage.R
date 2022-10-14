
library(ggplot2)


timeseries<-read.csv("data/TimeSeries.csv")

str(timeseries)

## Soil Moisture Content Figure by Day of treatment

smcday<-data.frame(timeseries[4], stack(as.data.frame(timeseries[2:3])))

str(smcday)

names(smcday)[1] <- "Day"
names(smcday)[2] <- "SMC"
names(smcday)[3] <- "Treatment"

ggplot(smcday, aes(x= Day,y = SMC, color= Treatment)) + geom_line()

p1day<- ggplot(smcday, aes(x= Day,y = SMC, color= Treatment)) + geom_line()

p1day<- p1day + ggtitle ("Time Series of Soil Moisture Content by Treatment")


p1day <- p1day +
  theme(axis.text.x= element_text(color = "black", size=15),
        axis.text.y= element_text(color = "black", size=15),
        panel.background = element_rect(fill = "white"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        legend.position = "bottom", 
        plot.title = element_text(lineheight = .8, face = "bold")
        )

p1day <- p1day + labs(x="Day", y= "Soil Moisture Content")

p1day

## Soil Moisture Content Figure by Day of Treatment

smcdate<-data.frame(timeseries[1], stack(as.data.frame(timeseries[2:3])))

names(smcdate)[1] <- "D"
names(smcdate)[2] <- "SMC"
names(smcdate)[3] <- "Treatment"

p1date<- ggplot(smcdate, aes(x= D,y = SMC, color= Treatment)) + geom_line()

p1date<- p1date + ggtitle ("Time Series of Soil Moisture Content by Treatment")

p1date <- p1date +
  theme(axis.text.x= element_text(color = "black", size=15),
        axis.text.y= element_text(color = "black", size=15),
        panel.background = element_rect(fill = "white"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        legend.position = "bottom", 
        plot.title = element_text(lineheight = .8, face = "bold")
  )

p1date <- p1date + labs(x="Date", y= "Soil Moisture Content")

p1date

