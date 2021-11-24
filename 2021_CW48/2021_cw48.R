library(tidyverse)
library(lubridate)
library(tidytuesdayR)





tuesdata <- tidytuesdayR::tt_load('2021-11-23')
#saveRDS(tuesdata,file="2021-11-23")
#tuesdata <-readRDS(file="2021-11-23")
#dataset$writers
#dataset1$directors
#dataset1$episodes
#dataset1$imdb
dataset1 <- tuesdata$imdb



### get dat from imdb to check the rate in every year
### dont concider the episone or series, just average

tidyCW47Data <- dataset1 %>% 
  transmute("Episode"=ep_num, "SendDate"=dmy(air_date),"rate"=as.double(rating),"rateCount"=rating_n) %>%
  mutate("CW"=factor(isoweek(SendDate)), "Year"=factor(year(SendDate))) %>% 
  group_by(Year) %>%
  summarise("AverageRate"=mean(rate), "NumberRate"=sum(rateCount)) %>%
  ungroup() %>%
  mutate(Year=as.Date(str_c(Year,"-01-01"))) %>%
  pivot_longer(c("AverageRate","NumberRate"),names_to="rateType",values_to="rateValue") %>%
  mutate(rateType=factor(rateType))
  

# Define the labels for the graph
#highestRate <- tidyCW47Data %>% filter(rateType=="AverageRate") %>% filter(rateValue==max(rateValue))
#lowestNumber <- tidyCW47Data %>% filter(rateType=="NumberRate") %>% filter(rateValue==min(rateValue))

graphLabels <- data.frame(
  label = c("no show in 2009", "highest rate(8.36)", "Lowest number(2808)"),
  rateType = c("AverageRate", "AverageRate", "NumberRate"),
  x     = c(ymd("2009-01-01"), ymd("2008-01-01"), ymd("2021-01-01")),
  y     = c(6.8, 7.4, 80000)
)



tidyCW47Data %>%
  ggplot(aes(x=Year, y=rateValue)) + 
  facet_wrap("rateType",ncol=1, scales = "free_y") +
  geom_line() + 
  geom_point() +
  labs(title="Ratings per year",
       x ="Year", y="Ratings",
         subtitle = "This shows the average rate for the show \"Dr. Who\" per year. We see that in 2009 no
 episodes have been send. Since 2018 the rating decreased dramatically! The number of ratings
 decreased over time constantly!"
       ) +
  theme(
    plot.title =  element_text(size = rel(2.5), hjust = .5,  face = "bold", margin = margin(t = -2, b = 5)),
    plot.subtitle =  element_text(size = rel(1), hjust = 0, face = "bold.italic", margin = margin(b = 12)),
    text = element_text(color = "darkgray"),
    
    strip.text = element_text(size=12,color = "darkgreen"),
    
    axis.text.y =  element_text(color = "green",size = rel(1.0), hjust = .1, ),
    axis.title.y = element_text(  color = "darkgreen",face = "bold", size = rel(1.8)),

    axis.text.x =  element_text(angle = 90, hjust = 1,color = "green",size = rel(1.2)),
    axis.title.x = element_text(  color = "darkgreen",face = "bold", size = rel(1.8)),
    
    plot.background = element_rect(fill = "#411111", color = NA),
    panel.spacing.y  = unit(1, "cm"),
    plot.margin = margin(40,50,40,50))+
  geom_text(angle = 90,size = rel(3.2),data    = graphLabels,mapping = aes(x = x, y = y, label = label))

ggsave("doctor_who.png", width=14, height=12)













    
