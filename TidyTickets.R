.libPaths("D:/R/Library")
library(tidyverse)
library(viridis)
library(qdap)

tickets <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv") %>% 
  separate(issue_datetime, c("issue_date","time"), sep="T") %>% 
  separate(issue_date, c("year","month", "day"))

cols.num <- c("year","month", "day")
tickets[cols.num] <- sapply(tickets[cols.num],as.numeric)

violations <- tickets %>% 
  group_by(month,day,time) %>% 
  summarise(number=n()) %>% 
  arrange(month,day)

violations$month <- month.abb[violations$month]
violations$month <- factor(violations$month, levels = unique(violations$month))
violations$time <- gsub(".{4}$","", violations$time)


ggplot(violations,aes(day,time,fill=number))+
  geom_tile() + 
  scale_fill_viridis(name="Number of violations",option ="C")+
  facet_grid(.~month)+ 
  theme(plot.title=element_text(size = 17))+
  theme(axis.text.y=element_text(size=10)) +
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks.x=element_blank())+
  theme(axis.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=12) )+
  theme_minimal(base_size = 12)+
  labs(title= "Tickets handed out in Philadelphia over 2017", x="Day", y="Time")+
  scale_y_discrete(breaks =c("00:00", "01:00", "02:00","03:00","04:00", "05:00","06:00",
                             "07:00", "08:00", "09:00", "10:00", "11:00",
                             "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00",
                             "19:00", "20:00", "21:00", "22:00", "23:00"))


