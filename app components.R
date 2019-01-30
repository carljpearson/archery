library(tidyverse)
library(lmerTest)
library(googlesheets)
library(ggthemes)


connect_to_shiny
rsconnect::deployApp('/Users/cjpearson/Documents/school/r_github/archery/archery-metrics',account="carljpearson")
#read data



key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/17qRcQmidYnkvlPqK7uTLuLpyzVyU0D7498WZnRXsPUI/edit?usp=sharing")

key %>%
  gs_key() %>%
  gs_read('tracking') -> df

df %>%
  group_by(round=as.numeric(Round),end=as.factor(End_unordered)) %>%
  summarise(score=mean(real_score),
            sd=sd(real_score,na.rm=TRUE),
            range_max=max(real_score,na.rm=TRUE),
            range_min=min(real_score,na.rm=TRUE),
            distance=mean(distance_yards)
  ) -> df_sum

df %>% 
  group_by(round=Round) %>%
  summarise(score=mean(real_score),
            sd=sd(real_score,na.rm=TRUE),
            range_max=max(real_score,na.rm=TRUE),
            range_min=min(real_score,na.rm=TRUE),
            distance=mean(distance_yards)
  ) -> df_round

df_sum %>%
  ggplot(aes(x=round,
             y=score,
             fill=end)) +
  geom_bar(stat="identity",
           position="dodge") +
  geom_errorbar(aes(ymin=ifelse(sd>0,0,score-sd),
                    ymax=ifelse(sd>5,5,score+sd)),
                color="black",
                position="dodge",
                alpha=.3) +
  theme_tufte(base_family="sans") +
  coord_cartesian(ylim=c(0,5)) +
  scale_fill_brewer(palette="BrBG") 







