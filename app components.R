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


df %>%
  ggplot(aes(x=round,
             y=real_score,
             fill=end)) +
  geom_point(stat="identity",
           position="jitter") 
  theme_tufte(base_family="sans") +
  coord_cartesian(ylim=c(0,5)) +
  scale_fill_brewer(palette="BrBG") 

#current end
  df %>%
    mutate(round=as.factor(Round)) %>%
    mutate(End=as.factor(End_unordered)) %>%
    ggplot(aes(x=round,y=real_score)) +
    geom_boxplot(aes(fill=End)) +
    geom_point(aes(fill=End),position=position_jitterdodge(jitter.width = .3,seed=5))+
     theme_tufte(base_family="sans") +
    coord_cartesian(ylim=c(0,5)) +
    scale_fill_brewer(palette="BrBG") +
    labs(title="Accuracy per arrow over ends and rounds",
         x="Rounds",
         y="Score")
  
  df %>%
    mutate(round=as.factor(Round)) %>%
    mutate(End=as.factor(End_unordered)) %>%
    ggplot(aes(x=round,y=real_score)) +
    geom_boxplot(aes(fill=End)) +
    geom_point(aes(fill=End),position=position_jitterdodge(jitter.width = .3,seed=5))+
    theme_tufte(base_family="sans") +
    coord_cartesian(ylim=c(0,5)) +
    scale_fill_brewer(palette="BrBG") +
    labs(title="Accuracy per arrow over ends and rounds",
         x="Rounds",
         y="Score")
  
  
  df_round %>%
    ggplot(aes(y=score,x=round)) +
    geom_line()

  
  df_sum %>%
   # mutate(round=as.factor(round)) %>%
    ggplot(aes(x=as.factor(round),y=score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(position=position_jitter(width = .2)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    theme_tufte(base_family="sans") +
    coord_cartesian(ylim=c(0,5)) +
    scale_fill_brewer(palette="BrBG") +
    labs(title="Average round accuracy",
         subtitle = "Dots are end accuracy",
         x="Rounds",
         y="Score")
  
  
  #we need ends to even out
  df %>%
    group_by(round=as.factor(Round),end=as.factor(End_unordered)) %>%
    summarise(score=sum(official_score),n=n()) %>%
    mutate(perfect=n*10) %>%
    mutate(percentage_score = score/perfect)%>%
    ggplot(aes(x=round,y=percentage_score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(position=position_jitter(height=0,width = .2)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    coord_cartesian(ylim = c(0,1)) +
   # scale_y_continuous(labels=percent) +
    theme_tufte(base_family="sans") +
    labs(title="Offical score sum percentage compared to possible perfect score",
         subtitle = "Dots are end score",
         x="Rounds",
         y="Score") 
  
    


