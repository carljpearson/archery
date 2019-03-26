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
            distance=mean(distance_yards)) %>%
  mutate(full_lab=paste("1,",distance,"yards")
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
    mutate(full_lab=paste0(round,", ",distance_yards," yd")) %>%
    ggplot(aes(x=full_lab,y=real_score)) +
    stat_summary(aes(fill=End),fun.y = "mean", geom = "bar",position="dodge")+
    geom_point(aes(fill=End),
               position=position_jitterdodge(jitter.width = .1,seed=5))+
    theme_tufte(base_family="sans"
    ) +
    #theme(text = element_text(size=20)) +
    coord_cartesian(ylim=c(0,5)) +
    scale_fill_brewer(palette="BrBG") +
    labs(title="Accuracy per arrow over ends and rounds",
         x="Rounds",
         y="Score") 
  
  
  
  
  
  
  
  
  df_sum %>%
    mutate(distance=as.factor(distance)) %>%
    ggplot(aes(x=round,y=score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(aes(color=distance),position=position_jitter(height=0,width = .2)) +
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
    summarise(score=sum(official_score),n=n(),Distance=as.factor(mean(distance_yards))) %>%
    mutate(perfect=n*10) %>%
    mutate(percentage_score = score/perfect)%>%
    ggplot(aes(x=round,y=percentage_score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(aes(color=Distance),position=position_jitter(height=0,width = .2)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    coord_cartesian(ylim = c(0,1)) +
    scale_fill_brewer(palette="BrBG") +
    theme_tufte(base_family="sans") +
    labs(title="Offical score sum percentage compared to possible perfect score",
         subtitle = "Dots are end score",
         x="Rounds",
         y="Score") 
  
  
  #we need ends to even out
  df %>%
    group_by(round=as.factor(Round),end=as.factor(End_unordered)) %>%
    summarise(score=sum(official_score),n=n(),Distance=as.factor(mean(distance_yards))) %>%
    mutate(perfect=n*10) %>%
    mutate(percentage_score = score/perfect)%>%
    ggplot(aes(x=round,y=percentage_score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(aes(color=Distance),position=position_jitter(height=0,width = .2)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    coord_cartesian(ylim = c(0,1)) +
    scale_fill_brewer(palette="BrBG") +
    theme_tufte(base_family="sans") +
    labs(title="Offical score sum percentage compared to possible perfect score",
         subtitle = "Dots are end score",
         x="Rounds",
         y="Score") 
  
  key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/17qRcQmidYnkvlPqK7uTLuLpyzVyU0D7498WZnRXsPUI/edit?usp=sharing")
  
  key %>%
    gs_key() %>%
    gs_read('tracking_60cm') -> df60
  
data60 <- df60

  data60 %>%
  group_by(round=as.numeric(Round),end=as.factor(End_unordered)) %>%
    summarise(score=mean(real_score),
              sd=sd(real_score,na.rm=TRUE),
              range_max=max(real_score,na.rm=TRUE),
              range_min=min(real_score,na.rm=TRUE),
              distance=mean(distance_yards)
    ) -> df_sum60
  
  key %>%
    gs_key() %>%
    gs_read('grouping') -> df_group
  
  df_group %>% 
    na.omit(date )%>%
    select(round=Round,end=End,Distance,Distance_actual) %>%
    inner_join(.,df_sum60) -> df_group

  df_group %>%
    ggplot(aes(x=Distance_actual,score)) +
    geom_point() +
    geom_smooth(method='lm',formula=y~x) +
    labs(
      title="Score plotted against grouping",
      x="Outer point distances in CM",
      y="Point score"
    ) +
    theme_tufte(base_family="sans") 
  
  df_group %>%
    mutate(Distance_actual=as.numeric(Distance_actual)) %>%
    ggplot(aes(x=round,color=Distance_actual,size=Distance_actual,y=score)) +
    geom_point(position="jitter") +
    ggthemes::theme_tufte() +
    labs(title = "Accuracy and grouping over rounds")
  
  data60 %>%
    mutate(Round=factor(Round,levels=c(min(Round):max(Round)))) %>%
    mutate(End=as.factor(End_unordered)) %>%
    #filter(End %in% 3:4) %>%
    mutate(full_lab=paste0(round,", ",distance_yards," yd")) %>%
    ggplot(aes(x=full_lab,y=real_score)) +
    stat_summary(aes(fill=End),fun.y = "mean", geom = "bar",position="dodge")+
    geom_point(aes(fill=End),
               position=position_jitterdodge(jitter.width = .1,seed=5))+
    theme_tufte(base_family="sans"
    ) +
    #theme(text = element_text(size=20)) +
    coord_cartesian(ylim=c(0,5)) +
   # scale_fill_brewer(palette="BrBG") +
    labs(title="Accuracy per arrow over ends and rounds",
         subtitle = "60cm target",
         x="Rounds",
         y="Score") 
  
  
 
  data60 %>%
    mutate(Round=factor(Round,levels=c(min(Round):max(Round)))) %>%
    group_by(round=Round,end=as.factor(End_unordered)) %>%
    filter(end %in% 1:4) %>%
    summarise(score=sum(official_score),n=n(),Distance=as.factor(mean(distance_yards))) %>%
    mutate(perfect=n*10) %>%
    mutate(percentage_score = score/perfect)%>%
    ggplot(aes(x=round,y=percentage_score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(aes(color=Distance),position=position_jitter(height=0,width = .2),size=2) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    coord_cartesian(ylim = c(0,1)) +
    scale_color_brewer(palette="Set1") +
    theme_tufte(base_family="sans") +
    labs(title="Offical score sum percentage compared to possible perfect score",
         subtitle = "60cm target, dots are end score",
         x="Rounds",
         y="Score") 
  
  
  
  lmerTest::lmer(real_score ~ End_unordered + Round + (1|End_unordered), data = data60)
summary(lm(real_score ~ End_unordered + Round, data = data60))



psych::corr.test(df_group$Distance_actual,df_group$score)


