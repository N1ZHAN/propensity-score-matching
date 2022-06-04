hn <- read.csv("~/Documents/HighNote Data.csv")
View(hn)
library(ggplot2)
library(dplyr)
library(stargazer)
library(tidyverse)
sum(is.na(hn))

# Summary Statistics
premium <- subset(hn,adopter==1)
free <- subset(hn,adopter==0)
stargazer(premium[,2:16],type="text",title="Adopter Summary",median=TRUE,iqr=TRUE,digits=2)
stargazer(free[,2:16],type="text",title="Non-Adopter Summary",median=TRUE,iqr=TRUE,digits=2)

hn_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male','friend_country_cnt',
         'subscriber_friend_cnt', 'songsListened','lovedTracks', 'posts', 'playlists','shouts',
         'tenure','good_country')
hn %>% group_by(adopter) %>% summarise_all(funs(mean)) %>% select(one_of(hn_cov))
lapply(hn_cov, function(v) {t.test(hn[, v] ~ hn$adopter)})

# Data Visualization
# i) Demographics
hn %>% ggplot(aes(x=factor(good_country),fill=factor(good_country)))+geom_bar()+
  scale_x_discrete(labels = c("Other Countries","US,UK,Germany"))+
  scale_fill_manual(values = c("#7a0019", "#00759a"))+ labs(x="Country",y="Number of Users")+
  theme(legend.position="none",axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())+facet_wrap(~ adopter)

hn %>% ggplot(aes(x=factor(male),fill=factor(male)))+geom_bar()+
  scale_x_discrete(labels = c("Female","Male"))+
  scale_fill_manual(values = c("#7a0019", "#00759a"))+ labs(x="Gender",y="Number of Users")+
  theme(legend.position="none",axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())+facet_wrap(~ adopter)

hn %>% ggplot(aes(x=age))+geom_density()+labs(x="Age",y="Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

# ii) Peer Influence
hn %>% ggplot(aes(subscriber_friend_cnt,friend_cnt))+geom_point(color="#00759a",alpha = 0.2)+
  labs(x="Number of Premium Member Friends",y="Number of Total Friends")+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())+facet_wrap(~ adopter)

hn %>% ggplot(aes(x = avg_friend_age))+geom_density(fill = "#00759a", color = "#ffffff")+
  labs(x = "Average Age of the Friends", y = "Density") +xlim(0,60)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())+facet_wrap(~ adopter)

hn %>% ggplot(aes(x = avg_friend_male)) + geom_density(fill = "grey70", color = "#ffffff") +
  geom_histogram(aes(y=..density..),bins = 20, color = "#00759a", alpha = 0.1) + 
  labs(x = "Average Proportion of Male Friends", y = "Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
      panel.background=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major.y=element_blank(),
      panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=friend_country_cnt,fill=factor(adopter)))+
  geom_density(color = "white", alpha = 0.9)+scale_fill_manual(values=c("#00759a", "grey80"))+
  labs(x="Friends' Country of Origin",y="Density",fill="Adopter")+
  theme(axis.line = element_line(color="black"),panel.background=element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

# iii) User Engagement
hn %>% ggplot(aes(x =factor(adopter), y=songsListened))+geom_boxplot(alpha=0.1)+
  labs(x="Adopter",y = "Songs Listened") + stat_summary(fun.y=mean, geom="point", color="red", fill="red")+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=lovedTracks))+geom_density()+labs(x="Loved Tracks",y="Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=posts))+geom_density()+labs(x="Posts",y="Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=playlists))+geom_density()+labs(x="Playlists",y="Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=shouts))+geom_density()+labs(x="Shouts",y="Density")+facet_wrap(~ adopter)+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

hn %>% ggplot(aes(x=tenure,fill=factor(adopter)))+
  geom_density(color = "white", alpha = 0.8)+scale_fill_manual(values=c("#00759a", "grey80"))+
  labs(x="Tenure",y="Density",fill="Adopter")+
  theme(axis.line = element_line(color="black"),panel.background=element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

# Propensity Score Matching
hn$ynsf = ifelse(hn$subscriber_friend_cnt >= 1, 1, 0)
t.test(hn$adopter~hn$ynsf)
hn_cov2 <-c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt',
             'songsListened', 'lovedTracks', 'posts', 'playlists','shouts', 'tenure', 'good_country')
hn %>%group_by(ynsf)%>%summarise_all(funs(mean))%>%select(one_of(hn_cov2))
lapply(hn_cov2, function(v) {t.test(hn[,v] ~ hn[,'ynsf'])})

hn <- hn %>% mutate(songsListened_1k = songsListened/1000)
m_ps <- glm(ynsf ~ age + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt+
              songsListened_1k + lovedTracks + posts + playlists+ shouts + tenure +
              good_country, family = binomial(), data = hn)
summary(m_ps)

prs_df <-data.frame(pr_score = predict(m_ps, type = "response"),ynsf = m_ps$model$ynsf)
head(prs_df)
head(m_ps$model)

prs_df %>%mutate(ynsf = recode(ynsf,"0" = "No Fee Friends","1" = "Have Fee Friends"))%>%
  ggplot(aes(x = pr_score)) +geom_histogram(color="darkblue",fill = "lightblue") +
  facet_wrap(~ynsf) +labs(x="Probability of Having Fee Friends",y="Count")+
  theme(axis.line = element_line(color="black"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank())

library(MatchIt)
hn_nomiss <- hn %>%select(adopter, ynsf, one_of(hn_cov2)) %>%na.omit()
mod_match <- matchit(ynsf ~ age + friend_cnt + avg_friend_age + avg_friend_male+
                       friend_country_cnt+ songsListened + lovedTracks + posts +
                       playlists+ shouts + tenure + good_country,method="nearest",
                     data = hn_nomiss)
summary(mod_match)
plot(mod_match)
plot(mod_match,type="hist")

dta_m <- match.data(mod_match)
dim(dta_m)

dta_m %>%group_by(ynsf) %>%select(one_of(hn_cov2)) %>%summarise_all(funs(mean))
lapply(hn_cov2, function(v) {t.test(dta_m[, v] ~ dta_m$ynsf)})

t.test(dta_m$adopter~dta_m$ynsf)

treat1 <- glm(adopter ~ ynsf, family = binomial(), data = dta_m)
summary(treat1)
treat2 <- glm(adopter ~ ynsf + age + friend_cnt + avg_friend_age +
                    avg_friend_male +friend_country_cnt+ lovedTracks +
                    posts + playlists+ shouts + tenure + good_country+
                    I(songsListened / 1000), family = binomial(), data = dta_m)
summary(treat2)

# Regression Analysis
cor <- cor(hn)
library(corrplot)
corrplot(cor,method="square",order="alphabet")
library(car)
fit1 <- glm(adopter ~ age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt
                + subscriber_friend_cnt + lovedTracks + posts + playlists + songsListened_1k
                + shouts + tenure + good_country, family = binomial(), data = hn)
summary(fit1)
car::vif(fit1)
vif(fit1)> 4
outlierTest(fit1)

fit2 <- glm(adopter ~ age + male+subscriber_friend_cnt+lovedTracks+
              songsListened_1k+playlists+tenure+good_country, family = binomial(), data = hn)
summary(fit2)
vif(fit2)
vif(fit2)>4
outlierTest(fit2)

new_hn <- hn[c(-32663,-21293,-10623),]
fit3 <- glm(adopter ~ age + male+subscriber_friend_cnt+lovedTracks+
              songsListened_1k+playlists+tenure+good_country, family = binomial(), data = new_hn)
summary(fit3)
coef(fit3)
exp(coef(fit3))
