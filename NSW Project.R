#Loading all required library

library(tidyverse)
library(readxl)
library(plotly)


#Load our dataset from pre-determind working directory

df <- read_excel("Data Analysis - Data Sheets.xlsx", sheet = "PT & FT Data PivotTable format")
View(df)
glimpse(df)


#Pre-analysis Check

attach(df)
names(df)
df$Type<- df$`PT/FT`
length(unique(Agency))
any(is.character(df))
table(df$Cluster)
table(df$Type)

df %>% 
  group_by(Cluster,Year) %>% 
  summarise(Avg.Headcount= mean(Headcount), Max.Headcount= max(Headcount), Min.Headcount= min(Headcount))


#Anaysis Part

Trend<- df %>%
  select(Year,Gender, Headcount) %>%
  group_by(Year, Gender) %>%
  summarise(Total_headcount= sum(Headcount))

ggplotly(ggplot(Trend)+
           geom_bar(mapping = aes(Year, Total_headcount, fill=Gender), stat = "Identity",
                    position = "dodge")+
           labs(title = "Total headcount by Year"))
ggplot(Trend)+
  geom_smooth(aes(Year,Total_headcount, color= Gender))



df2<- df %>%
  filter( Type == "Part-Time")
Clus<- df2 %>%
  group_by(Cluster) %>%
  summarise(Total_headcount= sum(Headcount))
Clus_18<- df2 %>%
  filter(Year==2018) %>% 
  group_by(Cluster) %>%
  summarise(Total_headcount= sum(Headcount))


ggplot(Clus_18)+
  geom_bar(mapping = aes(Cluster, Total_headcount),stat="Identity",fill="blue")+
  coord_flip()+
  labs(title = "Total headcount among the clusters of 2018")



ggplot(Clus)+
  geom_bar(mapping = aes(Cluster, Total_headcount),stat="Identity",fill="blue")+
  coord_flip()+
  labs(title = "Total headcount among the clusters of 4 years")



df2 %>% 
  count(Cluster, name = "count") %>% 
  arrange(desc(count)) %>% 
  ggplot()+
  geom_bar(aes(Cluster,count), stat = "identity", fill="darkblue")+
  coord_flip()

df2 %>% 
  group_by(Cluster) %>% 
  summarise(Avg_Headcount = mean(Headcount)) %>% 
  ggplot()+
  geom_bar(aes(Cluster, Avg_Headcount), stat = "identity", fill="darkblue")+
  coord_flip()+
  labs(title = "Average headcount among the clusters of 4 years")



Prop<- df2 %>%
  group_by(Cluster)%>%
  mutate(Prop = Headcount/sum(Headcount)) %>%
  ungroup()

df2 %>%
  group_by(Cluster)%>%
  mutate(Prop = Headcount/sum(Headcount)) %>%
  ungroup() %>%
  ggplot(aes(Year, Prop, color=Gender))+
  geom_smooth(se=F)+
  facet_wrap(~Cluster)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Changes of proportions over the years among cluster and gender of Part time workers")

df2 %>%
  ggplot(aes(Year, Headcount, color=Gender))+
  geom_smooth(se=F)+
  facet_wrap(~Cluster)+
  labs(title = "Changes of headcounts over the years among clusters and gender of Part time workers")


#Projecting Number of headcount in 2025

df-> df_mod
df_mod$Cluster<- factor(df_mod$Cluster)
df_mod$Gender<- factor(df_mod$Gender)
df_mod$Type<- factor(df_mod$Type)
df_mod<- df_mod %>% 
  select(-Agency, -'PT/FT')

set.seed(1234)

library(caTools)
sample<- sample.split(df_mod$Headcount, SplitRatio = 0.8)
test<- subset(df_mod, sample == TRUE)
train<- subset(df_mod, sample == FALSE)

model<- lm(Headcount~., data = test)
summary(model)

predict<- predict(model, train)


 df_mod$Pred.Headcount <- predict(model,data.frame(Year=2025,Type=df_mod$Type, Gender=df_mod$Gender, Cluster=df_mod$Cluster))

 #Different insights of 2025

df_mod %>% 
  filter(Type=="Full-Time") %>% 
  ggplot()+
  geom_bar(aes(Cluster, Pred.Headcount, fill=Gender), stat = "identity", position = "dodge")+
  coord_flip()+
  labs(title = "Number of headcount in 2025 of Full-time workers ",
       y="Headcount")

df_mod %>% 
  filter(Type=="Part-Time") %>% 
  ggplot()+
  geom_bar(aes(Cluster, Pred.Headcount, fill=Gender), stat = "identity", position = "dodge")+
  coord_flip()+
  labs(title = "Number of headcount in 2025 of Part-Time workers ",
       y="Headcount")
#after neglecting negetive headcount
df_mod %>% 
  mutate(Pred.Headcount= if_else(Pred.Headcount<0, 0, Pred.Headcount)) %>% 
  filter(Type=="Part-Time") %>% 
  ggplot()+
  geom_bar(aes(Cluster, Pred.Headcount, fill=Gender), stat = "identity", position = "dodge")+
  coord_flip()+
  labs(title = "Number of headcount in 2025 of Part-Time workers ",
       y="Headcount")


