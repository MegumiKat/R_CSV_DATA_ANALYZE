library(tidyverse)
library(ggplot2)
library(knitr)
library(rpart)
# Opening
d =read.csv ("/Users/mark/Desktop/course/staa57/R_CSV_DATA_ANALYZE/g0811_05.csv") 

# 1.Description
names(d)

# 4. Tables
# 4.1  The Top 5 Most Popular Reasons for immigration
d_top5=d %>% filter(Reason!="Part-time employment, all reasons" &Immig.=="Total"
                    &Age.group=="15+" &Geography=="Canada") %>% 
  select(Year,Reason,Both.sexes) %>%  group_by(Reason) %>%
  mutate(total_popu_over_years=sum(Both.sexes))

d_top5=d_top5 %>% filter(Year=="2016") %>% 
  select(Reason,total_popu_over_years) %>% arrange(desc(total_popu_over_years))

head(d_top5,n=5)

# 4.2 Total population of part-time job over Canada
d_table1=d %>% 
  filter(Reason=="Part-time employment, all reasons" &Immig.=="Total" &Age.group=="15+") 
%>% select(Year,Geography,Both.sexes,Men,Women) %>% arrange(Geography)

kable(d_table1)

# 4.3 The proportion of men and women who choose to work part-time because of "Caring for children"
d_ratio= d %>% filter(Geography=="Canada")

d_ratio=d_ratio %>% 
  filter(Reason=="  Caring for children" & Age.group=="15+" & Immig.=="Total" ) %>% 
  mutate( Ratio_men=Men/Both.sexes, Ratio_women=Women/Both.sexes) %>% select(Year,Ratio_men, Ratio_women) 

kable(d_ratio)

# 5. Graphs
# 5.1 Graph of population of par-time job men over years in different geography
ggplot()+geom_line(aes(x=Year,y=Men,col=Geography),data=d_table1) + ylab("population of part-time job men") + 
  ggtitle("Graph of population of par-time job men over years in different geography")

# 5.2 Graph of ratio of men working part-time due to children-caring over year
ggplot(aes(x=Year,y=Ratio_men),data=d_ratio)+geom_point()+
  ylab("ratio of men working part-time due to children-caring")+
  geom_smooth(method="lm")+ 
  ggtitle("Graph of ratio of men working part-time because of children-caring over year") 

# 6. Confidence interval and test of hypothesis
t.test(d_ratio %>% select(Ratio_men),alternative="less",mu=0.5,conf.level=0.95)

# 7. Calculate p-value of the Hypothesis in part 6 using Bootstrapping
mu_H0=0.5 # null Hypothesis: mean=0.5
ratio_men=d_ratio %>% select(Ratio_men)
obs_mean=mean(ratio_men[,1]) # mean of Ratio_men

new_x=ratio_men-obs_mean+mu_H0

boot_function2=function(){
  obs.sample=d_ratio %>% select(Ratio_men)
  boot.d = sample_n(new_x,size=nrow(d_ratio), replace=T)
  return(mean(boot.d[,1])) 
}

set.seed(1)
boot_new_x_bar = replicate(1000, boot_function2())

mean(boot_new_x_bar<obs_mean)

# 8 Regression analysis
# 8.1 random forest
d1=d %>% filter(Reason!="Part-time employment, all reasons")
d1=d1%>% mutate(reason=case_when(Reason=="  Own illness"~0,
                                 Reason=="  Caring for children"~0,
                                 Reason=="  Other personal or family responsibilities"~0,
                                 Reason=="  Going to school"~0,
                                 Reason=="  Personal preference"~0,
                                 Reason=="  Other voluntary"~0,
                                 Reason=="  Business conditions, did not look for full-time work in last month"~0,
                                 Reason=="  Could not find full-time work, did not look for full-time work in last month"~0,
                                 Reason=="  Business conditions, looked for full-time work in last month"~1,
                                 Reason=="  Could not find full-time work, looked for full-time work in last month"~1))

library(randomForest)

rforest.m = randomForest(as.factor(reason) ~ 
                           Year+Geography+Immig.+Age.group++Both.sexes+Men+Women,
                         data=d1,ntree=500,importance=TRUE)

varImpPlot(rforest.m)

d_randomforest=d1%>%mutate(rforest_pred=predict(rforest.m,new_data=d1))
matrix=table(d_randomforest$reason,d_randomforest$rforest_pred)
matrix
mean(sum(diag(matrix))/sum(matrix))

# 8.2 Linear regression
m2=lm(Both.sexes~Year,data=d_table1)
m2=lm(Both.sexes~Year,data=d_table1)

# 9. Cross validation
d_cross=d1 %>% mutate(group_ind = sample(c("train","test"),
                                         size=nrow(d1),
                                         prob = c(0.6,0.4), 
                                         replace = T))
rforest.cross = randomForest(as.factor(reason) ~
                               Year+Geography+Immig.+Age.group++Both.sexes+Men+Women,
                             data=d_cross %>% filter(group_ind=="train"),
                             ntree=500, 
                             importance=TRUE)

reason.hat=predict(rforest.cross)
matrix_train=table(d_cross$reason[d_cross$group_ind=="train"],reason.hat)
matrix_train
mean(sum(diag(matrix_train))/sum(matrix_train))

# for test data
reason.hat=predict(rforest.cross,newdata=d_cross %>% filter(group_ind=="test"))
matrix_test=table(d_cross$reason[d_cross$group_ind=="test"],reason.hat)
matrix_test
mean(sum(diag(matrix_test))/sum(matrix_test))

