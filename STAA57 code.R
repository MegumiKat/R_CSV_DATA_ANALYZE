# Read the CSV file located at the specified path into a data frame named 'd'
d =read.csv ("~/Desktop/part_time.csv") 
# Load the 'tidyverse' package, which includes a collection of R packages for data manipulation and visualization
library(tidyverse)
# Load the 'ggplot2' package, a popular R package for creating graphics and plots
library(ggplot2)
# Load the 'knitr' package, which provides tools for dynamic report generation in R
library(knitr)
# Load the 'rpart' package, which provides functions for recursive partitioning and regression trees
library(rpart)

#part1 discription
# Display the column names of the data frame 'd'
names(d)

#4.1 table for The top 5 reasons why Canadians choose to work part-time
d_top5=d %>% 
  filter(Reason!="Part-time employment, all reasons" &Immig.=="Total" 
         &Age.group=="15+" &Geography=="Canada") %>% 
  select(Year,Reason,Both.sexes) %>%  group_by(Reason) %>%
  mutate(total_popu_over_years=sum(Both.sexes))
d_top5=d_top5 %>% filter(Year=="2016") %>% 
  select(Reason,total_popu_over_years) %>% arrange(desc(total_popu_over_years))
kable(d_top5[1:5,])


#4.2 Total population of part-time job over Canada
# Create a new data frame 'd_table1' by filtering the data frame 'd' to select 
#rows where:
# - Reason is "Part-time employment, all reasons"
# - Immig. is "Total"
# - Age.group is "15+"
# Then, select only the columns 'Year', 'Geography', 'Both.sexes', 'Men', and 'Women' from the filtered data frame.
# Arrange the resulting data frame by the 'Geography' column.
d_table1=d %>%
  filter(Reason=="Part-time employment, all reasons" &Immig.=="Total" &Age.group=="15+") %>% select(Year,Geography,Both.sexes,Men,Women) %>% arrange(Geography)
kable(d_table1)


#4.3 The proportion of men and women who choose to work part-time because of "Caring for children" in Canada

# Filter the data frame 'd' to select rows where 'Geography' is "Canada"
d_ratio= d %>% filter(Geography=="Canada") 

# Further filter the 'd_ratio' data frame to select rows where:
# - 'Reason' is "Caring for children"
# - 'Age.group' is "15+"
# - 'Immig.' is "Total"
# Then, calculate the ratio of 'Men' to 'Both.sexes' and create a new column 
# named 'Ratio_men'，same step for Ratio_women.
# Finally, select the 'Year' and 'Ratio_men','Ratio_women columns.
d_ratio=d_ratio %>%
  filter(Reason=="  Caring for children" & Age.group=="15+" & Immig.=="Total" ) %>% 
  mutate( Ratio_men=Men/Both.sexes, Ratio_women=Women/Both.sexes) %>% select(Year,Ratio_men, Ratio_women) 

# Display the data in 'd_ratio' as a nicely formatted table using the 'kable' function
kable(d_ratio)



# 5.1 Graph of population of par-time job men over years in different geography
# Create a ggplot object and add a line plot using the 'geom_line' function.
# Map the 'Year' variable to the x-axis, 'Men' variable to the y-axis, and 'Geography' variable to the color aesthetic.
# Use data from the 'd_table1' data frame.
# Label the y-axis as "population of part-time job men in different geography"
# Set the title of the plot as "Graph of population of par-time job men over years"
ggplot()+geom_line(aes(x=Year,y=Men,col=Geography),data=d_table1)+
  ylab("population of part-time job men")+
  ggtitle("Graph of population of par-time job men over years in different geography")



# 5.2 Graph of ratio of men working part-time due to children-caring over year

# Plot a scatter plot using ggplot, with 'Year' on the x-axis and 'ratio' on the y-axis, based on the data in the 'd_ratio' dataframe.
# Add points to the plot to represent the data points.
# Add a label to the y-axis indicating that it represents the 'ratio'.
# Add a linear regression line to the plot using the geom_smooth function with method="lm" (linear model).
# Add a title to the plot indicating that it represents the graph of the ratio of men working part-time because of children-caring over years.
ggplot(aes(x=Year,y=Ratio_men),data=d_ratio)+geom_point()+
  ylab("ratio of men working part-time due to children-caring")+
  geom_smooth(method="lm")+ 
  ggtitle("Graph of ratio of men working part-time because of children-caring over year") 




# 6. Confidence interval and test of hypothesis
# Perform a one-sample t-test on the 'Ratio_men' variable from the 'd_ratio' data frame.
# Use a confidence level of 95%.
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

# Perform bootstrapping by calling 'boot_function2' 1000 times and store the output in 'output'.
set.seed(1)
boot_new_x_bar = replicate(1000, boot_function2())

mean(boot_new_x_bar<obs_mean)


# 8.1 random forest
# Filter the data frame 'd' to exclude rows where 'Reason' is "Part-time employment, all reasons".
d1=d %>% filter(Reason!="Part-time employment, all reasons")
# Assign a binary value to the 'reason' variable based on 'Reason' values:
# - 0 if the reason does not involve looking for full-time work in the last month
# - 1 if the reason involves looking for full-time work in the last month
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
# Load the 'randomForest' package for building random forest models
library(randomForest)
# Build a random forest model ('rforest.m') to predict the 'reason' variable based on other variables.
rforest.m = randomForest(as.factor(reason) ~ Year+Geography+Immig.+Age.group++Both.sexes+Men+Women,data=d1,ntree=500,importance=TRUE)
# Visualize variable importance using the 'varImpPlot' function
varImpPlot(rforest.m)
# Make predictions using the random forest model on the same data ('d1') and calculate the accuracy
d_randomforest=d1%>%mutate(rforest_pred=predict(rforest.m,new_data=d1))
matrix=table(d_randomforest$reason,d_randomforest$rforest_pred)
matrix
mean(sum(diag(matrix))/sum(matrix))


# 8.2 Linear regression
# Fit a linear regression model ('m2') with 'Both.sexes' as the response variable and 'Year' as the predictor variable using data from 'd_table1'.
m2=lm(Both.sexes~Year,data=d_table1 %>% filter(Geography=="Canada") )
# Display a summary of the linear regression model
summary(m2)

# 9. Cross validation

# Create a new dataset `d_cross` by adding a new column `group_ind`, 
# where each observation is randomly assigned to either "train" or "test" group
d_cross=d1 %>% mutate(group_ind = sample(c("train","test"),
                                         size=nrow(d1),
                                         prob = c(0.6,0.4), 
                                         replace = T))
# Fit a random forest model (`randomForest`) to predict the `reason` variable,
# using the specified independent variables, on the observations from the "train" group
rforest.cross = randomForest(as.factor(reason) ~
                               Year+Geography+Immig.+Age.group++Both.sexes+Men+Women,
                             data=d_cross %>% filter(group_ind=="train"),
                             ntree=500, # Number of trees in the random forest
                             importance=TRUE) # Compute variable importance measures
# Predict the values of the `reason` variable using the fitted random forest model
reason.hat=predict(rforest.cross)
# Create a confusion matrix (`matrix_train`) to evaluate the performance of the model
# on the training data by comparing the actual values of `reason` with the predicted values
matrix_train=table(d_cross$reason[d_cross$group_ind=="train"],reason.hat)
# Display the confusion matrix
matrix_train
# Calculate the overall accuracy of the model on the training data
mean(sum(diag(matrix_train))/sum(matrix_train))


# cross validation for test data
# Predict the values of the `reason` variable using the fitted random forest model
# on the observations from the "test" group
reason.hat=predict(rforest.cross,newdata=d_cross %>% filter(group_ind=="test"))
# Create a confusion matrix (`matrix_test`) to evaluate the performance of the model
# on the test data by comparing the actual values of `reason` with the predicted values
matrix_test=table(d_cross$reason[d_cross$group_ind=="test"],reason.hat)
# Display the confusion matrix for the test data
matrix_test
# Calculate the overall accuracy of the model on the test data
mean(sum(diag(matrix_test))/sum(matrix_test))

