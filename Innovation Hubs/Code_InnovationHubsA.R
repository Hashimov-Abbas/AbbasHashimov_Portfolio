# ECONOMETRICS 2 PROJECT



# Preliminary Code
# First we import datasets
library(readr)
Hub <- read_csv("C:/Users/Abbas/Desktop/ECONOMETRICS PROJECT/test_50/Hub.csv")

library(haven)
Register <- read_dta("C:/Users/Abbas/Desktop/ECONOMETRICS PROJECT/test_50/Register.dta")

install.packages("pacman")

install.packages("ggplot2")

# Then we use pacman package to install all the libraries we will use

library(pacman)
p_load(rddensity,rddtools,rdd,Matrix,AER,plm,glmnet,readr,modelsummary)
p_load(dplyr,tidyr,haven,stargazer,knitr,ggplot2,magrittr)

# Library List
library(AER)
library(dyplr)
library(car)
library(tidyverse)
library(namespace)
library(readr)
library(openxlsx)
library(haven)
library(dplyr)
library(tidyr)
library(summarytools)
library(stargazer)
library(ggplot2)
library(knitr)
library(magrittr)
library(RColorBrewer)
library(sandwich)
library(lmtest)
library(modelsummary)


# Part 1



# b
mydata<-Register %>%
  left_join(Hub,by="personid")


mydata <- transform(mydata, age2015 = ifelse(year == 2017, age2015 + 2, age2015))

mydata <- mydata %>%
  rename(age = age2015)

# Part 2

# a

mydata <- as.data.frame(mydata)


stargazer(mydata, 
          type="text", # type="html", out="output.html"
          title="Summary Statistics",
          summary.stat = NULL,
          digits=1,
          align=TRUE
)

# For stargazer we will be using HTML output as it produces nicer tables
# Copy output save it as html open in browser copy and paste
# However, in the code it will be type text as it makes analys easier



# b

open_2017 <- sum(mydata$business[mydata$year==2017]) # businesses in 2017
open_2015 <- sum(mydata$business[mydata$year==2015]) # businesses in 2015
(open_2017 - open_2015)/500000  # their ratio

# Part 3



#a 
# dummy variables for DiD
mydata$treat <- ifelse(mydata$attend == 1, 1, 0) # treatment dummy

mydata$post <- ifelse(mydata$year >= 2016, 1, 0) # period dummy



# making a table
Treatment_pre<-sum(mydata$business[mydata$year==2015&mydata$attend==1])
Control_pre<-sum(mydata$business[mydata$year==2015&mydata$attend==0])
Treatment_post<-sum(mydata$business[mydata$year==2017&mydata$attend==1])
Control_post<-sum(mydata$business[mydata$year==2017&mydata$attend==0])

# make a table 
summary_table <- data.frame(
  Treatment = c(Treatment_pre, Treatment_post),  # columns
  Control = c(Control_pre, Control_post)  # rows
)

attr(summary_table, "name") <- "Number of Businesses" # name table
rownames(summary_table) <- c("Pre", "Post") # name rows
colnames(summary_table) <- c("Treatment", "Control") # name columns

summary_table





# b

# Did model
DiD_Covarieties <- lm(business ~ post*treat + income + age + 
                        male, data=mydata)
coeftest(DiD_Covarieties,vcov=vcovHC(DiD_Covarieties, type="HC1"))
summary(DiD_Covarieties)


# Naive OLS
Naive_OLS <- lm(business~attend+income+age+male, data=subset(mydata, year==2017))
summary(Naive_OLS)

#c
models <- list(Naive_OLS,DiD_Covarieties)
names <- c("Naive OLS", "DiD")


stargazer(models, column.labels = names, 
          type="text", # type="html", out="output.html",
          title = "Naive OLS and DiD Results")



# Part 4

# a
# We will create a new variable for eligibility
# we will also store in 2017 rows whether individual was eligible
mydata$eligible <- ifelse(mydata$year == 2015 & mydata$age < 30, 1,
                          ifelse(mydata$year == 2017 & mydata$age < 32, 1, 0))



a <- sum(mydata$eligible[mydata$attend==1&mydata$eligible==1])
b <-sum(mydata$eligible)
a/b

# Also check if there are people who were uneligible and got treatment
sum(mydata$attend[mydata$eligibility==0])  # yields 0 

# b

# As for take-up rate only year 2015 matters we will create a subset
# of original data for year 2015

Take_up_Data <- subset(mydata, year == 2015)

# We will first specify linear regression then use Probit and Logit
take_up_linear<-lm(attend~income+male+educ+age+distance, 
                   data=Take_up_Data)

# calculate Probit and Logit
# These two lines below sometimes work and sometimes give 
# glm.fit: fitted probabilities numerically 0 or 1 occurred,' [...] error
Probit <- glm(take_up_linear, family = binomial(link = "probit"))
Logit <- glm(take_up_linear, family = binomial(link = "logit"))

# calculate pseudo R-squared
pseudo.probit.R2<- 1-(Probit$deviance/Probit$null.deviance)  
pseudo.logit.R2<-1-(Logit$deviance/Logit$null.deviance)
print(pseudo.probit.R2)
print(pseudo.logit.R2)

# Make a new row to add Pseudo R-squares to stargazer

new_row_stargazer <- c("Pseudo R-squared",
                       round(pseudo.probit.R2,digits=2),
                       round(pseudo.logit.R2,digits = 2))

stargazer(Probit, Logit, 
          type="text" #type="html", out="output.html"
          , add.lines = new_row_stargazer,
          title = "Results of Binary Models", align = TRUE)



# Part 5 Instrumental Variable

# a


# Exogeniuty check

model <- lm(attend~distance, data=mydata) # OLS to see relationship
stargazer(model, type="text")

# correlate to see relationship
cor(mydata$distance[mydata$year==2017],mydata$attend[mydata$year==2017])


# b
# Estimate IV regression model with diagnostics
iv_model <- ivreg(business ~ attend + age +income + male | 
                    distance + income +age +male, 
                  data = subset(mydata, year == 2017), diagnostics = TRUE)

coeftest(iv_model,vcov=vcovHC(iv_model,type="HC1"))

stargazer(iv_model, column.labels = "IV Regression", 
          type="text", #type="html", out="output.html"
          title = "IV Results")





# Part 6
# a
# Creating the data

# cut age into groups for RDD
mydata <- mydata %>% mutate(agebin = 
                              cut(age, breaks 
                                  = seq(from = 25, to = 36, by = 1)))

# calculate means for these groups
bindata <- mydata %>% group_by(agebin) %>% summarise(agemean = mean(age[year==2017]), 
                                                     businessmean = mean(business[year==2017]),
                                                     attendmean=mean(attend[year==2017]))

# Here we get a problem with additional last row with null values
# It skews all the graphs thus we will remove the last row to solve this

bindata <- na.omit(bindata)



# Treatment Discontinuity

# graph age on x axis and proportion Treated on y axis
subset(bindata) %>% 
  ggplot(aes(x=agemean, y=attendmean)) + 
  geom_point() +                                     # add points
  geom_vline(xintercept = 32, color = "red", size = 1, linetype = "dashed") + 
  annotate("text", x = 32, y = -0.15, label = "Eligiblity Threshold",
           size=4) +
  scale_y_continuous(limits = c(-0.25,0.5)) +  # Adjust y-axis scale
  coord_cartesian(xlim = c(25, 35), ylim = c(-0.25, 0.5)) +  # Change graph center
  labs(y = "Proportion of Treated", 
       x = "Age in 2017",
       title = "Discontinuity In Treatment")     # add names

# Discontinuity Graph

subset(bindata) %>% 
  ggplot(aes(x=agemean, y=businessmean)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, data = subset(bindata, agemean < 32), color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, data = subset(bindata, agemean >= 32), color = "green") +  
  geom_vline(xintercept = 32, color = "red", size = 1, linetype = "dashed") + 
  annotate("text", x = 32, y = 0, label = "Eligibility Threshold",
           size=4) +
  scale_y_continuous(limits = c(0, 0.15)) +  # Adjust y-axis scale
  coord_cartesian(xlim = c(25, 35), ylim = c(0.05, 0.15)) +  # Change graph center
  labs(y = "Probability of Succesful Startup",
       x = "Age in 2017",
       title = "Discontinuity Graph")


# b

# same slope window of observations = 10 years
lm_same_slope <- lm(attend ~ eligible + I(age - 32), 
                    data = subset(mydata, age < 42 & age > 22))

# different slope window of observations = 10 years
lm_different_slope <- lm(business ~ eligible + I(age - 32) + eligible:I(age - 32),
                         data = subset(mydata, age < 42 & age > 22))

stargazer(lm_same_slope,lm_different_slope, type="text")

# we get LATE = 4%


# We will use eligibility as an instrumental variable in a Fuzzy RDD
# The window of observations = 10 years
RDD_iv_Window10 <- ivreg( business ~ attend + I(age - 32) + eligible:I(age - 32) 
                          | eligible + I(age - 32) + eligible:I(age - 32), 
                          data = subset(mydata, age < 42 & age > 22))

# Window of observation is 5 years
RDD_iv_Window5 <- ivreg( business ~ attend + I(age - 32) + eligible:I(age - 32) 
                         | eligible + I(age - 32) + eligible:I(age - 32), 
                         data = subset(mydata, age < 37 & age > 27))
# Window of observations is 3 years
RDD_iv_Window3 <- ivreg( business ~ attend + I(age - 32) + eligible:I(age - 32) 
                         | eligible + I(age - 32) + eligible:I(age - 32), 
                         data = subset(mydata, age < 35 & age > 29))

# table to compare windows of observations
stargazer(RDD_iv_Window10,RDD_iv_Window5,RDD_iv_Window3, 
          type = "text", # type="html", out="output.html",
          title = "RDD results")

# Part 7 
#a
# Final graph with everything
stargazer(Naive_OLS, DiD_Covarieties, iv_model, RDD_iv_Window10, 
          type = "text", # type="html", out="output.html",
          column.labels = c("Naive OLS", "DiD", "IV Model","RDD window = 10"),
          title="Final Results",
          digits=3)