install.packages("lme4")  

library(lme4)
install.packages("dplyr")       
install.packages("tidyverse")  
library(tidyverse)
library(ggplot2)
library(dplyr)

data("sleepstudy")
?sleepstudy

view(sleepstudy)

head(sleepstudy) 
#> head(sleepstudy)
#Reaction Days Subject
#1 249.5600    0     308
#2 258.7047    1     308
#3 250.8006    2     308
#4 321.4398    3     308
#5 356.8519    4     308
#6 414.6901    5     308

str(sleepstudy)  
#> str(sleepstudy) 
#'data.frame':	180 obs. of  3 variables:
 # $ Reaction: num  250 259 251 321 357 ...
#$ Days    : num  0 1 2 3 4 5 6 7 8 9 ...
#$ Subject : Factor w/ 18 levels "308","309","310",..: 1 1 1 1 1 1 1 1 1 1 ..
#shows that days and reactions are numeric characters and subjects are factors 
#shows that we have 180 observations and 3 variabes or coumns 

sleepstudy<- sleepstudy %>% 
  filter(!(Days %in% c(1, 2)))
# this i used to remove the first two rows 1 and 2 as we started counting from 3, 2 is the baseline 



plot(sleepstudy$Days, sleepstudy$Reaction,
     xlab = "Days", ylab = "Reaction Time", pch = 20.5,        
     col = "green",  cex = 1.3) 
# showed that as the number of days increased where participants went without sleep, the higher their reaction time to stimuli


boxplot(Reaction ~ Days, col=c("white","lightgray"),sleepstudy)

# shows that with every increase in the number of days that the participants went with sleep deprivation, the slower
#their reaction time 

summary(sleepstudy$Reaction)
#> summary(sleepstudy$Reaction)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#194.3   255.4   288.7   298.5   336.8   466.4

summary(sleepstudy$Days)
#> summary(sleepstudy$Days)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     2.0     4.5     4.5     7.0     9.0 



ggplot(sleepstudy, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal()


ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point(color = "blue", size = 1.3) + 
  theme_minimal()

# the more days gone without sleep, or the higher the number of seep deprived days, the slower the participant
#reaction time 



model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(model)
#Linear mixed model fit by REML ['lmerMod']
#Formula: Reaction ~ Days + (1 | Subject)
#Data: sleepstudy
#REML criterion at convergence: 1438.009
#Random effects:
 # Groups   Name        Std.Dev.
#Subject  (Intercept) 41.32   
#Residual             31.19   
#Number of obs: 144, groups:  Subject, 18
#Fixed Effects:
 # (Intercept)         Days  
#252.97        10.27

#the explanation is in the PDF document 

#Residual analysis

res <- residuals(model)

par(mfrow = c(1, 3))

hist(res)

qqnorm(res)

plot(fitted(model), res)

# the plots show satisfaction of the assumption about the residuals, with the histogram 
#normally distributed and plot also showing normal distribution
#meaning the model is satisfactory 



