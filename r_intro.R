#########
# Data meetings, day 2
# Let's R
#########

# What version of R are you using? What packages are loaded in the workspace?
sessionInfo()

# Install/Load packages
#install.packages("tidyverse")
library(tidyverse) # load tidyverse


#####
# We will simulate data to create a growth curve for worms
# Data:  measure of worm length over time, assuming a linear relationship

set.seed(100) #to replicate random assignments

n <- 500 # number of observations
a <- 5 # intercept - size of an egg
b <- 14 # slope, 14 um increase in size with a unit increase (1 hour) in time
sdev <- 10 # average deviation of measures

time <- runif(n, min = 0, max = 24*3) # set up measures of time - uniform distribution- over three days

mean(time)
hist(time)

er <- rnorm(n, mean = 0, sd = sdev) # normally distributed error

mean(er)
hist(er)

length <- a + b*time + er # length measured (simulated)

mean(length)
hist(length)

growthfit  <- lm(length ~ time) #fit a linear regression model using the lm() function in base R

summary(growthfit) #get summary of regression fit
summary(growthfit)$sigma

plot(time, length) # relationship between time and length 

worm.grow <- data.frame(time, length, er) %>% # create  a dataframe to use in ggplot below
    mutate(stage = case_when(time < 20 ~ "L1", # create a factor variable to store stage information 
                             time >= 20 & time < 27 ~ "L2",
                             time >= 27 & time < 34 ~ "L3",
                             time >= 34 & time < 43 ~ "L4",
                             time >= 43 ~ "Adult"))

# Plot the relationship between length and time using ggplot()
ggplot(data = worm.grow, #dataframe
       mapping = aes(x = time, y = length))+  #mapping information, x is continuos
    geom_point() + #the geom pattern
    theme_classic() + #theme of the graph (there are many option)
    geom_smooth(method = 'lm') + #fit a line using lm() (linear regression)
    theme(axis.title=element_text(size=14,face="bold")) 

# Plot boxplots using a factor variable on the x-axis
ggplot(data= worm.grow, #dataframe
       aes(x = stage, y = length)) + #mapping variables, x is a factor
    geom_boxplot() + # The geom pattern
    theme_minimal() # minimal theme

# reorder factor levels to be: L1, L2, L3, L4 and Adult
fct_reorder(worm.grow$stage, worm.grow$length) %>%  # Reorder based on median length
    levels() %>% head() 

ggplot(data= worm.grow, aes(x = fct_reorder(stage, length), y = length)) +
    geom_boxplot() +
    labs( x = "Worm stage", #label x axis
          y = "Worm length (mm)") + #label y-axis
    theme_bw() #black and white theme

######
# read this site:
# https://stat545.com/ 
# very useful, gives good tips and tricks.

######
#  How does changing the simulated error term  affect the regression fit?

set.seed(50)

sdev <- 10

er.many <- vector() #initialize variables that will be used in a for loop
er.loop <- vector()

for(i in  1:6){
    er.loop <- as.matrix(rnorm(n, mean = 0, sd = sdev),  ncol  = 1)
    colnames(er.loop) <- paste0("sd.",sdev) #paste0() function in base R
    er.many <- cbind(er.many, er.loop) #cbind - column binding
    sdev = sdev+(i*2)
}

er.many[1:5,]

error.pval <- matrix(NA, nrow = 6, ncol =3) #initialize a matrix to hold information from for loop below

for(i in 1:6){
    length <- a + b*time + er.many[,i] # length measured (simulated)
    growthfit  <- lm(length ~ time) #fit the regression model
    error.pval[i,1:3] <- cbind(colnames(er.many)[i], summary(growthfit)$coef[1,4], summary(growthfit)$sigma)
    # save the sdev given, the p-value, and the sd generated from the model
}

colnames(error.pval) <- c("given_sd", "estimated_p.value", "simulated_sd")

print(error.pval)

