#######
# Create a function: https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/
# Run the simulation 1000 times (like doing an experiment 1000 times

#install.packages(c("purrr", "broom"))

library(purrr)
library(broom)
suppressMessages(library(dplyr))
library(ggplot2)

# Lets create our first function
worm_grow_fun <-  function(n = 500, a = 5, b = 14, sdev = 10, days = 3) {
    time <- runif(n, min = 0, max = 24*days) # set up measures of time - uniform distribution- over three days
    er <- rnorm(n, mean = 0, sd = sdev) # normally distributed error
    length <- a + b*time  + er # length measured (simulated)
    growthfit  <- lm(length ~ time)
    #return(growthfit)
}

growthfit <- worm_grow_fun()

sims <- rerun(1000, worm_grow_fun()) #use rerun() function to run simulation 1000 times

tidy(growthfit) # observe what tidy version of model output  looks like. Will be used as input below
summary(growthfit)$sigma

sims %>%
    map_df(tidy) %>%
    filter(term == "time") %>% #time coefficient
    ggplot(aes(estimate)) + # plot the estimate
    geom_density(fill = "blue", alpha = .5) + #geom pattern
    geom_vline(xintercept = 14) #add line at expectation

sims %>%
    map_dbl(~summary(.x)$sigma) %>%  #extract the standard deviation
    data.frame(sigma = .) %>% 
    ggplot(aes(sigma)) +
    geom_density(fill = "blue", alpha = .5) +
    geom_vline(xintercept = 10) #add line at expectation

sims %>%
    map_dbl(~summary(.x)$sigma) %>%
    {. < 10} %>% #how many times was the standard deviation less than expected?
    mean()


