library(tidyverse)
library(pwr)
library(effsize)

##################
## assignment 1 ##
###################

rm(list = ls()) #clear all
### A. pwr package

# compute Cohen's d (by hand) 
d = (7.3 - 7.1) / 0.5 

pwr.t.test(n = NULL, d = d, sig.level = 0.05, power = 0.8, type = 'two.sample')

### B. simulation

#general settings
sample_sizes <- seq(10, 200, 10) #set the sample-sizes that you want to simulate
iter <- c(1:1000) #number of iterations per sample-size. To get a good estimate, use >1000 iterations

#specific settings for assignment
meanSHP <- 7.3  #mean grade of SHP students
meanOthers <- 7.1   #mean grade of students at some other univeristy
sd <- 0.5       #sd of grades (equal across conditions, for sake of simplicity)

outcomes_per_sample <- vector() #create an empty vector to store outcomes per sample-size
indx <- 1 #index of the tested sample-sizes
for (iSample in sample_sizes) { #loop over sample-sizes
  outcomes_per_test <- vector() #create an empty vector to store the results per iteration
  for (i in iter) {
    SHP = rnorm(iSample, meanSHP, sd) #simulate data of SHP students for specific sample-size 
    Others = rnorm(iSample, meanOthers, sd)   #simulate data of M&S students for specific sample-size 
    stat_out <- t.test(SHP, Others)       #here we perform the actual test (per iteration, per sample-size)
    outcomes_per_test[i] <- stat_out$p.value < 0.05 #we want to know in what % our test resulted in a significant results, so we store TRUE/FALSE statements of p < 0.05
  }
  outcomes_per_sample[indx] <- mean(outcomes_per_test) #by taking the mean of the vector of TRUE/FALSE statements (the length of which is the number of iterations) we obtain the % in which our test gave a significant result
  indx <- indx +1 #add 1 to the index when continuing to the next sample-size
  print(paste('done computing % of significant test-outcomes for sample-size', iSample))
}

dat <- tibble(outcomes_per_sample, n_per_group = sample_sizes) #put the sample-sizes and the % of significant results per sample-size in a tibble (to visualize)

ggplot(dat) +
  geom_point(aes(x = n_per_group, y = outcomes_per_sample)) + #plot the outcomes of the simulation
  geom_hline(yintercept = 0.8, linetype = "dashed") #insert a horizontal dashed line at the required power level (to read out the sample size)


# Check in the plot where the dashed line intersects with the simulated datapoint. The x-coordinate of the interception gives your the sample-size! Voila!




##################
## assignment 2 ##
##################

rm(list = ls()) #clear all

### A. pwr package

# compute Cohen's d (by hand) 
d = (7.3 - 7.1) / 0.5 

# ASSIGNMENT: FILL IN THE CORRECT ARGUMENTS AND COMPUTE THE SAMPLE SIZE
pwr.t.test()

### B. simulation


# ASSIGNMENT: THINK ABOUT WHICH LINES OF CODE WE NEED TO CHANGE IN THE PREVIOUS SIMULATION

#general settings
sample_sizes <- seq(10, 200, 10) #set the sample-sizes that you want to simulate
iter <- c(1:1000) #number of iterations per sample-size. To get a good estimate, use >1000 iterations

#specific settings for assignment
meanDifference <- 0.2 #mean differences of pre and post measurments
sd <- 0.5 #standard deviation of differences


outcomes_per_sample <- vector() #create an empty vector to store outcomes per sample-size
indx <- 1 #index of the tested sample-sizes
for (iSample in sample_sizes) { #loop over sample-sizes
  outcomes_per_test <- vector() #create an empty vector to store the results per iteration
  for (i in iter) {
    simulatedDiffs = rnorm(iSample, meanDifference, sd) #simulated differences of pre and post measurments
    stat_out <- t.test(simulatedDiffs)       #here we perform the actual test (per iteration, per sample-size)
    outcomes_per_test[i] <- stat_out$p.value < 0.05 #we want to know in what % our test resulted in a significant results, so we store TRUE/FALSE statements of p < 0.05
  }
  outcomes_per_sample[indx] <- mean(outcomes_per_test) #by taking the mean of the vector of TRUE/FALSE statements (the length of which is the number of iterations) we obtain the % in which our test gave a significant result
  indx <- indx +1 #add 1 to the index when continuing to the next sample-size
  print(paste('done computing % of significant test-outcomes for sample-size', iSample))
}

dat <- tibble(outcomes_per_sample, n_pairs = sample_sizes) #put the sample-sizes and the % of significant results per sample-size in a tibble (to visualize)

ggplot(dat) +
  geom_point(aes(x = n_pairs, y = outcomes_per_sample)) + #plot the outcomes of the simulation
  geom_hline(yintercept = 0.8, linetype = "dashed") #insert a horizontal dashed line at the required power level (to read out the sample size)


# Check in the plot where the dashed line intersects with the simulated datapoint. The x-coordinate of the interception gives your the sample-size! Voila!

##################
## assignment 3 ##
##################

#######################
## WORK IN PROGRESS  ##
#######################

