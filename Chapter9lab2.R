setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")

#install.packages(c("tidyverse", "bootstrap"))

library("tidyverse")
library("bootstrap")

n = 100               #Sample size 
R = 1000              #Bootstrap replications
M = 1000              #Monte Carlo samples size

#Question(a)

#Simulating a sample with size n from the Weibull distribution with the scale parameter
#lamda = 13 and shape parameter k = 1

##Generating Weibul random variables
set.seed(1)              #For reproducibility
k = 1                     #K is the shape parameter
lambda = 13               #scale parameter

sample.weibull = rweibull(n, shape = 1, scale = 13)
xmed = median(sample.weibull)          # simulated median
zigma = sd(sample.weibull)             # simulated SD

#Building a two-sided bootstrap percentile confidence intervals for s = sigma and x_med at the significance level a = 0:95

#initializing variables
test.median = 0
test.sd = 0
alpha = 0.95
LB.median = 0     #Lower Bound of the Median
UB.median = 0     #Upper Bound of the Median
LB.SD = 0         #Lower Bound of the SD
UB.SD = 0         #Upper Bound of the SD

for (j in 1:M) {  #We use M Monte Carlo samples to estimate the coverage probability of the CIs
  sample.weibull = rweibull(n, shape = k, scale = lambda)
  boot.median = 0
  boot.SD = 0
  for (i in 1:R) {
    myBootstrap = sample(sample.weibull, n, replace = T) 
    boot.median[i] = median(myBootstrap)
    boot.SD[i] = sd(myBootstrap)
  }
  
  boot.median = sort(boot.median)
  boot.SD = sort(boot.SD)
  
  LB.median[j] = boot.median[floor(R*(1-alpha))/2]
  UB.median[j] = boot.median[floor(R*(1-(1-alpha)/2))]
  LB.SD[j] = boot.SD[floor(R*(1-alpha))/2]
  UB.SD[j] = boot.SD[floor(R*(1-(1-alpha)/2))]
  df1 = data.frame(LB.median, UB.median, LB.SD, UB.SD)
}

#Putting all computations in a dataframe
df1 = df1 %>% 
  mutate(test.median = xmed >= LB.median & xmed <= UB.median)
df1 = df1 %>% 
  mutate(test.sd = zigma >= LB.SD & zigma <= UB.SD)


#Estimating the Coverage Probabilities from M Monte Carlo Samples of both CI
coverage.prob1 = c(sum(df1$test.median)/M, sum(df1$test.sd)/M)

#Average interval length
ave.interval.length1 = c(sum(df1$UB.median - df1$LB.median)/M, sum(df1$UB.SD - df1$LB.SD)/M)


#Now we estimate the average interval length with n = R = 1000 
n = 1000
R = 1000
test.median2 = 0
test.sd2 = 0
alpha = 0.95
LB.median2 = 0
UB.median2 = 0
LB.SD2 = 0
UB.SD2 = 0

for (j in 1:M) {
  sample.weibull = rweibull(n, shape = 1, scale = 13)
  boot.median2 = 0
  boot.SD2 = 0
  for (i in 1:R) {
    myBootstrap = sample(sample.weibull, n, replace = T)
    boot.median2[i] = median(myBootstrap)
    boot.SD2[i] = sd(myBootstrap)
  }
  
  boot.median2 = sort(boot.median2)
  boot.SD2 = sort(boot.SD2)
  
  LB.median2[j] = boot.median2[floor(R*(1-alpha))/2]
  UB.median2[j] = boot.median2[floor(R*(1-(1-alpha)/2))]
  LB.SD2[j] = boot.SD2[floor(R*(1-alpha))/2]
  UB.SD2[j] = boot.SD2[floor(R*(1-(1-alpha)/2))]
  df2 = data.frame(LB.median2, UB.median2, LB.SD2, UB.SD2)
}
df2 = df2 %>% 
  mutate(test.median = xmed >= LB.median2 & xmed <= UB.median2)
df2 = df2 %>% 
  mutate(test.sd = zigma >= LB.SD2 & zigma <= UB.SD2)


#Estimating the coverage probability of both confidence intervals
coverage.prob2 = c(sum(df2$test.median)/M, sum(df2$test.sd)/M)

#Estimating the average interval length
ave.interval.length2 = c(sum(df2$UB.median2 - df2$LB.median2)/M, sum(df2$UB.SD2 - df2$LB.SD2)/M)

#Now we estimate the average interval length with n = 100, R = 500
n = 100
R = 5000
test.median3 = 0
test.sd3 = 0
alpha = 0.95
LB.median3 = 0
UB.median3 = 0
LB.SD3 = 0
UB.SD3 = 0

for (j in 1:M) {
  sample.weibull = rweibull(n, k, lambda)
  boot.median3 = 0
  boot.SD3 = 0
  for (i in 1:R) {
    myBootstrap = sample(sample.weibull, n, replace = T)
    boot.median3[i] = median(myBootstrap)
    boot.SD3[i] = sd(myBootstrap)
  }
  
  boot.median3 = sort(boot.median3)
  boot.SD3 = sort(boot.SD3)
  
  LB.median3[j] = boot.median3[floor(R*(1-alpha))/2]
  UB.median3[j] = boot.median3[floor(R*(1-(1-alpha)/2))]
  LB.SD3[j] = boot.SD3[floor(R*(1-alpha))/2]
  UB.SD3[j] = boot.SD3[floor(R*(1-(1-alpha)/2))]
  df3 = data.frame(LB.median3, UB.median3, LB.SD3, UB.SD3)
}
df3 = df3 %>% 
  mutate(test.median3 = xmed >= LB.median3 & xmed <= UB.median3)
df3 = df3 %>% 
  mutate(test.sd3 = zigma >= LB.SD3 & zigma <= UB.SD3)


#Estimating the coverage probability of both confidence intervals
coverage.prob3 = c(sum(df3$test.median3)/M, sum(df3$test.sd3)/M)

#Average interval length
ave.interval.length3 = c(sum(df3$UB.median3 - df3$LB.median3)/M, sum(df3$UB.SD3 - df3$LB.SD3)/M)




#Bootstrap accelerated bias-corrected confidence intervals

?bcanon


#initializing variables
set.seed(77)
LB.median4 = rep(0, M)
UB.median4 = rep(0, M)
LB.SD4 = 0
UB.SD4 = 0
zed.hat.median = 0
a.not.median = 0
zed.hat.sd = 0
a.not.sd = 0

for (j in 1:M) {
  sample.weibull = rweibull(100, k, lambda)
  bcanon.median = bcanon(sample.weibull, R, theta = median, alpha = c(0.025, 0.975))
  zed.hat.median[j] = bcanon.median$z0
  a.not.median[j] = bcanon.median$acc
  LB.median4[j] = bcanon.median$confpoints[1,2]
  UB.median4[j] = bcanon.median$confpoints[2,2]
}

median.bacon.df4 = data.frame(LB.median4, UB.median4)

median.bacon.df4 = median.bacon.df4 %>% 
  mutate(test.median4 = xmed >= LB.median4 & xmed <= UB.median4)

#Now for the sd

for (j in 1:M) {
  sample.weibull = rweibull(100, k, lambda)
  bcanon.SD = bcanon(sample.weibull, R, theta = sd, alpha = c(0.025, 0.975))
  zed.hat.sd[j] = bcanon.SD$z0
  a.not.sd[j] = bcanon.SD$acc
  LB.SD4[j] = bcanon.SD$confpoints[1,2]
  UB.SD4[j] = bcanon.SD$confpoints[2,2]
}

SD.bacon.df4 = data.frame(LB.SD4, UB.SD4)

SD.bacon.df4 = SD.bacon.df4 %>% 
  mutate(test.sd4 = zigma >= LB.SD4 & zigma <= UB.SD4)

#We now create dataframe for the estimates
estim =  data.frame(zed.hat.sd, a.not.sd, zed.hat.median, a.not.median) 
df4 = data.frame(median.bacon.df4, SD.bacon.df4)

#Estimatiing the coverage probability of both confidence intervals
coverage_prob4 = c(sum(df4$test.median4)/M, sum(df4$test.sd4)/M)

#Estimation of the average interval length
ave_interval_length4 = c(sum(df4$UB.median4 - df4$LB.median4)/M, sum(df4$UB.SD4 - df4$LB.SD4)/M)

#Taking the averages of estimates
comment1 = mean(zed.hat.sd)
comment2 = mean(a.not.sd)
comment3 = mean(zed.hat.median)
comment4 = mean(a.not.median)


#Question(9b)
bootstraps.data = read.table("shhs2.txt", header = TRUE)
rdi4p.histogram = ggplot(bootstraps.data, aes(x=rdi4p)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  ggtitle("Histogram of rdi4p Variable") +
  theme_classic()
rdi4p.histogram



#Implementing CI for the median and SD with bootstrap percentiles

#Calculating the median and SD of the variable, rdi4p
variable.median = median(bootstraps.data$rdi4p)
variable.SD = sd(bootstraps.data$rdi4p)

#Defining other parameters sample size, bootstraps replications, alpha
n = length(bootstraps.data$rdi4p)
R = 1000
alpha = 0.95


#Initialize variables
data.median = 0
data.sd = 0

for (i in 1:R) {
  myBootstrap = sample(bootstraps.data$rdi4p, n, replace = T)
  data.median[i] = median(myBootstrap)
  data.sd[i] = sd(myBootstrap)
}

data.median = sort(data.median)
data.sd = sort(data.sd)
LB.variable.median = data.median[floor(R*(1-alpha))/2]
UB.variable.median = data.median[floor(R*(1-(1-alpha)/2))]
LB.variable.SD = data.sd[floor(R*(1-alpha))/2]
UB.variable.SD = data.sd[floor(R*(1-(1-alpha)/2))]

boostrap_percentile_CI = data.frame(LB.variable.median, UB.variable.median, LB.variable.SD, UB.variable.SD)
test.median5 = variable.median >= LB.variable.median & variable.median <= UB.variable.median
test.SD5 = variable.SD >= LB.variable.SD & variable.SD <= UB.variable.SD

#Building the bootstrap accelerated bias-corrected confidence intervals 

#The median

#Remove all the median values from the data 
variable.median.remove = bootstraps.data$rdi4p [! bootstraps.data$rdi4p %in% median(bootstraps.data$rdi4p)] #we remove all values equal to the median, to avoid errors
variable.bootstraps = bcanon(variable.median.remove, R, theta = median, alpha = c(0.025, 0.975))
zed.variable.median = variable.bootstraps$z0
a.not.variable.median = variable.bootstraps$acc
LB.variable.median2 = variable.bootstraps$confpoints[1,2]
UB.variable.median2 = variable.bootstraps$confpoints[2,2]

test.median6 = variable.median >= LB.variable.median2 & variable.median <= UB.variable.median2



#Now to the SD
variable.bootstraps = bcanon(bootstraps.data$rdi4p, R, theta = sd, alpha = c(0.025, 0.975))
zed.variable.SD = variable.bootstraps$z0
a.not.variable.SD = variable.bootstraps$acc
LB.variable.SD2 = variable.bootstraps$confpoints[1,2]
UB.variable.SD2 = variable.bootstraps$confpoints[2,2]

test.SD6 = variable.SD >= LB.variable.SD2 & variable.SD <= UB.variable.SD2

conf.int.bcanon = data.frame(LB.variable.median2, UB.variable.median2, LB.variable.SD2, UB.variable.SD2, 
                    zed.variable.median, a.not.variable.median, zed.variable.SD, a.not.variable.SD)
