setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")

library(ggplot2)


##Question (a)
#Switch the default random number generator in R to Wichmann-Hill
RNGkind(kind = "Wichmann-Hill", normal.kind = NULL)
set.seed(250, kind = "Wichmann-Hill", normal.kind = NULL)
.Random.seed = c("Wichmann-Hill", 1, 4)


#Inversion method to simulate a binomial random variable
N = 1000
n = 10
p = 0.4
uniform <- runif(N)
bins <- .bincode(uniform, breaks = c(0, pbinom(0:10, 10, 0.4)), right = F, include.lowest = T) ##To bin a numeric vector and return integer codes for the binning.


inversion.bins <- numeric()
for(i in 1:N){
  inversion.bins[i] <- bins[i]-1
}



#Simulation of a binomial random variable by simulating corresponding Bernoulli random variables by inversion method
bernoulli.bin <- numeric()
for (i in 1:N){
  value <- runif(n)
  bernoulli.bin[i] <- sum(value < p)
}



#Simulation of a binomial random variable with rbinom
binomial.bin <- rbinom(N, n, p)



#Plot the histograms of all three samples on one panel
#First the dataframes for ggplot, then the plot
inversion.bins <- data.frame(inversion.bins)
inversion.bins$Type <- rep("Inverse CDF", 1000)
colnames(inversion.bins) <- c("random.number", "method")

bernoulli.bin <- data.frame(bernoulli.bin)
bernoulli.bin$Type <- rep("Bernoulli", 1000)
colnames(bernoulli.bin) <- c("random.number", "method")

binomial.bin <- data.frame(binomial.bin)
binomial.bin$Type <- rep("rbinom", 1000)
colnames(binomial.bin) <- c("random.number", "method")

RNG.table <- rbind(inversion.bins, bernoulli.bin, binomial.bin)


#Plot the histograms of all three samples on one panel
#pdf("SamplesHistogram.pdf")
ggplot(RNG.table, aes(x = random.number, fill = method)) +
  geom_histogram(binwidth= .5, position="dodge") +
  labs(fill = "method", x = "Samples", y = "Frequency", title = "plot the histograms of all three samples on one panel") +
  theme_classic()
#dev.off()


#Switch the random number generator back to its default
RNGkind(kind = "default", normal.kind = NULL)


##Question(b)
##Generating Standard Normal RV Using Accept-Reject Method
f <- function(x){                            
  ((2*pi)^(-1/2))*exp(-(x^2)/2)
}

g = function(x){                            
  (pi*(1 + x^2))^(-1)
}

#First determine the best value of the constant c, such that f(x) <= g(x)
c = sqrt((2*pi)/exp(1))

#10000 standard normal random variables using 
#the accept-reject method, generating Cauchy distributed random variables using inversion method
N = 10000
accept = 0
random.num <- numeric()
while(length(random.num) != N){
  w <- runif(1) 
  cauchy <- tan((w-(1/2))*pi)       
  U = runif(1)                     
  if(U*c*g(cauchy) <= f(cauchy)){        
    random.num[accept] <- cauchy
    accept = accept + 1
  }
}


#Histogram of the obtained sample with the standard normal density Accept Reject
#pdf("NormalHistogram.pdf")
k = rnorm(N)
normal.table = data.frame(random.num, k)
ggplot(normal.table) +
  geom_histogram(aes(x = random.num, y = ..density.., colour = random.num), colour ="white") +
  geom_density(aes(x = k), colour = "blue") +
  ggtitle("Histogram of the obtained sample with the standard normal density ") +
  xlab("Samples") + ylab("Density") +
  theme_classic()
#dev.off()

#QQ-plot
#pdf("QQPlot.pdf")
ggplot(data = normal.table, mapping = aes(sample = random.num)) +
  ggtitle("Plot of  Normal Q-Q Plot") +
  stat_qq(color = "blue") +
  theme(plot.title = element_text(hjust = .5)) + 
  xlab("Hypage") + ylab("Zstunt")+
  theme_classic()
#dev.off()