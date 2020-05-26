 setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("fitdistrplus")
install.packages("DHARMa")

library(DHARMa)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)

#Question (2a) First we are  to identify the distribution of each of G1, G2, G3 
ggle.data = read.csv("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets/student-mat.csv")

##Checking for normality of G1, G2 and G3
#pdf("G1Histogram.pdf")
hist(ggle.data$G1)



#pdf("G1Compactness.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G1)) + 
  geom_density(aes(x = ggle.data$G1), fill = "chartreuse") +
  ggtitle("Practical Compactness of G1") + theme_classic() + theme(plot.title = element_text(hjust = 0.4)) +
  xlab("G1") + 
  ylab("Compactness")


#pdf("G2Histogram.pdf")
hist(ggle.data$G2)


#pdf("G2Compactness.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G2)) + 
  geom_density(aes(x = ggle.data$G2), fill = "chartreuse") +
  ggtitle("Practical Compactness of G2") + theme_classic()  + theme(plot.title = element_text(hjust = 0.4)) +
  xlab("G2") +
  ylab("Compactness")


#pdf("G3Histogram.pdf")
hist(ggle.data$G3)


#pdf("G3Compactness.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G3)) + 
  geom_density(aes(x = ggle.data$G2), fill = "chartreuse") +
  ggtitle("Practical Compactness of G3") + theme_classic() + theme(plot.title = element_text(hjust = 0.4)) +
  xlab("G3") + 
  ylab("Compactness")



##Poisson distributed?checking for poisson
#pdf("PlotG1QQ.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G1)) + 
  stat_qq(distribution = stats::qpois, dparams = list(lambda = mean(ggle.data$G1))) +
  geom_abline(alpha = 0.24, color = 3) +  ggtitle("PlotQ-Q  Poisson for G1") +
  theme(plot.title = element_text(hjust = 0.4)) + 
  theme_classic()
  

#pdf("PlotQQG2.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G2)) + 
  stat_qq(distribution = stats::qpois, dparams = list(lambda = mean(ggle.data$G2))) +
  geom_abline(alpha = 0.24, color = 3) + ggtitle("PlotQ-Q Poisson for G2") +
  theme(plot.title = element_text(hjust = 0.4)) +
  theme_classic()
  

#pdf("PlotQQG3.pdf")
ggplot(data = ggle.data, mapping = aes(sample = ggle.data$G3)) + 
  stat_qq(distribution = stats::qpois, dparams = list(lambda = mean(ggle.data$G3))) +
  geom_abline(alpha = 0.24, color = 3) +  ggtitle("plotQ-Q  Poisson for G3") +
  theme(plot.title = element_text(hjust = 0.4)) +
  theme_classic()
#dev.off()
# Are there signs for over-dispersion or any other anomalies in the distributions of any of G1, G2, G3
##Creating a dataframe to compute the variance-to-mean ratio for overdispersion check
table_needed = data.frame(ggle.data$G1, ggle.data$G2, ggle.data$G3)
apply(table_needed, 2, mean) 
apply(table_needed, 2, var)
                            




##Question(2b)
##Fit a suitable (generalised) linear model to explain G1 including all explanatory variables (Model 1)  
model.1 = glm(G1~. -G2-G3, family = poisson, data = ggle.data)
summary(model.1) ## Are all covariates signi???cant? This is to determine the significant

##Comment on the goodness-of-???t of this model : Goodness-of-fit of Model.1
#goodness-of-???t
#pdf("PeterFitmodel.pdf")
plot(model.1)
#dev.off()

##Calculate the Pearson residuals and Anscombe residual

#pdf("ResidualANDPearson.pdf")
residual.pearson = residuals(model.1, "pearson")
x = hist(residual.pearson)
x = hist(residual.pearson, add = FALSE)
#dev.off()

anscombe.residuals = function(y, mu){
  (3*(y^(2/3)-mu^(2/3)))/2*(mu^(1/6))
  
}

#pdf("AnscombeResidual.pdf")
residual_anscombe =  anscombe.residuals(ggle.data$G1, model.1$fitted.values)
hist(residual_anscombe)
#dev.off()

##Pursue the residual analysis 
plot(model.1)



##comment if the ???tted (generalised) linear model is adequate for the data.
## We can conclude that almost all points are on the abline.


##Question(2c)
##Model 2
model.2 = glm(G1~ sex + Fedu + studytime + failures + schoolsup + famsup + goout, family = poisson, data = ggle.data)
summary(model.2) #Are all the covariates signi???can?

#Perform analysis of deviance test to compare Model 1 and Model 2
anova(model.1, model.2, test = "Chisq")

##Model 3
model.3 = glm(G1~ sex + Fedu + studytime + failures + schoolsup + famsup + Walc, family = poisson, data = ggle.data)
summary(model.3) ## We are to determine the significance of covariates


#How one can compare Model 2 and Model 3?
#Analysis of deviance test to compare Model 2 and Model 3.
anova(model.2, model.3, test = "Chisq")


plot(model.3)



 1 - pchisq((2000.2 - 1976.1),21)
