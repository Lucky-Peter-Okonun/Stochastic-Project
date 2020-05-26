setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")

#install.packages("survival")
#install.packages("survminer")


library(survival)
library(survminer)
library(ggplot2)



#Question(8a)
thor = read.table("Thoracic.txt")
colnames(thor) = c("DGN", "PRE4", "PRE5", "PRE6", "PRE7", "PRE8", "PRE9", "PRE10", "PRE11", "PRE14", "PRE17", "PRE19", "PRE25", "PRE30", "PRE32", "AGE", "Risky1Y") 
thor.analysis = c("PRE30", "AGE", "Risky1Y")
thor = thor[thor.analysis]
thor = transform(thor, PRE30 = as.logical(PRE30))

#Computing nonparametric estimators of the survivor function: Kaplan-Meier
survivor.kmeier = survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = thor, type = 'kaplan-meier')
#Computing nonparametric estimators of the survivor function: Flemming-Harrington
survivor.fleming = survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = thor, type = 'fleming')
fits = list(Kaplan_Meier = survivor.kmeier, Fleming_Harrington = survivor.fleming)


##Joined plot of km and fh
#pdf("NonParametricJOINED.pdf")
ggsurvplot(fits, thor, combine = TRUE, palette = "jco", conf.int = TRUE, 
           conf.int.style = "footstep") + 
      ggtitle("Functions of Kaplan-Meier and Flemming-Harrington on the Thoraxic Data")

#dev.off()

##fit the exponential model to the data
survivor.kmeier.exponential = survreg(Surv(AGE, Risky1Y) ~ 1, data = thor, dist="exponential")
lambda = exp(-survivor.kmeier.exponential$coefficients)
estimates_exp = exp(-lambda*c(0:90))
#fit the Weibull model to the data
survivor.kmeier.weibull = survreg(Surv(AGE, Risky1Y) ~ 1, data = thor, dist="weibull")
lambda.weibull.fit = exp(-survivor.kmeier.weibull$coefficients)
alpha = 1/survivor.kmeier.weibull$scale
estimates_weibull = exp(-(lambda.weibull.fit*c(0:90))^alpha)


#The Estimator of Kaplan-Meier with the other parametric estimators
#pdf("LuckyExponential.pdf")
plot(estimates_exp, col = "green")
lines(estimates_weibull, col = "blue")
lines(survivor.kmeier, col = "red")
legend("bottomleft", inset=.0001,
       legend =  c("Kaplan-Meier", "Exponential", "Weibull"),
       col =  c("green", "blue", "red"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
 title(main="Exponential, Weibull and Kaplan-Meier Estimators of the\nSurvivor Function")
#dev.off()

##Use appropriate graphical tools to check if the Weibull model is adequate for the data
#pdf("CheckingWeibull.pdf")
plot(log(survivor.kmeier$time), log(-log(survivor.kmeier$surv)), color = "yellow")
abline(a = alpha*log(lambda.weibull.fit), b = alpha, col = "red")




#Question(8b)
#Fit the Weibull model to both groups: we do it by spliting the data into smokers and non-smokers
thor.smoker = subset(thor, PRE30 == 'TRUE')
thor.non.smoker = subset(thor, PRE30 =='FALSE')



#How large is the proportion of smokers in the sample
Proportion = nrow(thor.smoker)/nrow(thor)


##Non-Parametric Estimations of the Survival Functions
survivor.smoker.kmeier = survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = thor.smoker, type = 'kaplan-meier')

##Non-Parametric Estimations of the Survival Functions
survivor.non.smoker.kmeier = survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = thor.non.smoker, type = 'kaplan-meier')



#pdf("TheSmokersTheNonSmokers.pdf")
plot(survivor.smoker.kmeier, col = "yellow")
lines(survivor.non.smoker.kmeier, col = "blue")
legend("bottomleft", inset=.02,
       legend =  c("Smokers", "Non-Smokers"),
       col =  c("blue", "red"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
title(main="nonparametric & parametric estimators \n of the survivor function by group")



#Test formally if the survival time depends on being a smoker using the log-rank test
survdiff(Surv(AGE, Risky1Y)~PRE30, data = thor , rho = 0)


#fitting the Weibull model to smokers' group
survivor.smoker.kmeier.weibull = survreg(Surv(AGE, Risky1Y) ~ 1, data = thor.smoker, dist="weibull")
smokers.lambda.weibull.fit = exp(-survivor.smoker.kmeier.weibull$coefficients)
smokers.alpha = 1/survivor.smoker.kmeier.weibull$scale
estimates.smokers.weibull = exp(-(smokers.lambda.weibull.fit*c(0:90))^alpha)

#fitting the Weibull model to nonsmokers' group
survivor.non.smoker.kmeier.weibull = survreg(Surv(AGE, Risky1Y) ~ 1, data = thor.non.smoker, dist="weibull")
non.smokers.lambda.weibull.fit = exp(-survivor.non.smoker.kmeier.weibull$coefficients)
non.smokers.alpha = 1/survivor.non.smoker.kmeier.weibull$scale
estimates.non.smokers.weibull = exp(-(non.smokers.lambda.weibull.fit*c(0:90))^alpha)


##plot
#pdf("WeibullKaplanGroups.pdf")
plot(estimates.non.smokers.weibull, col = "blue")
lines(estimates.smokers.weibull, col = "red")
lines(survivor.smoker.kmeier, col = "green")
lines(survivor.non.smoker.kmeier, col = "black")
legend("bottomleft", inset=.0001,
       legend =  c("Weibull Smokers", "Weibull Non-Smokers", "Kaplan-Meier Smokers", "Kaplan-Meier Non-Smokers"),
       col =  c("blue", "red", "green", "black"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
title(main="nonparametric & parametric estimators \n of the survivor function by group")

