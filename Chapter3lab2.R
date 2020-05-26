setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")
install.packages("JoSAE")
install.packages("mapdata")
install.packages("maps")
install.packages("lme4")

library(JoSAE)
library(mapdata)
library(maps)
library(maptools)
library(nlme)
library(lme4)
library(tidyverse)
library(Matrix)
library(rgdal)
library(ggrepel)


#Question(3a)
#Loading the data
my.data <- data(landsat)
summary(landsat)
head(landsat)
which(!complete.cases(landsat)) 
##Creating groupedData
corn.group <- groupedData(HACorn ~ PixelsCorn | CountyName, data = landsat)
soy.group <- groupedData(HASoybeans ~ PixelsSoybeans | CountyName, data = landsat)

#("PeterModelCorn.pdf")
plot(corn.group)


#("PeterModelSoy.pdf")
plot(soy.group)

# Make individual lm fits according to the grouped data
#pdf("PeterFitCorn.pdf")
corn.model = lmList(HACorn ~ PixelsCorn | CountyName, data = landsat)
corn.model
plot(corn.model)


#pdf("PeterFitSoy.pdf")
soy.model = lmList(HASoybeans ~ PixelsSoybeans | CountyName, data = landsat)
soy.model
plot(soy.model)


#Question(3b)
# Fit a linear mixed model for both crops such that segments share the same countywide random effect
corn.mixed.model <- lme(HACorn ~ PixelsCorn, data = corn.group, random = ~ 1)
corn.mixed.model
corn.beta <- corn.mixed.model$coefficients$fixed


soy.mixed.model <- lme(HASoybeans ~ PixelsSoybeans, data = soy.group, random = ~ 1)
soy.mixed.model
soy.beta <- soy.mixed.model$coefficients$fixed

#Question(3c)
#Mean of the population for explanatory variables
corn.mean <- unique(landsat$MeanPixelsCorn)
soy.mean <- unique(landsat$MeanPixelsSoybeans)

##Mean over the observed segments
segments.mean = aggregate(landsat[3:6], by = list(landsat$CountyName), mean)

##Number of observations in each county
number.observed.county <- plyr::count(landsat, "CountyName")
number.observed <- number.observed.county[, 2]
county.names <- number.observed.county[, 1]


#Model for corn and soy
corn.var.estimate <- VarCorr(corn.mixed.model)#estimated variance for corn
soy.var.estimate <- VarCorr(soy.mixed.model)#estimated variance for soy
corn.sigma.estimate <- as.numeric(corn.var.estimate[2])
soy.sigma.estimate <- as.numeric(soy.var.estimate[2])
sigma.rand.corn <- as.numeric(corn.var.estimate[1])
sigma.rand.soy <- as.numeric(soy.var.estimate[1])

#Calculate covariance matrix V.hat of beta.hat
corn.cov.matrix <- list()
soy.cov.matrix <- list()
for (i in 1:12){
  corn.cov.matrix[[i]] = matrix(sigma.rand.corn, number.observed[i], number.observed[i])
  soy.cov.matrix[[i]] = matrix(sigma.rand.soy, number.observed[i], number.observed[i])
}

##Calculating the parameters for computing the estimates
corn.I.matrix <- diag(x = corn.sigma.estimate, nrow = sum(number.observed), ncol = sum(number.observed))
soy.I.matrix <- diag(x = soy.sigma.estimate, nrow = sum(number.observed), ncol = sum(number.observed))
block.corn.cov.matrix <- bdiag(corn.cov.matrix)
block.soy.cov.matrix <- bdiag(soy.cov.matrix)
V.corn <- corn.I.matrix + block.corn.cov.matrix
V.soy <- soy.I.matrix + block.soy.cov.matrix

aux.corn = cbind(1, landsat$PixelsCorn)
aux.soy = cbind(1, landsat$PixelsSoybeans)
corn.V.hat <- solve(t(aux.corn) %*% solve(V.corn) %*% aux.corn)
soy.V.hat <- solve(t(aux.soy) %*% solve(V.corn) %*% aux.soy)

corn.gamma <- sigma.rand.corn/(sigma.rand.corn + corn.sigma.estimate/number.observed)
soy.gamma <- sigma.rand.soy/(sigma.rand.soy + soy.sigma.estimate/number.observed)



#Calculate the different predictors for mu
corn.reg.predictor = cbind(1, corn.mean) %*% corn.beta#Regression predictor for corn
soy.reg.predictor = cbind(1, soy.mean) %*% soy.beta#Regression predictor for soy

#Adjusted Survey Predictor for both groups
corn.adj.survey.predictor <- cbind(1, corn.mean) %*% corn.beta + (segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% corn.beta))
soy.adj.survey.predictor <- cbind(1, soy.mean) %*% soy.beta + (segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% soy.beta))
  

##Empirical BLUP for both groups
corn.BLUP <- cbind(1, corn.mean) %*% corn.beta + corn.gamma*(segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% corn.beta))
soy.BLUP <- cbind(1, soy.mean) %*% soy.beta + soy.gamma*(segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% soy.beta))
  

#Survey Predictor for both groups
corn.survey.predictor <- segments.mean$HACorn
soy.survey.predictor <- segments.mean$HASoybeans

#Save resulting predictors in a data frame
table.of.predictors <- data.frame(County = county.names, corn.reg.predictor = corn.reg.predictor,
                      soy.reg.predictor = soy.reg.predictor, corn.adj.survey.predictor = corn.adj.survey.predictor,
                      soy.adj.survey.predictor = soy.adj.survey.predictor, corn.BLUP = corn.BLUP,
                      soy.BLUP = soy.BLUP, corn.survey.predictor = corn.survey.predictor, 
                      soy.survey.predictor = soy.survey.predictor)
table.of.predictors
ggtitle("table of four for both crops")


#Estimate MSE
MSE.predictors <- function(d, crop){ 
  
  
  if (length(d) == 1){
    d = rep(d, 12)
  }
  if (crop == "corn"){
    sigma.estimate = corn.sigma.estimate
    sigma.rand = sigma.rand.corn
    gamma = corn.gamma
    crop.mean = corn.mean
    segments.mean = segments.mean$PixelsCorn
    V.hat = corn.V.hat
  } else {
    sigma.estimate = soy.sigma.estimate
    sigma.rand = sigma.rand.soy
    gamma = soy.gamma
    crop.mean = soy.mean
    segments.mean = segments.mean$PixelsSoybeans
    V.hat = soy.V.hat
  }
  
  res = rep(0, 12)
  for (i in 1:12){
    if (d[1] == 2){
      aux = (cbind(1, crop.mean[i]) - cbind(1, segments.mean[i]))
      
      res[i] = sigma.estimate/number.observed[i] + aux %*% V.hat %*% t(aux)
    } else {
      aux1 = (cbind(1, crop.mean[i]) - d[i]*cbind(1, segments.mean[i]))
      aux2 = cbind(1, segments.mean[i])
      
      term1 = (1 - d[i])^2*sigma.rand + d[i]^2*sigma.estimate/number.observed[i]
      term2 = 2*(d[i] - gamma[i])*aux1 %*% V.hat %*% t(aux2)
      term3 = aux1 %*% V.hat %*% t(aux1)
      
      res[i] = term1 + term2 + term3
    }
  }
  return(res)
}

corn.reg.predictor.MSE <- MSE.predictors(0, crop = "corn")
soy.reg.predictor.MSE <- MSE.predictors(0, crop = "soy")
corn.adj.survey.predictor.MSE <- MSE.predictors(1, crop = "corn")
soy.adj.survey.predictor.MSE <- MSE.predictors(1, crop = "soy")
corn.BLUP.MSE <- MSE.predictors(corn.gamma, crop = "corn")
soy.BLUP.MSE <- MSE.predictors(soy.gamma, crop = "soy")
corn.survey.predictor.MSE <- MSE.predictors(2, crop = "corn")
soy.survey.predictor.MSE <- MSE.predictors(2, crop = "soy")
#saving all results in dataframes
table.of.MSE <- data.frame(County = county.names,
                          corn.reg.predictor = corn.reg.predictor.MSE,
                          soy.reg.predictor = soy.reg.predictor.MSE,
                          corn.adj.survey.predictor = corn.adj.survey.predictor.MSE,
                          soy.adj.survey.predictor = soy.adj.survey.predictor.MSE,
                          corn.BLUP = corn.BLUP.MSE,
                          soy.BLUP = soy.BLUP.MSE,
                          corn.survey.predictor = corn.survey.predictor.MSE,
                          soy.survey.predictor = soy.survey.predictor.MSE)
table.of.MSE



#Question(d)
#Estimate the total county field size for both crops
corn.county.segments <- unique(landsat$MeanPixelsCorn)
soy.county.segments <- unique(landsat$MeanPixelsSoybeans)

#Estimates for total county field size
corn.BLUP.total <- corn.BLUP * corn.county.segments
soy.BLUP.total <- soy.BLUP * soy.county.segments
corn.survey.predictor.total <- corn.survey.predictor * corn.county.segments
soy.survey.predictor.total <- soy.survey.predictor * soy.county.segments



total.table = data.frame(County = county.names, 
                       BLUP.corn = corn.BLUP.total,
                       BLUP.soy = soy.BLUP.total,
                       Survey.corn = corn.survey.predictor.total,
                       Survey.soy = soy.survey.predictor.total)
total.table

###Plot the results onto a map of Iowa
states <- map_data("state")
iowa <- subset(states, region == "iowa")

counties <- map_data("county")
iowa.counties <- subset(counties, region == "iowa")

##Translate characters to lower case
lower.case <- tolower(as.character(county.names))


iowa.counties.polygon = select(iowa.counties, long, lat, subregion)
centroids <- aggregate(iowa.counties.polygon[,1:2], by=list(iowa.counties.polygon$subregion), FUN = mean)
centroids$County <- unique(iowa.counties.polygon[,3])
centroids <- filter(centroids, County %in% lower.case)


##Create label for the two crops
corn.label <- data.frame(total.BLUP = "BLUP:", BLUP = round(total.table$BLUP.corn, 0),
                         total.Survey = "Survey:", Survey = round(total.table$Survey.corn , 0))
corn.label <- data.frame(BLUP = paste("", corn.label$total.BLUP, "", corn.label$BLUP),
                        Survey = paste(corn.label$total.Survey, corn.label$Survey))
corn.label <- data.frame(Total_HA = paste("", corn.label$BLUP, "\n", corn.label$Survey))


soy.label <- data.frame(total.BLUP = "BLUP:", BLUP = round(total.table$BLUP.soy, 0),
                        total.Survey = "Survey:", Survey = round(total.table$Survey.soy, 0))
soy.label <- data.frame(BLUP = paste("", soy.label$total.BLUP, "", soy.label$BLUP),
                        Survey = paste(soy.label$total.Survey, soy.label$Survey))

soy.label <- data.frame(Total_HA = paste("", soy.label$BLUP, "\n", soy.label$Survey))

iowa.counties$fill_value <- 0
iowa.counties$fill_value[iowa.counties$subregion %in% lower.case] <- 1

##Map of Corn
#pdf("MapIowaCorn.pdf")
ggplot(data = iowa.counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "palegreen", color = "black") +
  geom_polygon(data = iowa.counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = corn.label$Total_HA),
                   size = 3.5, alpha = 0.7, point.padding = 1.3,
                   min.segment.length = 0, segment.size = 0.2) +
  ggtitle("Map of Iowa Indicating the Estimated Total County Field Size for Corn") +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  theme_void()


##Map of Soy
#pdf("MapIowaSoy.pdf")
ggplot(data = iowa.counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "palegreen", color = "black") +
  geom_polygon(data = iowa.counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "midnightblue", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = soy.label$Total_HA),
                   size = 3, alpha = 0.7, point.padding = 1.3,
                   min.segment.length = 0, segment.size = 0.2) +
  ggtitle("Map of Iowa Indicating the Estimated Total County Field Size for Soybeans") +
  scale_fill_manual(values = c("cornflowerblue", "orange")) +
  theme_void()
