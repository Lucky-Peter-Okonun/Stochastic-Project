setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")
install.packages("splines")

library(tidyverse)
library(splines)
library(nlme)

#Question(6a)

stem.cell.data = read.table("stemcell.txt", col.names = "order")


#Implement a function that fits a regression spline for f
regression.spline = function(Y, X, k, deg){
  # Y = response, X = covariate, k = number of knots and deg, degree of spline = order - 1 of spline
  
  m = deg + 1 #order
  
  #Determine knots (as a vector) = equidistant partion of domain of X 
  knots = seq(min(X), max(X), length.out = k+2)
  knots = c(min(X) - (m-1):1 , knots, max(X) + 1:(m-1)) 

  N = spline.des(knots, X, ord = m)$design        #Basis matrix for B-splines
  
  #Calculating least squares of Y as fitted by N for beta
  beta = lm(Y ~ N - 1)$coefficients # We utilize -1 to expunge the intercept
  
  #Definition of regression spline now
  f.hat = function(x){
    n = spline.des(knots, x, ord = m,  outer.ok = TRUE)$design
    as.numeric(n %*% beta)
  }
  return(f.hat)
}

#Now we fix the spline degree to 2 and estimate f with four different number of knots
Y = stem.cell.data$order
X = seq(10, 1440, 10) # Measurements of the order parameter every 10 minutes for 24 hours

#Estimating splines for f for different number of knots
f.fit.knot1 = regression.spline(Y, X, 4, deg = 2)
f.fit.knot2 = regression.spline(Y, X, 15, deg = 2)
f.fit.knot3 = regression.spline(Y, X, 23, deg = 2)
f.fit.knot4 = regression.spline(Y, X, 30, deg = 2)

#We create dataframes for the plots here
Time = seq(10, 1440, 1)
df1 = data.frame(x = Time, y = f.fit.knot1(Time))
df2 = data.frame(x = Time, y = f.fit.knot2(Time))
df3 = data.frame(x = Time, y = f.fit.knot3(Time))
df4 = data.frame(x = Time, y = f.fit.knot4(Time))


spline1 = ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_line(data = df1, aes(color = "4")) +
  geom_line(data = df2, aes(color = "15")) +
  geom_line(data = df3, aes(color = "23")) +
  geom_line(data = df4, aes(color = "30")) +
  scale_colour_manual(name = "Number \nof \nKnots", values = c("maroon", "darkblue", "black", "yellow")) +
  xlab("Time(minutes)") + ylab("Order Parameter") +
  theme_classic(base_size = 12) +
  theme(legend.key = element_rect(fill = "white", colour = "gray")) + 
  ggtitle("Regression Spline with Degree 2 for Four Different Knots")
spline1


#Estimating f with number of knots as 4 and splines of degrees 1, 2, 3, 4
f.fit.deg1 = regression.spline(Y, X, 4, deg = 1)
f.fit.deg2 = regression.spline(Y, X, 4, deg = 2)
f.fit.deg3 = regression.spline(Y, X, 4, deg = 3)
f.fit.deg4 = regression.spline(Y, X, 4, deg = 4)

#We create dataframes for these 4 spline of degrees
df5 = data.frame(x = Time, y = f.fit.deg1(Time))
df6 = data.frame(x = Time, y = f.fit.deg2(Time))
df7 = data.frame(x = Time, y = f.fit.deg3(Time))
df8 = data.frame(x = Time, y = f.fit.deg4(Time))


spline2 = ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_line(data = df5, aes(color = "1")) +
  geom_line(data = df6, aes(color = "2")) +
  geom_line(data = df7, aes(color = "3")) +
  geom_line(data = df8, aes(color = "4")) +
  scale_colour_manual(name = "Degree", values = c("maroon", "darkblue", "black", "yellow")) +
  xlab("Time(minutes)") + ylab("Order Parameter") +
  theme_classic(base_size = 12) + 
  theme(legend.key = element_rect(fill = "white", colour = "gray")) +
  ggtitle("Regression Spline with Number of Knots 4 for Four Degrees of 1, 2, 3 & 4")
spline2


#Question(6b)

#A function that estimates the optimal number of equidistant knots with GCV
GCV = function(k, deg){
  f.fitted = regression.spline(Y, X, k, deg = deg)
  f.fitted = f.fitted(X)
  
  a = norm(Y - f.fitted, type = "2")
  n = length(Y)
  
  res = (a**2)/(1 - k/n)**2
  
  return(res)
}

#Vectorizing the GCV now
GCV = Vectorize(GCV, vectorize.args = c("k"))

#Optimise over k for degrees 1 to 4
max.k = 50

optimal.k.deg1 = which(GCV(1:max.k, deg = 1) == min(GCV(1:max.k, deg = 1)))
optimal.k.deg2 = which(GCV(1:max.k, deg = 2) == min(GCV(1:max.k, deg = 2)))
optimal.k.deg3 = which(GCV(1:max.k, deg = 3) == min(GCV(1:max.k, deg = 3)))
optimal.k.deg4 = which(GCV(1:max.k, deg = 4) == min(GCV(1:max.k, deg = 4)))

df.optimal.k = data.frame(deg1 = optimal.k.deg1, deg2 = optimal.k.deg2,
                       deg3 = optimal.k.deg3, deg4 = optimal.k.deg4)
row.names(df.optimal.k) = "Optimal GCV knot number"
df.optimal.k

#Calculating fits with GCV knots number, for degrees 1 to 4
f.fit.GCV1 = regression.spline(Y, X, k = optimal.k.deg1, deg = 1)
f.fit.GCV2 = regression.spline(Y, X, k = optimal.k.deg2, deg = 2)
f.fit.GCV3 = regression.spline(Y, X, k = optimal.k.deg3, deg = 3)
f.fit.GCV4 = regression.spline(Y, X, k = optimal.k.deg4, deg = 4)

#Dataframes to enable plotting of fits in one plot
df9 = data.frame(x = Time, y = f.fit.GCV1(Time))
df10 = data.frame(x = Time, y = f.fit.GCV2(Time))
df11 = data.frame(x = Time, y = f.fit.GCV3(Time))
df12 = data.frame(x = Time, y = f.fit.GCV4(Time))

spline3= ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_line(data = df9, aes(color = "1")) +
  geom_line(data = df10, aes(color = "2")) +
  geom_line(data = df11, aes(color = "3")) +
  geom_line(data = df12, aes(color = "4")) +
  scale_colour_manual(name = "Degree",  values = c("maroon", "darkblue", "black", "yellow")) +
  xlab("Time(minutes)") + ylab("Order Parameter") +
  theme_classic(base_size = 12) +
  theme(legend.key = element_rect(fill = "white", colour = "gray")) +
  ggtitle("Regression Splines with Number of Knots from GCV for Four Degrees of 1, 2, 3 & 4")
spline3


#Question(c)

#Now we update the regression spline's function to accommodate the autoregressive process 
#of order one

autoregressive.spline = function(Y, X, k, deg){
  #Y = response, X = covariate, k = number of knots, deg, degree of spline = order-1 of spline
  
  m = deg + 1 #order
  
  #Determine knot points (equidistant partion of domain of X)
  knots = seq(min(X), max(X), length.out = k+2)
  knots = c(min(X) - (m-1):1 , knots, max(X) + 1:(m-1))
  
  #Basis matrix of B-splines
  N = spline.des(knots, X, ord = m)$design
  
  # Implement autoregressive process of order 1 to the fit ("rescale the fit")
  a = 0.55
  n = length(Y)
  R = toeplitz(a**(0:(n-1)))
  R.inverse = solve(R)
  NRN = t(N) %*% R.inverse %*% N
  NRY = t(N) %*% R.inverse %*% Y
  M = solve(NRN) %*% NRY
  
  #Regression spline
  f.hat = function(x){
    n = spline.des(knots, x, ord = m,  outer.ok = TRUE)$design      #B-splines evaluated at x
    as.numeric(n %*% M)
  }
  return(f.hat)
}

#Now we update the GCV's function to accommodate the autoregressive process of order one
autoregressive.GCV = function(k, deg){
  f.fitted = autoregressive.spline(Y, X, k, deg = deg)
  f.fitted = f.fitted(X)
  
  a = 0.55
  n = length(Y)
  R = toeplitz(a**(0:(n-1)))
  R.inverse = solve(R)
  
  aux = t(Y - f.fitted) %*% R.inverse %*% (Y - f.fitted)
  n = length(Y)
  
  res = aux/(1 - k/n)**2
  
  return(res)
}
autoregressive.GCV = Vectorize(autoregressive.GCV, vectorize.args = c("k"))

# Optimise over k for degrees 1 to 4 for the updated model
optimal.k.deg1.AR = which(autoregressive.GCV(1:max.k, deg = 1) == min(autoregressive.GCV(1:max.k, deg = 1)))
optimal.k.deg2.AR = which(autoregressive.GCV(1:max.k, deg = 2) == min(autoregressive.GCV(1:max.k, deg = 2)))
optimal.k.deg3.AR = which(autoregressive.GCV(1:max.k, deg = 3) == min(autoregressive.GCV(1:max.k, deg = 3)))
optimal.k.deg4.AR = which(autoregressive.GCV(1:max.k, deg = 4) == min(autoregressive.GCV(1:max.k, deg = 4)))

df.optimal.k.AR = data.frame(deg1 = optimal.k.deg1.AR, deg2 = optimal.k.deg2.AR,
                            deg3 = optimal.k.deg3.AR, deg4 = optimal.k.deg4.AR)
row.names(df.optimal.k.AR) = "Optimal GCV knot number (autoregressive)"
df.optimal.k.AR

#Calculating the estimators with number of knots from the updated GCV for degrees 1 to 4
f.fit.autoregressive1 = autoregressive.spline(Y, X, k = optimal.k.deg1.AR, deg = 1)
f.fit.autoregressive2 = autoregressive.spline(Y, X, k = optimal.k.deg2.AR, deg = 2)
f.fit.autoregressive3 = autoregressive.spline(Y, X, k = optimal.k.deg3.AR, deg = 3)
f.fit.autoregressive4 = autoregressive.spline(Y, X, k = optimal.k.deg4.AR, deg = 4)

#We obtain datafraes to enable us plot 
df13 = data.frame(x = Time, y = f.fit.autoregressive1(Time))
df14 = data.frame(x = Time, y = f.fit.autoregressive2(Time))
df15 = data.frame(x = Time, y = f.fit.autoregressive3(Time))
df16 = data.frame(x = Time, y = f.fit.autoregressive4(Time))

spline4 = ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_line(data = df13, aes(color = "1")) +
  geom_line(data = df14, aes(color = "2")) +
  geom_line(data = df15, aes(color = "3")) +
  geom_line(data = df16, aes(color = "4")) +
  scale_colour_manual(name = "Degree", values = c("maroon", "darkblue", "black", "yellow")) +
  xlab("Time(minutes)") + ylab("Order Parameter") +
  theme_classic(base_size = 12) + 
  theme(legend.key = element_rect(fill = "white", colour = "gray")) +
  ggtitle("Regression Splines with Autoregressives Updates")
spline4


#Fitting parametric model of polynomial degree 4
model1 = gls(Y ~ X + I(X**2) + I(X**3) + I(X**4), correlation = corAR1(0.55))

polynomial.fit1 = function(x){ as.numeric(model1$coefficients %*% x**(0:4)) }
polynomial.fit1 = Vectorize(polynomial.fit1)

spline5 = ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_line(data = df13, aes(color = "Degree 1")) +
  geom_line(data = df14, aes(color = "Degree 2")) +
  geom_line(data = df15, aes(color = "Degree 3")) +
  geom_line(data = df16, aes(color = "Degree 4")) +
  stat_function(fun = polynomial.fit1, aes(color = "4")) +
  scale_colour_manual(name = "Parametric \nFit Degree 4", values = c("maroon", "darkblue", "black", "yellow", "green")) +
  xlab("Time(minutes)") + ylab("Order Parameter") +
  theme_classic(base_size = 12) + 
  theme(legend.key = element_rect(fill = "white", colour = "gray19")) +
  ggtitle("Model of Order Parameter of Polynomial Degree 4")
spline5

# For degrees 3 and 5
model2 = gls(Y ~ X + I(X**2) + I(X**3), correlation = corAR1(0.55))
model3 = gls(Y ~ X + I(X**2) + I(X**3) + I(X**4) + I(X**5), correlation = corAR1(0.55))

polynomial.fit2 = function(x){ as.numeric(model2$coefficients %*% x**(0:3)) }
polynomial.fit3 = function(x){ as.numeric(model3$coefficients %*% x**(0:5)) }
polynomial.fit2 = Vectorize(polynomial.fit2)
polynomial.fit3 = Vectorize(polynomial.fit3)


spline6 = ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  stat_function(fun = polynomial.fit1, aes(color = "4")) +
  stat_function(fun = polynomial.fit2, aes(color = "3")) +
  stat_function(fun = polynomial.fit3, aes(color = "5")) + 
  scale_colour_manual(name = "Parametric \nFit Degree", values = c("maroon", "darkblue", "yellow")) +
  xlab("Time(minutes)") +
  ylab("Order Parameter") +
  theme_classic(base_size = 12) + 
  theme(legend.key = element_rect(fill = "white", colour = "gray19")) +
  ggtitle("Model of Order Parameter of Polynomial Degrees 3, 4 & 5")
spline6
