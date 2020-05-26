install.packages("tidyverse")
install.packages("haven")
library("tidyverse") 
library("NonpModelCheck")
library("haven")

#uploading data
kenya.children = read_dta("childrenfinal.dta")


#Question(5a)

#Implementing kernel density estimation in a function that depends on the sample, bandwidth and a kernel
i = function(x){ifelse((abs(x)<=1),1,0)}

#considering five kernels
uniform = function(x){0.5*i(x)}
triangular = function(x){(1-abs(x))*i(x)}
epanechnikov =function(x){(0.75*(1-x**2))*i(x)}
gaussian = function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}

kernel.names = list("uniform", "triangular", "epanechnikov", "gaussian")
kernel.functions = list(uniform, triangular, epanechnikov, gaussian)


#Function to fit Local Polynomial
local.polynomial.fit = function(Y, X, bw, l, kernel = "epanechnikov"){
  # Y = response variable, X = covariate, bw = bandwidth, l = degree of the polynomial
  # kernel = epanechnikov by default
  
  kernel = kernel.functions[[which(kernel.names == kernel)]]
  Y = as.matrix(Y)
  X = as.matrix(X)
  d = ncol(X)
  n = nrow(X)
  X_mat = matrix(0, d*n, l+1)
  
  A.hat = function(x, derivative){
    # derivative = f derivative approximated.
    first.term = (X - x)/bw
    second.term = as.vector(t(X - x))
    
    V = apply(first.term, MARGIN = 1, FUN = kernel)
    
    for (i in 0:l){
      X_mat[ ,i+1] = second.term^i
    }
  res = lm(Y ~ X_mat - 1, weights = V) # weighted least squares (exclude intercept)
  res = res$coefficients
  res = factorial(derivative)*res[derivative + 1]
  return(res)
  }
  VA.hat = Vectorize(A.hat, vectorize.args = "x")
  return(VA.hat)
}

#Now we estimate f with ploynomial degree of 1, and four bandwidths
fit.1 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 2, l = 1)
fit.2 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 5, l = 1)
fit.3 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 8, l = 1)
fit.4 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 11, l = 1)

#Fitting the domain of hypage = 0:59
domain.fit.1 = fit.1(x = 0:59, derivative = 0)
domain.fit.2 = fit.2(x = 0:59, derivative = 0)
domain.fit.3 = fit.3(x = 0:59, derivative = 0)
domain.fit.4 = fit.4(x = 0:59, derivative = 0)

#creating dataframes for the purpose of plotting
df1 = data.frame(a = 0:59, b = domain.fit.1)
df2 = data.frame(a = 0:59, b = domain.fit.2)
df3 = data.frame(a = 0:59, b = domain.fit.3)
df4 = data.frame(a = 0:59, b = domain.fit.4)

zwast1= ggplot(kenya.children, aes(x = hypage, y = zwast)) +
  geom_point(color = "black") +
  geom_line(data = df1, aes(x = a, y = b, color = "2"), size = 1) +
  geom_line(data = df2, aes(x = a, y = b, color = "5"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "8"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "11"), linetype = "dashed", size = 1) +
  labs(color = "Bandwidth")  +
  xlab("Hypage") + ylab("Zwast") +
  ggtitle("Polynomial Fit for Estimating Zwast with Four Different Bandwidths")
zwast1


#With a reasonable bandwidth of 8, we now estimate f with 4 kernels while fixing the polynomial degree to 1
fit.5 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 8, l = 1, kernel = "epanechnikov")
fit.6 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 8, l = 1, kernel = "uniform")
fit.7 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 8, l = 1, kernel = "triangular")
fit.8 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = 8, l = 1, kernel = "gaussian")

#Fitting the domain of hypage = 0:59
domain.fit.5 = fit.5(x = 0:59, derivative = 0)
domain.fit.6 = fit.6(x = 0:59, derivative = 0)
domain.fit.7 = fit.7(x = 0:59, derivative = 0)
domain.fit.8 = fit.8(x = 0:59, derivative = 0)

#creating dataframes for the purpose of plotting
df5 = data.frame(a = 0:59, b = domain.fit.5)
df6 = data.frame(a = 0:59, b = domain.fit.6)
df7 = data.frame(a = 0:59, b = domain.fit.7)
df8 = data.frame(a = 0:59, b = domain.fit.8)

zwast2= ggplot(kenya.children, aes(x = hypage, y = zwast)) +
  geom_point(color = "black") +
  geom_line(data = df5, aes(x = a, y = b, color = "Epanechnikov"), size = 1) +
  geom_line(data = df6, aes(x = a, y = b, color = "Uniform"), size = 1) +
  geom_line(data = df7, aes(x = a, y = b, color = "Triangular"), size = 1) +
  geom_line(data = df8, aes(x = a, y = b, color = "Gaussian"),  size = 0.5) +
  labs(color = "Kernel") + labs(color = "Kernel")  +
  xlab("Hypage") + ylab("Zwast") +
  ggtitle("Polynomial Fit for Estimating Zwast with Four Different Kernels")
zwast2

#Question (5b)

#A function that calculates optimal bandwidth with Generalised Cross Validation (GCV)
GCV = function(Y, X, bw, l){
  # Y = covariate, X = response variable, bw = bandwidth, l = polynomial degree
  X_values = unique(sort(X))
  n = length(Y)
  X_mat_list = list()   #Computating the matrices X(x_i) in the formula for estimating A_hat
  for (i in X_values){
    index = which(X_values == i)
    X_mat_list[[index]] = matrix(0, n, 4+1)
    aux = (X - i)
    for (j in 0:4){
      X_mat_list[[index]][, j+1] = aux**j
    }
  }
  
  poly_fit = local.polynomial.fit(Y, X, bw, l)
  
  
  sum_square = rep(0, length(X_values)) #To obtain the MSE of Y vs fitted values of X
  W_trace = matrix(0, length(X_values)) #To obtain the sum of trace values of weight function W
  
  #Now we want to calculate the components of W_trace and sum_square
  for (i in X_values){
    index = which(X_values == i)
    aux = (X - i)
    X_mat = X_mat_list[[index]][, 1:(l+1)]
    V = diag(epanechnikov(aux/bw))
    weight_vector = solve(t(X_mat) %*% V %*% X_mat) %*% t(X_mat) %*% V
    weight_vector = weight_vector[1, ]
    
    sum_square[index] = sum((Y[(X == i)] - poly_fit(i, deriv = 0))**2)
    W_trace[index] = sum((X == i) * weight_vector)
    
  }
  res = sum(sum_square)/(1 - sum(W_trace)/n)**2
  
  return(res)
  
}

#optimal bandwidth by GCV for polynomial degrees 1 to 4
Y = kenya.children$zwast
X = kenya.children$hypage
GCV1 = Vectorize(function(bw){GCV(Y, X, bw, l = 1)})
GCV2 = Vectorize(function(bw){GCV(Y, X, bw, l = 2)})
GCV3 = Vectorize(function(bw){GCV(Y, X, bw, l = 3)})
GCV4 = Vectorize(function(bw){GCV(Y, X, bw, l = 4)})

optimal.GCV1 = optimize(GCV1, interval = c(2, 11))$minimum
optimal.GCV2 = optimize(GCV2, interval = c(2, 11))$minimum
optimal.GCV3 = optimize(GCV3, interval = c(2, 11))$minimum
optimal.GCV4 = optimize(GCV4, interval = c(2, 11))$minimum
#optimal.GCV1= 4.999956
#optimal.GCV2= 10.99995
#optimal.GCV3= 10.99994
#optimal.GCV4= 8.403991

#Plot all four fits putting the curves on the same plot
# First, fits for each polynomial degree with the corresponding optimal bw
fit.9 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = optimal.GCV1, l = 1, kernel = "epanechnikov")
fit.10 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = optimal.GCV2, l = 2, kernel = "epanechnikov")
fit.11 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = optimal.GCV3, l = 3, kernel = "epanechnikov")
fit.12 = local.polynomial.fit(kenya.children$zwast, kenya.children$hypage, bw = optimal.GCV4, l = 4, kernel = "epanechnikov")

#Fitting the domain of hypage = 0:59
domain.fit.9 = fit.9(x = 0:59, derivative = 0)
domain.fit.10 = fit.10(x = 0:59, derivative = 0)
domain.fit.11 = fit.11(x = 0:59, derivative = 0)
domain.fit.12 = fit.12(x = 0:59, derivative = 0)

#creating dataframes for the purpose of plotting
df9 = data.frame(a = 0:59, b = domain.fit.9)
df10 = data.frame(a = 0:59, b = domain.fit.10)
df11 = data.frame(a = 0:59, b = domain.fit.11)
df12 = data.frame(a = 0:59, b = domain.fit.12)

zwast3= ggplot(kenya.children, aes(x = hypage, y = zwast)) +
  geom_point(color = "black") +
  geom_line(data = df9, aes(x = a, y = b, color = "1")) +
  geom_line(data = df10, aes(x = a, y = b, color = "2")) +
  geom_line(data = df11, aes(x = a, y = b, color = "3")) +
  geom_line(data = df12, aes(x = a, y = b, color = "4")) +
  labs(color = "Polynomial\nDegrees") +
  xlab("Hypage") + ylab("Zwast") +
  ggtitle("Polynomial Fit with Optimal Bandwidth")
zwast3


#Question(5c)

#Calculating the first derivatives of the function of zwast with the GCV-bandwidth
fit.derivative.1 = localpoly.reg(X, Y, bandwidth = 4.999956, degree.pol = 1, deriv = 1)
fit.derivative.2 = localpoly.reg(X, Y, bandwidth = 10.99995, degree.pol = 2, deriv = 1)
fit.derivative.3 = localpoly.reg(X, Y, bandwidth = 10.99994, degree.pol = 3, deriv = 1)
fit.derivative.4 = localpoly.reg(X, Y, bandwidth = 8.403991, degree.pol = 4, deriv = 1)

#creating dataframes for the purpose of plotting
df13 = data.frame(a = unique(fit.derivative.1$x), b = unique(fit.derivative.1$predict))
df14 = data.frame(a = unique(fit.derivative.2$x), b = unique(fit.derivative.2$predict))
df15 = data.frame(a = unique(fit.derivative.3$x), b = unique(fit.derivative.3$predict))
df16 = data.frame(a = unique(fit.derivative.4$x), b = unique(fit.derivative.4$predict))

zwast4= ggplot(kenya.children, aes(x = hypage, y = zwast)) +
  geom_point(color = "black") +
  geom_line(data = df13, aes(x = a, y = b, color = "1")) +
  geom_line(data = df14, aes(x = a, y = b, color = "2")) +
  geom_line(data = df15, aes(x = a, y = b, color = "3")) +
  geom_line(data = df16, aes(x = a, y = b, color = "4")) +
  #ylim(-1, 1) +
  #xlim(2, 59)+
  labs(color = "Polynomial \n Degrees") +
  xlab("Hypage") + ylab("Zwast") +
  ggtitle("Ploynomial Derivative Fits")
zwast4