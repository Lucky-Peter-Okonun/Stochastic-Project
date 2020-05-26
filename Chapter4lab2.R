setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")


library(ggplot2)


#Question (4a)
#Implement kernel density estimation in a function that depends on the sample, bandwidth and a kernel
i.func <- function(x){ifelse((abs(x)<=1),1,0)} 

#kernels
uniform <- function(x){(1/2)*indicator.func(x)}
triangular <- function(x){(1-abs(x))*indicator.func(x)}
epanechnikov <- function(x){((3/4)*(1-x^2))*indicator.func(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x^2)/2))}

kernel.names <- list("uniform","gaussian", "epanechnikov", "triangular")
kernel.functions <- list(uniform, gaussian, epanechnikov, triangular)


#KDE = Kernel density estimator
KD.estimator <- function(sample, bw, kernel){                          
  kernel.type <- kernel.functions[[which(kernel.names == kernel)]]     
  f.x.h <- function(x){                                               
    ratio = (x - sample)/bw
    (1/(bw*length(sample)))*(sum(kernel.type(ratio)))
  }
  return(Vectorize(f.x.h))
}

#loading the data
student.performance.data <- read.csv("StudentsPerformance.csv") 
student.performance.data.analysis <- c("test.preparation.course", "math.score", "reading.score", "writing.score")
student.performance.data <- student.performance.data[student.performance.data.analysis]

#plot: diffrent bandwidth with epanechnikov kernel
for(i in c(2, 5, 8, 11)){
  kernel_epanechnikov = KD.estimator(student.performance.data$math.score, bw = i, kernel = "epanechnikov")
}

for(i in c(2, 5, 8, 11)){
  kernel_epanechnikov = density(student.performance.data$math.score, bw = i, kernel = "epanechnikov")
  plot(kernel_epanechnikov)
}
#plot: diffrent bandwidth with epanechnikov kernel
kernel_epanechnikov1 <- KD.estimator(sample <- student.performance.data$math.score, bw = 2, kernel <- "epanechnikov")
kernel_epanechnikov2 <- KD.estimator(sample <- student.performance.data$math.score, bw = 5, kernel <- "epanechnikov")
kernel_epanechnikov3 <- KD.estimator(sample <- student.performance.data$math.score, bw = 8, kernel <- "epanechnikov")
kernel_epanechnikov4 <- KD.estimator(sample <- student.performance.data$math.score, bw = 11, kernel <- "epanechnikov")



#Plots of math.score with Epanechnikov Kernel with 4 different Choices of Bandwidth
#pdf("KernelFig1.pdf")
kernel <- ggplot(student.performance.data, aes(x = math.score)) +
  labs(color = "Bandwidth") +
  ggtitle("Kernel Density Estimators of Different Bandwidth Sizes with Epanechnikov Kernel") +
  xlab("Mathematics Scores") + ylab("Density") +
  theme(legend.position=c(0.4,0.95))+
  scale_color_manual(values=c('#999999','#E69F00', "blue", "green")) +
  stat_function(fun = kernel_epanechnikov1, aes(color = "2"), size = 1.21) +
  stat_function(fun = kernel_epanechnikov2, aes(color = "5"), size = 1.21) +
  stat_function(fun = kernel_epanechnikov3, aes(color = "8"), size = 1.21) +
  stat_function(fun = kernel_epanechnikov4, aes(color = "11"), size = 1.21)
  kernel 


##Plots of math.score with Epanechnikov Kernel with 4 different Choices of Bandwidth
#pdf("FourKernels.pdf")
kernel.gaussian = KD.estimator(sample = student.performance.data$math.score, bw = 8, kernel = "gaussian")
kernel.epanechnikov = KD.estimator(sample = student.performance.data$math.score, bw = 8, kernel = "epanechnikov")
kernel.uniform = KD.estimator(sample = student.performance.data$math.score, bw = 8, kernel = "uniform")
kernel.triangular = KD.estimator(sample = student.performance.data$math.score, bw = 8, kernel = "triangular")

  kernels = ggplot(student.performance.data, aes(x = math.score)) +
  labs(color = "Kernel") +
  ggtitle("Kernel Density Estimators of Bandwidth 8 with Four(4) Kernel Types") +
  xlab("Mathematics Scores") + ylab("Density") +
  scale_color_manual(values=c('#999999','#E69F00', "blue", "green")) +
  stat_function(fun = kernel.gaussian, aes(color = "epanechnikov"), size = 1.21) +
  stat_function(fun = kernel.epanechnikov, aes(color = "Gaussian"), size = 1.21) +
  stat_function(fun = kernel.uniform, aes(color = "Triangular"), size = 1.21) +
  stat_function(fun = kernel.triangular, aes(color = "Uniform"), size = 1.21)
  kernels +theme(
  plot.title = element_text(color="red", size=10, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=9, face="bold"),
  axis.title.y = element_text(color="#993333", size=8, face="bold")
)

#Implement the cross-validation criterion to find the optimal bandwidth
#Question(4b)
math.score = student.performance.data$math.score
reading.score = student.performance.data$reading.score
writing.score = student.performance.data$writing.score

f.add <- 0
s.add <- 0
#We write the equation as a function 
cross.valid = function(sample, bw){
  n = length(sample)
  
  f.hat = KD.estimator(sample, bw, "epanechnikov")
  f.hat.sqr = function(x){
    f.hat(x)^2
    } 
  
  f.add = integrate(f.hat.sqr, lower = Inf, upper = Inf) 
  second.addend = (sum(gaussian(outer(sample, sample, FUN = "-")/bw)) - n*gaussian(0))*2/(n*(n-1)*bw)
  
  C = f.add$value - s.add
 return(C)
}


cross.valid.math.score = Vectorize(function(bw){cross.valid(math.score, bw)})
cross.valid.reading.score = Vectorize(function(bw){cross.valid(reading.score, bw)})
cross.valid.writing.score = Vectorize(function(bw){cross.valid(writing.score, bw)})

# Optimize objective function
cross.valid.math.score.optimize = optimize(cross.valid.math.score, interval = c(1, 20))$minimum
cross.valid.reading.score.optimize = optimize(cross.valid.reading.score, interval = c(1, 15))$minimum
cross.valid.writing.score.optimize = optimize(cross.valid.writing.score, interval = c(1, 20))$minimum



#with bw.ucv and bw.bcv
#bw.ucv
bw.bcv.math.score.optimize = bw.bcv(math.score, lower = 1, upper = 20)
bw.bcv.reading.score.optimize = bw.bcv(reading.score, lower = 1, upper = 15)
bw.bcv.writing.score.optimize = bw.bcv(writing.score, lower = 1, upper = 20)

#bw.bcv
bw.ucv.math.score.optimize = bw.ucv(math.score, lower = 1, upper = 20)
bw.ucv.reading.score.optimize = bw.ucv(reading.score, lower = 1, upper = 15)
bw.ucv.writing.score.optimize = bw.ucv(writing.score, lower = 1, upper = 20)



#Question(4c)
student.performance.complete = subset(student.performance.data, test.preparation.course == 'completed')
student.performance.none = subset(student.performance.data, test.preparation.course =='none')


##Comapring the densities of the three scores of students that completed but did not complete the prep course
math.completed = student.performance.complete$math.score
reading.completed = student.performance.complete$reading.score
writing.completed = student.performance.complete$writing.score

cross.valid.math.completed = Vectorize(function(bw){cross.valid(math.completed, bw)})
cross.valid.reading.completed = Vectorize(function(bw){cross.valid(reading.completed, bw)})
cross.valid.writing.completed = Vectorize(function(bw){cross.valid(writing.completed, bw)})


#optimal bandwidth for the group "completed"
cross.valid.math.completed.optimize = optimize(cross.valid.math.completed, interval = c(1, 20))$minimum
cross.valid.reading.completed.optimize = optimize(cross.valid.reading.completed, interval = c(1, 15))$minimum
cross.valid.writing.completed.optimize = optimize(cross.valid.writing.completed, interval = c(1, 20))$minimum

##With the computed optimal bandwidth, we compute now the KDE with the implemented function
subdata.complete.math = KD.estimator(sample = student.performance.complete$math.score, bw = cross.valid.math.completed.optimize, kernel = "gaussian")
subdata.complete.reading = KD.estimator(sample = student.performance.complete$reading.score, bw = cross.valid.reading.completed.optimize, kernel = "gaussian")
subdata.complete.writing = KD.estimator(sample = student.performance.complete$writing.score, bw = cross.valid.writing.completed.optimize, kernel = "gaussian")


math.none = student.performance.none$math.score
reading.none = student.performance.none$reading.score
writing.none = student.performance.none$writing.score

cross.valid.math.none = Vectorize(function(bw){cross.valid(math.none, bw)})
cross.valid.math.none = Vectorize(function(bw){cross.valid(reading.none, bw)})
cross.valid.writing.none = Vectorize(function(bw){cross.valid(writing.none, bw)})


#optimal bandwidth for the group "completed"
cross.valid.math.none.optimize = optimize(cross.valid.math.none, interval = c(1, 20))$minimum
cross.valid.reading.none.optimize = optimize(cross.valid.math.none, interval = c(1, 15))$minimum
cross.valid.writing.none.optimize = optimize(cross.valid.writing.none, interval = c(1, 20))$minimum


subdata.none.math = KD.estimator(sample = student.performance.none$math.score, bw = cross.valid.math.none.optimize, kernel = "gaussian")
subdata.none.reading = KD.estimator(sample = student.performance.none$reading.score, bw = cross.valid.reading.none.optimize, kernel = "gaussian")
subdata.none.writing = KD.estimator(sample = student.performance.none$writing.score, bw = cross.valid.writing.none.optimize, kernel = "gaussian")

#plots

##Math Scores
#pdf("MathematiScores.pdf")
ggplot(student.performance.data, aes(x = math.score)) + 
  ggtitle("Comparison of Graphical Densities of Maths Scores") +
  stat_function(fun = subdata.complete.math, aes(color = "Completed"), size = 0.5) +
  stat_function(fun = subdata.none.math, aes(color = "None"), size = 1) +
  scale_colour_manual(name="Preparation Course", values = c("red", "purple")) +
  theme(legend.position = c(0.4, 0.95),legend.justification = c("right", "top"), legend.title = element_text()) 




##Reading Scores
#pdf("ReadingScores.pdf")
ggplot(student.performance.data, aes(x = reading.score)) + 
  ggtitle("Comparison of Graphical Densities of Reading Scores") +
  stat_function(fun = subdata.complete.reading, aes(color = "Completed"), size = 0.5) +
  stat_function(fun = subdata.none.reading, aes(color = "None"), size = 1) +
  scale_colour_manual(name="Preparation Course", values = c("red", "purple")) +
  theme(legend.position = c(0.4, 0.95),legend.justification = c("right", "top"), legend.title = element_text())



##Writing Scores
#pdf("WritingScores.pdf")
ggplot(student.performance.data, aes(x = writing.score)) + 
  ggtitle("Comparison of Graphical Densities of Writing Scores") +
  stat_function(fun = subdata.complete.writing, aes(color = "Completed"), size = 0.5) +
  stat_function(fun = subdata.none.writing, aes(color = "None"), size = 1) +
  scale_colour_manual(name="Preparation Course", values = c("red", "purple")) +
  theme(legend.position = c(0.4, 0.95),legend.justification = c("right", "top"), legend.title = element_text()) 


