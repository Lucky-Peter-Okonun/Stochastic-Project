setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")

install.packages("pls")

library(Matrix)
library(pls)
library(tidyverse)

#Question(7a)
users = read.csv("users.csv")
likes = read.csv("likes.csv")
users.likes = read.csv("users-likes.csv")

#Matching positions of "users" and "likes"
position.users = match(users.likes$userid, users$userid)
position.likes = match(users.likes$likeid, likes$likeid)

#Adding the new columns to "users.likes"
users.likes = cbind(users.likes, position.users, position.likes)

#Build a matrix UL with 1 at position i; j, if user i made Like j.
UL = sparseMatrix(i = users.likes$position.users, j = users.likes$position.likes, x = 1)

#Setting IDs and column names to users and names of likes respectively
rownames(UL) = users$userid
colnames(UL) = likes$name

#We create another matrix such that rowSums(UL) >= 80 and colSums(UL) >= 150 with the following prep
repeat {
  i = sum(dim(UL))
  UL = UL[rowSums(UL) >= 80, colSums(UL) >= 150]
  if (sum(dim(UL)) == i) break
}

users = users[match(rownames(UL),users$userid), ]
likes = likes[match(colnames(UL),likes$likeid), ]

#We now convert UL into a matrix
users.likes.UL = as.matrix(UL)


#Question(b)
#We form train and test data sets by spliting the Matrix

set.seed(1122)  #For reproducibility

n = nrow(users.likes.UL)
id_split = sample(1:n, size = round((2/3)*n ))
train = users.likes.UL[id_split, ]
test = users.likes.UL[-id_split, ]

training.age = users$age[id_split]
test.age = users$age[-id_split]

#We model age as a dependent variable and with up to 50 PLS components using the training dataset
model.1 = plsr(training.age ~ train, ncomp = 50)

#We now predict using the test dataset
age.predict.1 = predict(model.1, newdata = test)

#We compare the predicted value with the actual values here, calculating the Pearson Corr Coeff
compare = 0
pb = txtProgressBar(min = 0, max = 50, style = 3)
for (i in 1:50) {
  compare[i] = cor(age.predict.1[,1,i], test.age, method = "pearson")
  setTxtProgressBar(pb, i)
}
close(pb)

#?compare

#Plot of the Pearson correlation coeffcients against model dimension
plot(compare)


#We find the PLS model dimension, dopt that corresponds to the maximal Pearson correlation
d.opt = which(compare == max(compare))
d.opt


#Plot of the predicted values of dimension dopt against corresponding age values from the test
age.predicted = age.predict.1[ , ,d.opt]
df = data.frame(age.predicted, test.age)
g1 = ggplot(df, aes(x = age.predicted, y =  test.age)) +
  geom_point() +
  geom_abline( aes(intercept=0, slope=1, color = 'Identity \nline'), size = 1) +
  scale_colour_manual(name = " ", values = c("maroon")) +
  theme_classic(base_size = 12) +
  ggtitle("Plot of the Predicted Values of Dimension d_opt Against the Corresponding Ages from Test Set") +
  xlab("Predicted Age") + ylab("Actual Age") +
  expand_limits(x = 0, y = 0) #Enforce the origin at 0
g1

#Question (7c)
#With optimal dimension, we obtain the 6 likes that had largest positive, and negative effects on the age, 
positive.effect = tail(sort(model.1$coefficients[ ,1 , d.opt]), 6)
negative.effect = head(sort(model.1$coefficients[ ,1 , d.opt]), 6)

#Question (7d)
#We repeat the above procedures for users with <60 Likes Users and likes with < 120 users

users = read.csv("users.csv")
likes = read.csv("likes.csv")
users.likes = read.csv("users-likes.csv")

#Matching positions of "users" and "likes"
position.users = match(users.likes$userid, users$userid)
position.likes = match(users.likes$likeid, likes$likeid)

#Adding the new columns to "users.likes"
users.likes = cbind(users.likes, position.users, position.likes)

#Build a matrix with 1 at position i; j, if user i made Like j.
UL2= sparseMatrix(i = users.likes$position.users, j = users.likes$position.likes, x = 1)

#Setting IDs and column names to users and names of likes respectively
rownames(UL2) = users$userid
colnames(UL2) = likes$name

#We create another matrix such that rowSums(UL2) >= 60 and colSums(UL2) >= 120 with the following prep
repeat {
  i = sum(dim(UL2))
  UL2 = UL2[rowSums(UL2) >= 60, colSums(UL2) >= 120]
  if (sum(dim(UL2)) == i) break
}

users = users[match(rownames(UL2), users$userid), ]
likes = likes[match(colnames(UL2), likes$likeid), ]

#We now convert UL2 into a matrix
users.likes.UL2 = as.matrix(UL2)



#Question(7d.b)
#We form train and test data sets by spliting the Matrix

set.seed(1122)  #For reproducibility

m = nrow(users.likes.UL2)
id_split2 = sample(1:m, size = round((2/3)*m ))
train2 = users.likes.UL2[id_split2, ]
test2 = users.likes.UL2[-id_split2, ]

training.age2 = users$age[id_split2]
test.age2 = users$age[-id_split2]

#We model age as dependent variable and with up to 50 PLS components using the training dataset
model.2 = plsr(training.age2 ~ train2, ncomp = 50)


#We now predict using the test dataset
age.predict.2 = predict(model.2, newdata = test2)

#We compare the predicted value with the actual values here, calculating the Pearson Corr Coeff
compare2 = 0
pb = txtProgressBar(min = 0, max = 50, style = 3)
for (i in 1:50) {
  compare2[i] = cor(age.predict.2[,1,i], test.age2, method = "pearson")
  setTxtProgressBar(pb, i)
}
close(pb)

pdf("7di.pdf")
#Plot of the Pearson correlation coeffcients against model dimension
plot(compare2)


#We find the PLS model dimension, dopt that corresponds to the maximal Pearson correlation
d.opt2 = which(compare2 == max(compare2))
d.opt2

pdf("7dii.pdf")
#Plot of the predicted values of dimension dopt against corresponding age values from the test
age.predicted2 = age.predict.2[ , ,d.opt2]
df2 = data.frame(age.predicted2, test.age2)
g2 = ggplot(df2, aes(x = age.predicted2, y =  test.age2)) +
  geom_point() +
  geom_abline( aes(intercept=0, slope=1, color = 'Identity \nline'), size = 1) +
  scale_colour_manual(name = " ", values = c("blue")) +
  theme_classic(base_size = 12) +
  ggtitle("Plot of the Predicted Values of Dimension d_opt2 Against the Corresponding Ages from Test Set") +
  xlab("Predicted Age") + ylab("Actual Age") +
  expand_limits(x = 0, y = 0) #Enforce the origin at 0
g2
#dev.off()

#Question (7c)
#With optimal dimension, we obtain the 6 likes that had largest positive, and negative effects on the age, 
positive.effect2 = tail(sort(model.2$coefficients[ ,1 , d.opt2]), 6)
negative.effect2 = head(sort(model.2$coefficients[ ,1 , d.opt2]), 6)