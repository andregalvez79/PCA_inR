#packages
library(lattice)

# Load data
data(iris)
head(iris, 3)
summary(iris)
str(iris)

densityplot(iris $ Sepal.Length)
densityplot(iris $ Sepal.Width)
densityplot(iris $ Petal.Width)
densityplot(iris $ Petal.Length)


#transfrom the data
#not sure why but ok
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
summary(iris)
summary(log.ir)
densityplot(log.ir $ Sepal.Length)
densityplot(log.ir $ Sepal.Width)
densityplot(log.ir $ Petal.Width)
densityplot(log.ir $ Petal.Length)


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#prcomp is the default package already built in R
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
#results
ir.pca
summary(ir.pca)
# sqrt of eigenvalues
ir.pca$sdev

#too choose wich components to use:
#An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables 
#in standardized data. This is commonly used as a cutoff point for which PCs are retained. 
#This holds true only when the data are standardized.
#You can also limit the number of component to that number that accounts for a certain fraction of the total variance. 
#For example, if you are satisfied with 70% of the total variance explained then use the number
#of components to achieve that.

#The proportion of variance indicates how much of total variance is there in variance of a particular principal component.
#Hence, PC1 variability explains 73% of total variance of the data.

#Rotation values shown are same as 'loadings'or the linear combination weights (coefficients).
#so PC1 is a combination of .5 sepal lenght -.3 sepal width and so on. this should theoretically make sense.
#Considering rotations of PC1, one can conclude that Sepal.Length, Petal.Length and Petal.Width are directly related, 
#and they all are inversely related to Sepal.Width (which has a negative value in rotation of PC1)
#however sepal widht is much more heavily related to the PC2



#Just for illustration pretend the last two rows
#of the iris data has just arrived and we want to see what is their PCs values

#predcit (we know by the course on ML that we must not use the same data that we used to train for cross validation)
#but ofr the example ok

predict(ir.pca, 
        newdata=tail(log.ir, 2))


#plots
# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(iris[1:4], ir.pca$x))

# data frame with arrows coordinates
arrows = data.frame(x1 = c(0, 0, 0, 0), y1 = c(0, 0, 0, 0), x2 = correlations$PC1, 
                    y2 = correlations$PC2)

# geom_path will do open circles
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs", 
                                                                                                                           y = "pc2 axis") + ggtitle("Circle of correlations")
#another plot
# load ggplot2
library(ggplot2)

# create data frame with scores
scores = as.data.frame(ir.pca$x)
scores
# plot of observations
ggplot(data = scores, aes(x = PC1, y = PC2, label = iris$Species)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "black", alpha = 0.8, size = 4) +
  ggtitle("PCA plot of plants")

########### using other package
install.packages("caret")
library(caret)

#do all teh preporcessing in one go
trans = caret::preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])

summary(PC)
PC

# Loadings
trans$rotation
#autoimatically chooses the importnat components
#results slightly differe mainly becuae this fromula uses n-1 and the above uses n. it's technical
#but the interpreation holds the same

# other graphs correlations 
# Use pairs
pairs(iris[,1:4], col = iris$Species, upper.panel = NULL, pch = 16, cex = 1)
legend("topright", bty = "n", legend = c("setosa","versicolor","virginica"), pch = 16, 
       col = c("black","red","green"),xpd = T, cex = 1, y.intersp = 0.5)


