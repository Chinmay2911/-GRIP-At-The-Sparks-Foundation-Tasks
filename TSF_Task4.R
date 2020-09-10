#Install and load rpart package
#install.packages("rpart")
#install.packages("rpart.plot")

library("rpart")
library("rpart.plot")
data("iris")
str(iris)

# We split out entire dataset into two parts - the training set and the testing set.
# We train a machine learning algorithm with the training data, and then test our model using the testing data.
indexes = sample(150, 110)
iris_train = iris[indexes,]
iris_test = iris[-indexes,]

#we're trying to build a model that tries to classify (classification tree) a test data point, into one of the three Species classes - i.e. setosa, virginica or versicolor.
target = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

#Building and plotting model
tree = rpart(target, data = iris_train, method = "class")
rpart.plot(tree)
predictions = predict(tree, iris_test)

#Party package has good plotting facilities for conditional trees.
#install.packages("party")
library(party)
tree = ctree(Species ~ ., data = iris)
plot(tree, main="Conditional Inference Tree for Iris")
table(predict(tree), iris$Species)

#Applying boosting method applied to the iris dataset.
install.packages("C50")
library(C50)

# build model
tree = C5.0(Species ~ ., data = iris, trials=10)

# make predictions
table(predict(tree, newdata=iris), iris$Species)

tree_ms3 = rpart(target, iris_train, control = rpart.control(minsplit = 3))
tree_ms10 = rpart(target, iris_train, control = rpart.control(minsplit = 10))

par(mfcol = c(1, 2))
rpart.plot(tree_ms3, main = "minsplit=3")
text(tree_ms3, cex = 0.7)
rpart.plot(tree_ms10, main = "minsplit=10")
text(tree_ms10, cex = 0.7)