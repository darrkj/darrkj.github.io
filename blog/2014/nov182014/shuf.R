
# Don't do dumb things with strings
options(stringsAsFactors = FALSE)

# Load libraries
library(httr)
library(XML)
library(devtools)
library(stringr)
library(lubridate)
library(dplyr)
library(rvest)
library(igraph)
library(RNeo4j)
library(devtools)
library(PlayerRatings)
library(randomForest)
library(ROCR)

graph = startGraph("http://localhost:7474/db/data/")

q <- '
MATCH (a)-[r:fought]->(b)-[s:fought]->(a)
WHERE r.result = "Win" and r.date = s.date
RETURN a.name as name, a.id as id, r.result as result, r.date as date, 
       r.exp as exp, s.exp as oexp, b.id as oid
UNION ALL
MATCH (a)-[r:fought]->(b)-[s:fought]->(a)
WHERE r.result = "Loss" and r.date = s.date
RETURN a.name as name, a.id as id, r.result as result, r.date as date, 
       r.exp as exp, s.exp as oexp, b.id as oid;'

f <- unique(cypher(graph, q))


load('pageRank.RData')



p1 <- pageRank[, c('id', 'oid', 'date', 'cl', 'bet', 'eig', 'hub', 'auth', 'page')]
p2 <- pageRank[, c('oid', 'id', 'date', 'ocl', 'obet', 'oeig', 'ohub', 'oauth', 'opage')]
names(p2) <- c('id', 'oid', 'date', 'cl', 'bet', 'eig', 'hub', 'auth', 'page')

p <- rbind(p1, p2)
p$date <- as.character(p$date)


f2 <- merge(f, p, all.x = T)

names(p) <- c('oid', 'id', 'date', 'ocl', 'obet', 'oeig', 'ohub', 'oauth', 'opage')

f2 <- merge(f2, p, all.x = T)
f2 <- f2[order(f2$date), ]

f3 <- f2[, c('date', "result", "exp", "oexp", "cl", "bet", "eig", "hub", "auth", 
             "page", "ocl", "obet", "oeig", "ohub", "oauth", "opage")]


f3$y <- ifelse(f3$result == 'Win', 1, 0)
f3$result <- NULL



f3[is.na(f3)] <- 0

f3 %>%
  mutate(expDiff = exp - oexp, clDiff = cl - ocl, betDiff = bet - obet,
         eigDiff = eig - oeig, hubDiff = hub - ohub, authDiff = auth - oauth,
         pageDiff = page - opage) -> f3


f3 <- f3[order(f3$date), ]


fights <- f3


# Create the target shuffle function
shuffle <- function(target, ...) {
  return(target[sample(length(target), ...)])
}


# This function will replicate the modelling process. 
model <- function(data, shuff = TRUE) {
  # This will shuffle the target variable in the training set.
  if (shuff) data$y <- shuffle(data$y)
  id <- sample(1:nrow(data), .7 * nrow(data))
  train <- data[id, -1]
  test <- data[setdiff(1:nrow(data), id), -1]

  # Train a decision tree model on the training set
  rmod <- randomForest(y ~ ., data = train, ntree = 20)
  # Evaluate model on test set
  pred <- as.numeric(predict(rmod, newdata = test, type = "response"))
  # Simple check of accuracy
  sum(test$y == round(pred)) / length(pred)
}

# Number of iterations for each case.
its <- 20

# Run the model on the correct target
Accuracy <- model(fights, shuff = FALSE)

# Iterate the above and shuffle the target.
for(i in seq(its)) {
  # Append new accuracy value to list.
  Accuracy <- c(Accuracy, model(fights))
  print(i)
}
  





mod2 <- randomForest(y ~ ., data = train, ntree = 400, do.trace = 1)
eval <- as.numeric(predict(mod, newdata = test, type = 'response'))

xp <- prediction(eval, test$y)
performance(xp, 'auc')

pp <- performance(xp, "tpr","fpr")
plot(pp)
points(seq(0, 1, .01), seq(0, 1, .01), type = 'l')





stats <- function(valueList) {
  # What is the accuracy in the case of no shuffling.
  real <- valueList[1]
  cat("Accuracy in the case of no shuffling", real, "\n")
  # The mean of the rest of the cases.
  mean <- mean(valueList[2:(its+1)])
  cat("Mean accuracy in of cases with shuffling", mean, "\n")
  # The standard deviation of the rest of the cases.
  sd <- sd(valueList[2:(its+1)])
  cat("Standard Deviation of accuracy in cases with shuffling", sd, "\n")
  # Confidence interval.
  sdl <- mean - 2 * sd
  sdu <- mean + 2 * sd
  return(list(real = real, 
              mean = mean, 
              sdl = sdl, 
              sdu = sdu))
}

# Function is completely non-pure having no return, only 
# ran for side effect.
shuf.plot <- function(valueList, name, ...) {
  # Get result from running the model.
  vals <- stats(valueList)
  hist(valueList, main = name, ...)
  # Add a line at the real data value.
  abline(v = vals[1], col = "red", lwd = 2)
  # Add a line at the mean of the shuffled results.
  abline(v = vals[2], col = "green", lwd = 2)
  # Add lines for the upper and lower 95% conf limits.
  abline(v = vals[3], col = "green", lwd = 2, lty = 3)
  abline(v = vals[4], col = "green", lwd = 2, lty = 3)
}



# Run the modeling algorithm.
Accuracy1 <- runModel(f3[-1,])

Accuracy2 <- runModel(carData2)
par(mfrow = c(1, 2))
shuf.plot(Accuracy1, "Getting Punked", breaks = 40)
## Accuracy in the case of no shuffling 0.8762 
## Mean accuracy in of cases with shuffling 0.8769 
## Standard Deviation of accuracy in cases with shuffling 0.002084
shuf.plot(Accuracy2, "Something", breaks = 40)







