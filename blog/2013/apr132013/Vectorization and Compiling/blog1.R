# Using the compiler tp speed up functions
# 
#   This is my first blog post.  I have two intentions with
# creating this blog.  First, I recently read that you 
# should take some of your weaknesses and and spend more 
# on them in order to get better.  I think I could work on
# documentation, presentation, and commenting my code.
# I think that this blog gives me some room to work on all
# three.  It forces me to write some documentation around
# some of the various things that I find interesting, while
# also forcing me to think about how to present information
# and make it useful to others.  In doing this I will also 
# forced to work on better commenting.  The second reason 
# is to use it more as a reference, if I learn someething 
# useful or do something interesting it seems this is a great
# way to catalog it for myself. 
# 
# Sometimes writing code can in a matrix based language can
# be awkward.  There can be situations where you have to
# write code in a vectorised manner for it to be able to 
# preform in an efficient manner.  This can make it look 
# diferent as well as make you think differently.  In this
# post I want to show how this may not always be the case.

library(testthat)
library(rpart)
library(compiler)
# Read in the the adult data set
# Can be download at http://archive.ics.uci.edu/ml/datasets/Adult
# commas were added to make it a csv

adult.train <- read.csv("adulttrain.csv", na.strings="?")
# Change dichotomous dependent variable to binary
adult.train$y <- as.character(adult.train$y)
adult.train$y <- ifelse(adult.train$y == "<=50K", 0, 1)

size <- nrow(adult.train)

# Number of obs in train set
n <- ceiling(size * .75)
y <- 1:size
z <- sample(y)
# Partition to train and test set
train <- adult.train[z[1:n],]
test <- adult.train[z[(n+1):size],]
# Check that data was partitioned correctly
rhs <- nrow(train) + nrow(test)
lhs <- nrow(adult.train)
expect_that(lhs == rhs, is_true())

# Create decision tree model on training set
rmod <- rpart(factor(y) ~ ., 
              test,
              control = rpart.control(minsplit = 2, cp = 0.0007))

plot(rmod)
plotcp(rmod)
rmod <- prune(rmod, cp=0.0011)
plot(rmod)

# Evaluate model on test set
pred <- predict(rmod, newdata = train, type="prob")[,2]


# Simple check of accuracy
prd <- ifelse(pred > .5, 1, 0)
acc1 <- sum(train$y == prd) / length(prd)

# Most methods of evaluating how good a suervised model
# work depend on four parameters; # of true positives,
# # of true negatives, # of false positives and # of 
# false negatives.  The following function does this in
# a straightforward loop, not vectorized.

params1 <- function(a, p, cutoff=.5) {
  p1 <- ifelse(p > cutoff, 1, 0)
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  x <- rep(NA, length(p))
  for (i in 1:length(p)) {
    if (a[i] == 1 && p1[i] == 1) {
      tp <- tp + 1
    } else if (a[i] == 0 && p1[i] == 0) {
      tn <- tn + 1
    } else if (a[i] == 1 && p1[i] == 0) {
      fn <- fn + 1
    } else if (a[i] == 0 && p1[i] == 1) {
      fp <- fp + 1
    }
  }
  return(list(tp=tp, fp=fp, fn=fn, tn=tn))
}
# Look at how well we did.
tmp <- params1(train$y, pred)
acc2 <- (tmp$tp + tmp$tn) / (tmp$tp + tmp$tn + tmp$fp + tmp$fn)
# Check that function works like the other method of
# used to find the accuracy above.
expect_that(acc1 == acc2, is_true())

# Create function to calculate ROC (reciever operating curve)
# There are a lot of things that can be done to both of these
# and will be will get to that.
roc1 <- function(a, p, gran=.1) {
  s1 <- rep(NA, (1/gran)+1)
  s2 <- rep(NA, (1/gran)+1)
  j <- 1
  for (i in seq(0, 1, gran)) {
    x <- params1(a, p, cutoff=i) 
    s1[j] <- (x$tp / (x$tp + x$fn))
    s2[j] <- (x$fp / (x$fp + x$tn))
    j <- j + 1
  }
  return(cbind(s2, s1))
}

r <- roc1(train$y, pred)
plot(r[,1], r[,2])

# How long does this take
system.time(roc1(train$y, pred, gran=.1))
system.time(roc1(train$y, pred, gran=.01))
# Kind of slow, lets get a better estimate
avgTime1a <- 0
avgTime01a <- 0
for (n in 1:10) {
  avgTime1a <- avgTime1a + system.time(roc1(train$y, pred, gran=.1))[3]
  avgTime01a <- avgTime01a + system.time(roc1(train$y, pred, gran=.01))[3]
}
Time10a <- avgTime1a/10
Time100a <- avgTime01a/10
# Not a rigorous asymptotoc analysis but we can that is was 
# 10 to 100 calls of params and the time went from 1.504 to 13.292
# both almost seen a 10x jump.
# I dont want to wait, for ten obs but one may be useful, I'll
# grab a cup of coffee.
Time1000a <- system.time(roc1(train$y, pred, gran=.001))[3]
# Okay, I got 137.65 seems to be linear still

# Can we make this faster? roc does not follow all the idioms for 
# fast R code. 
# It does preallocate s1 and s2, but it falls short in having a for
# loop and using cbind instead of preallocation.  This loop is small
# though, and the cbind only happens once.  cbind(or rbind) can
# be expensive if it is done in a loop, and loop are expensive in 
# general. I have a good gues as to what is making it slow.  Maybe
# I will use some actual profiling to determine where the hot spots are.
# Lets revise the heart of the roc function though, params1.  Basically 
# all it does is call params1, so lets start there. Nothing is growing 
# or being appended, but there is aone big for loop. It is being used to
# sum the type of each observation. Lets vectorize this.

tpos <- params1(train$y, pred, .5)$tp
# We have 3490 true positives at a cutoff of .5.
binPred <- ifelse(pred > .5, 1, 0)
# These are the actual predicted classes with that threshold.
prd <- ifelse(pred > .5, 1, 0)
acc1 <- sum(train$y == prd) / length(prd)
# We did this calculation earlier which found all cases were the 
# actual was equal to the predicted in a vectorized manner. We can 
# take a similar action on each case since they are binary.
tpos1 <- sum(train$y & prd)
expect_that(tpos1 == tpos, is_true())
# It worked, now we can apply binary negations to calculate all of
# the rest of the cases.

# Create vectorized version of the function.
params2 <- function(a, p, cutoff=.5) {
  p1 <- ifelse(p > cutoff, 1, 0)
  tp <- sum(a & p1)
  tn <- sum(!a & !p1)
  fp <- sum(!a & p1)
  fn <- sum(a & !p1)
  
  return(list(tp=tp, fp=fp, fn=fn, tn=tn))
}

roc2 <- function(a, p, gran=.1) {
  s1 <- rep(NA, (1/gran)+1)
  s2 <- rep(NA, (1/gran)+1)
  j <- 1
  for (i in seq(0, 1, gran)) {
    x <- params2(a, p, cutoff=i) 
    s1[j] <- (x$tp / (x$tp + x$fn))
    s2[j] <- (x$fp / (x$fp + x$tn))
    j <- j + 1
  }
  return(cbind(s2, s1))
}

# How long does this take
system.time(roc2(train$y, pred, gran=.1))
system.time(roc2(train$y, pred, gran=.01))
# It already seems much faster
avgTime1b <- 0
avgTime01b <- 0
for (n in 1:10) {
  avgTime1b <- avgTime1b + system.time(roc2(train$y, pred, gran=.1))[3]
  avgTime01b <- avgTime01b + system.time(roc2(train$y, pred, gran=.01))[3]
}
Time10b <- avgTime1b/10
Time100b <- avgTime01b/10
# It looks like we have a 10x gain in speed
Time1000b <- system.time(roc2(train$y, pred, gran=.001))[3]
# Yep, now our for loop in roc could be hindering us.
# This could is odd though, especially if you arenot used to R,
# or come from a Matlab/ocatve background.  This can be very non-intuitive
# for anyone coming from something like c++ or java and are using
# R for modeling purposes.  It can also be wierd for those new to R but using
# coming from a background like SAS.

# The compiler library which is newer to the R framework may help.
# It could allow you to write code that is not vectorized but still
# get speed improvements.
params3 <- cmpfun(params1)

roc3 <- function(a, p, gran=.1) {
  s1 <- rep(NA, (1/gran)+1)
  s2 <- rep(NA, (1/gran)+1)
  j <- 1
  for (i in seq(0, 1, gran)) {
    x <- params3(a, p, cutoff=i) 
    s1[j] <- (x$tp / (x$tp + x$fn))
    s2[j] <- (x$fp / (x$fp + x$tn))
    j <- j + 1
  }
  return(cbind(s2, s1))
}

# How long does this take
system.time(roc3(train$y, pred, gran=.1))
system.time(roc3(train$y, pred, gran=.01))
# It already seems much faster
avgTime1c <- 0
avgTime01c <- 0
for (n in 1:10) {
  avgTime1c <- avgTime1c + system.time(roc3(train$y, pred, gran=.1))[3]
  avgTime01c <- avgTime01c + system.time(roc3(train$y, pred, gran=.01))[3]
}
Time10c <- avgTime1c/10
Time100c <- avgTime01c/10
# It looks like we have almost 3x gain in speed, than the 
# baseline method, but still slower than the vectorized version.
Time1000c <- system.time(roc3(train$y, pred, gran=.001))[3]
Time1000c

lst <- c(Time10a, Time100a, Time1000a,
         Time10b, Time100b, Time1000b,
         Time10c, Time100c, Time1000c)

times <- matrix(lst, nrow = 3, ncol=3, byrow = FALSE,
                dimnames = list(
                  c("10 Iters", "100 Iters", "1000 Iters"),
                  c("Baseline", "Vectorized", "Compiled")))
times
