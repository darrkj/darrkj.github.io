
set.seed(3)
lot <- 80
load("~/Desktop/r/gwu/GWU.RData")

mod1 <- glm(IsBadBuy ~ Color, data = train, family = "binomial")

# Currently 1000 cars for sell
buy <- test[sample(nrow(test), 1000), ]


sum(buy$IsBadBuy)

# I run my model
res1 <- predict(mod1, newdata = buy, type = 'response')
# My cutoff
cutoff <- .12

# I buy the 100 best
res1 <- data.frame(p = res1, act = buy$IsBadBuy)
res1 <- res1[order(res1$p), ][1:lot, ]

# And I'm stuck with
sum(res1$act)
prop.test(c(sum(res1$act), nrow(res1)), 
          c(sum(buy$IsBadBuy), 1000), 
          alternative = 'less')$p.value


m1 <- c(pval(train, 'ORANGE'))

copy_train <- train
col <- levels(train$Color)
for (i in 1:1000) {
  copy_train$IsBadBuy <- sample(copy_train$IsBadBuy)
  ps <- sapply(col, function(x) pval(copy_train, x))
  m1 <- c(m1, min(ps))
  if(i %% 100 == 0) cat('*')
}

min(which(sort(m1) == m1[1])) / length(m1)


t.test(train[train$IsBadBuy == 0, ]$VehOdo, 
       train[train$IsBadBuy == 1, ]$VehOdo, 
       alternative = 'less')$p.value


pvals <- function(df) {
  t.test(df[df$IsBadBuy == 0, ]$VehOdo, df[df$IsBadBuy == 1, ]$VehOdo, alternative = 'less')$p.value
}


m2 <- c(pvals(train))


copy_train <- train
for (i in 1:1000) {
  copy_train$IsBadBuy <- sample(copy_train$IsBadBuy)
  ps <-  pvals(copy_train)
  m2 <- c(m2, min(ps))
}

min(which(sort(m2) == m2[1])) / length(m2)


mod2 <- glm(IsBadBuy ~ VehOdo, 
            data = train, family = "binomial")


# I run my model
res2 <- predict(mod2, newdata = buy, type = 'response')
# My cutoff
cutoff <- .12

# I buy the 100 best
res2 <- data.frame(p = res2, act = buy$IsBadBuy)
res2 <- res2[order(res2$p), ][1:lot, ]

# And I'm stuck with
sum(res2$act)



sum(buy$IsBadBuy)
sum(res1$act)
min(which(sort(m1) == m1[1])) / length(m1)
prop.test(c(sum(res1$act), nrow(res1)), c(sum(buy$IsBadBuy), 1000), 
          alternative = 'less')$p.value

sum(res2$act)
min(which(sort(m2) == m2[1])) / length(m2)
prop.test(c(sum(res2$act), nrow(res2)), c(sum(buy$IsBadBuy), 1000), 
          alternative = 'less')$p.value

