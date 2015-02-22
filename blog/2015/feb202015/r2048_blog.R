


devtools::source_url('http://darrkj.github.io/home/rcode/r2048.R')


x <- c()
for (i in seq(300)) {
  if (i %% 100 == 0) print(i)
  x <- c(x, r2048())
}

table(x)











#auto_play(board)

library(profr)

Rprof("2048.out")
x <- c()
#t <- c()
for (i in seq(50)) {
  print(i)
  #t <- c(t, system.time(
  x <- c(x, auto_play(gen_board(), c(.25, .25, .25, .25)) )
#[3])
}
Rprof(NULL)
summaryRprof('2048.out')[2]


library(profr)

Rprof("2048.out")
b <- c()
runs <- 10
  for (i in seq(runs)) {
    #if (i %% 100 == 0) 
    print(i)
    b <- c(b, auto_play(gen_board(), c(.25, .25, .25, .25)) )
    b <- c(b, auto_play(gen_board(), c(.4, .01, .19, .4)) )
  }
Rprof(NULL)
summaryRprof('2048.out')[2]



