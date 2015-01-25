
# Function to generate the game board
gen_board <- function() {
  start <- rep(0, 16)
  inits <- sample(1:16, 2)
  start[inits] <- 2
  matrix(start, 4, 4)
}

# This function will create a rather crude visual of the game board
# at the command line.
print_board <- function(b) {
  for(i in seq(4)) {
    for (j in seq(4)) {
      if (b[i, j] == 0) {
        cat('  .  ')
      } else {
        cat(' ', b[i, j], ' ')
      }
    }
    cat('\n')
  }
}


moves <- c('u', 'd', 'l', 'r')

# Is the given move valid
valid_move <- function(b, dir) {
  if (dir == 'd') any(diff(sign(b)) == -1)
  else if (dir == 'u') any(diff(sign(b)) == 1) 
  else if (dir == 'l') any(diff(sign(t(b))) == 1)
  else any(diff(sign(t(b))) == -1)
}



two_in_order <- function(x) {
  x[1] == x[2] & x[1] != 0 |
    x[2] == x[3] & x[2] != 0 |
    x[3] == x[4] & x[3] != 0
}


condense <- function(b, dir) {
  if (dir == 'd') any(apply(b, 2, two_in_order))
  else if (dir == 'u') any(apply(b, 2, two_in_order))   
  else if (dir == 'l') any(apply(b, 1, two_in_order))
  else any(apply(b, 1, two_in_order))
}



# If there are valid moves it is not game over.
not_game_over <- function(b) {
  valid_move(b, 'u') || valid_move(b, 'd') || 
    valid_move(b, 'l') || valid_move(b, 'r') ||
    condense(b, 'u') || condense(b, 'd') || 
    condense(b, 'l') || condense(b, 'r')
}




rotate <- function(x) t(apply(x, 2, rev))

ind <- function(x) {
  if (x[1] == x[2] & x[3] == x[4] & x[1] != 0 & x[3] != 0) {
    c(2*x[1], 2*x[3], 0, 0)
  } else if (x[1] == x[2] & x[1] != 0) {
    c(2*x[1], x[3], x[4], 0)
  } else if (x[2] == x[3] & x[2] != 0) {
    c(x[1], 2*x[2], x[4], 0)
  } else if (x[3] == x[4] & x[3] != 0) {
    c(x[1], x[2], 2*x[4], 0)
  } else x
}
library(compiler)
ind <- cmpfun(ind)


ind2 <- function(x) {
  if (x[1] == x[2] & x[3] == x[4] & x[1] != 0 & x[3] != 0) {
    c(0, 0, 2*x[1], 2*x[3])
  } else if (x[1] == x[2] & x[1] != 0) {
    c(0, 2*x[1], x[3], x[4])
  } else if (x[2] == x[3] & x[2] != 0) {
    c(x[1], 0, 2*x[2], x[4])
  } else if (x[3] == x[4] & x[3] != 0) {
    c(x[1], x[2], 0, 2*x[4])
  } else x
}

ind2 <- cmpfun(ind2)


# Give extra zeros to make length 4.
down <- function(v) c(rep(0, 4 - sum(sign(v))), v[v != 0])

up <- function(v) c(v[v != 0], rep(0, 4 - sum(sign(v))))


move <- function(b, dir) {
  if(dir == 'd') apply(b, 2, down)
  else if (dir == 'u') apply(b, 2, up)
  else if (dir == 'l') t(apply(b, 1, up))
  else t(apply(b, 1, down))
}

cmove <- cmpfun(move)

agg <- function(b, dir) {
  if(valid_move(b, dir) | condense(b, dir)) {
    b <- cmove(b, dir)
    b <- if(dir == 'd') apply(b, 2, ind)
    else if (dir == 'u') apply(b, 2, ind2)
    else if (dir == 'l') t(apply(b, 1, ind))
    else t(apply(b, 1, ind2))
    b <- cmove(b, dir)
    b[sample(which(b == 0), 1)] <- 2
    b
  } else b
}

agg <- cmpfun(agg)


play <- function(b) {
  print_board(b)
  while(not_game_over(b)) {
    n <- scan(nmax = 1, quiet = T, what = character())
    b <- agg(b, n)
    print_board(b)
  }
  print('Game Over')
}




auto_play <- function(b) {
  while(not_game_over(b)) {
    n <- sample(c('l', 'r', 'u', 'd'), 1)
    b <- agg(b, n)
  }
  max(b)
}



auto_play2 <- function(b) {
  while(not_game_over(b)) {
    n <- sample(c('l', 'r', 'u', 'd'), 1, prob = c(.4, .1999, .0001, .4))
    b <- agg(b, n)
  }
  max(b)
}


play(board)



x <- c()
for (i in seq(30)) {
  #if (i %% 100 == 0) 
    print(i)
  x <- c(x, auto_play2(gen_board()))
}


#auto_play(board)

library(profr)

Rprof("2048.out")
x <- c()
#t <- c()
for (i in seq(100)) {
  print(i)
  #t <- c(t, system.time(
  x <- c(x, auto_play(gen_board()))
#[3])
}
Rprof(NULL)
summaryRprof('2048.out')






