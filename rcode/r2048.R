
# Generate an initial game board
gen_board <- function() {
  start <- rep(0, 16)
  inits <- sample(1:16, 2)
  start[inits] <- 2
  matrix(start, 4, 4)
}

# Print the game board to the screen if playing interactively
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

# Is the given move valid
valid_move <- function(b, dir) {
  if (dir == 'd') any(diff(sign(b)) == -1)
  else if (dir == 'u') any(diff(sign(b)) == 1) 
  else if (dir == 'l') any(diff(sign(t(b))) == 1)
  else any(diff(sign(t(b))) == -1)
}

# Determine if two adjacent cells are the same.
two_in_order <- function(x) {
  x[1] == x[2] & x[1] != 0 |
    x[2] == x[3] & x[2] != 0 |
    x[3] == x[4] & x[3] != 0
}

# Collapse cells that are the same in a give direction.
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

# THese functions recreate the board after a move.
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



# Give extra zeros to make length 4.
down <- function(v) c(rep(0, 4 - sum(sign(v))), v[v != 0])

up <- function(v) c(v[v != 0], rep(0, 4 - sum(sign(v))))


# pad(b[, 1][b[, 1] != 0]
move <- function(b, dir) {
  if(dir == 'd') apply(b, 2, down)
  else if (dir == 'u') apply(b, 2, up)
  else if (dir == 'l') t(apply(b, 1, up))
  else t(apply(b, 1, down))
}



agg <- function(b, dir) {
  b <- move(b, dir)
  b <- if(dir == 'd') apply(b, 2, ind)
  else if (dir == 'u') apply(b, 2, ind2)
  else if (dir == 'l') t(apply(b, 1, ind))
  else t(apply(b, 1, ind2))
  b <- move(b, dir)
  set <- which(b == 0)
  set <- if(length(set) == 1) set else sample(set, 1)
  b[set] <- 2
  b
}

selection <- function(b) {
  x <- sapply(c('l', 'r', 'u', 'd'), function(x) valid_move(b, x))
  y <- sapply(c('l', 'r', 'u', 'd'), function(x) condense(b, x))
  x | y
}

# If you really want to play you can use this.
play <- function(b) {
  print_board(b)
  while(not_game_over(b)) {
    n <- scan(nmax = 1, quiet = T, what = character())
    b <- agg(b, n)
    print_board(b)
  }
  print('Game Over')
}



auto_play <- function(p = c(.25, .25, .25, .25)) {
  b <- gen_board()
  j <- 1
  p <- p + .001
  while(not_game_over(b)) {
    sel <- selection(b)
    posMove <- c('l', 'r', 'u', 'd')[sel]
    n <- sample(posMove, 1, prob = (p[sel] / sum(p[sel])))
    b <- agg(b, n)
    j <- j + 1
    b
  }
  data.frame(board_sum = sum(b), max = max(b), moves = j, log_sum = sum(log2(b)), depth = log2(max(b)))
}


#auto_play(c(.25, .25, .25, .25)) 
