# Asuume 5 point form some sort of star
library(dplyr)


rever <- function(string)
{
  # split string by characters
  string_split = strsplit(string, split = "")
  # reverse order
  rev_order = nchar(string):1
  # reversed characters
  reversed_chars = string_split[[1]][rev_order]
  # collapse reversed characters
  paste(reversed_chars, collapse="")
}



remove <- function(a, b) {
  c(intersect(grep(a, sp), grep(b, sp)),
    intersect(grep(rever(a), sp), grep(b, sp)),
    intersect(grep(a, sp), grep(rever(b), sp)),
    intersect(grep(rever(a), sp), grep(rever(b), sp)))
}


######################################
num <- 5
set <- letters[1:num]

paths <- unique(as.data.frame(t(replicate(500000, sample(set)))))

row.names(paths) <- NULL

names(paths) <- paste0('v', 1:num)

sp <- paste0(paths$v1, paths$v2, paths$v3, paths$v4, paths$v5)




takeout <- unique(c(remove('ac', 'bd'),
                    remove('ec', 'bd'),
                    remove('ac', 'eb'),
                    remove('ad', 'eb'),
                    remove('ad', 'ec')))
length(remove('ac', 'bd'))
length(remove('ec', 'bd'))
length(remove('ac', 'eb'))
length(remove('ad', 'eb'))
length(remove('ad', 'ec'))


sp[-takeout]

#

# 6 city case

num <- 6
set <- letters[1:num]

paths <- unique(as.data.frame(t(replicate(500000, sample(set)))))

row.names(paths) <- NULL

names(paths) <- paste0('v', 1:num)

sp <- paste0(paths$v1, paths$v2, paths$v3, paths$v4, paths$v5, paths$v6)


takeout <- unique(c(remove('ac', 'bd'),
                    remove('ec', 'bd'),
                    remove('ac', 'eb'),
                    remove('ad', 'eb'),
                    remove('ad', 'ec')))

takeout <- unique(c(remove('ac', 'bd'),
                    remove('ec', 'bd'),
                    remove('ac', 'eb'),
                    remove('ad', 'eb'),
                    remove('ad', 'ec')))

length(remove('ac', 'bd'))
length(remove('ec', 'bd'))
length(remove('ac', 'eb'))
length(remove('ad', 'eb'))
length(remove('ad', 'ec'))








#

num <- 10

data <- data.frame(city = letters[1:num], x = runif(num, 0, 10), 
                   y = runif(num, 0, 10))



hull <- data.frame(x = numeric(), y = numeric())

hull <- rbind(hull, data[data$x == min(data$x), c('x', 'y')])
hull <- rbind(hull, data[data$x == max(data$x), c('x', 'y')])
hull <- rbind(hull, data[data$y == min(data$y), c('x', 'y')])
hull <- rbind(hull, data[data$y == max(data$y), c('x', 'y')])

plot(data$x, data$y)
points(hull$x, hull$y, col = 'red')

#

# grahm alg
num <- 40

data <- data.frame(city = 1:num, x = runif(num, 0, 10), 
                   y = runif(num, 0, 10))

miny <- data[data$y == min(data$y), ]

if (nrow(miny) > 1) {
  miny <- miny[miny$x == min(miny$x), ]
}

plot(data$x, data$y, xlim = c(0, 10), ylim = c(0, 10))
points(miny$x, miny$y, col = 'red')

data <- data[data$city != miny$city, ]

data <- data[order(data$x), ]

delta_y <- (data$y - miny$y) 
delta_x <- (data$x - miny$x) 

data$angle <- asin(delta_y / sqrt(delta_y ^ 2 + delta_x ^ 2))
data$deg <- data$angle * 180 / pi


data$deg2 <- ifelse(delta_x < 0, 90 - data$deg + 90, data$deg)

data <- data[order(data$deg2), ]



new <- plyr::rbind.fill(miny, data, miny)



plot(new$x, new$y, xlim = c(0, 10), ylim = c(0, 10))
points(new$x[1], new$y[1], col = 'red')

new$hull <- NA
new$hull[1] <- TRUE

f <- 1; s <- 2; t <- 3


cross <- (new$x[s] - new$x[f]) * (new$y[t] - new$y[f]) - 
         (new$y[s] - new$y[f]) * (new$x[t] - new$x[f]) 

if (cross > 0) {
  new$hull[s] <- TRUE
  points(new$x[s], new$y[s], col = 'red')
  f <- s; s <- t; t <- t + 1;
} else {
  new$hull <- FALSE
  points(new$x[s], new$y[s], col = 'blue')
  s <- t; t <- t + 1;
}




#
  
#m <- (miny$y - data$y) / (miny$x - data$x)
#b <- data$y - slope * data$x
#int <- -(b) / m
#


#
