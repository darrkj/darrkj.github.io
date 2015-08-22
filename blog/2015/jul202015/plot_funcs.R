
and <- function(num, ...) sum(table(c(...)) >= num)


x <- draw.triple.venn(
  area1 = length(a), area2 = length(b), area3 = length(c),
  n12 = and(2, a, b), n13 = and(2, a, c),
  n23 = and(2, b, c),
  n123 = and(3, a, b, c),
  category = c("CADE", "Mahalanobis", "Local Outlier Factor"),
  fill = c("dodgerblue", "goldenrod1", "darkorange1"),
  cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
  cat.cex = 2,
  margin = 0.05
)

x <- draw.pairwise.venn(
  area1 = length(a), area2 = length(b), cross.area = and(2, a, b),
  category = c("CADE", "Local Outlier Factor"),
  fill = c("goldenrod1", "darkorange1"),
  cat.col = c("goldenrod1", "darkorange1"),
  cat.cex = 2,
  margin = 0.05
)
a




plot_venn <- function(field) {
  x <- d[d[, 'z'] == field, ]
  
  x <- unique(c(x$x, x$y))
  if (length(x) == 2) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    ab <- length(intersect(a, b))
    draw.pairwise.venn(length(a), length(b), 
                      length(intersect(a, b)), 
                      category = c(x[1], x[2]),
                      fill = c("goldenrod1", "darkorange1"),
                      cat.col = c("goldenrod1", "darkorange1"),
                      cat.cex = 2)
  } else if (length(x) == 3) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    c <- unique(gtfs_data[[x[3]]][, field])
    draw.triple.venn(
      area1 = length(a), area2 = length(b), area3 = length(c),
      n12 = and(2, a, b), n13 = and(2, a, c),
      n23 = and(2, b, c),
      n123 = and(3, a, b, c),
      category = c(x[1], x[2], x[3]),
      fill = c("dodgerblue", "goldenrod1", "darkorange1"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
      cat.cex = 2)
  }
}

plot_venn('agency_id')
plot_venn('service_id')






field <- 'agency_id'
field <- 'service_id'

plot_intersect <- function(field) {
  x <- d[d[, 'z'] == field, ]
  
  x <- unique(c(x$x, x$y))
  if (length(x) == 2) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    ab <- length(intersect(a, b))
    vd <- venneuler(c(A = length(a) - ab, B = length(b) - ab, 
                      "A&B" = length(intersect(a, b))))
  } else if (length(x) == 3) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    c <- unique(gtfs_data[[x[3]]][, field])
    
    tot <- length(union(union(a, b), c))
    ab <- length(intersect(a, b))
    ac <- length(intersect(a, b))
    bc <- length(intersect(a, b))
    vd <- venneuler(c(A = length(a) / tot, B = length(b) / tot, 
                      C = length(c) / tot, 
                      "A&B" = length(intersect(a, b)) / tot, 
                      "A&C" = length(intersect(a, c)) / tot, 
                      "B&C" = length(intersect(b, c)) / tot,
                      "A&B&C" = length(intersect(intersect(a, b), c)) / tot))
  }
  plot(vd) 
}








plot_venn <- function(field) {
  x <- d[d[, 'z'] == field, ]
  
  x <- unique(c(x$x, x$y))
  if (length(x) == 2) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    ab <- length(intersect(a, b))
    draw.pairwise.venn(length(a), length(b), 
                       length(intersect(a, b)), 
                       category = c(x[1], x[2]),
                       fill = c("goldenrod1", "darkorange1"),
                       cat.col = c("goldenrod1", "darkorange1"),
                       cat.cex = 2)
  } else if (length(x) == 3) {
    a <- unique(gtfs_data[[x[1]]][, field])
    b <- unique(gtfs_data[[x[2]]][, field])
    c <- unique(gtfs_data[[x[3]]][, field])
    draw.triple.venn(
      area1 = length(a), area2 = length(b), area3 = length(c),
      n12 = and(2, a, b), n13 = and(2, a, c),
      n23 = and(2, b, c),
      n123 = and(3, a, b, c),
      category = c(x[1], x[2], x[3]),
      fill = c("dodgerblue", "goldenrod1", "darkorange1"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1"),
      cat.cex = 2)
  }
}


